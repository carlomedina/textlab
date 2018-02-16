# read arguments from command line
args <- commandArgs(trailingOnly=T)

# file to process
infile <- args[1]

# location of output file
outpath <- args[2]

# check if file exists and path exists
if (!file.exists(infile)) {
  stop('[ERROR]: File does not exists \n')
}

if (!file.exists(outpath)) {
  cat('[WARNING]: Path for output file does not exists \n')
  cat('[WARNING]: Creating path \n')
  dir.create(outpath, showWarnings=F)
  if (file.exists(outpath)) {
    cat('[INFO]: The location of the path to output file will be in: \n')
    cat(paste0('[INFO]: ', normalizePath(dirname(outpath)), '/', outpath, '\n'))
  }
}


cat('[INFO]: Starting the segment extraction process')

t <- proc.time()
# load the libraries
library(magrittr)
library(dplyr)
library(tidytext)
library(dbscan)
library(tm)
library(foreach)
library(doParallel)
library(parallel)
library(igraph)

# load the list of data frames containing ngrams
ngrams.list <- readRDS(infile)

ngrams <- do.call('rbind', ngrams.list)

# count the number of times a particular ngram appears
countNgrams <- ngrams %>%
  group_by(id) %>%
  mutate(index = 1:n()) %>%
  group_by(ngram) %>%
  mutate(count = n()) %>%
  ungroup
       
# set-up parallel process here
cores <- detectCores()
cl <- makeCluster(cores[1]-4)
registerDoParallel(cl)
	
# get segments where the ngrams of that segment was found in 5 or more other texts
# also, and segment only if it contains more than 10 ngrams
# the parameters/threshold here are only for exploration (not necessarily the optimal values)

#content <- data.frame(id = integer(0), 
#                      contentId = character(0), 
#                      first = integer(0), 
#                      last = integer(0))
content <-foreach(i= unique(ngrams$id), .combine=rbind.data.frame) %dopar% {
  library(magrittr)
  library(dbscan)
  library(dplyr)
  cat(paste0('[INFO]: Processing ', i))
  mat <- na.omit(cbind(1:length(countNgrams$count[countNgrams$id == i]), 
                       ifelse(countNgrams$count[countNgrams$id == i] >= 5,
                              countNgrams$count[countNgrams$id == i],
                              NA)))
  if (nrow(mat) == 0) {
    return(NULL)
  }
  
  # perform dbscan to get clusters of content =>. segments
  db <- dbscan::dbscan(mat, 30, 20)
  group <- data.frame(x = mat[,1],
                     y = mat[,2],
                     cluster = db$cluster)
  group %>%
    filter(cluster != 0) %>%
    group_by(cluster) %>%
    summarise(id = i,
              contentId = paste(i, first(cluster)),
              first = min(x),
              last = max(x),
              count = n()) %>%
    filter(count > 10) %>%
    select(id, contentId, first, last)
}

stopCluster(cl)

cat(paste0('[INFO]: Finished processing the segments for each transcript \n'))

############
##  ADDING THE TEXTS TO THE SEGMENTS
############

cat(paste0('[INFO]: Getting the segment contents \n'))
# get ngrams specified by the index and transform the ngrams into text
getNgramsToText <- function(ngramdata, id, first, last) {
  # list of ngrams
  ngram.list <- ngramdata %>%
    filter(id == id) %$%
    ngram[first:last]
  
  # take first word  of each ngrams and paste them together 
  #  and the entire string of the last ngram
  ngram.list[-length(ngram.list)] %>%
    mclapply(mc.cores=8, FUN = function(x) {
      strsplit(x, ' ') %>%
        unlist() %>%
        .[1]
    }) %>%
    paste(collapse = " ") %>%
    paste(ngram.list[length(ngram.list)])
}


# remove stop words from the content
content$text <- ""
for (i in 1:nrow(content)) {
  content$text[i] <- getNgramsToText(ngrams, content$id[i], content$first[i], content$last[i])
}

textToClean <- Corpus(VectorSource(content$text)) %>%
  tm_map(removeWords, stopwords('english'))


cleanText <- mclapply(textToClean, mc.cores=8, FUN = function(x) {
  x
}) %>% unlist
content$cleanText <- cleanText


content$words <- ""
# sort non-stopwords by alphabetically and only get unique
content$words <- cleanText %>% 
  mclapply(mc.cores=8, FUN = function(x) {
    x %>% 
      strsplit(' ') %>%
      unlist() %>%
      unique %>%
      sort()
  })


cat(paste0('[INFO]: Calculating similarity scores \n'))
# segments similarity is based on the relative frequency of the words used between them
# segment similarity (intersect(x,y)/length(x)) : NOTE THAT THIS MEASURE IS DIRECTED
segmentSimilarity <- function(text.x, text.y) {
  sizeIntersection <- length(intersect(text.x, text.y))
  similarity <- sizeIntersection/length(text.x)
  return(similarity) 
}

similarityScores <- matrix(nrow = nrow(content), ncol = nrow(content))
for (i in 1:nrow(content)) {
  for (j in 1:nrow(content)) {
    text.x <- content$words[[i]]
    text.y <- content$words[[j]]
    similarityScores[i,j] <- segmentSimilarity(text.x, text.y)
    
  }
}

# create threshold: similarityScore > 0.8 between two pairs mean that the segments contain the same content 
ggg <- similarityScores > 0.8

library(igraph)
clusters(graph_from_adjacency_matrix(ggg, mode = "directed"), mode = "strong")
content$cluster <- clusters(graph_from_adjacency_matrix(ggg, mode = "directed"), mode = "strong")$membership

segments <- content[order(content$cluster), c("id", "contentId", "text", "cluster")]

library(stringr)
#### get the text sample with the middle length per each segment cluster
segments$segmentLength <- str_extract_all(segments$text, "\\s") %>% lapply(FUN = length) %>% unlist

uniqueSegments <- segments %>%
  group_by(cluster) %>%
  arrange(desc(segmentLength)) %>%
  summarise(sampleText = text[ceiling(length(text)/2)])


# create outfile names
outfile <- gsub('.*?[/]|[.]RDS', '', infile, perl=T)

write.csv(segments, file=paste0(outpath, '/segments_', outfile, ".csv"), row.names = F)
write.csv(uniqueSegments, file=paste0(outpath, '/segments_', outfile, ".csv"), row.names = F)

print(proc.time()-t)



