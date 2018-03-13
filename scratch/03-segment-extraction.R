library(magrittr)
library(dplyr)
library(tidytext)
library(dbscan)
library(tm)


load('./data/environment-10-17.2017.RData')

# count the number of times a particular ngram appears
# the object ngrams is from the load() statement above

countNgrams <- ngrams %>%
  group_by(id) %>%
  mutate(index = 1:n()) %>%
  group_by(ngram) %>%
  mutate(count = n()) %>%
  ungroup
       

# get segments where the ngrams of that segment was found in 5 or more other texts
# also, and segment only if it contains more than 10 ngrams
# the parameters/threshold here are only for exploration (not necessarily the optimal values)
content <- data.frame(id = integer(0), 
                      contentId = character(0), 
                      first = integer(0), 
                      last = integer(0))
for (i in 1:50) {
  mat <- na.omit(cbind(1:length(countNgrams$count[countNgrams$id == i]), 
                       ifelse(countNgrams$count[countNgrams$id == i] >= 5,
                              countNgrams$count[countNgrams$id == i],
                              NA)))
  if (nrow(mat) == 0) {
    next
  }
  
  # perform dbscan to get clusters of content =>. segments
  db <- dbscan(mat, 30, 20)
  group <- data.frame(x = mat[,1],
                     y = mat[,2],
                     cluster = db$cluster)
  content <- group %>%
    filter(cluster != 0) %>%
    group_by(cluster) %>%
    summarise(id = i,
              contentId = paste(i, first(cluster)),
              first = min(x),
              last = max(x),
              count = n()) %>%
    filter(count > 10) %>%
    select(id, contentId, first, last) %>%
    rbind(content, .)
}


############
##  ADDING THE TEXTS TO THE SEGMENTS
############


# get ngrams specified by the index and transform the ngrams into text
getNgramsToText <- function(ngramdata, id, first, last) {
  # list of ngrams
  ngram.list <- ngramdata %>%
    filter(id == id) %$%
    ngram[first:last]
  
  # take first word  of each ngrams and paste them together 
  #  and the entire string of the last ngram
  ngram.list[-length(ngram.list)] %>%
    lapply(FUN = function(x) {
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


cleanText <- lapply(textToClean, FUN = function(x) {
  x
}) %>% unlist
content$cleanText <- cleanText


content$words <- ""
# sort non-stopwords by alphabetically and only get unique
content$words <- cleanText %>% 
  lapply(FUN = function(x) {
    x %>% 
      strsplit(' ') %>%
      unlist() %>%
      unique %>%
      sort()
  })


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

write.csv(segments, file="./data/segments-extracted-10-26.csv", row.names = F)
write.csv(uniqueSegments, file="./data/unique-segments-extracted-10-26.csv", row.names = F)








# #######
# # SANDBOX
# #######
# 
# 
# ########
# ## sample dbscan
# 
# testindex = 5
# mat <- na.omit(cbind(1:length(countNgrams$count[countNgrams$id == testindex]), 
#                      ifelse(countNgrams$count[countNgrams$id == testindex] >= 5,
#                             countNgrams$count[countNgrams$id == testindex],
#                             NA)))
# db <- dbscan(mat, 40, 20)
# group <- data.frame(x = mat[,1],
#                     y = mat[,2],
#                     cluster = db$cluster)
# group 
# plot(group$x, group$y, col=factor(db$cluster))
#      
# 


