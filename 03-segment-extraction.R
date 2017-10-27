library(magrittr)
library(dplyr)
library(tidytext)
library(dbscan)
library(tm)


load('./data/environment-10-17.2017.RData')

# count the number of times a particular ngram appears
countNgrams <- ngrams %>%
  group_by(id) %>%
  mutate(index = 1:n()) %>%
  group_by(ngram) %>%
  mutate(count = n()) %>%
  arrange(count,
            docId = first(sort(id)),
            indexOnDocIx = first(index[sort(id)])) %>%
  arrange(desc(count))


# get segments where the ngrams of that segment was found in 3 or more other texts
# also, and segment only if it contains more than 20 ngrams
content <- data.frame(id = integer(0), 
                      contentId = character(0), 
                      first = integer(0), 
                      last = integer(0))
for (i in 1:50) {
  mat <- na.omit(cbind(1:length(countNgrams$count[countNgrams$id == i]), 
                       ifelse(countNgrams$count[countNgrams$id == i] >= 2,
                              countNgrams$count[countNgrams$id == i],
                              NA)))
  db <- dbscan(mat, 30, 15)
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
##  ADDING THE CONTENTS
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

# sort text by word order and only get unique
content$words <- cleanText %>% 
  lapply(FUN = function(x) {
    x %>% 
      strsplit(' ') %>%
      unlist() %>%
      unique %>%
      sort()
  })


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
ggg <- similarityScores > 0.5

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

write.csv(uniqueSegments, file="./data/segments-extracted-10-26.csv")

########
testindex = 1
mat <- na.omit(cbind(1:length(countNgrams$count[countNgrams$id == testindex]), 
                     ifelse(countNgrams$count[countNgrams$id == testindex] >= 2,
                            countNgrams$count[countNgrams$id == testindex],
                            NA)))
db <- dbscan(mat, 30, 25)
group <- data.frame(x = mat[,1],
                    y = mat[,2],
                    cluster = db$cluster)
group 
plot(group$x, group$y, col=factor(db$cluster))
     
     

