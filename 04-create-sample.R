# this file uses a sample processed data from the script that creates 6-grams 
# from the XML file of the master csv table

# processed file: processed_6gram__AHCA_LocalNews_092617_101017_processed_
# for this demo purposes: only 101017 transcripts will be analyzed.
load("./data/processed_6gram__AHCA_LocalNews_092617_101017_processed_.RData")
# return ids of 101017 news
ids <- lapply(processed_6gram__AHCA_LocalNews_092617_101017_processed_, function(x) {
  return(x$id[1])
}) %>% unlist()

# return the relevant list of data frames
sub <- processed_6gram__AHCA_LocalNews_092617_101017_processed_[(1:3970)[grepl("10-10-2017", ids)]]

# the following script is from 03-segment-extraction.R
library(magrittr)
library(dplyr)
library(tidytext)
library(dbscan)
library(tm)

# count the number of times a particular ngram appears
# the object ngrams is from the load() statement above

countNgrams <- sub %>%
  do.call('rbind', .) %>%
  group_by(id) %>%
  mutate(index = 1:n()) %>%
  group_by(ngram) %>%
  mutate(count = n()) %>%
  ungroup

# massage the data a little to conform to the previous code
# ids need to be numeric that goes from 1 to the length(unique(segment id))
countNgrams$segmentId <- countNgrams$id
countNgrams$id <- as.factor(countNgrams$id) %>% as.numeric


# get segments where the ngrams of that segment was found in 5 or more other texts
# also, and segment only if it contains more than 10 ngrams
# the parameters/threshold here are only for exploration (not necessarily the optimal values)
content <- data.frame(id = integer(0), 
                      contentId = character(0), 
                      first = integer(0), 
                      last = integer(0),
                      start_line = integer(0),
                      end_line = integer(0),
                      start_timestamp = integer(0),
                      end_timestamp = integer(0),
                      station_id = integer(0))
for (i in 1:100) {
  mat <- na.omit(cbind(1:length(countNgrams$count[countNgrams$id == i]), 
                       ifelse(countNgrams$count[countNgrams$id == i] >= 3,
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
  group <- cbind(countNgrams[countNgrams$id == i,][group$x,], group)
  content <- group %>%
    filter(cluster != 0) %>%
    group_by(cluster) %>%
    summarise(id = i,
              contentId = paste(i, first(cluster)),
              first = min(x),
              last = max(x),
              start_line = linenumber[which(x==min(x))],
              end_line = linenumber[which(x==max(x))],
              start_timestamp = timestamp[which(x==min(x))],
              end_timestamp = timestamp[which(x==max(x))],
              station_id = first(segmentId),
              count = n()) %>%
    filter(count > 10) %>%   # only keep cluster if there are more than 10 words
    select(id, contentId, first, last, start_line, end_line, start_timestamp, end_timestamp, station_id) %>%
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
    if (i==j) {
      similarityScores[i,j] <- NA
      next
    }
    text.x <- content$words[[i]]
    text.y <- content$words[[j]]
    similarityScores[i,j] <- segmentSimilarity(text.x, text.y)
  }
}

# create threshold
ggg <- similarityScores > 0.8

library(igraph)
g1 <- graph_from_adjacency_matrix(ggg, mode = "directed")
plot(g1, mode = "strong", arrow.size=0.1, layout=layout_on_grid(g1))
clusters(g1, mode = "strong")
content$cluster <- clusters(graph_from_adjacency_matrix(ggg, mode = "directed"), mode = "strong")$membership

segments <- content[order(content$cluster), c("id", "contentId", "text", "cluster", "start_line", 
                                              "end_line", "start_timestamp", "end_timestamp", "station_id")]

library(stringr)
#### get the text sample with the middle length per each segment cluster
segments$segmentLength <- str_extract_all(segments$text, "\\s") %>% lapply(FUN = length) %>% unlist

uniqueSegments <- segments %>%
  group_by(cluster) %>%
  arrange(desc(segmentLength)) %>%
  summarise(sampleText = text[ceiling(length(text)/2)])

write.csv(segments, file="./data/segments/092617_extracted-02-16.csv", row.names = F)
write.csv(uniqueSegments, file="./data/segments/092617_extracted-02-16_unique.csv", row.names = F)


# CREATING THE JSON FILE
library(jsonlite)
library(readr)
json <- list()
for (i in 1:nrow(uniqueSegments)) {
  segment_list <- split(segments[segments$cluster == i, ] , seq(nrow(segments[segments$cluster == i, ]))) %>%
    lapply(function(x) {return(x %>% as.list())}) %>% 
    unname() %>% 
    list() # cover with a "master list"
  json[[i]] <- c("cluster_message"=uniqueSegments$sampleText[uniqueSegments$cluster == i], "stations"=segment_list)
}
json <- list("date"="10/10/2017", "clusters"=json)

json %>% jsonlite::toJSON(pretty = T, auto_unbox = T) %>% write_lines('./data/segments/092617_extracted-02-16.json')



## TRYING TO GET THE VIDEO ELEMENTS
getVideoQuery <- function(station_id, start_timestamp) {
  timestamps <- lubridate::mdy_hms(segments$start_timestamp)
  station_id %>%
    stringr::str_extract('[A-Z]*') %>%
    paste(month(timestamps), day(timestamps), year(timestamps) %>% substr(3,4), sep = "") %>%
    {paste('FileName LIKE "%', ., '%" OR ', sep = "")} %>%
    unique() %>%
    paste(collapse = " ")
}
write_lines(getVideoQuery(segments$station_id, segments$start_timestamp), "test.txt")

# I queried the entries of the file below using: 
# select FileName from video where FileName LIKE "%WALA101017%" OR  FileName LIKE "%KYTX101017%" OR  FileName LIKE "%KXMC101017%" OR  FileName LIKE "%KVOA101017%" OR  FileName LIKE "%KTWO101017%" OR  FileName LIKE "%NECN101017%" OR  FileName LIKE "%KTVODT101017%" OR  FileName LIKE "%KSWB101017%" OR  FileName LIKE "%KSTU101017%" OR  FileName LIKE "%KOKI101017%" OR  FileName LIKE "%KRGV101017%" OR  FileName LIKE "%KTVT101017%" OR  FileName LIKE "%KULR101017%" OR  FileName LIKE "%KMTV101017%" OR  FileName LIKE "%KHQA101017%" OR  FileName LIKE "%CFNEWS101017%" OR  FileName LIKE "%KQTV101017%" OR  FileName LIKE "%KFXO101017%" OR  FileName LIKE "%WAGA101017%" OR  FileName LIKE "%CLTV101017%" OR  FileName LIKE "%KWWL101017%" OR  FileName LIKE "%KOSA101017%" OR  FileName LIKE "%KTNV101017%" OR  FileName LIKE "%KGAN101017%" OR  FileName LIKE "%KGUN101017%" OR  FileName LIKE "%KMGH101017%" OR  FileName LIKE "%KFXK101017%" OR  FileName LIKE "%KGTV101017%" OR  FileName LIKE "%KNIN101017%" OR  FileName LIKE "%KREM101017%" OR  FileName LIKE "%KREX101017%" OR  FileName LIKE "%KSHB101017%" OR  FileName LIKE "%KTBC101017%" OR  FileName LIKE "%KTTV101017%" OR  FileName LIKE "%KTVI101017%" OR  FileName LIKE "%KVHP101017%" OR  FileName LIKE "%KVRR101017%" OR  FileName LIKE "%KXRM101017%" OR  FileName LIKE "%KYOU101017%" OR  FileName LIKE "%WBRC101017%”
videos <- read.csv("./data/segments/101017_videos.csv")


findClosestTimeStamp <- function(station_date, timestamp, list_videos){
  
}



# #######
# # SANDBOX
# #######
# 
# 
# ########
# ## sample dbscan
# 
# testindex = 1
# mat <- na.omit(cbind(1:length(countNgrams$count[countNgrams$id == testindex]),
#                      ifelse(countNgrams$count[countNgrams$id == testindex] >= 3,
#                             countNgrams$count[countNgrams$id == testindex],
#                             NA)))
# db <- dbscan(mat, 30, 20)
# group <- data.frame(x = mat[,1],
#                     y = mat[,2],
#                     cluster = db$cluster)
# group
# plot(group$x, group$y, col=factor(db$cluster))
#      
# 


