# this file uses a sample processed data from the script that creates 6-grams 
# from the XML file of the master csv table
library(magrittr)
library(dplyr)
library(tidytext)
library(dbscan)
library(tm)



# processed file: processed_6gram__AHCA_LocalNews_092617_101017_processed_
# for this demo purposes: only 101017 transcripts will be analyzed.
load("./data/processed_6gram__AHCA_LocalNews_072517_080917_processed_.RData")
# return ids of 101017 news
ids <- lapply(processed_6gram__AHCA_LocalNews_072517_080917_processed_, function(x) {
  return(x$id[1])
}) %>% 
  unlist() %>%
  unique()
relevant_ids <- ids[grepl("07-25-2017", ids)] %>% unique()

# return the relevant list of data frames
# sub <- processed_6gram__AHCA_LocalNews_072517_080917_processed_[(1:3970)[grepl("07-25-2017", ids)]]

ngram <-  processed_6gram__AHCA_LocalNews_072517_080917_processed_ %>%
  do.call('rbind', .) %>%
  filter(id %in% relevant_ids)

# the following script is from 03-segment-extraction.R


# count the number of times a particular ngram appears
# the object ngrams is from the load() statement above

countNgrams <- ngram %>%
  group_by(id) %>%
  mutate(index = 1:n()) %>%
  group_by(ngram) %>%
  mutate(count = n()) %>%
  ungroup

# massage the data a little to conform to the previous code
# ids need to be numeric that goes from 1 to the length(unique(segment id))
countNgrams$segmentId <- countNgrams$id
countNgrams$num_id <- as.factor(countNgrams$id) %>% as.numeric


# get segments where the ngrams of that segment was found in 5 or more other texts
# also, and segment only if it contains more than 10 ngrams
# the parameters/threshold here are only for exploration (not necessarily the optimal values)
content <- data.frame(num_id = integer(0), 
                      contentId = character(0), 
                      first = integer(0), 
                      last = integer(0),
                      start_line = integer(0),
                      end_line = integer(0),
                      start_timestamp = integer(0),
                      end_timestamp = integer(0),
                      station_id = integer(0))
pdf("./data/segments/plot.pdf")
for (i in 1:100) {
  mat <- na.omit(cbind(1:length(countNgrams$count[countNgrams$num_id == i]), 
                       ifelse(countNgrams$count[countNgrams$num_id == i] >= 5,
                              countNgrams$count[countNgrams$num_id == i],
                              NA)))
  if (nrow(mat) == 0) {
    next
  }
  
  # perform dbscan to get clusters of content =>. segments
  db <- dbscan(mat, 30, 20)
  group <- data.frame(x = mat[,1],
                      y = mat[,2],
                      cluster = db$cluster) %>%
    filter(cluster!=0)
  group <- cbind(countNgrams[countNgrams$num_id == i,][group$x,], group)
  # plot(group$x, group$y, col=factor(db$cluster))
  
  
  content <- group %>%
    filter(cluster != 0) %>%
    group_by(cluster) %>%
    summarise(num_id = i,
              contentId = paste(i, first(cluster)),
              first = min(x),
              last = max(x),
              start_line = linenumber[which(x==min(x))],
              end_line = linenumber[which(x==max(x))],
              start_timestamp = timestamp[which(x==min(x))],
              end_timestamp = timestamp[which(x==max(x))],
              station_id = first(id),
              count = n()) %>%
    filter(count > 10) %>%   # only keep cluster if there are more than 10 words
    select(num_id, contentId, first, last, start_line, end_line, start_timestamp, end_timestamp, station_id) %>%
    rbind(content, .)
  
  print(i)
}
   dev.off()

############
##  ADDING THE TEXTS TO THE SEGMENTS
############


# get ngrams specified by the index and transform the ngrams into text
getNgramsToText <- function(ngramdata, id_to_search, first, last) {
  # list of ngrams
  ngram.list <- ngramdata %>%
    dplyr::filter(id == id_to_search) %$%
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


# get ngrams specified by the index and transform the ngrams into text
getNgramsToVector <- function(ngramdata, id_to_search, first, last) {
  # list of ngrams
  ngramdata %>%
    dplyr::filter(id == id_to_search) %$%
    ngram[first:last]
}

getNgramsToVector(ngram, "BN9--07-25-2017-16-12-00", 634, 750)

# get text and remove stop words from the content
content$text <- ""
for (i in 1:nrow(content)) {
  content$text[i] <- getNgramsToText(ngram, content$station_id[i], content$first[i], content$last[i])
}
content_list <- list()
for (i in 1:nrow(content)) {
  content_list[[i]] <- getNgramsToVector(ngram, content$station_id[i], content$first[i], content$last[i])
  print(i)
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




## INTRODUCE NEW SEGMENT SIMILARITY USING N-GRAM!!!



segmentSimilarityNgram<- function(text.x, text.y) {
  # split.x <- str_split(text.x, " ") %>% unlist
  # split.y <- str_split(text.y, " ") %>% unlist
  # ngram.x <- lapply(1:(length(split.x)-2), function(x) {
  #   split.x[x:(x+2)] %>% paste(collapse=" ")
  # }) %>% unlist
  # ngram.y <- lapply(1:(length(split.y)-2), function(y) {
  #   split.y[y:(y+2)]  %>% paste(collapse=" ")
  # }) %>% unlist
  sizeIntersection <- length(intersect(text.x, text.y))
  similarity <- sizeIntersection/length(text.x)
  return(similarity) 
}


similarityScoresNgram <- matrix(nrow = nrow(content), ncol = nrow(content))
for (i in 1:nrow(content)) {
  for (j in 1:nrow(content)) {
    if (i==j) {
      similarityScoresNgram[i,j] <- NA
      next
    }
    text.x <- content_list[[i]]
    text.y <- content_list[[j]]
    similarityScoresNgram[i,j] <- segmentSimilarityNgram(text.x, text.y)
  }
  print(paste(i, j))
}




# create threshold
ggg <- similarityScoresNgram > 0.4

library(igraph)
g1 <- graph_from_adjacency_matrix(ggg, mode = "directed")
groups <- cluster_walktrap(g1, weights = E(g1)$weights)
communities(groups)
cluster <- clusters(g1, mode = "weak")$membership
content$cluster <- cluster
plot(g1, mode = "strong", edge.arrow.size=0.1,  layout=layout.fruchterman.reingold(g1), vertex.size=5, vertex.color=cluster)


#### TRYING TO GET THE VIDEO ELEMENTS ####
# I queried the entries of the file below using: 
# select FileName from video where FileName LIKE "%WALA101017%" OR  FileName LIKE "%KYTX101017%" OR  FileName LIKE "%KXMC101017%" OR  FileName LIKE "%KVOA101017%" OR  FileName LIKE "%KTWO101017%" OR  FileName LIKE "%NECN101017%" OR  FileName LIKE "%KTVODT101017%" OR  FileName LIKE "%KSWB101017%" OR  FileName LIKE "%KSTU101017%" OR  FileName LIKE "%KOKI101017%" OR  FileName LIKE "%KRGV101017%" OR  FileName LIKE "%KTVT101017%" OR  FileName LIKE "%KULR101017%" OR  FileName LIKE "%KMTV101017%" OR  FileName LIKE "%KHQA101017%" OR  FileName LIKE "%CFNEWS101017%" OR  FileName LIKE "%KQTV101017%" OR  FileName LIKE "%KFXO101017%" OR  FileName LIKE "%WAGA101017%" OR  FileName LIKE "%CLTV101017%" OR  FileName LIKE "%KWWL101017%" OR  FileName LIKE "%KOSA101017%" OR  FileName LIKE "%KTNV101017%" OR  FileName LIKE "%KGAN101017%" OR  FileName LIKE "%KGUN101017%" OR  FileName LIKE "%KMGH101017%" OR  FileName LIKE "%KFXK101017%" OR  FileName LIKE "%KGTV101017%" OR  FileName LIKE "%KNIN101017%" OR  FileName LIKE "%KREM101017%" OR  FileName LIKE "%KREX101017%" OR  FileName LIKE "%KSHB101017%" OR  FileName LIKE "%KTBC101017%" OR  FileName LIKE "%KTTV101017%" OR  FileName LIKE "%KTVI101017%" OR  FileName LIKE "%KVHP101017%" OR  FileName LIKE "%KVRR101017%" OR  FileName LIKE "%KXRM101017%" OR  FileName LIKE "%KYOU101017%" OR  FileName LIKE "%WBRC101017%”
videos <- read_csv("./data/segments/072517_videos.csv")
names(videos) <- c("filename", "dirpath", "starttime", "endtime", "segmentid")
videos <- videos %>%
  mutate(starttime = lubridate::mdy_hms(starttime),
         endtime = lubridate::mdy_hms(endtime))

findClosestTimeStamp <- function(station_date_trans, timestamp_trans, video_df){
  
  clean_station_date_trans = station_date_trans %>% 
    strsplit(split = "-{1,2}") %>%
    unlist %>%
    {paste(paste(.[1:3], collapse=""), .[4] %>% substr(3,4), sep="")}
  
  file <- video_df %>%
    mutate(station_name = str_replace(filename %>% str_trim("both"), "[0-9]{6}[.]mp4$", "")) %>%
    filter(station_name == clean_station_date_trans) %>%
    mutate(search = as.POSIXct(timestamp_trans, format="%m/%d/%Y %H:%M:%S", tz="UTC"),
           diff =  difftime(search, starttime)) %>%
    select(filename, diff, starttime, search) %>%
    filter(diff > 0) %>%
    arrange(diff) %>%
    {paste(.[1,1], sep="")} %>%
    str_trim("both")
  return(ifelse(file == "NANA.mp4", "404", file))
}


## test
# findClosestTimeStamp(segments$station_id[50], segments$start_timestamp[50], videos)


#### PREPARING THE SEGMENTS FOR EXPORT ####

segments <- content[order(content$cluster), c("num_id", "contentId", "text", "cluster", "start_line", 
                                              "end_line", "start_timestamp", "end_timestamp", "station_id")]
segments$url <- ""
for (i in 1:nrow(segments)) {
  segments$url[i] <- findClosestTimeStamp(segments$station_id[i], segments$start_timestamp[i], videos)
}

#### GET VIDEO FILES TO TRANSFER TO TMPVIDEO ####
getVideoQuery <- function(station_id, start_timestamp) {
  timestamps <- lubridate::mdy_hms(segments$start_timestamp)
  station_id %>%
    stringr::str_extract('.*?(?=-)') %>%
    paste(lubridate::month(timestamps) %>%
            str_pad(2, "left", "0"), 
          lubridate::day(timestamps) %>%
            str_pad(2, "left", "0"), lubridate::year(timestamps) %>% substr(3,4), sep = "") %>%
            {paste('FileName LIKE "%', ., '%" OR ', sep = "")} %>%
    unique() %>%
    paste(collapse = " ")
}
write_lines(getVideoQuery(segments$station_id, segments$start_timestamp), "test.txt")

segments$url %>% unique() %>% paste(collapse=",") %>% write(file="copycommand.txt")



library(stringr)
#### get the text sample with the middle length per each segment cluster
segments$segmentLength <- str_extract_all(segments$text, "\\s") %>% lapply(FUN = length) %>% unlist

uniqueSegments <- segments %>%
  group_by(cluster) %>%
  arrange(desc(segmentLength)) %>%
  summarise(sampleText = text[ceiling(length(text)/2)],
            num_transcripts=n()) %>%
  arrange(desc(num_transcripts))

write.csv(segments, file="./data/segments/07-25-2017_extracted-03-01.csv", row.names = F)
write.csv(uniqueSegments, file="./data/segments/07-25-2017_extracted-03-01_unique.csv", row.names = F)




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
json <- list("date"="07-25-2017", "clusters"=json)

json %>% jsonlite::toJSON(pretty = T, auto_unbox = T) %>% write_lines('./data/segments/07-25-2017_extracted-03-01-18.json')

# #### RE-DOING GROUPS ####
# # find neighbors of vertex
# # find the intersection
# # get the text of the intersection
# # expand the window of the intersection
# # remove the vertex from the for loop
# 
# visited_neighbors <- c()
# for (i in 1:nrow(content)) {
#   if (i %in% visited_neighbors) {
#     next
#   } 
#   neighbors <- neighbors(g1, i, "out") %>% as.vector()
#   neighbors_ngram <- lapply(neighbors, function(x) {
#     content_list[[x]]
#   })
#   Reduce("intersect", neighbors_ngram)
#   sampleText <- content[i,"text"] %>% 
#     tidytext::unnest_tokens(ngram, text, token="ngrams", n=6) %>%
#     select(ngram)
#   
#   countNgrams %>%
#     filter(ngram %in% sampleText$ngram) %$% segmentId %>% unique
# }
# 








# #######
# # SANDBOX
# #######
# 
# 
# ########
# ## sample dbscan
# 
# testindex = 2
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


