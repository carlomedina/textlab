library(magrittr)
library(dplyr)
library(tidytext)
library(dbscan)

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

content <- data.frame(id = integer(0), 
                      contentId = character(0), 
                      first = integer(0), 
                      last = integer(0))

for (i in 1:50) {
  mat <- na.omit(cbind(1:length(countNgrams$count[countNgrams$id == i]), 
                       ifelse(countNgrams$count[countNgrams$id == i] >= 3,
                              countNgrams$count[countNgrams$id == i],
                              NA)))
  db <- dbscan(mat, 50, 25)
  group <- data.frame(x = mat[,1],
                     y = mat[,2],
                     cluster = db$cluster)
  content <- group %>%
    group_by(cluster) %>%
    summarise(id = i,
              contentId = paste(i, first(cluster)),
              first = min(x),
              last = max(x)) %>%
    select(id, contentId, first, last) %>%
    rbind(content, .)
}

