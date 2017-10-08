
install.packages("quanteda")
install.packages("tidytext")

library(magrittr)
library(tm)
library(quanteda)
library(tidytext)

# english dict
dict <- names(data_int_syllables)

# read file and remove particular features:
# annotations - [toothbrush sound], reporter:, etc.
setwd("./data")
text <- read.csv("text_sim.csv", stringsAsFactors = F)
text$text <- gsub("\\[.*?\\]", "", text$text)
text$text <- gsub("[a-zA-Z0-9]*:", "", text$text, perl=T)

# use tm for further cleaning
data <- Corpus(DataframeSource(text))
data <- tm_map(data, tolower)
data <- tm_map(data, removePunctuation)
data <- tm_map(data, removeNumbers)

# bring back to df format
dataframe <- data.frame(text=unlist(sapply(data, `[`, "content")), 
                        stringsAsFactors=F)
df <- do.call("rbind", get("content", data))

# basic cleaning of non-english words
cleanText <- df[1:10,2] %>%
  strsplit(" ") %>%
  lapply(FUN = function (wordvector) {
    Filter(function (word) word %in% dict, wordvector) %>%
      paste()
  })
 

# sample
a <- cleanText %>% 
  lapply(FUN = function(x) paste(x, collapse = " ")) %>% 
  unlist %>%
  data.frame(id = 1:10, text = ., stringsAsFactors = F)


ngrams <- a %>% 
  unnest_tokens(ngram, text, token="ngrams", n=6)



countIntersect <- function(ngram1, ngram2) {
  return(intersect(ngram1, ngram2) %>% length)
}

lapply(1:10, FUN = function(x) lapply(1:10, FUN = function(y) countIntersect(ngrams$ngram[ngrams$id == x],ngrams$ngram[ngrams$id == y]))) %>%
  unlist %>%
  matrix(ncol = 10, nrow = 10)
  
