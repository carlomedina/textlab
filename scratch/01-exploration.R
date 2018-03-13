
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
text <- read.csv("./data/text_sim.csv", stringsAsFactors = F)
text$text <- gsub("\\[.*?\\]", "", text$text)
text$text <- gsub("[a-zA-Z0-9]*:", "", text$text, perl=T)

# use tm for further cleaning
data <- Corpus(DataframeSource(text))
data <- tm_map(data, tolower)
data <- tm_map(data, removePunctuation)
data <- tm_map(data, removeNumbers)

# bring back to df format
# dataframe <- data.frame(text=unlist(sapply(data, `[`, "content")), 
#                         stringsAsFactors=F)
df <- do.call("rbind", get("content", data))

# basic cleaning of non-english words
cleanText <- df[1:199,2] %>%
  strsplit(" ") %>%
  lapply(FUN = function (wordvector) {
    Filter(function (word) word %in% dict, wordvector) %>%
      paste()
  })
 

# sample
a <- cleanText %>% 
  lapply(FUN = function(x) paste(x, collapse = " ")) %>% 
  unlist %>%
  data.frame(id = 1:199, text = ., stringsAsFactors = F)


ngrams <- a %>% 
  unnest_tokens(ngram, text, token="ngrams", n=6)



countIntersect <- function(ngram1, ngram2) {
  return(intersect(ngram1, ngram2) %>% length)
}

confmat <- lapply(1:50, FUN = function(x) lapply(1:50, FUN = function(y) countIntersect(ngrams$ngram[ngrams$id == x],ngrams$ngram[ngrams$id == y]))) %>%
  unlist %>%
  matrix(ncol = 50, nrow = 50)

pdf("test.pdf")
for (i in 1:25) {
  index.match <- which(ngrams$ngram[ngrams$id ==i] %in% ngrams$ngram[ngrams$id ==1])
  graph <- data.frame(index = 1:length(ngrams$ngram[ngrams$id == i]),
                      bin = rep(0, length(ngrams$ngram[ngrams$id ==i])))
  graph$bin[index.match] <- 1
  
  plot(bin~index, data = graph, type='l')
}
dev.off()



matcher <- function(sourcengram, targetngram) {
  lapply(sourcengram, FUN = function(ngram) {
    which(ngram == targetngram)[1]
  }) %>% unlist
}

# match source and target ngrams side-by-side
index.match <- matcher(ngrams$ngram[ngrams$id ==4], ngrams$ngram[ngrams$id ==16])

match.df.6gram <- data.frame(source = ngrams$ngram[ngrams$id ==4], 
                             target = lapply(index.match, 
                                             FUN = function (x) {
                                               ngrams$ngram[ngrams$id == 16][x]
                                             }) %>% unlist, 
                             stringsAsFactors = F)
  #get matching words
match.df.word <- data.frame(source = match.df.6gram$source %>% strsplit(split = " ") %>% lapply(FUN = function(x) x[[1]]) %>% unlist, 
                            target = match.df.6gram$target %>% strsplit(split = " ") %>% lapply(FUN = function(x) x[[1]]) %>% unlist,
                            stringsAsFactors = F)


    # multiple matching
    # match source and target ngrams side-by-side
    match.df.6gram <- data.frame(source = ngrams$ngram[ngrams$id ==4],
                                 stringsAsFactors = F)
    
    runtime <- proc.time()
    for (i in 1:50) {
      index.match <- matcher(ngrams$ngram[ngrams$id ==4], ngrams$ngram[ngrams$id ==i])
      temp <- lapply(index.match, 
                     FUN = function (x) {
                       ngrams$ngram[ngrams$id == i][x]
                     }) %>% unlist
      match.df.6gram <- cbind(match.df.6gram, temp, stringsAsFactors = F)
      print(i)
    }
    proc.time() - runtime
    #get matching words
    match.df.word <- data.frame(source = match.df.6gram$source %>% strsplit(split = " ") %>% lapply(FUN = function(x) x[[1]]) %>% unlist)
    
    for (i in 1:50) {
      match.df.word[,i+1] <- match.df.6gram[,i+1] %>% strsplit(split = " ") %>% lapply(FUN = function(x) x[[1]]) %>% unlist
      print(i)
    } 
    
    names(match.df.word) <- c("source", text$fname[1:50])
    
    
    
    
    
    
    # match source and target ngrams side-by-side (INDEX ONLY)
    match.df.6gram.index <- data.frame(source = ngrams$ngram[ngrams$id ==1],
                                 stringsAsFactors = F)
    
    runtime <- proc.time()
    for (i in 1:50) {
      index.match <- matcher(ngrams$ngram[ngrams$id ==1], ngrams$ngram[ngrams$id ==i])
      match.df.6gram.index <- cbind(match.df.6gram.index, index.match, stringsAsFactors = F)
      print(i)
    }
    proc.time() - runtime
  
    
    names(match.df.6gram.index) <- c("source", text$fname[1:50])
                            
library(ggplot2)
library(reshape2)
library(magrittr)
    
    # binary plot
    match.df.word %>% {ifelse(is.na(.), 0, 1)} %>%
            melt %>%
            ggplot(aes(Var1, Var2, fill=factor(value))) +
           geom_raster()
    
    # index plot
    match.df.6gram.index[,-1] %>% as.matrix() %>%
      melt %>%
      ggplot(aes(Var1, Var2, fill=value)) +
      geom_raster() + 
      scale_fill_gradientn(colours = terrain.colors(10)) +
      theme_minimal() +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
    
    
    








index.match <- which(ngrams$ngram[ngrams$id ==4] %in% ngrams$ngram[ngrams$id ==16])
graph <- data.frame(index = 1:length(ngrams$ngram[ngrams$id == 4]),
                    bin = rep(0, length(ngrams$ngram[ngrams$id ==4])))
graph$bin[index.match] <- 1

cleanText[[4]][lapply(index.match, FUN = function(x) {x:(x+5)}) %>% unlist %>% unique]

plot(bin~index, data = graph, type='l')
ngrams$ngram[ngrams$id==1][index.match]
plot(bin~index, data = graph, type='l')
