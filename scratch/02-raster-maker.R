
library(ggplot2)
library(reshape2)
library(magrittr)
library(RColorBrewer)
for (j in 1:199) {
  match.df.6gram.index <- data.frame(source = ngrams$ngram[ngrams$id ==j],
                                     stringsAsFactors = F)
  
  runtime <- proc.time()
  for (i in 1:199) {
    index.match <- matcher(ngrams$ngram[ngrams$id ==j], ngrams$ngram[ngrams$id ==i])
    match.df.6gram.index <- cbind(match.df.6gram.index, index.match, stringsAsFactors = F)
  }
  proc.time() - runtime
  

  names(match.df.6gram.index) <- c("source", text$fname[1:50])


    # index plot
  png(file=paste0("./figs/10-24-2017/", j, "to-1-199.png"))
  cols <- brewer.pal(5, "Spectral")
  p1 <- match.df.6gram.index[,-1] %>% as.matrix() %>%
    melt %>%
    ggplot(aes(Var1, Var2, fill=value)) +
    geom_raster() + 
    scale_fill_gradientn(colours = cols) +
    theme_minimal() +
    labs(title=paste(j, "to 199 items")) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(p1)
  dev.off()
  
  print(paste("[INFO]: done printing", j))
}
