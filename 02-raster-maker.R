for (j in 1:50) {
  match.df.6gram.index <- data.frame(source = ngrams$ngram[ngrams$id ==j],
                                     stringsAsFactors = F)
  
  runtime <- proc.time()
  for (i in 1:50) {
    index.match <- matcher(ngrams$ngram[ngrams$id ==j], ngrams$ngram[ngrams$id ==i])
    match.df.6gram.index <- cbind(match.df.6gram.index, index.match, stringsAsFactors = F)
    print(i)
  }
  proc.time() - runtime

  names(match.df.6gram.index) <- c("source", text$fname[1:50])
  
  library(ggplot2)
  library(reshape2)
  library(magrittr)

    # index plot
  png(file=paste0("./figs/", j, "to-1-50.png"))
  p1 <- match.df.6gram.index[,-1] %>% as.matrix() %>%
    melt %>%
    ggplot(aes(Var1, Var2, fill=value)) +
    geom_raster() + 
    scale_fill_gradientn(colours = terrain.colors(10)) +
    theme_minimal() +
    labs(title=paste(j, "to first 50 items")) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  print(p1)
  dev.off()
  

}
