library(ggplot2)
library(plotly)
library(magrittr)
library(tm)
library(quanteda)
library(tidytext)
library(reshape2)
library(RColorBrewer)


# helper function to clean the source text similar to how the targets were cleaned
cleanSourceText <- function(text) {
  text %>%
    gsub("\\[.*?\\]", "", .) %>%
    gsub("[a-zA-Z0-9]*:", "", ., perl=T) %>%
    tolower %>%
    removePunctuation %>%
    removeNumbers %>%
    return()
}

# helper function to partition the cleaned text (cleanText) to ngrams
ngrammifySourceText <- function(cleanText) {
  cleanText %>%
    data.frame(id = "source", text = ., stringsAsFactors = F) %>%
    unnest_tokens("ngram", "text", token = "ngrams", n = 6) %>%
    return()
}

# helper function to get the index of the matched ngram in the target
matcher <- function(sourcengram, targetngram) {
  a <- lapply(sourcengram, FUN = function(ngram) {
    which(ngram == targetngram)[1]
  }) %>% unlist
  return(a)
}

# helper function to get the correlationPlot 
getCorrelationIndices <- function(sourcengram, targetngramsdf) {
  match.df.6gram.index <- data.frame(source = sourcengram,
                                     stringsAsFactors = F)
  for (i in 1:length(unique(targetngramsdf$id))) {
    index.match <- matcher(sourcengram %$% ngram, 
                           targetngramsdf %>%
                             as.data.frame() %>%
                             {.[.[,"id"] == i,"ngram"] }
                          )
    match.df.6gram.index[,i+2] <- index.match
  }
   colnames(match.df.6gram.index) <- c("source", "sourcengram", paste0("indexFromText", 1:i))
  return(match.df.6gram.index)
}

# helper function to show the target
showTargetText <- function(targetngramsdf, targetId) {
  #params: targetngramsdf - data.frame, dataframe of all ngrams with cols id and ngrams
  #       targetId - int, id of the target
  targetngramsdf %>%
    filter(id == targetId) %$%
    ngram %>%
    lapply(FUN = function(ngram) {
      strsplit(ngram,' ') %>% unlist() %>% .[1]
    }) %>%
    unlist() %>% 
    return()
    
}

function(input, output) {
  cols <- brewer.pal(5, "Spectral")
  corrPlot <- eventReactive(input$sendSourceText, {
    data <- input$sourceText %>%
      cleanSourceText() %>%
      ngrammifySourceText() %>%
      getCorrelationIndices(., TARGETNGRAMSDF) %>%
      as.matrix() %>%
      .[,-c(1,2)] %>%
      melt
      p1 <- ggplot(na.omit(data), aes(Var1, factor(Var2), color="black", fill=value %>% as.character %>% as.numeric )) +
        geom_tile() + 
        scale_fill_gradientn(name="Target Index", colours = cols) +
        theme_minimal() +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(y="Target ID",
             x="Source Index",
             title = "Source-Target Index Correlation")
      ggplotly(p1) 
      
  })
  
  
  output$correlationPlot <- renderPlotly({
    corrPlot()
  })
  
  
  output$sourceTextSubset <- renderText({
    # cleans the source text then prints the range selected from the ui
    input$sourceText %>%
      cleanSourceText() %>%
      ngrammifySourceText() %$%
      ngram %>%
      lapply(FUN = function(ngram) {
        strsplit(ngram,' ') %>% unlist() %>% .[1]
      }) %>%
      unlist %>%
      .[input$sourceRange[1]:input$sourceRange[2]] %>%
      paste(collapse = " ")
  })
  
  
  output$targetTextSubset <- renderText({
    targetTextEvent() %>%
      strsplit(.,' ') %>%
      unlist %>%
      .[input$targetRange[1]:input$targetRange[2]] %>%
      paste(collapse = " ")
  })
  
  targetTextEvent <- reactive({
    TARGETNGRAMSDF %>%
      showTargetText(input$targetId) 
  })
  
  output$sourceRangeSelector <- renderUI({
    LENGTH_OF_SOURCE = strsplit(input$sourceText, ' ') %>% unlist %>% length
    conditionalPanel(
      condition = "input.sourceText != ''",
      sliderInput('sourceRange', 'Sample Size', 
                  min=1, max=LENGTH_OF_SOURCE,
                  value = c(0,1), step=5, round=0)
    )
  })
  output$targetRangeSelector <- renderUI({
    targetTextEvent()
    sliderInput('targetRange', 'Sample Size', 
              min=1, max=length(targetTextEvent()),
              value = c(0,1), round=0, step=5, width = "100%")
  })
}