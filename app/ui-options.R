library(ggplot2)

fluidPage(
  
  title = "Text Finder",
  hr(),
  
  fluidRow(
    fluidRow(
      
    )
    fluidRow(
      # choose from repo
      column(3,
             numericInput("sourceId", 
                          label=h5("Choose a source ID"), 
                          value = 1,
                          min = MIN_VALUE_TARGET_ID, 
                          max = MAX_VALUE_TARGET_ID)
             ),
      # upload text here
      column(6,
             textAreaInput("sourceText", label = h5("Paste Text to Analyze:"), value = "", width="100%", height="200px"),
             actionButton("sendSourceText", "Analyze")
            )
    )
  ),
  fluidRow(
    # show heatmap here
 
           plotlyOutput('correlationPlot')

  ),
  
  fluidRow(
    h4("Compare Texts"),
    # filter range of source
    column(3,
         h5("Source Range"),
         uiOutput("sourceRangeSelector"),
         br()
    ),
    # filter range of target
    column(9,
           h5("Target Range"),
           numericInput("targetId", "Target ID", value = 1, min = MIN_VALUE_TARGET_ID, max = MAX_VALUE_TARGET_ID),
           uiOutput("targetRangeSelector"),
           br()
           )
  ),
  fluidRow(
    # show range of source
    column(5,
           textOutput("sourceTextSubset")
          ),
    # show range of target
    column(5,
           textOutput("targetTextSubset")
           )
  )
)