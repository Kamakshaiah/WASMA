contentUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h3("Content Analytics & Topic Modeling"),
    fileInput(ns("file"), "Upload Content CSV (text column)"),
    plotOutput(ns("topicPlot"))
  )
}

contentServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    docs <- reactive({
      req(input$file)
      read.csv(input$file$datapath)$text
    })
    
    output$topicPlot <- renderPlot({
      corpus <- VCorpus(VectorSource(docs()))
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removeWords, stopwords("english"))
      
      dtm <- DocumentTermMatrix(corpus)
      lda <- LDA(dtm, k = 2)
      terms(lda, 5)
    })
  })
}
