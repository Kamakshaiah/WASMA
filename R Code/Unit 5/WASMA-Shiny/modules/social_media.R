socialUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h3("Social Media Sentiment Analysis"),
    fileInput(ns("file"), "Upload Social Media Posts CSV"),
    plotOutput(ns("sentimentPlot")),
    DTOutput(ns("sentimentTable"))
  )
}

socialServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    posts <- reactive({
      req(input$file)
      read.csv(input$file$datapath)
    })
    
    output$sentimentTable <- renderDT({
      posts()
    })
    
    output$sentimentPlot <- renderPlot({
      posts() %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("bing")) %>%
        count(sentiment) %>%
        ggplot(aes(sentiment, n, fill = sentiment)) +
        geom_col() +
        labs(title = "Sentiment Distribution")
    })
  })
}
