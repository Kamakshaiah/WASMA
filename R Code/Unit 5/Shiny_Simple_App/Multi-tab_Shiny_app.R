library(shiny)

ui <- fluidPage(
  titlePanel("Iris Tabs Demo"),
  tabsetPanel(
    tabPanel("Plot", plotOutput("plot")),
    tabPanel("Data", tableOutput("table")),
    tabPanel("Summary", verbatimTextOutput("summary"))
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, pch = 19)
  })
  output$table <- renderTable(head(iris))
  output$summary <- renderPrint(summary(iris))
}

shinyApp(ui, server)
