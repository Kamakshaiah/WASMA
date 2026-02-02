library(shiny)

# -------------------
# UI
# -------------------
ui <- fluidPage(
  
  titlePanel("Simple Iris Data Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      selectInput(
        inputId = "xvar",
        label = "Select X Variable:",
        choices = names(iris)[1:4]
      ),
      
      selectInput(
        inputId = "yvar",
        label = "Select Y Variable:",
        choices = names(iris)[1:4],
        selected = names(iris)[2]
      )
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      hr(),
      tableOutput("dataTable")
    )
  )
)

# -------------------
# Server
# -------------------
server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    plot(
      iris[[input$xvar]],
      iris[[input$yvar]],
      col = iris$Species,
      pch = 19,
      xlab = input$xvar,
      ylab = input$yvar,
      main = "Iris Scatter Plot"
    )
    legend("topright",
           legend = levels(iris$Species),
           col = 1:3,
           pch = 19)
  })
  
  output$dataTable <- renderTable({
    head(iris, 10)
  })
}

# -------------------
# Run App
# -------------------
shinyApp(ui = ui, server = server)
