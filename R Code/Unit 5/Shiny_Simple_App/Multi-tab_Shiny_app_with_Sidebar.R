library(shiny)

# -------------------
# UI
# -------------------
ui <- fluidPage(
  
  titlePanel("Iris Dataset – Sidebar with Tabs"),
  
  sidebarLayout(
    
    # Sidebar controls
    sidebarPanel(
      h4("Variable Selection"),
      
      selectInput(
        "xvar",
        "X-axis Variable:",
        choices = names(iris)[1:4],
        selected = "Sepal.Length"
      ),
      
      selectInput(
        "yvar",
        "Y-axis Variable:",
        choices = names(iris)[1:4],
        selected = "Sepal.Width"
      )
    ),
    
    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        
        tabPanel(
          "Plot",
          plotOutput("scatterPlot")
        ),
        
        tabPanel(
          "Data",
          tableOutput("dataTable")
        ),
        
        tabPanel(
          "Summary",
          verbatimTextOutput("summaryText")
        )
      )
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
    legend(
      "topright",
      legend = levels(iris$Species),
      col = 1:3,
      pch = 19
    )
  })
  
  output$dataTable <- renderTable({
    head(iris, 10)
  })
  
  output$summaryText <- renderPrint({
    summary(iris)
  })
}

# -------------------
# Run App
# -------------------
shinyApp(ui = ui, server = server)
