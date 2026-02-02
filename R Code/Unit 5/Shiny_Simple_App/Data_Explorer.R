library(shiny)
library(ggplot2)
library(DT)

# UI
ui <- fluidPage(
  
  titlePanel("Data Explorer App"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      selectInput("dataset", "Choose Dataset:",
                  choices = c("mtcars", "iris", "diamonds")),
      
      selectInput("xvar", "X Variable:", choices = NULL),
      selectInput("yvar", "Y Variable:", choices = NULL),
      
      selectInput("color", "Color by:", choices = NULL),
      
      radioButtons("plotType", "Plot Type:",
                   choices = c("Scatter" = "scatter",
                               "Boxplot" = "box",
                               "Histogram" = "hist")),
      
      sliderInput("sampleSize", "Sample Size (%):",
                  min = 10, max = 100, value = 100, step = 10),
      
      actionButton("update", "Update Plot", class = "btn-primary")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Plot",
                 plotOutput("plot", height = "500px"),
                 br(),
                 downloadButton("downloadPlot", "Download Plot")),
        
        tabPanel("Data",
                 DTOutput("table")),
        
        tabPanel("Summary",
                 verbatimTextOutput("summary"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load dataset
  selected_data <- reactive({
    switch(input$dataset,
           "mtcars" = mtcars,
           "iris" = iris,
           "diamonds" = diamonds[sample(nrow(diamonds), 1000), ])
  })
  
  # Update variable choices
  observe({
    data <- selected_data()
    num_vars <- names(data)[sapply(data, is.numeric)]
    all_vars <- names(data)
    
    updateSelectInput(session, "xvar", choices = num_vars, selected = num_vars[1])
    updateSelectInput(session, "yvar", choices = num_vars, selected = num_vars[2])
    updateSelectInput(session, "color", choices = c("None", all_vars))
  })
  
  # Sample data
  filtered_data <- eventReactive(input$update, {
    data <- selected_data()
    n <- round(nrow(data) * input$sampleSize / 100)
    data[sample(nrow(data), n), ]
  }, ignoreInit = TRUE)
  
  # Plot
  output$plot <- renderPlot({
    req(filtered_data(), input$xvar)
    
    data <- filtered_data()
    color_var <- if (input$color == "None") NULL else input$color
    
    if (input$plotType == "scatter") {
      ggplot(data, aes_string(input$xvar, input$yvar, color = color_var)) +
        geom_point(size = 3, alpha = 0.7) +
        theme_minimal()
      
    } else if (input$plotType == "box") {
      ggplot(data, aes_string(x = color_var, y = input$xvar, fill = color_var)) +
        geom_boxplot() +
        theme_minimal()
      
    } else {
      ggplot(data, aes_string(x = input$xvar, fill = color_var)) +
        geom_histogram(bins = 30, alpha = 0.7) +
        theme_minimal()
    }
  })
  
  # Data table
  output$table <- renderDT({
    filtered_data()
  })
  
  # Summary
  output$summary <- renderPrint({
    summary(filtered_data())
  })
  
  # Download plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 10, height = 6)
    }
  )
}

# Run App
shinyApp(ui = ui, server = server)
