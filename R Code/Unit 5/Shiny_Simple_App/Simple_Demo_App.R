library(shiny)

# UI
ui <- fluidPage(
  
  titlePanel("FluidPage Demo App"),
  
  textInput(
    inputId = "name",
    label = "Enter your name:",
    placeholder = "Type here"
  ),
  
  textOutput("greeting")
)

# Server
server <- function(input, output) {
  
  output$greeting <- renderText({
    paste("Hello", input$name, "Welcome to Shiny!")
  })
}

# Run App
shinyApp(ui = ui, server = server)
