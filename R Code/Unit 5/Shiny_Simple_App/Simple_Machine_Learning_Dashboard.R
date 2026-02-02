library(shiny)
library(caret)
library(ggplot2)
library(DT)

# ======================
# UI
# ======================
ui <- fluidPage(
  
  titlePanel("Simple Machine Learning Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      fileInput("file", "Upload CSV File",
                accept = c(".csv")),
      
      selectInput("target", "Target Variable:", choices = NULL),
      
      selectInput("model", "Select Model:",
                  choices = c("Decision Tree" = "rpart",
                              "Random Forest" = "rf",
                              "Naive Bayes" = "nb",
                              "Logistic Regression" = "glm")),
      
      sliderInput("split", "Train/Test Split (%):",
                  min = 50, max = 90, value = 70, step = 5),
      
      actionButton("train", "Train Model", class = "btn-success"),
      actionButton("reset", "Reset", class = "btn-warning")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        
        tabPanel("Data",
                 h4("Data Preview"),
                 DTOutput("preview"),
                 h4("Summary"),
                 verbatimTextOutput("dataSummary")),
        
        tabPanel("Model Results",
                 h4("Confusion Matrix"),
                 verbatimTextOutput("confusion"),
                 h4("Performance Metrics"),
                 tableOutput("metrics"),
                 h4("Feature Importance"),
                 plotOutput("importance")),
        
        tabPanel("Predictions",
                 h4("Test Set Predictions"),
                 DTOutput("predictions"),
                 downloadButton("downloadPreds", "Download CSV"))
      )
    )
  )
)

# ======================
# SERVER
# ======================
server <- function(input, output, session) {
  
  values <- reactiveValues(
    data = NULL,
    model = NULL,
    results = NULL
  )
  
  # Load data
  observeEvent(input$file, {
    values$data <- read.csv(input$file$datapath)
    updateSelectInput(session, "target",
                      choices = names(values$data))
  })
  
  # Reset
  observeEvent(input$reset, {
    values$data <- NULL
    values$model <- NULL
    values$results <- NULL
  })
  
  # Data preview
  output$preview <- renderDT({
    req(values$data)
    head(values$data, 20)
  })
  
  # Data summary
  output$dataSummary <- renderPrint({
    req(values$data)
    cat("Dataset Dimensions:", dim(values$data), "\n\n")
    cat("Column Names:\n")
    print(names(values$data))
    cat("\n\nSummary:\n")
    print(summary(values$data))
  })
  
  # Train model
  observeEvent(input$train, {
    req(values$data, input$target)
    
    df <- values$data
    target_col <- input$target
    
    # Ensure target is factor
    df[[target_col]] <- as.factor(df[[target_col]])
    
    set.seed(123)
    train_idx <- createDataPartition(df[[target_col]],
                                     p = input$split / 100,
                                     list = FALSE)
    
    train_data <- df[train_idx, ]
    test_data  <- df[-train_idx, ]
    
    withProgress(message = "Training model...", {
      
      formula <- as.formula(paste(target_col, "~ ."))
      
      ctrl <- trainControl(method = "cv", number = 5)
      
      model <- train(
        formula,
        data = train_data,
        method = input$model,
        trControl = ctrl,
        family = if (input$model == "glm") binomial() else NULL
      )
      
      predictions <- predict(model, test_data)
      cm <- confusionMatrix(predictions, test_data[[target_col]])
      
      values$model <- model
      values$results <- list(
        confusion = cm,
        predictions = data.frame(
          Actual = test_data[[target_col]],
          Predicted = predictions,
          Correct = test_data[[target_col]] == predictions
        )
      )
    })
    
    showNotification("Model training complete!", type = "success")
  })
  
  # Confusion matrix
  output$confusion <- renderPrint({
    req(values$results)
    values$results$confusion$table
  })
  
  # Metrics
  output$metrics <- renderTable({
    req(values$results)
    cm <- values$results$confusion
    
    data.frame(
      Metric = c("Accuracy", "Kappa"),
      Value = round(c(cm$overall["Accuracy"],
                      cm$overall["Kappa"]), 3)
    )
  })
  
  # Feature importance
  output$importance <- renderPlot({
    req(values$model)
    
    imp <- tryCatch(varImp(values$model), error = function(e) NULL)
    if (is.null(imp)) return(NULL)
    
    imp_df <- data.frame(
      Feature = rownames(imp$importance),
      Importance = imp$importance$Overall
    )
    
    imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]
    
    ggplot(head(imp_df, 10),
           aes(x = Importance, y = reorder(Feature, Importance))) +
      geom_col(fill = "steelblue") +
      theme_minimal() +
      labs(title = "Top 10 Important Features")
  })
  
  # Predictions table
  output$predictions <- renderDT({
    req(values$results)
    values$results$predictions
  })
  
  # Download predictions
  output$downloadPreds <- downloadHandler(
    filename = function() {
      paste0("predictions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$results$predictions, file, row.names = FALSE)
    }
  )
}

# ======================
# RUN APP
# ======================
shinyApp(ui = ui, server = server)
