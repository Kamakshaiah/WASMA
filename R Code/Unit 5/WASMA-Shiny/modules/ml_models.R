# ml_models.R

mlUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h3("Machine Learning Models"),
    
    # File upload
    fileInput(ns("file"), "Upload Dataset (CSV)", accept = ".csv"),
    
    # Action button
    actionButton(ns("trainBtn"), "🚀 Train Model", class = "btn-primary"),
    
    # Status
    uiOutput(ns("status")),
    
    # Tabset for results
    tabsetPanel(
      id = ns("resultsTabs"),
      
      # Tab 1: Model Summary
      tabPanel(
        "Model Summary",
        br(),
        verbatimTextOutput(ns("modelSummary")),
        br(),
        h4("Confusion Matrix:"),
        verbatimTextOutput(ns("confusionMatrix")),
        br(),
        h4("Performance Metrics:"),
        tableOutput(ns("metricsTable"))
      ),
      
      # Tab 2: Plots
      tabPanel(
        "Visualizations",
        br(),
        fluidRow(
          column(6,
                 h4("Feature Distributions"),
                 plotOutput(ns("featurePlot"))
          ),
          column(6,
                 h4("Confusion Matrix Heatmap"),
                 plotOutput(ns("confusionHeatmap"))
          )
        ),
        br(),
        fluidRow(
          column(6,
                 h4("Feature Importance"),
                 plotOutput(ns("importancePlot"))
          ),
          column(6,
                 h4("ROC Curve (if binary classification)"),
                 plotOutput(ns("rocPlot"))
          )
        )
      ),
      
      # Tab 3: Predictions
      tabPanel(
        "Predictions",
        br(),
        h4("Test Set Predictions:"),
        tableOutput(ns("predictionsTable")),
        br(),
        h4("Prediction Probabilities:"),
        tableOutput(ns("probabilitiesTable"))
      ),
      
      # Tab 4: Data
      tabPanel(
        "Data Preview",
        br(),
        h4("Dataset Overview:"),
        verbatimTextOutput(ns("dataSummary")),
        br(),
        h4("First 20 Rows:"),
        dataTableOutput(ns("dataPreview"))
      )
    ),
    
    # Loading spinner
    conditionalPanel(
      condition = paste0("input['", ns("trainBtn"), "'] > 0 && $('html').hasClass('shiny-busy')"),
      div(
        style = "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 1000; background: white; padding: 20px; border-radius: 10px; box-shadow: 0 0 20px rgba(0,0,0,0.2);",
        h4("Training model...", style = "color: #007bff;"),
        tags$div(class = "spinner-border text-primary", role = "status",
                 tags$span(class = "sr-only", "Loading..."))
      )
    )
  )
}

mlServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values
    dataset <- reactiveVal(NULL)
    model_results <- reactiveVal(NULL)
    predictions <- reactiveVal(NULL)
    
    # Status message
    output$status <- renderUI({
      if (is.null(dataset())) {
        return(div(style = "color: orange; padding: 10px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px;",
                   "📁 Please upload a CSV file with 'label' column"))
      } else if (is.null(model_results())) {
        df <- dataset()
        return(div(style = "color: green; padding: 10px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 5px;",
                   paste0("✓ Data loaded: ", nrow(df), " rows, ", ncol(df), " columns. Click 'Train Model' to start.")))
      } else {
        return(div(style = "color: blue; padding: 10px; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 5px;",
                   "✅ Model training complete! View results in tabs above."))
      }
    })
    
    # Read dataset
    observeEvent(input$file, {
      req(input$file)
      
      tryCatch({
        df <- read.csv(input$file$datapath)
        
        # Validate
        if (!"label" %in% names(df)) {
          showNotification("Error: Dataset must contain 'label' column", type = "error", duration = 5)
          return()
        }
        
        # Convert label to factor
        df$label <- as.factor(df$label)
        dataset(df)
        model_results(NULL)  # Reset results
        
        showNotification("Data loaded successfully!", type = "success", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
      })
    })
    
    # Data summary
    output$dataSummary <- renderPrint({
      req(dataset())
      df <- dataset()
      
      cat("DATASET SUMMARY\n")
      cat("===============\n\n")
      cat("Dimensions:", nrow(df), "rows ×", ncol(df), "columns\n")
      cat("\nColumn Names:\n")
      print(names(df))
      cat("\nData Types:\n")
      print(sapply(df, class))
      cat("\nLabel Distribution:\n")
      print(table(df$label))
      cat("\nSummary Statistics:\n")
      print(summary(df))
    })
    
    # Data preview
    output$dataPreview <- renderDataTable({
      req(dataset())
      dataset()
    }, options = list(pageLength = 10, scrollX = TRUE))
    
    # Train model
    observeEvent(input$trainBtn, {
      req(dataset())
      
      # Show loading
      showNotification("Training model... This may take a moment.", type = "warning", duration = 2)
      
      # Use tryCatch for error handling
      tryCatch({
        df <- dataset()
        
        # Set seed for reproducibility
        set.seed(123)
        
        # Split data (70% train, 30% test)
        train_idx <- createDataPartition(df$label, p = 0.7, list = FALSE)
        train_data <- df[train_idx, ]
        test_data <- df[-train_idx, ]
        
        # Load required packages
        if (!require(e1071, quietly = TRUE)) {
          install.packages("e1071", quiet = TRUE)
          library(e1071)
        }
        
        if (!require(caret, quietly = TRUE)) {
          install.packages("caret", quiet = TRUE)
          library(caret)
        }
        
        # Train Naive Bayes model
        model <- naiveBayes(label ~ ., data = train_data)
        
        # Make predictions
        pred_classes <- predict(model, test_data)
        pred_probs <- predict(model, test_data, type = "raw")
        
        # Calculate confusion matrix
        cm <- confusionMatrix(pred_classes, test_data$label)
        
        # Calculate additional metrics
        metrics <- data.frame(
          Metric = c("Accuracy", "Kappa", "Sensitivity", "Specificity", 
                     "Precision", "Recall", "F1-Score", "Balanced Accuracy"),
          Value = c(
            round(cm$overall["Accuracy"], 4),
            round(cm$overall["Kappa"], 4),
            round(cm$byClass["Sensitivity"], 4),
            round(cm$byClass["Specificity"], 4),
            round(cm$byClass["Precision"], 4),
            round(cm$byClass["Recall"], 4),
            round(cm$byClass["F1"], 4),
            round(cm$byClass["Balanced Accuracy"], 4)
          )
        )
        
        # Store results
        results <- list(
          model = model,
          confusion_matrix = cm,
          metrics = metrics,
          predictions = data.frame(
            Actual = test_data$label,
            Predicted = pred_classes,
            pred_probs,
            Correct = test_data$label == pred_classes
          ),
          test_data = test_data,
          train_data = train_data
        )
        
        model_results(results)
        predictions(results$predictions)
        
        # Switch to first results tab
        updateTabsetPanel(session, "resultsTabs", selected = "Model Summary")
        
        showNotification("Model training complete!", type = "success", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Training failed:", e$message), type = "error", duration = 5)
      })
    })
    
    # Model summary output
    output$modelSummary <- renderPrint({
      req(model_results())
      results <- model_results()
      
      cat("NAIVE BAYES MODEL SUMMARY\n")
      cat("=========================\n\n")
      
      cat("Training Data: ", nrow(results$train_data), " samples\n")
      cat("Testing Data:  ", nrow(results$test_data), " samples\n")
      cat("\nModel Priors:\n")
      print(results$model$apriori)
      cat("\nConditional Probabilities (Means):\n")
      print(results$model$tables)
    })
    
    # Confusion matrix output
    output$confusionMatrix <- renderPrint({
      req(model_results())
      print(model_results()$confusion_matrix$table)
    })
    
    # Metrics table
    output$metricsTable <- renderTable({
      req(model_results())
      model_results()$metrics
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Predictions table
    output$predictionsTable <- renderTable({
      req(predictions())
      head(predictions()[, 1:3], 20)  # Show first 3 columns
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Probabilities table
    output$probabilitiesTable <- renderTable({
      req(predictions())
      probs <- predictions()
      # Get probability columns (all columns after first 3)
      if (ncol(probs) > 3) {
        prob_cols <- probs[, 4:(ncol(probs)-1)]  # Exclude last "Correct" column
        cbind(Actual = probs$Actual, Predicted = probs$Predicted, prob_cols)
      }
    }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 3)
    
    # PLOTS
    
    # Feature distribution plot
    output$featurePlot <- renderPlot({
      req(dataset())
      df <- dataset()
      
      # Exclude label column for plotting
      feature_cols <- setdiff(names(df), "label")
      
      # Create multiple histograms
      par(mfrow = c(2, 2))
      for (i in 1:min(4, length(feature_cols))) {
        col <- feature_cols[i]
        if (is.numeric(df[[col]])) {
          hist(df[[col]], main = paste("Distribution of", col),
               xlab = col, col = "lightblue", border = "white")
        }
      }
    })
    
    # Confusion matrix heatmap
    output$confusionHeatmap <- renderPlot({
      req(model_results())
      cm <- model_results()$confusion_matrix$table
      
      # Convert to data frame for ggplot
      cm_df <- as.data.frame(as.table(cm))
      names(cm_df) <- c("Predicted", "Actual", "Frequency")
      
      # Create heatmap
      ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Frequency)) +
        geom_tile(color = "white") +
        geom_text(aes(label = Frequency), color = "black", size = 6) +
        scale_fill_gradient(low = "white", high = "#3498db") +
        theme_minimal() +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold")
        ) +
        labs(title = "Confusion Matrix Heatmap",
             x = "Actual Class",
             y = "Predicted Class")
    })
    
    # Feature importance plot (using mean differences between classes)
    output$importancePlot <- renderPlot({
      req(dataset())
      df <- dataset()
      
      # Calculate mean by class for each feature
      feature_cols <- setdiff(names(df), "label")
      importance <- data.frame(Feature = character(), Importance = numeric())
      
      for (col in feature_cols) {
        if (is.numeric(df[[col]])) {
          # Calculate difference between class means (simple importance measure)
          class_means <- tapply(df[[col]], df$label, mean, na.rm = TRUE)
          if (length(class_means) >= 2) {
            imp_value <- abs(diff(range(class_means)))
            importance <- rbind(importance, 
                                data.frame(Feature = col, Importance = imp_value))
          }
        }
      }
      
      # Sort by importance
      importance <- importance[order(importance$Importance, decreasing = FALSE), ]
      
      # Create horizontal bar plot
      ggplot(importance, aes(x = Importance, y = reorder(Feature, Importance))) +
        geom_bar(stat = "identity", fill = "#2ecc71", alpha = 0.8) +
        geom_text(aes(label = round(Importance, 3)), 
                  hjust = -0.2, size = 4) +
        theme_minimal() +
        labs(
          title = "Feature Importance",
          x = "Importance (Difference between class means)",
          y = "Feature"
        ) +
        theme(
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14, face = "bold")
        )
    })
    
    # ROC curve (for binary classification only)
    output$rocPlot <- renderPlot({
      req(model_results())
      results <- model_results()
      
      # Only plot ROC if binary classification
      if (length(levels(results$test_data$label)) == 2) {
        
        # Get probabilities for positive class
        pred_probs <- predict(results$model, results$test_data, type = "raw")
        
        # Ensure we have probability matrix
        if (is.matrix(pred_probs)) {
          # Use second column as positive class probability
          pos_class <- levels(results$test_data$label)[2]
          prob_positive <- pred_probs[, pos_class]
          
          # Create ROC curve
          roc_obj <- roc(results$test_data$label, prob_positive)
          
          # Plot ROC
          plot(roc_obj, main = "ROC Curve", col = "#e74c3c", lwd = 2)
          abline(a = 0, b = 1, lty = 2, col = "gray")
          
          # Add AUC
          auc_value <- auc(roc_obj)
          legend("bottomright", 
                 legend = paste("AUC =", round(auc_value, 3)),
                 col = "#e74c3c", lwd = 2)
        }
      } else {
        # Message for multi-class
        plot.new()
        text(0.5, 0.5, "ROC curve available only for binary classification", 
             cex = 1.2, col = "gray")
      }
    })
  })
}