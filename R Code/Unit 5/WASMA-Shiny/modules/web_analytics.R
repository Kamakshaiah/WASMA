webUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h3("Website Traffic Analytics"),
    fileInput(ns("file"), "Upload Website Traffic CSV"),
    plotOutput(ns("trafficPlot")),
    DTOutput(ns("summaryTable"))
  )
}

webServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data <- reactive({
      req(input$file)
      
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      
      # 🔴 CRITICAL FIX
      df$Date <- as.Date(df$Date)
      
      df
    })
    
    output$trafficPlot <- renderPlot({
        df <- data()
        
        # Linear trend model
        trend_model <- lm(Sessions ~ as.numeric(Date), data = df)
        trend_slope <- round(coef(trend_model)[2], 2)
        
        trend_text <- ifelse(
            trend_slope > 0,
            paste("Overall upward trend (+", trend_slope, "sessions/day)"),
            paste("Overall downward trend (", trend_slope, "sessions/day)")
        )
        
        ggplot(df, aes(x = Date, y = Sessions)) +
            geom_line(color = "#2C7FB8", linewidth = 1.2) +
            geom_point(size = 2, color = "#2C7FB8") +
            
            geom_smooth(
                method = "lm",
                se = FALSE,
                color = "darkred",
                linewidth = 1
            ) +
            
            ggplot2::annotate(
                "text",
                x = min(df$Date),
                y = max(df$Sessions),
                label = trend_text,
                hjust = 0,
                size = 4,
                color = "darkred"
            ) +
            
            labs(
                title = "Website Traffic Over Time with Trend Analysis",
                subtitle = "Daily fluctuations with overall traffic direction",
                x = "Date",
                y = "Sessions"
            ) +
            theme_minimal(base_size = 14)

    })

    
    output$summaryTable <- renderDT({
      df <- data()
      
      summary_df <- data.frame(
        Total_Sessions = sum(df$Sessions),
        Avg_Session_Duration = round(mean(df$Session_Duration), 2),
        Avg_Bounce_Rate = round(mean(df$Bounce_Rate), 2)
      )
      
      datatable(summary_df, options = list(dom = 't'))
    })
  })
}
