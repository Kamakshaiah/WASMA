# Comprehensive Analysis
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# Load data
web_data <- read.csv("comprehensive_website_data.csv")
web_data$date <- as.Date(web_data$date)

# 1. Create summary dashboard metrics
dashboard_metrics <- web_data %>%
  summarise(
    total_sessions = sum(sessions),
    total_conversions = sum(conversions),
    total_revenue = sum(revenue),
    avg_bounce_rate = mean(bounce_rate),
    conversion_rate = sum(conversions) / sum(sessions),
    revenue_per_session = sum(revenue) / sum(sessions)
  )

# 2. Multi-dimensional analysis
# By device and traffic source
device_source_analysis <- web_data %>%
  group_by(device, traffic_source) %>%
  summarise(
    sessions = sum(sessions),
    conversions = sum(conversions),
    revenue = sum(revenue),
    conv_rate = sum(conversions) / sum(sessions),
    avg_order_value = sum(revenue) / sum(conversions),
    .groups = "drop"  # This removes the grouping after summarise
  ) %>%
  filter(!is.na(avg_order_value) & !is.infinite(avg_order_value))

# 3. Time series analysis with multiple metrics
time_series_data <- web_data %>%
  group_by(date) %>%
  summarise(
    sessions = sum(sessions),
    conversions = sum(conversions),
    revenue = sum(revenue),
    bounce_rate = mean(bounce_rate)
  ) %>%
  mutate(
    conv_rate = conversions / sessions,
    revenue_per_session = revenue / sessions
  )

# 4. Correlation heatmap
library(corrplot)
correlation_data <- web_data %>%
  select(sessions, bounce_rate, avg_session_duration, 
         pageviews, conversions, revenue)

cor_matrix <- cor(correlation_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45)

# 5. Predictive modeling for conversions
library(caret)
library(randomForest)

# Prepare data for modeling
model_data <- web_data %>%
  mutate(conversion_rate = conversions / sessions) %>%
  select(-date, -revenue)

# Split data
set.seed(123)
train_index <- createDataPartition(model_data$conversion_rate, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Train random forest model
rf_model <- randomForest(conversion_rate ~ ., 
                         data = train_data, 
                         ntree = 100,
                         importance = TRUE)

# Variable importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$variable <- rownames(importance_df)

ggplot(importance_df, aes(x = reorder(variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance for Conversion Rate Prediction",
       x = "Variable", y = "Importance (% Increase MSE)") +
  theme_minimal()

# 6. Create comprehensive report
generate_report <- function(data) {
  report <- list()
  
  report$summary <- data %>%
    summarise(
      total_sessions = sum(sessions),
      total_conversions = sum(conversions),
      overall_cr = sum(conversions) / sum(sessions),
      total_revenue = sum(revenue)
    )
  
  report$top_performing_source <- data %>%
    group_by(traffic_source) %>%
    summarise(conv_rate = sum(conversions) / sum(sessions)) %>%
    arrange(desc(conv_rate)) %>%
    slice(1)
  
  report$recommendations <- c(
    paste("Focus on", report$top_performing_source$traffic_source, 
          "traffic which has the highest conversion rate of", 
          round(report$top_performing_source$conv_rate, 2), "%"),
    "Optimize mobile experience to reduce bounce rate",
    "Implement retargeting campaigns for high-bounce segments"
  )
  
  return(report)
}

website_report <- generate_report(web_data)
