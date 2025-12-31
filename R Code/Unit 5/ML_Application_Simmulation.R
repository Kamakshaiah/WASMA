# =========================================================================
# MACHINE LEARNING APPLICATIONS IN ANALYTICS
# =========================================================================

# -------------------------------------------------------------------------
# 1. USER SEGMENTATION USING CLUSTERING
# -------------------------------------------------------------------------

# Simulate user behavior data for clustering
set.seed(123)
n_users_cluster <- 2000

cluster_data <- data.frame(
  user_id = paste0("u", 1:n_users_cluster),
  
  # Engagement patterns
  daily_visits = rnorm(n_users_cluster, mean = 2.5, sd = 1.5),
  avg_session_duration = rnorm(n_users_cluster, mean = 300, sd = 120),
  pages_per_visit = rnorm(n_users_cluster, mean = 6, sd = 2.5),
  
  # Conversion behavior
  conversion_rate = runif(n_users_cluster, min = 0.01, max = 0.3),
  avg_order_value = rnorm(n_users_cluster, mean = 75, sd = 25),
  
  # Content preferences
  tech_articles = rpois(n_users_cluster, lambda = 3),
  entertainment_videos = rpois(n_users_cluster, lambda = 5),
  business_content = rpois(n_users_cluster, lambda = 2),
  
  # Social engagement
  social_shares = rpois(n_users_cluster, lambda = 2),
  comments_made = rpois(n_users_cluster, lambda = 1)
)

# Standardize data for clustering
scaled_data <- scale(cluster_data[, -1])

# -------------------------------------------------------------------------
# 2. K-MEANS CLUSTERING
# -------------------------------------------------------------------------

# Determine optimal number of clusters using elbow method
wss <- numeric(10)
for (k in 1:10) {
  kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)
  wss[k] <- kmeans_result$tot.withinss
}

# Plot elbow curve
plot(1:10, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within-cluster Sum of Squares",
     main = "Elbow Method for Optimal K")

# Apply K-means with optimal clusters (let's choose 4)
set.seed(123)
k <- 4
kmeans_model <- kmeans(scaled_data, centers = k, nstart = 25)

# Add cluster assignments
cluster_data$cluster <- as.factor(kmeans_model$cluster)

# -------------------------------------------------------------------------
# 3. CLUSTER PROFILING AND ANALYSIS
# -------------------------------------------------------------------------

# Calculate cluster centroids (original scale)
cluster_means <- aggregate(. ~ cluster, 
                           data = cluster_data[, -1], 
                           FUN = mean)

cat("=== USER SEGMENTATION CLUSTERS ===\n")
print(cluster_means)

# Calculate cluster sizes
cluster_sizes <- table(cluster_data$cluster)
cat("\nCluster Sizes:\n")
print(cluster_sizes)

# Calculate percentage distribution
cluster_percentage <- prop.table(cluster_sizes) * 100
cat("\nCluster Distribution (%):\n")
print(round(cluster_percentage, 1))

# -------------------------------------------------------------------------
# 4. RANDOM FOREST FOR CONVERSION PREDICTION
# -------------------------------------------------------------------------

library(randomForest)

# Prepare data for classification
# Create binary target: High-value user (avg_order_value > 80)
cluster_data$high_value <- ifelse(cluster_data$avg_order_value > 80, 1, 0)

# Select features for prediction
features <- c("daily_visits", "avg_session_duration", "pages_per_visit",
              "conversion_rate", "tech_articles", "entertainment_videos")

rf_data <- cluster_data[, c(features, "high_value")]

# Split data
set.seed(123)
train_idx <- sample(1:nrow(rf_data), 0.7 * nrow(rf_data))
train_rf <- rf_data[train_idx, ]
test_rf <- rf_data[-train_idx, ]

# Train Random Forest
rf_model <- randomForest(as.factor(high_value) ~ .,
                         data = train_rf,
                         ntree = 100,
                         importance = TRUE,
                         mtry = 2)

# Predictions
predictions <- predict(rf_model, test_rf)

# Evaluate
conf_matrix_rf <- table(Predicted = predictions, 
                        Actual = as.factor(test_rf$high_value))

cat("\n=== RANDOM FOREST CLASSIFICATION ===\n")
cat("Confusion Matrix:\n")
print(conf_matrix_rf)

# Feature importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$feature <- rownames(importance_df)
importance_df <- importance_df[order(-importance_df$MeanDecreaseGini), ]

cat("\nFeature Importance (Mean Decrease in Gini):\n")
print(importance_df[, c("feature", "MeanDecreaseGini")])

# -------------------------------------------------------------------------
# 5. RECOMMENDATION SYSTEM (COLLABORATIVE FILTERING)
# -------------------------------------------------------------------------

# Simulate user-content interaction matrix
set.seed(123)
n_users_rec <- 500
n_items <- 100

# Create sparse interaction matrix
interaction_matrix <- matrix(0, nrow = n_users_rec, ncol = n_items)
rownames(interaction_matrix) <- paste0("user_rec", 1:n_users_rec)
colnames(interaction_matrix) <- paste0("item", 1:n_items)

# Simulate interactions (ratings 1-5)
for(i in 1:n_users_rec) {
  # Each user interacts with 20-40 items
  n_interactions <- sample(20:40, 1)
  items <- sample(1:n_items, n_interactions)
  ratings <- sample(1:5, n_interactions, replace = TRUE, 
                    prob = c(0.1, 0.2, 0.3, 0.25, 0.15))
  interaction_matrix[i, items] <- ratings
}

# Convert to sparse format
sparse_matrix <- as(interaction_matrix, "sparseMatrix")

# User similarity (cosine similarity)
cosine_similarity <- function(x, y) {
  sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}

# Example: Find similar users for user 1
user1_vector <- interaction_matrix[1, ]
similarities <- apply(interaction_matrix, 1, 
                      function(x) cosine_similarity(x, user1_vector))

# Get top 5 similar users
top_similar_users <- head(sort(similarities, decreasing = TRUE)[-1], 5)

cat("\n=== RECOMMENDATION SYSTEM ===\n")
cat("Top 5 users similar to User 1:\n")
print(top_similar_users)

# Generate recommendations
user_items <- which(interaction_matrix[1, ] > 0)
similar_user_items <- unique(unlist(
  lapply(names(top_similar_users), 
         function(u) which(interaction_matrix[u, ] > 0))
))

# Items not rated by user 1 but rated by similar users
recommendations <- setdiff(similar_user_items, user_items)

cat("\nRecommended items for User 1 (based on similar users):\n")
cat("Number of recommendations:", length(recommendations), "\n")
if(length(recommendations) > 0) {
  cat("Item IDs:", head(recommendations, 10), "...\n")
}

# -------------------------------------------------------------------------
# 6. TIME SERIES FORECASTING (ARIMA)
# -------------------------------------------------------------------------

# Simulate daily website traffic
set.seed(123)
n_days <- 365
dates <- seq.Date(as.Date("2024-01-01"), by = "day", length.out = n_days)

# Create time series with trend, seasonality, and noise
trend <- seq(1000, 2000, length.out = n_days)
seasonality <- 200 * sin(2 * pi * (1:n_days) / 30)  # Monthly seasonality
noise <- rnorm(n_days, mean = 0, sd = 50)

traffic_ts <- ts(trend + seasonality + noise, 
                 start = c(2024, 1), 
                 frequency = 365)

# Fit ARIMA model
library(forecast)
arima_model <- auto.arima(traffic_ts)

# Forecast next 30 days
forecast_result <- forecast(arima_model, h = 30)

cat("\n=== TIME SERIES FORECASTING (ARIMA) ===\n")
cat("ARIMA Model:", arima_model$arma, "\n")
cat("AIC:", round(arima_model$aic, 2), "\n")
cat("BIC:", round(arima_model$bic, 2), "\n")

# Plot forecast
plot(forecast_result, 
     main = "Website Traffic Forecast (ARIMA)",
     xlab = "Date",
     ylab = "Daily Visitors")
grid()

# -------------------------------------------------------------------------
# 7. MODEL DEPLOYMENT METRICS
# -------------------------------------------------------------------------

# Calculate model monitoring metrics
model_metrics <- data.frame(
  metric = c("Model Accuracy", "Precision", "Recall", "F1-Score", 
             "AUC-ROC", "Training Time (sec)", "Inference Time (ms)"),
  value = c(
    round(sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf), 3),
    round(conf_matrix_rf[2, 2] / sum(conf_matrix_rf[2, ]), 3),
    round(conf_matrix_rf[2, 2] / sum(conf_matrix_rf[, 2]), 3),
    round(2 * (precision * recall) / (precision + recall), 3),
    0.872,  # Simulated AUC
    15.3,   # Simulated training time
    45      # Simulated inference time
  )
)

cat("\n=== MODEL DEPLOYMENT METRICS ===\n")
print(model_metrics)

# Calculate business impact
business_impact <- data.frame(
  metric = c("Conversion Lift", "Revenue Increase", "Customer Retention", 
             "Reduced CAC", "ROI"),
  value = c("15%", "$45,000/month", "12% improvement", "8% reduction", "320%")
)

cat("\nBusiness Impact:\n")
print(business_impact)

# -------------------------------------------------------------------------
# 8. SAVE ML RESULTS
# -------------------------------------------------------------------------

write.csv(cluster_data, "user_segments_ml.csv", row.names = FALSE)
write.csv(importance_df, "feature_importance.csv", row.names = FALSE)
write.csv(model_metrics, "model_performance.csv", row.names = FALSE)

cat("\n✓ Machine learning applications simulation complete!\n")
