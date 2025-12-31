# =========================================================================
# REAL-TIME ANALYTICS AND DASHBOARDS
# =========================================================================

# -------------------------------------------------------------------------
# 1. SIMULATE REAL-TIME DATA STREAM
# -------------------------------------------------------------------------

# Simulate streaming web traffic data
simulate_stream <- function(n_events = 1000) {
  events <- data.frame(
    timestamp = Sys.time() + cumsum(rexp(n_events, rate = 0.1)),  # Random intervals
    user_id = paste0("user_", sample(1:5000, n_events, replace = TRUE)),
    session_id = paste0("session_", 1:n_events),
    page_url = sample(c("/home", "/products", "/cart", "/checkout", 
                        "/blog", "/about", "/contact"), 
                      n_events, replace = TRUE,
                      prob = c(0.3, 0.25, 0.1, 0.05, 0.15, 0.1, 0.05)),
    http_method = sample(c("GET", "POST"), n_events, replace = TRUE, 
                         prob = c(0.85, 0.15)),
    status_code = sample(c(200, 404, 500, 302), n_events, replace = TRUE,
                         prob = c(0.95, 0.03, 0.01, 0.01)),
    response_time_ms = rexp(n_events, rate = 1/100),  # Mean 100ms
    bytes_sent = rpois(n_events, lambda = 50000),
    user_agent = sample(c("Mobile", "Desktop", "Tablet"), n_events, 
                        replace = TRUE, prob = c(0.6, 0.35, 0.05)),
    location = sample(c("US", "UK", "India", "Germany", "Canada"), 
                      n_events, replace = TRUE)
  )
  return(events)
}

# Generate initial stream
stream_data <- simulate_stream(1000)

# -------------------------------------------------------------------------
# 2. REAL-TIME AGGREGATIONS
# -------------------------------------------------------------------------

# Function to process data in sliding windows
sliding_window_analysis <- function(data, window_minutes = 5) {
  
  current_time <- max(data$timestamp)
  window_start <- current_time - window_minutes * 60
  
  # Filter data in window
  window_data <- data[data$timestamp >= window_start & 
                        data$timestamp <= current_time, ]
  
  # Calculate metrics
  metrics <- list(
    window_start = window_start,
    window_end = current_time,
    total_requests = nrow(window_data),
    unique_users = length(unique(window_data$user_id)),
    avg_response_time = mean(window_data$response_time_ms),
    error_rate = sum(window_data$status_code >= 400) / nrow(window_data) * 100,
    throughput = nrow(window_data) / (window_minutes * 60),  # requests/sec
    popular_pages = names(sort(table(window_data$page_url), 
                               decreasing = TRUE))[1:3],
    bandwidth_usage = sum(window_data$bytes_sent) / (1024^2)  # MB
  )
  
  return(metrics)
}

# Process current window
current_metrics <- sliding_window_analysis(stream_data, 5)

cat("=== REAL-TIME METRICS (5-MINUTE WINDOW) ===\n")
print(current_metrics)

# -------------------------------------------------------------------------
# 3. TIME-SERIES AGGREGATION FOR DASHBOARD
# -------------------------------------------------------------------------

# Create minute-by-minute aggregations
stream_data$minute <- as.POSIXct(round(stream_data$timestamp, "mins"))

minute_agg <- aggregate(cbind(response_time_ms, bytes_sent) ~ minute,
                        data = stream_data,
                        FUN = function(x) c(count = length(x), 
                                            mean = mean(x),
                                            p95 = quantile(x, 0.95)))

# Extract metrics
minute_stats <- data.frame(
  minute = minute_agg$minute,
  request_count = minute_agg$response_time_ms[, "count"],
  avg_response_time = round(minute_agg$response_time_ms[, "mean"], 2),
  p95_response_time = round(minute_agg$response_time_ms[, "p95"], 2),
  bandwidth_mbps = round(minute_agg$bytes_sent[, "mean"] * 
                           minute_agg$response_time_ms[, "count"] * 8 / 
                           (1024^2 * 60), 2)  # Mbps
)

# -------------------------------------------------------------------------
# 4. REAL-TIME ALERTING SYSTEM
# -------------------------------------------------------------------------

# Define alert rules
alert_rules <- list(
  high_error_rate = function(metrics) metrics$error_rate > 5,
  slow_response = function(metrics) metrics$avg_response_time > 500,
  high_traffic = function(metrics) metrics$throughput > 100,
  bandwidth_exceeded = function(metrics) metrics$bandwidth_usage > 100
)

# Check for alerts
check_alerts <- function(metrics, rules) {
  alerts <- list()
  
  for(rule_name in names(rules)) {
    if(rules[[rule_name]](metrics)) {
      alerts[[rule_name]] <- list(
        rule = rule_name,
        value = switch(rule_name,
                       "high_error_rate" = paste0(round(metrics$error_rate, 1), "%"),
                       "slow_response" = paste0(round(metrics$avg_response_time, 0), "ms"),
                       "high_traffic" = paste0(round(metrics$throughput, 1), " req/sec"),
                       "bandwidth_exceeded" = paste0(round(metrics$bandwidth_usage, 1), "MB")
        ),
        timestamp = Sys.time()
      )
    }
  }
  
  return(alerts)
}

# Check current metrics
current_alerts <- check_alerts(current_metrics, alert_rules)

cat("\n=== REAL-TIME ALERTS ===\n")
if(length(current_alerts) > 0) {
  for(alert in current_alerts) {
    cat("ALERT:", alert$rule, " - Value:", alert$value, "\n")
  }
} else {
  cat("No alerts triggered.\n")
}

# -------------------------------------------------------------------------
# 5. DASHBOARD VISUALIZATION
# -------------------------------------------------------------------------

# Set up dashboard layout
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

# Plot 1: Requests per minute
plot(minute_stats$minute, minute_stats$request_count, 
     type = "o", col = "blue", pch = 16, cex = 0.8,
     main = "Requests per Minute",
     xlab = "Time", ylab = "Request Count")
grid()

# Plot 2: Response time trends
plot(minute_stats$minute, minute_stats$avg_response_time, 
     type = "l", col = "red", lwd = 2,
     main = "Average Response Time",
     xlab = "Time", ylab = "Response Time (ms)")
lines(minute_stats$minute, minute_stats$p95_response_time, 
      col = "orange", lwd = 2, lty = 2)
legend("topright", legend = c("Mean", "P95"), 
       col = c("red", "orange"), lwd = 2, lty = 1:2)
grid()

# Plot 3: Bandwidth usage
plot(minute_stats$minute, minute_stats$bandwidth_mbps,
     type = "h", col = "darkgreen", lwd = 2,
     main = "Bandwidth Usage",
     xlab = "Time", ylab = "Bandwidth (Mbps)")
grid()

# Plot 4: Status code distribution
status_counts <- table(stream_data$status_code)
status_df <- data.frame(
  code = names(status_counts),
  count = as.numeric(status_counts),
  percentage = as.numeric(status_counts) / sum(status_counts) * 100
)

pie(status_df$count, labels = paste0(status_df$code, "\n", 
                                     round(status_df$percentage, 1), "%"),
    col = ifelse(status_df$code >= 400, "red", 
                 ifelse(status_df$code >= 300, "yellow", "green")),
    main = "HTTP Status Codes")

# Plot 5: User agent distribution
ua_counts <- table(stream_data$user_agent)
barplot(ua_counts, col = rainbow(length(ua_counts)),
        main = "User Agents", ylab = "Count",
        las = 2)

# Plot 6: Geographic distribution
geo_counts <- table(stream_data$location)
barplot(geo_counts, col = terrain.colors(length(geo_counts)),
        main = "Traffic by Location", ylab = "Requests",
        las = 2)

par(mfrow = c(1, 1))

# -------------------------------------------------------------------------
# 6. REAL-TIME PERSONALIZATION ENGINE
# -------------------------------------------------------------------------

# Simulate real-time user profile updates
update_user_profile <- function(user_id, event_data) {
  # This would normally update a database or cache
  profile <- list(
    user_id = user_id,
    last_seen = max(event_data$timestamp),
    total_sessions = length(unique(event_data$session_id)),
    avg_session_duration = mean(diff(event_data$timestamp)),
    favorite_pages = names(sort(table(event_data$page_url), 
                                decreasing = TRUE))[1:3],
    conversion_likelihood = runif(1)  # Simulated ML prediction
  )
  
  return(profile)
}

# Example: Update profile for a random user
sample_user <- sample(stream_data$user_id, 1)
user_events <- stream_data[stream_data$user_id == sample_user, ]
if(nrow(user_events) > 0) {
  user_profile <- update_user_profile(sample_user, user_events)
  
  cat("\n=== REAL-TIME USER PROFILE ===\n")
  cat("User ID:", user_profile$user_id, "\n")
  cat("Last Seen:", as.character(user_profile$last_seen), "\n")
  cat("Total Sessions:", user_profile$total_sessions, "\n")
  cat("Favorite Pages:", paste(user_profile$favorite_pages, collapse = ", "), "\n")
  cat("Conversion Likelihood:", round(user_profile$conversion_likelihood * 100, 1), "%\n")
}

# -------------------------------------------------------------------------
# 7. PERFORMANCE MONITORING METRICS
# -------------------------------------------------------------------------

# Calculate performance metrics
performance_metrics <- data.frame(
  metric = c("Uptime", "Availability", "Reliability", 
             "Mean Time Between Failures", "Mean Time To Recovery",
             "Service Level Objective", "Error Budget Remaining"),
  value = c("99.95%", "99.97%", "99.99%", 
            "720 hours", "15 minutes", 
            "99.9%", "45 hours"),
  target = c("99.9%", "99.95%", "99.99%", 
             ">500 hours", "<30 minutes",
             "99.9%", ">24 hours"),
  status = c("OK", "OK", "OK", "OK", "OK", "OK", "WARNING")
)

cat("\n=== PERFORMANCE MONITORING DASHBOARD ===\n")
print(performance_metrics)

# -------------------------------------------------------------------------
# 8. REAL-TIME ANALYTICS EXPORT
# -------------------------------------------------------------------------

# Export for dashboard consumption
dashboard_data <- list(
  summary_metrics = current_metrics,
  time_series = minute_stats,
  alerts = current_alerts,
  performance = performance_metrics,
  timestamp = Sys.time()
)

# Save as JSON for web consumption
library(jsonlite)
write_json(dashboard_data, "realtime_dashboard_data.json", pretty = TRUE)

# Also save as CSV for backup
write.csv(minute_stats, "realtime_minute_stats.csv", row.names = FALSE)
write.csv(performance_metrics, "performance_metrics.csv", row.names = FALSE)

cat("\n✓ Real-time analytics simulation complete!\n")
cat("Dashboard data exported to realtime_dashboard_data.json\n")
