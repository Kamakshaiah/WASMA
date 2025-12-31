

# -------------------------------------------------------------------------
# 1. QUICK DESCRIPTIVE STATISTICS
# -------------------------------------------------------------------------
cat("=== BASIC STATISTICS ===\n")
cat("Total Records:", nrow(traffic_data), "\n")
cat("Date Range:", format(min(traffic_data$date), "%b %d"), "to", 
    format(max(traffic_data$date), "%b %d"), "\n")
cat("Traffic Sources:", toString(unique(traffic_data$source)), "\n\n")

# Key metrics summary
metrics <- c("sessions", "users", "pageviews", "bounce_rate", "avg_session_duration")
for(metric in metrics) {
  cat(toupper(metric), ":\n")
  cat("  Mean:", round(mean(traffic_data[[metric]]), 1), "\n")
  cat("  Median:", round(median(traffic_data[[metric]]), 1), "\n")
  cat("  Range:", round(range(traffic_data[[metric]]), 1), "\n")
}

# -------------------------------------------------------------------------
# 2. TRAFFIC SOURCE PERFORMANCE
# -------------------------------------------------------------------------
cat("\n=== TRAFFIC SOURCE ANALYSIS ===\n")

# Aggregate by source
source_stats <- aggregate(cbind(sessions, users, pageviews) ~ source, 
                          traffic_data, sum)
source_stats$bounce_rate <- aggregate(bounce_rate ~ source, traffic_data, mean)$bounce_rate
source_stats$duration <- aggregate(avg_session_duration ~ source, traffic_data, mean)$avg_session_duration
source_stats$sessions_pct <- round(source_stats$sessions/sum(source_stats$sessions)*100, 1)

print(source_stats[order(-source_stats$sessions), ])

# -------------------------------------------------------------------------
# 3. TIME TREND ANALYSIS
# -------------------------------------------------------------------------
cat("\n=== TIME TREND ANALYSIS ===\n")

# Daily totals
daily_totals <- aggregate(sessions ~ date, traffic_data, sum)
daily_totals$growth <- c(NA, diff(daily_totals$sessions)/daily_totals$sessions[-nrow(daily_totals)]*100)

cat("Best Day:", format(daily_totals$date[which.max(daily_totals$sessions)], "%b %d"), 
    "(", max(daily_totals$sessions), "sessions)\n")
cat("Worst Day:", format(daily_totals$date[which.min(daily_totals$sessions)], "%b %d"), 
    "(", min(daily_totals$sessions), "sessions)\n")
cat("Avg Daily Sessions:", round(mean(daily_totals$sessions), 0), "\n")

# -------------------------------------------------------------------------
# 4. VISUALIZATION (BASE R GRAPHICS)
# -------------------------------------------------------------------------

# Set up 2x2 plot layout
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Plot 1: Daily traffic trend
plot(daily_totals$date, daily_totals$sessions, type = "l", lwd = 2, col = "blue",
     main = "Daily Traffic Trend", xlab = "Date", ylab = "Sessions")
abline(h = mean(daily_totals$sessions), col = "red", lty = 2)

# Plot 2: Traffic by source (bar chart)
barplot(source_stats$sessions, names.arg = source_stats$source, col = rainbow(4),
        main = "Sessions by Source", ylab = "Sessions", las = 2)

# Plot 3: Bounce rate comparison
boxplot(bounce_rate ~ source, traffic_data, col = "lightblue",
        main = "Bounce Rate by Source", ylab = "Bounce Rate", las = 2)

# Plot 4: Correlation matrix
cor_matrix <- cor(traffic_data[, c("sessions", "pageviews", "bounce_rate", "avg_session_duration")])
image(cor_matrix, col = heat.colors(12), axes = FALSE, main = "Metric Correlations")
axis(1, at = seq(0, 1, length.out = 4), labels = colnames(cor_matrix), las = 2)
axis(2, at = seq(0, 1, length.out = 4), labels = colnames(cor_matrix), las = 2)

# Reset plot parameters
par(mfrow = c(1, 1))

# -------------------------------------------------------------------------
# 5. KEY INSIGHTS & RECOMMENDATIONS
# -------------------------------------------------------------------------
cat("\n=== KEY INSIGHTS ===\n")

# Find best and worst performing sources
best_source <- source_stats[which.max(source_stats$sessions), "source"]
worst_bounce <- source_stats[which.max(source_stats$bounce_rate), "source"]
best_engagement <- source_stats[which.max(source_stats$duration), "source"]

cat("1. Top Traffic Source:", best_source, "\n")
cat("2. Highest Bounce Rate:", worst_bounce, "(", 
    round(max(source_stats$bounce_rate)*100, 1), "%)\n")
cat("3. Best Engagement:", best_engagement, "(",
    round(max(source_stats$duration)/60, 1), "minutes)\n")
cat("4. Sessions to Pageviews Ratio:", 
    round(sum(traffic_data$pageviews)/sum(traffic_data$sessions), 1), "pages/session\n")

# -------------------------------------------------------------------------
# 6. QUICK QUALITY METRICS
# -------------------------------------------------------------------------
cat("\n=== QUALITY METRICS ===\n")

# Calculate engagement score
traffic_data$engagement_score <- with(traffic_data, 
                                      (sessions/max(sessions)*0.3) + 
                                        ((1-bounce_rate)*0.4) + 
                                        (avg_session_duration/max(avg_session_duration)*0.3)
)

# Quality traffic (engagement score > 0.7)
quality_traffic <- traffic_data[traffic_data$engagement_score > 0.7, ]
cat("High Quality Sessions (>0.7 score):", nrow(quality_traffic), 
    "(", round(nrow(quality_traffic)/nrow(traffic_data)*100, 1), "%)\n")

# Source quality ranking
source_quality <- aggregate(engagement_score ~ source, traffic_data, mean)
cat("\nSource Quality Ranking (by Engagement):\n")
print(source_quality[order(-source_quality$engagement_score), ])

# -------------------------------------------------------------------------
# 7. PERFORMANCE SUMMARY
# -------------------------------------------------------------------------
cat("\n=== PERFORMANCE SUMMARY ===\n")

cat("Overall Performance Score:", round(mean(traffic_data$engagement_score)*100, 1), "/100\n")
cat("Total Sessions Analyzed:", sum(traffic_data$sessions), "\n")
cat("Total Users:", sum(traffic_data$users), "\n")
cat("Total Pageviews:", sum(traffic_data$pageviews), "\n")
cat("Overall Bounce Rate:", round(mean(traffic_data$bounce_rate)*100, 1), "%\n")
cat("Avg Session Duration:", round(mean(traffic_data$avg_session_duration)/60, 1), "minutes\n")

# -------------------------------------------------------------------------
# 8. EXPORT KEY FINDINGS
# -------------------------------------------------------------------------

# Save summary report
summary_report <- data.frame(
  Metric = c("Total_Sessions", "Total_Users", "Total_Pageviews", 
             "Avg_Bounce_Rate", "Avg_Session_Duration", "Best_Source",
             "Worst_Bounce_Source", "Overall_Score"),
  Value = c(sum(traffic_data$sessions), sum(traffic_data$users), 
            sum(traffic_data$pageviews), round(mean(traffic_data$bounce_rate)*100, 1),
            round(mean(traffic_data$avg_session_duration)/60, 1),
            best_source, worst_bounce, round(mean(traffic_data$engagement_score)*100, 1))
)

write.csv(summary_report, "traffic_summary_report.csv", row.names = FALSE)
cat("\n✓ Summary report saved: traffic_summary_report.csv\n")

# -------------------------------------------------------------------------
# 9. QUICK COMMANDS FOR REPEAT ANALYSIS
# -------------------------------------------------------------------------
cat("\n=== QUICK COMMANDS ===\n")
cat("# To reload and analyze:\n")
cat("data <- read.csv('website_traffic.csv')\n")
cat("data$date <- as.Date(data$date)\n")
cat("aggregate(sessions ~ source, data, sum)\n")
cat("plot(aggregate(sessions ~ date, data, sum), type='l')\n")

# Clean up
rm(list = setdiff(ls(), "traffic_data"))
cat("\n✓ Analysis complete. Data ready in 'traffic_data' variable.\n")