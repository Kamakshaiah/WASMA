# =========================================================================
# WEBSITE TRAFFIC ANALYSIS - CRISP BASE R VERSION
# =========================================================================

# Generate sample traffic data
set.seed(123)
dates <- seq.Date(as.Date("2024-01-01"), as.Date("2024-01-31"), by = "day")
traffic_data <- data.frame(
  date = rep(dates, 4),
  source = rep(c("Organic", "Direct", "Social", "Paid"), each = 31),
  sessions = round(runif(124, min = 50, max = 500)),
  users = round(runif(124, min = 40, max = 450)),
  pageviews = round(runif(124, min = 100, max = 2000)),
  bounce_rate = runif(124, min = 0.3, max = 0.8),
  avg_session_duration = round(runif(124, min = 30, max = 600))
)

# Save data
write.csv(traffic_data, "website_traffic.csv", row.names = FALSE)