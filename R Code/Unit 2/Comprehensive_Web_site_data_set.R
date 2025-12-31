set.seed(123)
comprehensive_data <- data.frame(
  date = rep(seq.Date(as.Date("2024-01-01"), as.Date("2024-01-30"), by = "day"), 3),
  device = rep(c("Mobile", "Desktop", "Tablet"), each = 30),
  traffic_source = sample(c("Organic", "Direct", "Social", "Paid", "Referral"), 90, replace = TRUE),
  sessions = round(runif(90, min = 100, max = 1000)),
  bounce_rate = runif(90, min = 0.2, max = 0.7),
  avg_session_duration = round(runif(90, min = 60, max = 600)),
  pageviews = round(runif(90, min = 150, max = 3000)),
  conversions = rpois(90, lambda = 10),
  revenue = round(runif(90, min = 0, max = 5000))
)

write.csv(comprehensive_data, "comprehensive_website_data.csv", row.names = FALSE)
