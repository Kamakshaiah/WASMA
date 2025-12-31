# Generate A/B test data
set.seed(123)
ab_test_data <- data.frame(
  user_id = paste0("user_", 1:2000),
  variant = c(rep("A", 1000), rep("B", 1000)),
  visited_site = rbinom(2000, 1, 0.7),
  pages_viewed = ifelse(rbinom(2000, 1, 0.7) == 1, 
                        rpois(2000, lambda = 4), 
                        rpois(2000, lambda = 2)),
  time_on_site = round(abs(rnorm(2000, mean = 180, sd = 120))),
  converted = c(rbinom(1000, 1, 0.12),   #Variant A: 12% conversion
                rbinom(1000, 1, 0.15))   #Variant B: 15% conversion
)

# Add some noise to simulate real-world data
ab_test_data$pages_viewed[ab_test_data$pages_viewed < 1] <- 1
ab_test_data$time_on_site[ab_test_data$time_on_site < 5] <- 5

write.csv(ab_test_data, "ab_test_sample.csv", row.names = FALSE)
