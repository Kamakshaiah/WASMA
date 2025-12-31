# Generate user-level funnel data - CORRECTED VERSION
set.seed(123)

# Step 1: Create basic columns first
user_journeys <- data.frame(
  user_id = paste0("user_", 1:5000),
  visited_home = rbinom(5000, 1, 0.8)
)

# Step 2: Add dependent columns one by one
user_journeys$viewed_products <- ifelse(rbinom(5000, 1, 0.5) == 1 & 
                                          rbinom(5000, 1, 0.6) == 1, 1, 0)

user_journeys$added_to_cart <- ifelse(rbinom(5000, 1, 0.3) == 1 & 
                                        user_journeys$viewed_products == 1, 1, 0)

user_journeys$started_checkout <- ifelse(rbinom(5000, 1, 0.4) == 1 & 
                                           user_journeys$added_to_cart == 1, 1, 0)

user_journeys$completed_purchase <- ifelse(rbinom(5000, 1, 0.6) == 1 & 
                                             user_journeys$started_checkout == 1, 1, 0)

# Save the data
write.csv(user_journeys, "conversion_funnel_sample.csv", row.names = FALSE)