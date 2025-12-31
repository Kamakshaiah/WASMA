data <- read.csv("personalization_dataset.csv")

# Click Through Rate
mean(data$RecommendationClicked)

# Conversion Rate
mean(data$Purchased)

# Correlation
cor(data$TimeSpent.min., data$Clicks)
