data <- read.csv("checkout_flow_dataset.csv")

# Filter groups
multi <- data[data$CheckoutType == "Multi-step", ]
single <- data[data$CheckoutType == "Single-page", ]

# Conversion rates
mean(multi$CompletedPurchase)
mean(single$CompletedPurchase)

# t-test for time spent
t.test(multi$TimeSpent.sec., single$TimeSpent.sec.)

# Chi-square test
table_data <- table(data$CheckoutType, data$CompletedPurchase)
chisq.test(table_data)
