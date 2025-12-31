# Funnel visualization and analysis
library(ggplot2)

# 1. Funnel visualization
funnel_data$stage <- factor(funnel_data$stage, 
                            levels = funnel_data$stage)
funnel_data$conversion_rate <- c(100, 
                                 funnel_data$count[2]/funnel_data$count[1],
                                 funnel_data$count[3]/funnel_data$count[1],
                                 funnel_data$count[4]/funnel_data$count[1],
                                 funnel_data$count[5]/funnel_data$count[1])

ggplot(funnel_data, aes(x = stage, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  geom_text(aes(label = paste0(count, "\n(", round(conversion_rate, 1), "%)")), 
            vjust = -0.5) +
  labs(title = "Conversion Funnel Analysis",
       x = "Stage", y = "Count") +
  theme_minimal()

# 2. Drop-off analysis
funnel_data$drop_off <- c(0,
                          (funnel_data$count[1] - funnel_data$count[2])/funnel_data$count[1],
                          (funnel_data$count[2] - funnel_data$count[3])/funnel_data$count[2],
                          (funnel_data$count[3] - funnel_data$count[4])/funnel_data$count[3],
                          (funnel_data$count[4] - funnel_data$count[5])/funnel_data$count[4])

# 3. User journey analysis
journeys <- read.csv("conversion_funnel_sample.csv")

# Calculate conversion rates at each stage
conversion_rates <- data.frame(
  stage = c("Home Visit", "Product View", "Add to Cart", 
            "Checkout Start", "Purchase"),
  rate = c(
    sum(journeys$visited_home) / nrow(journeys),
    sum(journeys$viewed_products) / sum(journeys$visited_home),
    sum(journeys$added_to_cart) / sum(journeys$viewed_products),
    sum(journeys$started_checkout) / sum(journeys$added_to_cart),
    sum(journeys$completed_purchase) / sum(journeys$started_checkout)
  )
)
# 4. Identify bottlenecks
ggplot(conversion_rates, aes(x = stage, y = rate)) +
  geom_line(group = 1, color = "red", size = 1.5) +
  geom_point(size = 3) +
  labs(title = "Conversion Rate by Stage",
       x = "Stage", y = "Conversion Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
