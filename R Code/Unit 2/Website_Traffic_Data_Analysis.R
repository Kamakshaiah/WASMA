library(dplyr)
library(ggplot2)
library(lubridate)

traffic <- read.csv("website_traffic_sample.csv")
traffic$date <- as.Date(traffic$date)

# 1. Overall traffic trend
ggplot(traffic, aes(x = date, y = sessions)) +
  geom_line(aes(color = source), size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(title = "Website Traffic Trend by Source",
       x = "Date", y = "Sessions") +
  theme_minimal()

# 2. Traffic source distribution
source_summary <- traffic %>%
  group_by(source) %>%
  summarise(
    total_sessions = sum(sessions),
    avg_bounce_rate = mean(bounce_rate),
    avg_duration = mean(avg_session_duration)
  )

ggplot(source_summary, aes(x = reorder(source, -total_sessions), y = total_sessions)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_sessions), vjust = -0.5) +
  labs(title = "Total Sessions by Traffic Source",
       x = "Source", y = "Total Sessions")

# 3. Bounce rate analysis by source
ggplot(traffic, aes(x = source, y = bounce_rate, fill = source)) +
  geom_boxplot() +
  labs(title = "Bounce Rate Distribution by Traffic Source",
       x = "Source", y = "Bounce Rate") +
  theme_minimal()
