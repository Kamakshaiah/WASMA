# Content Performance Measurement

impressions <- c(1000, 2000, 1500)
clicks <- c(120, 180, 90)
conversions <- c(30, 45, 20)

ctr <- clicks / impressions
conversion_rate <- conversions / impressions

data.frame(impressions, clicks, ctr, conversion_rate)

# Content Engagement Analysis

views <- c(3000, 2500, 4000)
likes <- c(300, 200, 500)
comments <- c(40, 30, 70)
shares <- c(60, 50, 120)

engagement_score <- (likes + comments + shares) / views

data.frame(views, engagement_score)

# Content Optimization Strategies

before <- c(0.03, 0.04, 0.05)
after <- c(0.05, 0.06, 0.07)

improvement <- (after - before) / before

data.frame(before, after, improvement)

# Text Mining and Topic Modeling

library(tidytext)
library(tibble)

content <- tibble(
  text = c('data analytics improves content strategy',
           'content analytics supports marketing decisions',
           'data driven content optimization')
)

content %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)

# Analyzing Content Effectiveness
case_data <- tibble(
  content_type = c('Blog','Video','Infographic'),
  views = c(5000, 8000, 4000),
  engagement = c(0.06, 0.08, 0.05)
)

## Case Analysis (R):
  case_data %>%
  summarise(avg_engagement = mean(engagement))
