# Engagement Rate 
likes <- c(120, 340, 560)
comments <- c(30, 45, 80)
shares <- c(20, 60, 90)
followers <- c(1000, 2500, 4000)

engagement_rate <- (likes + comments + shares) / followers
engagement_rate

# Sentiment Score 

library(tibble)
sentiment_data <- tibble(
  sentiment = c('positive','negative','positive','neutral','negative')
)

## Sentiment Calculation (R):
  sentiment_data %>%
  count(sentiment)

# Influencer Score 

  influencer_data <- tibble(
    influencer = c('A','B','C'),
    followers = c(10000, 25000, 40000),
    likes = c(500, 800, 1200),
    comments = c(50, 120, 300)
  )
  
  influencer_data %>%
    mutate(engagement_rate = (likes + comments)/followers,
           influencer_score = followers * engagement_rate)
  
# Hashtag Growth Rate 
  
  hashtag_data <- tibble(
    date = as.Date(c('2025-01-01','2025-01-02','2025-01-03')),
    hashtag = c('#AI','#AI','#AI')
  )
  
  hashtag_data %>%
    group_by(date, hashtag) %>%
    summarise(mentions = n())
  
# End-to-end anlaysis 
  
  workshop_data <- tibble(
    platform = c('Instagram','Twitter','LinkedIn'),
    engagement = c(0.05, 0.03, 0.07)
  )
  
  
    workshop_data %>%
    summarise(avg_engagement = mean(engagement))
  