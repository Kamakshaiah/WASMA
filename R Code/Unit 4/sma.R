# 1. engagement
likes <- c(120, 340, 560)
mean(likes)

likes_tata_punch <- round(runif(30, 100, 1000))
likes_maruti_swift <- round(runif(30, 100, 1000))

mean(tata_punch)
summary(tata_punch)
scatter.smooth(tata_punch)
mean(maruti_swift)
summary(maruti_swift)
scatter.smooth(maruti_swift)

comments <- c(30, 45, 80)
comments_tata_punch <- round(runif(30, 30, 90))
comments_maruti_swift <- round(runif(30, 30, 90))
summary(comments_maruti)
summary(comments_tata)

shares <- c(20, 60, 90)
followers <- c(1000, 2500, 4000)
auto_mobile_df <- data.frame(likes_maruti_swift, likes_tata_punch, comments_maruti_swift, comments_tata_punch)
head(auto_mobile_df)
lm(comments_tata_punch ~ likes_tata_punch, data= auto_mobile_df)

engagement_rate <- (likes + comments + shares) / followers
engagement_rate

## 2. sentiment

# library(tibble)
# sentiment_data <- tibble(
#   sentiment = c('positive','negative','positive','neutral','negative')
# )

## Sentiment Calculation (R):
# sort(table(sentiment_data), decreasing = TRUE)

# 3. Influencer Score 
# library(dplyr)
# influencer_data <- tibble(
#   influencer = c('A','B','C'),
#   followers = c(10000, 25000, 40000),
#   likes = c(500, 800, 1200),
#   comments = c(50, 120, 300)
# )
# 
# influencer_data %>%
#   mutate(engagement_rate = (likes + comments)/followers,
#          influencer_score = followers * engagement_rate)


# 4. Hashtag Growth Rate 

# hashtag_data <- tibble(
#   date = as.Date(c('2025-01-01','2025-01-02','2025-01-03')),
#   hashtag = c('#AI','#AI','#AI')
# )
# 
# hashtag_data %>%
#   group_by(date, hashtag) %>%
#   summarise(mentions = n())

posts <- read.csv(file.choose())
names(posts)
head(posts)

# 1. Basic Text Cleaning (Pure Base R)
text <- posts$content

# lowercase
text <- tolower(text)
text
# remove punctuation
text <- gsub("[[:punct:]]", " ", text)

# remove numbers
text <- gsub("[[:digit:]]", " ", text)

# remove extra whitespace
text <- gsub("\\s+", " ", text)
text

# 2. Tokenization (Words) – CRAN: tm
library(tm)

corpus <- VCorpus(VectorSource(text))

# remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("en"))


# strip whitespace
corpus <- tm_map(corpus, stripWhitespace)

# 3. Word Frequency Patterns (Base R)
dtm <- DocumentTermMatrix(corpus)
dtm

typeof(dtm); class(dtm)

# convert to matrix
m <- as.matrix(dtm)
typeof(m); class(m)
smc <- as.data.frame(m)
smc_5 <- smc[,1:5]

smc_13 <- smc[,c(1,3,12)]
# word frequencies
word_freq <- colSums(m)

# sort
word_freq <- sort(word_freq, decreasing = TRUE)

head(word_freq, 15)

# Visualization (Base R)
barplot(
  word_freq[1:10],
  las = 2,
  main = "Most Frequent Words",
  ylab = "Frequency"
)

# 6. Sentiment Analysis – CRAN: syuzhet
library(syuzhet)

sentiment_scores <- get_sentiment(text, method = "bing")

summary(sentiment_scores)

# Sentiment Over Time (Base R Plot)
plot(
  as.Date(posts$timestamp),
  sentiment_scores,
  type = "l",
  main = "Sentiment Over Time",
  xlab = "Date",
  ylab = "Sentiment Score"
)

# 7. Topic Discovery (Advanced) – CRAN: topicmodels
library(topicmodels)

lda <- LDA(dtm, k = 3, control = list(seed = 123))

terms(lda, 8)


# 8. Linking Text to Engagement (Base R)
# Word Count vs Reactions
word_count <- sapply(strsplit(text, " "), length)

plot(
  word_count,
  posts$reactions,
  xlab = "Word Count",
  ylab = "Reactions",
  main = "Post Length vs Engagement"
)

abline(lm(posts$reactions ~ word_count))
