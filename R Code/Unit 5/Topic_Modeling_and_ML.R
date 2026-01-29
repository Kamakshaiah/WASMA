#####################################################################
# DOCUMENT–TERM MATRIX (DTM) + MACHINE LEARNING
# COMPLETE, POLISHED, END-TO-END PIPELINE IN R
#
# Covers:
# 1. Large document handling
# 2. Text preprocessing
# 3. DTM & TF–IDF
# 4. Supervised ML (external labels)
# 5. Deriving labels from DTM (weak supervision)
# 6. Using a DTM column as label (word prediction)
# 7. Unsupervised learning (LDA, Clustering)
#
# IMPORTANT PEDAGOGICAL NOTE:
# - DTM columns are FEATURES (X)
# - Labels (Y) should normally come from METADATA
#####################################################################


#############################
# 0. INSTALL & LOAD PACKAGES
#############################

# install.packages(c("tm", "SnowballC", "e1071", "topicmodels", "cluster"))

library(tm)
library(SnowballC)
library(e1071)
library(topicmodels)
library(cluster)


#############################
# 1. SAMPLE LARGE DOCUMENT DATA
#############################

# Each row represents a LARGE document
documents <- data.frame(
  doc_id = 1:8,
  text = c(
    "data mining analytics machine learning for business decision making",
    "predictive analytics models using machine learning techniques",
    "marketing strategy branding advertising customer engagement",
    "digital marketing campaigns social media and branding",
    "data science analytics models for forecasting and prediction",
    "advertising marketing analytics and customer behavior analysis",
    "machine learning algorithms for data analytics applications",
    "marketing research consumer behavior advertising strategy"
  ),
  # TRUE LABELS FROM METADATA (not from DTM)
  label = c(
    "Analytics", "Analytics", "Marketing", "Marketing",
    "Analytics", "Marketing", "Analytics", "Marketing"
  ),
  stringsAsFactors = FALSE
)


#############################
# 2. CREATE TEXT CORPUS
#############################

corpus <- VCorpus(VectorSource(documents$text))


#############################
# 3. TEXT PREPROCESSING
#############################

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)


#############################
# 4. DOCUMENT–TERM MATRIX
#############################

dtm <- DocumentTermMatrix(corpus)

# Remove sparse terms (important for large corpora)
dtm <- removeSparseTerms(dtm, 0.8)

# Convert DTM to data frame
dtm_df <- as.data.frame(as.matrix(dtm))


#############################
# 5. SUPERVISED LEARNING (EXTERNAL LABELS)
#############################

# Attach true labels from metadata
dtm_df$label <- as.factor(documents$label)

### 5A. NAIVE BAYES
nb_model <- naiveBayes(label ~ ., data = dtm_df)
nb_pred <- predict(nb_model, dtm_df)

cat("\nNaive Bayes Accuracy (True Labels):\n")
print(mean(nb_pred == dtm_df$label))


### 5B. LOGISTIC REGRESSION
log_model <- glm(label ~ ., data = dtm_df, family = binomial)
log_prob <- predict(log_model, type = "response")
log_pred <- ifelse(log_prob > 0.5, "Marketing", "Analytics")

cat("\nLogistic Regression Accuracy:\n")
print(mean(log_pred == dtm_df$label))


### 5C. SUPPORT VECTOR MACHINE
svm_model <- svm(label ~ ., data = dtm_df, kernel = "linear")
svm_pred <- predict(svm_model, dtm_df)

cat("\nSVM Accuracy:\n")
print(mean(svm_pred == dtm_df$label))


#############################
# 6. TF–IDF REPRESENTATION
#############################

dtm_tfidf <- DocumentTermMatrix(
  corpus,
  control = list(weighting = weightTfIdf)
)

tfidf_df <- as.data.frame(as.matrix(dtm_tfidf))
tfidf_df$label <- as.factor(documents$label)


#############################
# 7. DERIVING LABELS FROM DTM (WEAK SUPERVISION)
#############################

# Labels created using heuristic rules on term frequencies
dtm_df$derived_label <- ifelse(
  (dtm_df$market + dtm_df$advertis + dtm_df$brand) >= 2,
  "Marketing",
  "Analytics"
)

dtm_df$derived_label <- as.factor(dtm_df$derived_label)

cat("\nDerived (Weak) Labels:\n")
print(dtm_df$derived_label)

# Train model using derived labels
nb_derived <- naiveBayes(derived_label ~ . -label, data = dtm_df)
pred_derived <- predict(nb_derived, dtm_df)

cat("\nAccuracy using Derived Labels:\n")
print(mean(pred_derived == dtm_df$derived_label))


#############################
# 8. USING A DTM COLUMN AS LABEL (WORD-PREDICTION TASK)
#############################

# This is NOT text classification
# Task: Predict presence of a specific word using other words

target_word <- "market"

# Create binary label from DTM column
dtm_df$word_label <- ifelse(dtm_df[[target_word]] > 0, "Yes", "No")
dtm_df$word_label <- as.factor(dtm_df$word_label)

# IMPORTANT: Remove target word from predictors (avoid leakage)
word_pred_df <- dtm_df
word_pred_df[[target_word]] <- NULL

# Remove other labels
word_pred_df$label <- NULL
word_pred_df$derived_label <- NULL

# Train model
nb_word <- naiveBayes(word_label ~ ., data = word_pred_df)
pred_word <- predict(nb_word, word_pred_df)

cat("\nAccuracy for Word-Prediction Task:\n")
print(mean(pred_word == word_pred_df$word_label))


#############################
# 9. UNSUPERVISED LEARNING (NO LABELS)
#############################

### 9A. TOPIC MODELING (LDA)
lda_model <- LDA(dtm, k = 2)
lda_topics <- topics(lda_model)

cat("\nLDA Topic Assignment:\n")
print(lda_topics)


### 9B. DOCUMENT CLUSTERING (TF–IDF)
tfidf_matrix <- as.matrix(dtm_tfidf)
dist_matrix <- dist(tfidf_matrix)
hc <- hclust(dist_matrix)

clusters <- cutree(hc, k = 2)

cat("\nDocument Clusters:\n")
print(clusters)


#############################
# 10. KEY TAKEAWAYS (FOR EXAMS / PAPERS)
#############################

# 1. DTM columns represent FEATURES, not labels
# 2. Labels should ideally come from metadata
# 3. Labels derived from DTM are WEAK labels
# 4. Using a DTM column as label is a WORD-PREDICTION task
# 5. Always remove target word to avoid data leakage
# 6. If no labels exist, use LDA or clustering

#####################################################################
# END OF SCRIPT
#####################################################################
