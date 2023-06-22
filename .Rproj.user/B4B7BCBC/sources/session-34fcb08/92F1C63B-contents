# Install necessary packages
install.packages(c("rtweet", "dplyr", "syuzhet"))

# Load necessary packages
library(rtweet)
library(dplyr)
library(syuzhet)


# CODE TO ACCESS REAL TWITTER DATA
# Authenticate with Twitter API
## Replace with your own API keys
twitter_token <- create_token(
  app = "your_app_name",
  consumer_key = "your_consumer_key",
  consumer_secret = "your_consumer_secret",
  access_token = "your_access_token",
  access_secret = "your_access_secret"
)

# Define the search terms (the candidates' names)
search_terms <- c("Joe Biden", "Donald Trump")

# Collect tweets related to the election
tweets <- search_tweets(
  paste(search_terms, collapse = " OR "), 
  n = 10000, 
  include_rts = FALSE,
  retryonratelimit = TRUE
)

# Extract the text of the tweets
tweet_text <- tweets$text

# Perform sentiment analysis on the tweets
sentiments <- get_nrc_sentiment(tweet_text)

# Add the sentiment scores to the tweets data frame
tweets <- cbind(tweets, sentiments)

# Calculate the age of each tweet in days
tweets$age <- as.numeric(difftime(Sys.Date(), tweets$created_at, units = "days"))

# Calculate a time decay factor for each tweet
tweets$time_decay <- exp(-0.1 * tweets$age)

# Adjust the sentiment scores based on the time decay factor
tweets$adjusted_sentiment <- tweets$positive * tweets$time_decay - tweets$negative * tweets$time_decay

# Calculate the total adjusted sentiment score for each candidate
sentiment_scores <- sapply(search_terms, function(x) {
  sum(tweets$adjusted_sentiment[grepl(x, tweets$text, ignore.case = TRUE)])
})

# Normalize the sentiment scores by the total number of tweets
sentiment_scores <- sentiment_scores / nrow(tweets)

## Calculate the total sentiment score for each candidate
# sentiment_scores <- sapply(search_terms, function(x) {
#  sum(tweets$positive[grepl(x, tweets$text, ignore.case = TRUE)]) -
#    sum(tweets$negative[grepl(x, tweets$text, ignore.case = TRUE)])
#})

# Create a data frame with the results
results <- data.frame(
  candidate = search_terms,
  sentiment_score = sentiment_scores
)

# Print the sentiment scores for each candidate
print(results)

# Predict the winner as the candidate with the highest sentiment score
winner <- results %>%
  filter(sentiment_score == max(sentiment_score)) %>%
  pull(candidate)

print(paste("The predicted winner is:", winner))








# CODE USING FAKE TWITTER DATA
# Load necessary packages
library(dplyr)

# Define the number of synthetic tweets
n_tweets <- 1000

# Generate synthetic tweet text
set.seed(123)  # for reproducibility
tweet_text <- sample(c("Joe Biden", "Donald Trump"), n_tweets, replace = TRUE)

# Generate synthetic sentiment scores
positive_scores <- rnorm(n_tweets, mean = 5, sd = 2)
negative_scores <- rnorm(n_tweets, mean = 5, sd = 2)

# Create a data frame with the synthetic tweets
tweets <- data.frame(
  text = tweet_text,
  positive = ifelse(tweet_text == "Joe Biden", positive_scores + rnorm(n_tweets, mean = 0, sd = 1), positive_scores),
  negative = ifelse(tweet_text == "Joe Biden", negative_scores, negative_scores + rnorm(n_tweets, mean = 0, sd = 1))
)

# Calculate the total sentiment score for each candidate
sentiment_scores <- sapply(c("Joe Biden", "Donald Trump"), function(x) {
  sum(tweets$positive[grepl(x, tweets$text, ignore.case = TRUE)]) -
    sum(tweets$negative[grepl(x, tweets$text, ignore.case = TRUE)])
})

# Create a data frame with the results
results <- data.frame(
  candidate = c("Joe Biden", "Donald Trump"),
  sentiment_score = sentiment_scores
)

# Print the sentiment scores for each candidate
print(results)

# Predict the winner as the candidate with the highest sentiment score
winner <- results %>%
  filter(sentiment_score == max(sentiment_score)) %>%
  pull(candidate)

print(paste("The predicted winner is:", winner))





