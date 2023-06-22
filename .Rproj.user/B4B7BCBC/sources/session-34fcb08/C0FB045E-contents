# Install and load the gtrendsR package
install.packages("gtrendsR")
library(gtrendsR)

# Install and load the dplyr package for data manipulation
install.packages("dplyr")
library(dplyr)

# Install and load the caret package
install.packages("caret")
library(caret)

# Set up the keywords you want to search for
keywords <- c("Joe Biden", "Donald Trump", "Kamala Harris", "Mike Pence")
keywords_general <- c("COVID-19", "economy", "healthcare")
keywords_parties <- c("Democratic Party", "Republican Party")

# Define the time period for the data
time_period <- "2020-01-01 2020-11-03"

# Get Google Trends data
trends_data <- gtrends(keywords, geo = "US", time = time_period)

# HIER DATA ZU CSV FILE SPEICHERN

# Print the data
print(trends_data)


# DATA PREPROCESSING
# Remove rows where 'hits' is "<1"
interest_over_time <- trends_data$interest_over_time %>%
  filter(hits != "<1")

# Print the updated data
print(interest_over_time)


# BASIC COUNTING MODEL
# Ensure hits is numeric
interest_over_time$hits <- as.numeric(interest_over_time$hits)

# Summarize the data by candidate
summary <- interest_over_time %>%
  group_by(keyword) %>%
  summarise(total_searches = sum(hits, na.rm = TRUE))
print(summary)

# Predict the winner as the candidate with the most searches
winner <- summary %>%
  filter(total_searches == max(total_searches)) %>%
  pull(keyword)

print(paste("The predicted winner is:", winner))

# HIER ABSOLUTE DATEN FÜR G-TRENDS EINFÜGEN

# HIER RUNNING MATES UND TRUMP / BIDEN ZUSAMMENFÜGEN

# GEWICHTUNG NACH ACTUAL ELECTION DATA WIE IN DEM TWITTER DATA










# Assume you have a data frame: clean_data
# Create a time-based feature: change in 'Joe Biden' search interest from the previous week
clean_data <- clean_data %>%
  arrange(date) %>%
  mutate(biden_interest_change = `Joe Biden` - lag(`Joe Biden`))

# Create an interaction feature: 'Joe Biden' search interest * unemployment rate
clean_data$biden_unemployment_interaction = clean_data$`Joe Biden` * clean_data$unemployment_rate

# Create a polynomial feature: square of 'Joe Biden' search interest
clean_data$biden_interest_squared = clean_data$`Joe Biden`^2









# Assume you have a data frame: clean_data
# Assume the outcome variable is called "outcome"

# Split the data into a training set and a test set
set.seed(123)  # for reproducibility
train_index <- createDataPartition(clean_data$outcome, p = 0.8, list = FALSE)
train_set <- clean_data[train_index, ]
test_set <- clean_data[-train_index, ]

# Train a logistic regression model
model <- train(outcome ~ ., data = train_set, method = "glm", family = "binomial")