install.packages("FEC")
library(FEC)

# Set your API key obtained from the FEC website
fec_key <- "zcH1NS9ReajvU8zMDUmPaDkriLhLT700Yu9Bjmjx"

# Set the election cycle to 2020 presidential election
fec_set_key(fec_key)
fec_set_cycle("2020")

# Get presidential candidate data
presidential_candidates <- get_candidates(state = "", office = "P", election_year = "2020")

# Create an empty data frame to store financial campaign data
financial_data <- data.frame()

# Iterate through each candidate
for (i in 1:length(presidential_candidates$id)) {
  candidate_id <- presidential_candidates$id[i]
  
  # Get financial summary for the candidate
  financial_summary <- get_candidate_financial_summary(candidate_id)
  
  # Extract relevant information from financial summary
  candidate_data <- data.frame(
    candidate_id = candidate_id,
    candidate_name = presidential_candidates$name[i],
    receipts = financial_summary$receipts,
    disbursements = financial_summary$disbursements,
    cash_on_hand = financial_summary$cash_on_hand
  )
  
  # Append candidate data to the financial_data data frame
  financial_data <- rbind(financial_data, candidate_data)
}

# Convert numeric columns to appropriate data types
financial_data$receipts <- as.numeric(financial_data$receipts)
financial_data$disbursements <- as.numeric(financial_data$disbursements)
financial_data$cash_on_hand <- as.numeric(financial_data$cash_on_hand)

# Remove rows with missing values
financial_data <- financial_data[complete.cases(financial_data), ]

# Create a binary variable indicating the election outcome
financial_data$outcome <- ifelse(financial_data$candidate_name == "WinnerCandidateName", 1, 0)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(financial_data), 0.7 * nrow(financial_data))
train_data <- financial_data[train_indices, ]
test_data <- financial_data[-train_indices, ]

# Build a logistic regression model
model <- glm(outcome ~ receipts + disbursements + cash_on_hand, data = train_data, family = "binomial")

# Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "response")

# Convert predicted probabilities to binary predictions
predicted_outcome <- ifelse(predictions > 0.5, 1, 0)

# Calculate the accuracy of the model
accuracy <- sum(predicted_outcome == test_data$outcome) / nrow(test_data)
