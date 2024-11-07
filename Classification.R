### LAB- 8
## (1) ~~ (3)

### NO NA and missing values
# Load required libraries
library(MASS) # for LDA and QDA
library(caret) # for data splitting and evaluation

###(1)###
# Load the dataset
data <- read.csv("heart.csv")

# Split data into training and test sets (last 10% as test data)
n <- nrow(data)
train_index <- 1:(n * 0.9)
test_index <- (n * 0.9 + 1):n

train_data <- data[train_index, ]
test_data <- data[test_index, ]

# (a) Fisher Linear Discriminant Analysis (LDA)
lda_model <- lda(coronary.hd ~ ., data = train_data)

# Predictions and misclassification rate for training data
lda_train_pred <- predict(lda_model, train_data)$class
lda_train_misclassification <- mean(lda_train_pred != train_data$coronary.hd)
lda_train_misclassification

# Predictions and misclassification rate for test data
lda_test_pred <- predict(lda_model, test_data)$class
lda_test_misclassification <- mean(lda_test_pred != test_data$coronary.hd)
lda_test_misclassification

# (b) Quadratic Discriminant Analysis (QDA)
qda_model <- qda(coronary.hd ~ ., data = train_data)

# Predictions and misclassification rate for training data
qda_train_pred <- predict(qda_model, train_data)$class
qda_train_misclassification <- mean(qda_train_pred != train_data$coronary.hd)
qda_train_misclassification

# Predictions and misclassification rate for test data
qda_test_pred <- predict(qda_model, test_data)$class
qda_test_misclassification <- mean(qda_test_pred != test_data$coronary.hd)
qda_test_misclassification

# Print misclassification rates
cat("Misclassification rates:\n")
cat("LDA - Training Data:", lda_train_misclassification, "\n")
cat("LDA - Test Data:", lda_test_misclassification, "\n")
cat("QDA - Training Data:", qda_train_misclassification, "\n")
cat("QDA - Test Data:", qda_test_misclassification, "\n")


###(3)###
# Load necessary libraries
library(MASS)
library(dplyr)

# Load the wine data
wine_data <- read.csv("wine.csv")
wine_data <- wine_data[,c(-15:-33)]

## NA and missing values present
# Fill NA values in numeric columns with column means
wine_data <- wine_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Define a function to get the mode of a character column
get_mode <- function(x) {
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

# Fill NA values in the 'Type' column with the mode
wine_data <- wine_data %>%
  mutate(Type = ifelse(Type == "", get_mode(Type[1:130), Type))

#wine_data <- wine_data %>%
#  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Define the last 10% of rows as the test set
set.seed(42)  # For reproducibility
n <- nrow(wine_data)
train_indices <- 1:(0.9 * n)
test_indices <- (0.9 * n + 1):n

# Split data into training and test sets
train_data <- wine_data[train_indices, ]
test_data <- wine_data[test_indices, ]

# Extract features and labels
train_features <- train_data[, -which(names(wine_data) == "Type")]
train_labels <- train_data$Type
test_features <- test_data[, -which(names(wine_data) == "Type")]
test_labels <- test_data$Type

# (a) Fisher Linear Discriminant Analysis (LDA)
lda_model <- lda(Type ~ ., data = train_data)

# Predict on training and test data
lda_train_pred <- predict(lda_model, train_features)$class
lda_test_pred <- predict(lda_model, test_features)$class

# Calculate misclassification rates for LDA
lda_train_misclassification_rate <- mean(lda_train_pred != train_labels)
lda_test_misclassification_rate <- mean(lda_test_pred != test_labels)

cat("LDA Misclassification Rate - Training:", lda_train_misclassification_rate, "\n")
cat("LDA Misclassification Rate - Test:", lda_test_misclassification_rate, "\n")

# (b) Quadratic Discriminant Analysis (QDA)
qda_model <- qda(Type ~ ., data = train_data)

# Predict on training and test data
qda_train_pred <- predict(qda_model, train_features)$class
qda_test_pred <- predict(qda_model, test_features)$class

# Calculate misclassification rates for QDA
qda_train_misclassification_rate <- mean(qda_train_pred != train_labels)
qda_test_misclassification_rate <- mean(qda_test_pred != test_labels)

cat("QDA Misclassification Rate - Training:", qda_train_misclassification_rate, "\n")
cat("QDA Misclassification Rate - Test:", qda_test_misclassification_rate, "\n")

