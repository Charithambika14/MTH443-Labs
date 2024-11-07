### LAB- 8
## (1) ~~ (3)

### NO NA and missing values
# Load required libraries
library(MASS) # for LDA and QDA
library(caret) # for data splitting and evaluation

################################################(1)###################################################
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


##############################################(3)##############################################
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

#########################################(2)#####################################################
# Load necessary libraries
library(dplyr)
library(MASS)  # For LDA and QDA

# Load the dataset
currency_crisis <- read.csv("currency_crisis.csv",header = TRUE)
labels <- currency_crisis[1,-1]
currency_crisis <- currency_crisis[-1,]
for(i in c(2:18)){
  currency_crisis[,i] <- as.numeric(currency_crisis[,i])
}

# Data Preparation: Check and handle missing values if necessary
# Assuming there are no missing values; if there are, handle them appropriately (e.g., by imputation).

# Define the classification variable as factor for LDA and QDA
currency_crisis$CRISIS.INDICATOR <- as.factor(currency_crisis$CRISIS.INDICATOR)

# Scenario (i): Split data with the last 10% of rows as the test set
set.seed(123)
n <- nrow(currency_crisis)
test_size <- floor(0.1 * n)
train_data_i <- currency_crisis[1:(n - test_size),-1]
test_data_i <- currency_crisis[(n - test_size + 1):n,-1]

# Scenario (ii): Split data with every 10th observation in the test set
test_indices_ii <- seq(10, n, by = 10)
train_data_ii <- currency_crisis[-test_indices_ii,-1]
test_data_ii <- currency_crisis[test_indices_ii,-1]

# Function to calculate misclassification rate
misclassification_rate <- function(pred, actual) {
  mean(pred != actual)
}

# (a) Fisher Linear Discriminant Analysis
# Train LDA model on both splits
lda_model_i <- lda(CRISIS.INDICATOR ~ ., data = train_data_i)
lda_model_ii <- lda(CRISIS.INDICATOR ~ ., data = train_data_ii)

# Predictions and misclassification rates for Scenario (i)
lda_pred_train_i <- predict(lda_model_i, train_data_i)$class
lda_pred_test_i <- predict(lda_model_i, test_data_i)$class
lda_train_misclassification_i <- misclassification_rate(lda_pred_train_i, train_data_i$CRISIS.INDICATOR)
lda_test_misclassification_i <- misclassification_rate(lda_pred_test_i, test_data_i$CRISIS.INDICATOR)

# Predictions and misclassification rates for Scenario (ii)
lda_pred_train_ii <- predict(lda_model_ii, train_data_ii)$class
lda_pred_test_ii <- predict(lda_model_ii, test_data_ii)$class
lda_train_misclassification_ii <- misclassification_rate(lda_pred_train_ii, train_data_ii$CRISIS.INDICATOR)
lda_test_misclassification_ii <- misclassification_rate(lda_pred_test_ii, test_data_ii$CRISIS.INDICATOR)

# (b) Quadratic Discriminant Analysis
# Train QDA model on both splits
qda_model_i <- qda(CRISIS.INDICATOR ~ ., data = train_data_i)
qda_model_ii <- qda(CRISIS.INDICATOR ~ ., data = train_data_ii)

# Predictions and misclassification rates for Scenario (i)
qda_pred_train_i <- predict(qda_model_i, train_data_i)$class
qda_pred_test_i <- predict(qda_model_i, test_data_i)$class
qda_train_misclassification_i <- misclassification_rate(qda_pred_train_i, train_data_i$CRISIS.INDICATOR)
qda_test_misclassification_i <- misclassification_rate(qda_pred_test_i, test_data_i$CRISIS.INDICATOR)

# Predictions and misclassification rates for Scenario (ii)
qda_pred_train_ii <- predict(qda_model_ii, train_data_ii)$class
qda_pred_test_ii <- predict(qda_model_ii, test_data_ii)$class
qda_train_misclassification_ii <- misclassification_rate(qda_pred_train_ii, train_data_ii$CRISIS.INDICATOR)
qda_test_misclassification_ii <- misclassification_rate(qda_pred_test_ii, test_data_ii$CRISIS.INDICATOR)

# Display the results
cat("Misclassification Rates:\n")
cat("LDA - Scenario (i): Train =", lda_train_misclassification_i, ", Test =", lda_test_misclassification_i, "\n")
cat("LDA - Scenario (ii): Train =", lda_train_misclassification_ii, ", Test =", lda_test_misclassification_ii, "\n")
cat("QDA - Scenario (i): Train =", qda_train_misclassification_i, ", Test =", qda_test_misclassification_i, "\n")
cat("QDA - Scenario (ii): Train =", qda_train_misclassification_ii, ", Test =", qda_test_misclassification_ii, "\n")
