#### LAB - 7

install.packages("arules")
install.packages("arulesViz")

# Load libraries
library(arules)
library(arulesViz)

# Load the dataset
# Make sure to set the appropriate path to the dataset
transactions <- suppressWarnings(read.transactions("grocery_baskets.csv", sep = ",", format = "basket"))

# Part (a) Find the top 25 items with respect to support of items
itemFrequencyPlot(transactions, topN = 25, type = "absolute", main = "Top 25 Items by Support")

# Part (b) Use apriori algorithm to find association rules for the given minsup and minconf combinations
# Define the support and confidence levels
support_confidence_combinations <- list(
  c(0.1, 0.6),
  c(0.01, 0.2),
  c(0.05, 0.3),
  c(0.5, 0.7)
)

# Function to generate rules for given minsup and minconf
generate_rules <- function(sup, conf) {
  apriori(transactions, parameter = list(supp = sup, conf = conf))
}

# Generate rules for each combination and store in a list
rules_list <- lapply(support_confidence_combinations, function(x) {
  generate_rules(x[1], x[2])
})

# Part (c) Calculate LIFT for all generated rules
# Function to calculate and add lift to rules
calculate_lift <- function(rules) {
  # Add lift measure to each rule
  quality(rules)$lift <- interestMeasure(rules, measure = "lift", transactions = transactions)
  return(rules)
}

# Apply lift calculation to each set of rules
rules_list_with_lift <- lapply(rules_list, calculate_lift)

# Display rules for each minsup and minconf combination with lift values
for (i in 1:length(rules_list_with_lift)) {
  cat("Rules for minsup =", support_confidence_combinations[[i]][1], 
      "and minconf =", support_confidence_combinations[[i]][2], "\n")
  inspect(rules_list_with_lift[[i]])
  cat("\n\n")
}
