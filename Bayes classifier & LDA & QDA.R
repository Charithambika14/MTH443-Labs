################ LAB - 9 ###################
#################(1)######################
library(dplyr)
library(MASS)

iris <- read.csv("iris.csv")

## Randomly sample 10% of data
indies <- sample(2,nrow(iris),replace=TRUE,prob=c(0.9,0.1))
train_data <- iris[indies == 1,]
test_data <- iris[indies == 2, ]

########################## LDA ###########################################
lda_model <- lda(Species_name ~ ., data = train_data,prior = c(1,1,1)/3)
lda_train <- predict(lda_model,train_data)$class
lda_test <- predict(lda_model, test_data)$class

## misclassification
lda_train_misclass <- mean(lda_train != train_data$Species_name)
lda_test_misclass <- mean(lda_test != test_data$Species_name)

######################### QDA #################################
qda_model <- qda(Species_name ~ ., data = train_data,prior = c(1,1,1)/3)
qda_train <- predict(qda_model,train_data)$class
qda_test <- predict(qda_model, test_data)$class

## misclassification
qda_train_misclass <- mean(qda_train != train_data$Species_name)
qda_test_misclass <- mean(qda_test != test_data$Species_name)

#################### Bayes Classifer ###############################
# Kernel Density Naive Bayes

train_data$Species_name <- as.factor(train_data$Species_name)
test_data$Species_name <- as.factor(test_data$Species_name)

*********************library(klaR)*****************************************
nb_model <- NaiveBayes(Species_name ~ ., data = train_data, usekernel = TRUE,prior = c(1,1,1)/3)

# Predict on training and test sets
nb_train_pred <- predict(nb_model, train_data)$class
nb_test_pred <- predict(nb_model, test_data)$class

# Misclassification rates
nb_train_error <- mean(nb_train_pred != train_data$Species_name)
nb_test_error <- mean(nb_test_pred != test_data$Species_name)

cat("Bayes Misclassification Rate - Training Set:", nb_train_error, "\n")
cat("Bayes Misclassification Rate - Test Set:", nb_test_error, "\n")


##############(2)##################
library(dplyr)
library(MASS)

wine <- read.csv("wine_italy.csv")

## Randomly sample 10% of data
indies <- sample(2,nrow(wine),replace=TRUE,prob=c(0.9,0.1))
train_data <- wine[indies == 1,]
test_data <- wine[indies == 2, ]

train_data$Type <- as.factor(train_data$Type)
test_data$Type <- as.factor(test_data$Type)

########################## LDA ####################################
lda_model <- lda(Type ~ ., data = train_data,prior = c(1,1,1)/3)
lda_train <- predict(lda_model,train_data)$class
lda_test <- predict(lda_model, test_data)$class

## misclassification
lda_train_misclass <- mean(lda_train != train_data$Type)
lda_test_misclass <- mean(lda_test != test_data$Type)

########################## QDA ####################################
qda_model <- qda(Type ~ ., data = train_data,prior = c(1,1,1)/3)
qda_train <- predict(qda_model,train_data)$class
qda_test <- predict(qda_model, test_data)$class

## misclassification
qda_train_misclass <- mean(qda_train != train_data$Type)
qda_test_misclass <- mean(qda_test != test_data$Type)

################# Bayes Classifer ###############################
# Kernel Density Naive Bayes
library(klaR)
nb_model <- NaiveBayes(Type ~ ., data = train_data, usekernel = TRUE,prior = c(1,1,1)/3)

# Predict on training and test sets
nb_train_pred <- predict(nb_model, train_data)$class
nb_test_pred <- predict(nb_model, test_data)$class

# Misclassification rates
nb_train_error <- mean(nb_train_pred != train_data$Type)
nb_test_error <- mean(nb_test_pred != test_data$Type)

cat("Bayes Misclassification Rate - Training Set:", nb_train_error, "\n")
cat("Bayes Misclassification Rate - Test Set:", nb_test_error, "\n")
