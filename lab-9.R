## lab - 9
library(klaR)
library(psych)
library(MASS)
library(devtools)

#(1) 
iris <- read.csv("iris.csv")
set.seed(123)
ind <- sample(2, nrow(iris),replace = TRUE,prob = c(0.9, 0.1))
training <- iris[ind==1,]
testing <- iris[ind==2,]

#LINEAR
linear_iris <- lda(Species_name ~ ., training, prior = c(1,1,1)/3)
linear_iris

p_train <- predict(linear_iris,training)
ldahist(data = p_train$x, g = training$Species_name)

tab_train <- table(Predicted = p_train$class, Actual =  training$Species_name)
tab_train

p_test <- predict(linear_iris, testing)
tab_test <- table(Predicted = p_test$class, Actual = testing$Species_name)

misclassification_train <- 1 - (sum(diag(tab_train))/sum(tab_train))
misclassification_train

accuracy_train <- (sum(diag(tab_train))/sum(tab_train))
accuracy_train

misclassification_test <- 1 - (sum(diag(tab_test))/sum(tab_test))
misclassification_test

accuracy_test <- (sum(diag(tab_test))/sum(tab_test))
accuracy_test

#QUADRATIC
quadratic_iris <- qda(Species_name ~ ., training, prior=c(1,1,1)/3)
quadratic_iris

p_train_qda <- predict(quadratic_iris,training)
tab_train_qda <- table(Predicted = p_train_qda$class, Actual = training$Species_name)
acc_train_qda <- (sum(diag(tab_train_qda))/sum(tab_train_qda))
acc_train_qda
missclass_train_qda <- 1 - acc_train_qda
missclass_train_qda

p_test_qda <- predict(quadratic_iris,testing)
tab_test_qda <- table(Predicted = p_test_qda$class, Actual = testing$Species_name)
acc_test_qda <- (sum(diag(tab_test_qda))/sum(tab_test_qda))
acc_test_qda
missclass_test_qda <- 1 - acc_test_qda
missclass_test_qda

#BAYESIAN
library(naivebayes)
X_train <- training[,2:5]
y_train <- training$Species_name
X_test <- testing[,2:5]
y_test <- testing$Species_name

d <- naive_bayes(X_train,y_train)
summary(d)
table(d,1)
