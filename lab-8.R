## lab - 8
#(1)

heart <- read.csv("Heart.csv")

library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)

set.seed(123)
ind <- sample(2, nrow(heart),replace = TRUE,prob = c(0.9, 0.1))
training <- heart[ind==1,]
testing <- heart[ind==2,]

# Linear discriminant analysis
linear <- lda(coronary.hd ~ .,training)
linear

# Histogram
p <- predict(linear,training)
ldahist(data = p$x, g = training$coronary.hd)
## overlap in between [0,1] have a choose of misclassification

# Confusion matrix [0 -> 1, 1 -> 2]
p1 <- p$class
tab_train <- table(Predicted = p1, Actual =  training$coronary.hd)
tab_train

p2 <- predict(linear, testing)$class
tab_test <- table(Predicted = p2, Actual =  testing$coronary.hd)
tab_test

# p1 = 0.66 , p2 = 0.33
## Misclassification rate = 1 - accuracy
# Accuracy = sum(diag(tab))/sum(tab)

misclassification_train = 1 - (sum(diag(tab_train))/sum(tab_train))
misclassification_train

misclassification_test = 1 - (sum(diag(tab_test))/sum(tab_test))
misclassification_test

## Qudaratic discriminant function
quadratic <- qda(coronary.hd ~ .,training)
quadratic

p_qda_tr <- predict(quadratic,training)
tab_train_qda <- table(Predicted = p_qda_tr$class, Actual =  training$coronary.hd)
tab_train_qda
misclass_train_qda = 1 - (sum(diag(tab_train_qda))/sum(tab_train_qda))
misclass_train_qda

p_qda_ts <- predict(quadratic,testing)
tab_test_qda <- table(Predicted = p_qda_ts$class, Actual =  testing$coronary.hd)
tab_test_qda
misclass_test_qda = 1 - (sum(diag(tab_test_qda))/sum(tab_test_qda))
misclass_test_qda

method <- c("Linear","Quadratic")
misclass_train <- c(misclassification_train, misclass_train_qda)
misclass_test <- c(misclassification_test,misclass_test_qda)

result <- data.frame(method, misclass_train, misclass_test)
result

##(2)
currency <- read.csv("currency_crisis.csv",header = FALSE)
set.seed(123)
ind <- sample(2, nrow(heart),replace = TRUE,prob = c(0.9, 0.1))
training <- heart[ind==1,]
testing <- heart[ind==2,]




