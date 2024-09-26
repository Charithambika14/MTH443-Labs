library(readr)
CHD <- read_csv("CHD.csv")
colSums(is.na(CHD))

# Replace NA with mean of totChol
chol <- CHD$totChol
ind <- which(is.na(chol))
mu <- mean(chol,na.rm = TRUE)
chol[ind] = mu
CHD$totChol <- chol
colSums(is.na(CHD))

## KNN
# Installing Packages 
install.packages("e1071") 
install.packages("caTools") 
install.packages("class") 

# Loading package 
library(e1071) 
library(caTools) 
library(class) 

## KNN
split <- sample.split(CHD$TenYearCHD, SplitRatio = 0.75)
train_set <- subset(CHD,split == TRUE)
test_set <- subset(CHD,split == FALSE)

## kernal density estimation
chol <- CHD$totChol
sysbp <- CHD$sysBP
diabp <- CHD$diaBP

hist(chol)
chol_r <- density(chol,kernel = "rectangular")
plot(chol_r, lwd = 2, main = "Rectangular kernel")
chol_t <- density(chol,kernel = "triangular")
plot(chol_t, lwd = 2, main = "triangular kernel")
chol_e <-  density(chol,kernel = "epanechnikov")
plot(chol_e, lwd = 2, main = "epanechnikov kernel")

hist(sysbp)
sysbp_r <- density(sysbp,kernel = "rectangular")
plot(sysbp_r, lwd = 2, main = "Rectangular kernel")
sysbp_t <- density(sysbp,kernel = "triangular")
plot(sysbp_t, lwd = 2, main = "triangular kernel")
sysbp_e <-  density(sysbp,kernel = "epanechnikov")
plot(sysbp_e, lwd = 2, main = "epanechnikov kernel")

hist(diabp)
diabp_r <- density(diabp,kernel = "rectangular")
plot(diabp_r, lwd = 2, main = "Rectangular kernel")
diabp_t <- density(diabp,kernel = "triangular")
plot(diabp_t, lwd = 2, main = "triangular kernel")
diabp_e <-  density(diabp,kernel = "epanechnikov")
plot(diabp_e, lwd = 2, main = "epanechnikov kernel")

## joint density estimate
library(MASS)
k <- kde2d(diabp,sysbp,n =50)
image(k)
