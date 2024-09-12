library(ggplot2)
library(aplpack)
library(dplyr)
library(factoextra)
library(dendextend)

#(a)
crime_data <- read.csv("cyber_crime_2020.csv")
# remove the States columns for pca analysis
crime_num <- crime_data[,-1]
crime_num <- scale(crime_num)
pca <- prcomp(crime_num, center = TRUE, scale. = TRUE)
summary(pca)

pca_df <- as.data.frame(pca$x)
pca_df$States <- crime_data$State.UT

mahalanobis_dist <- mahalanobis(pca_df[,1:2],colMeans(pca_df[,1:2]),cov(pca_df[,1:2]))
thershold <- qchisq(0.95,df=2)
pca_df$Distance <- mahalanobis_dist

ggplot(pca_df, aes(x = PC1, y = PC2, label = States)) +
  geom_point(aes(color = Distance)) + 
  xlim(-2.75, 2.5) + ylim(-2.5, 2.5) +
  geom_text(vjust = 1, hjust = 1, size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "PCA Projection with Outliers and Threshold Circle", x = "PC1", y = "PC2") +
  theme_minimal()


#(b)
outliers <- which(mahalanobis_dist > thershold)
### OUTLIERS
crime_data$State.UT[outliers]

#(c)
# Screen plot
fviz_eig(pca,addlabels = TRUE, ylim = c(0,70))
# we can consider first 5 PC for efficient data dimension reduction 
# as the total variance is approx. 93% of original variance of given data

#(d)
# original data
hc_complete <- hclust(dist(crime_num), method = "complete")
plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "", cex = 0.6)

#(e)
set.seed(123)
crime_clean <- crime_data[-outliers,]
crime_clean_data <- scale(crime_clean[,-1])
pca <- prcomp(crime_clean_data)
pca_df1 <- as.data.frame(pca$x)
pca_df1$States <- crime_clean$State.UT
kmeans_result <- kmeans(crime_clean_data, centers = 4, nstart = 10)
pca_df1$Cluster <- kmeans_result$cluster

ggplot(pca_df1, aes(x = PC1, y = PC2, color = Cluster, label = States)) +
  geom_point() +
  xlim(-6,5) + ylim(-6,5)
  geom_text(vjust = 1, hjust = 1, size = 4) +
  labs(title = "PCA Projection with K-means Clustering", x = "PC1", y = "PC2") +
  theme_minimal()

any(is.nan(pca_df1)) 




