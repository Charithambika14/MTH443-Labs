# lab - 1
# Load packages
library(aplpack)
library(dplyr)
###(1)
# Load the dataset
eco_data <- read.csv("eco_dev_data.csv")

# Check the structure and first few rows of the dataset
str(eco_data)
head(eco_data)

# Select relevant numeric columns (adjust column names as needed)
# Assuming the first column is the country name, and the rest are indicators
numeric_data <- eco_data %>% select(-Country)

# Scale the data to ensure fair representation of different features
scaled_data <- scale(numeric_data)

# Check the first few rows of the scaled data
head(scaled_data)

# Generate Chernoff faces
faces(scaled_data, 
      labels = eco_data$Country,  # Add country names as labels
      main = "Chernoff Faces for World Economic Development")

# Perform k-means clustering (e.g., with 3 clusters)
set.seed(123)  # For reproducibility
clusters <- kmeans(scaled_data, centers = 3)

# Add cluster information to the original data
eco_data$Cluster <- as.factor(clusters$cluster)

# Visualize Chernoff faces with clustering labels
faces(scaled_data, 
      labels =eco_data$Cluster, 
      main = "Chernoff Faces with Cluster Labels")

###(2)
wine <- read.csv("Wine_data.csv")
wine <- wine %>% select(-X,-X.1,-X.2,-X.3,-X.4,-X.5,-X.6,-X.7,-X.8,-X.9,-X.10,-X.11,-X.12,-X.13,-X.14,-X.15,-X.16,-X.17,-X.18)
numeric_data <- wine %>% select(-Type)
scaled_data <- scale(numeric_data)
faces(scaled_data[1:100,], 
      labels = wine[1:100, ]$Type, 
      main = "Chernoff Faces for Wine Data")

# Perform k-means clustering (e.g., with 3 clusters)
set.seed(123)  # For reproducibility
clusters <- kmeans(scaled_data[1:100, ], centers = 3)
## A = 2, B = 3, C = 1

# Add cluster information to the original data
wine[1:100, ]$Cluster <- as.factor(clusters$cluster)

# Visualize Chernoff faces with clustering labels
faces(scaled_data[1:100, ], 
      labels =wine[1:100, ]$Cluster, 
      main = "Chernoff Faces with Cluster Labels")

###(3)
bank <- read.csv("PS_bank_fin_ratio.csv")
bank <- bank[-109,]
years <- c("96-97", "97-98", "98-99", "99-00")
bank <- data.frame(bank)
Years <- rep(years, lenght.out = nrow(bank))
bank$Year <- Years

# Separate data by financial year (adjust column names based on your dataset)
data_96_97 <- bank %>% filter(Year == "96-97") %>% select(-Banks,-Year)
data_97_98 <- bank %>% filter(Year == "97-98") %>% select(-Banks,-Year)
data_98_99 <- bank %>% filter(Year == "98-99") %>% select(-Banks,-Year)
data_99_00 <- bank %>% filter(Year == "99-00") %>% select(-Banks,-Year)

# Scale the data for each year (standardizing data)
scaled_96_97 <- scale(data_96_97)
scaled_97_98 <- scale(data_97_98)
scaled_98_99 <- scale(data_98_99)
scaled_99_00 <- scale(data_99_00)

# Chernoff faces for 1996-1997
faces(scaled_96_97, 
      labels = bank$Banks[bank$Year == "96-97"], 
      main = "Chernoff Faces for 1996-1997")

# Chernoff faces for 1997-1998
faces(scaled_97_98, 
      labels = bank$Banks[bank$Year == "97-98"], 
      main = "Chernoff Faces for 1997-1998")

# Chernoff faces for 1998-1999
faces(scaled_98_99, 
      labels = bank$Banks[bank$Year == "98-99"], 
      main = "Chernoff Faces for 1998-1999")

# Chernoff faces for 1999-2000
faces(scaled_99_00, 
      labels = bank$Banks[bank$Year == "99-00"], 
      main = "Chernoff Faces for 1999-2000")

for (i in seq(1, 27)){
  data <- bank[i:i+3, ] %>% select(-Banks,-Year)
  data <- scale(data)
  faces(data,
      labels = c("1996-1997", "1997-1998", "1998-1999", "1999-2000"),
      main = paste("Chernoff faces for ",))
}

# lab - 2
library(stats)
library(factoextra)
library(dplyr)
library(outliers)
library(ggplot2)

##(1)
eco <- read.csv("eco_dev_data.csv")
eco_num <- eco[,-1] # remove char country
pca <- prcomp(eco_num, center = TRUE, scale. = TRUE)
summary(pca)

pca_df <- as.data.frame(pca$x)
pca_df$Country <- eco$Country

dev.off()  
# Plot PCA projection with country labels
ggplot(pca_df, aes(x = PC1, y = PC2, label = Country)) +
  geom_point(aes(color = PC1)) + 
  xlim(-5,5) + ylim(-6,6) +
  geom_text(vjust = 1, hjust = 1, size = 3) +
  labs(title = "PCA Projection of Countries", x = "PC1", y = "PC2") +
  theme_minimal()

pca_var <- summary(pca)$importance[2,]

biplot(pca)
fviz_eig(pca,addlabels = TRUE, ylim = c(0,30))
fviz_cos2(pca, choice = "var", axes = 1:2)


# Calculate Mahalanobis distance to identify outliers
mahalanobis_distance <- mahalanobis(pca_df[,1:2], center =  colMeans(pca_df[,1:2]), cov = cov(pca_df[,1:2]))

# Add Mahalanobis distance to the data
pca_df$Distance <- mahalanobis_distance

thersold <- qchisq(0.95, df = 2)
outliers <- which(mahalanobis_distance > thersold)

library(ggforce)
# Visualize outliers (extreme distances)
ggplot(pca_df, aes(x = PC1, y = PC2, label = Country)) +
  geom_point(aes(color = Distance)) + 
  xlim(-7.5, 7.5) + ylim(-7.5, 7.5) +
  geom_text(vjust = 1, hjust = 1, size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_circle(aes(x0 = -3.305785e-11 , y0 = 3.305785e-11, r = thersold), color = "black", linetype = "dashed") +
  labs(title = "PCA Projection with Outliers and Threshold Circle", x = "PC1", y = "PC2") +
  theme_minimal()


# Perform k-means clustering on the first two principal components
set.seed(123)
kmeans_result <- kmeans(pca_df[,1:2], centers = 3)  # Assuming 3 clusters

# Add cluster information to the PCA data
pca_df$Cluster <- as.factor(kmeans_result$cluster)

# Plot the clusters on PCA projection
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster, label = Country)) +
  geom_point() +
  geom_text(vjust = 1, hjust = 1, size = 3) +
  labs(title = "PCA Projection with K-means Clustering", x = "PC1", y = "PC2") +
  theme_minimal()

##(2)
car <- read.csv("cars_data.csv")
car_num <- car[,-1]
pca <- prcomp(car_num, center = TRUE, scale. = TRUE)
summary(pca)

pca_df <- as.data.frame(pca$x)
pca_df$Car.model <- car$Car.Model
# Plot PCA projection with country labels
ggplot(pca_df, aes(x = PC1, y = PC2, label = Car.model)) +
  geom_point(aes(color = PC1)) + 
  xlim(-6.5,5) + ylim(-4.5,5) +
  geom_text(vjust = 1, hjust = 1, size = 3) +
  labs(title = "PCA Projection of Countries", x = "PC1", y = "PC2") +
  theme_minimal()

pca_var <- summary(pca)$importance[2,]
pca_var
biplot(pca)
fviz_eig(pca,addlabels = TRUE, ylim = c(0,70))
fviz_cos2(pca, choice = "var", axes = 1:2)

# Calculate Mahalanobis distance to identify outliers
mahalanobis_distance <- mahalanobis(pca_df[,1:2], center =  colMeans(pca_df[,1:2]), cov = cov(pca_df[,1:2]))

# Add Mahalanobis distance to the data
pca_df$Distance <- mahalanobis_distance

# Visualize outliers (extreme distances)
ggplot(pca_df, aes(x = PC1, y = PC2, label = Car.model)) +
  geom_point(aes(color = Distance)) +
  xlim(-6.5,5) + ylim(-4.5,5) +
  geom_text(vjust = 1, hjust = 1, size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "PCA Projection with Outliers", x = "PC1", y = "PC2") +
  theme_minimal()

# Perform k-means clustering on the first two principal components
set.seed(123)
kmeans_result <- kmeans(pca_df[,1:2], centers = 3)  # Assuming 3 clusters

# Add cluster information to the PCA data
pca_df$Cluster <- as.factor(kmeans_result$cluster)

# Plot the clusters on PCA projection
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster, label = Car.model)) +
  geom_point() +
  xlim(-6.5,5) + ylim(-4.5,5) +
  geom_text(vjust = 1, hjust = 1, size = 3) +
  labs(title = "PCA Projection with K-means Clustering", x = "PC1", y = "PC2") +
  theme_minimal()

##(3)
protein <- read.csv("eur_protein_consump.csv")
protein_num <- protein[,-1]
pca <- prcomp(protein_num, center = TRUE, scale. = TRUE)
summary(pca)

pca_df <- as.data.frame(pca$x)
pca_df$Country <- protein$Country
# Plot PCA projection with country labels
ggplot(pca_df, aes(x = PC1, y = PC2, label = Country)) +
  geom_point(aes(color = PC1)) + 
  xlim(-3.5,4.5) + ylim(-2.5,5) +
  geom_text(vjust = 1, hjust = 1, size = 3) +
  labs(title = "PCA Projection of Countries", x = "PC1", y = "PC2") +
  theme_minimal()

pca_var <- summary(pca)$importance[2,]
pca_var
biplot(pca)
fviz_eig(pca,addlabels = TRUE, ylim = c(0,50))
fviz_cos2(pca, choice = "var", axes = 1:2)

# Calculate Mahalanobis distance to identify outliers
mahalanobis_distance <- mahalanobis(pca_df[,1:2], center =  colMeans(pca_df[,1:2]), cov = cov(pca_df[,1:2]))

# Add Mahalanobis distance to the data
pca_df$Distance <- mahalanobis_distance

# Visualize outliers (extreme distances)
ggplot(pca_df, aes(x = PC1, y = PC2, label = Country)) +
  geom_point(aes(color = Distance)) +
  xlim(-3.5,4.5) + ylim(-2.5,5) +
  geom_text(vjust = 1, hjust = 1, size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "PCA Projection with Outliers", x = "PC1", y = "PC2") +
  theme_minimal()

# Perform k-means clustering on the first two principal components
set.seed(123)
kmeans_result <- kmeans(pca_df[,1:2], centers = 3)  # Assuming 3 clusters

# Add cluster information to the PCA data
pca_df$Cluster <- as.factor(kmeans_result$cluster)

# Plot the clusters on PCA projection
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster, label = Country)) +
  geom_point() +
  xlim(-3.5,4.5) + ylim(-2.5,5) +
  geom_text(vjust = 1, hjust = 1, size = 3) +
  labs(title = "PCA Projection with K-means Clustering", x = "PC1", y = "PC2") +
  theme_minimal()

##(4)
# bank one, same as the (1) find pca, biplot, outliers, k-clustering

# lab - 3
library(factoextra)
library(dplyr)
library(ggplot2)
library(mvnormtest)
library(MASS)

# Load the dataset
eco_data <- read.csv("eco_dev_data.csv")

# Preview the dataset
str(eco_data)
head(eco_data)

# Remove the identifier column for PCA
eco_data_numeric <- eco_data[ ,-1]

# Perform PCA on the dataset
pca_result <- prcomp(eco_data_numeric, center = TRUE, scale. = TRUE)

# View PCA summary
summary(pca_result)

# Calculate Mahalanobis distance on the first two PCs
pca_scores <- as.data.frame(pca_result$x)
mahalanobis_distance <- mahalanobis(pca_scores[,1:2], colMeans(pca_scores[,1:2]), cov(pca_scores[,1:2]))

# Define a threshold for outliers (95% quantile of Chi-squared distribution)
threshold <- qchisq(0.95, df = 2)

# Identify outliers
outliers <- which(mahalanobis_distance > threshold)

# Remove outliers from the dataset
eco_data_clean <- eco_data[-outliers, ]

# Perform PCA again on cleaned data
eco_data_clean_numeric <- eco_data_clean[,-1]
pca_result_clean <- prcomp(eco_data_clean_numeric, center = TRUE, scale. = TRUE)
fviz_cos2(pca_result_clean, choice = "var", axes = 1:2)

# Rank countries based on the first principal component
pca_scores_clean <- as.data.frame(pca_result_clean$x)
pca_scores_clean$Country <- eco_data_clean$Country
pca_scores_clean <- pca_scores_clean %>% arrange(desc(PC1))  # Rank by PC1

# View the PCA-based ranking of countries
head(pca_scores_clean)
pca_scores_clean$Country

# Visualize the variable loadings (feature clusters)
fviz_pca_var(pca_result_clean, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE) +
  labs(title = "Feature Clusters using PCA")

# Plot the distribution of the first few principal components to check for normality
ggplot(pca_scores_clean, aes(x = PC1)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
  geom_density(color = "red") +
  labs(title = "Distribution of PC1 (Checking for Normality)", x = "PC1", y = "Density")

ggplot(pca_scores_clean, aes(x = PC2)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
  geom_density(color = "red") +
  labs(title = "Distribution of PC2 (Checking for Normality)", x = "PC2", y = "Density")

# Q-Q plot for the first two principal components
qqnorm(pca_scores_clean$PC1)
qqline(pca_scores_clean$PC1, col = "red")

qqnorm(pca_scores_clean$PC2)
qqline(pca_scores_clean$PC2, col = "red")


#install.packages("MVN")
#library(MVN)
# Perform Henze-Zirkler's test for multivariate normality on the cleaned dataset
#hz_test <- mvn(eco_data_clean_numeric, mvnTest = "hz")
#print(hz_test$multivariateNormality)
# Interpretation:
# H0: normal not follows v/s Ha: normal follows
# If the p-value is greater than 0.05, you fail to reject the null hypothesis, meaning the data follows a multivariate normal distribution.
# If the p-value is less than 0.05, the data does not follow a multivariate normal distribution.
# Perform Royston's test for multivariate normality
#royston_test <- mvn(eco_data_clean_numeric, mvnTest = "royston")
#print(royston_test$multivariateNormality)

##(2)
bank <- read.csv("PS_bank_fin_ratio.csv")
bank <- bank[-109,]
bank_num <- bank[,-1]
pca <- prcomp(bank_num, center = TRUE, scale. = TRUE)
summary(pca)

pca_df <- as.data.frame(pca$x)
pca_df$Banks <- bank$Banks
mahalanobis_distance <- mahalanobis(pca_df[,1:2], colMeans(pca_df[,1:2]), cov(pca_df[,1:2]))
threshold <- qchisq(0.95, df = 2)
outliers <- which(mahalanobis_distance > threshold)

bank_clean <- bank[-outliers,]
bank_clean_num <- bank_clean[,-1]
pca_clean <- prcomp(bank_clean_num,center = TRUE,scale. = TRUE)
summary(pca_clean)
fviz_cos2(pca_clean, choice = "var", axes = 1:2)

# Rank countries based on the first principal component
pca_scores_clean <- as.data.frame(pca_clean$x)
pca_scores_clean$Banks <- bank_clean$Banks
pca_scores_clean <- pca_scores_clean %>% arrange(desc(PC1))  # Rank by PC1
head(pca_scores_clean)
pca_scores_clean$Banks

# Visualize the variable loadings (feature clusters)
fviz_pca_var(pca_clean, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE) +
  labs(title = "Feature Clusters using PCA")

# Plot the distribution of the first few principal components to check for normality
ggplot(pca_scores_clean, aes(x = PC1)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
  geom_density(color = "red") +
  labs(title = "Distribution of PC1 (Checking for Normality)", x = "PC1", y = "Density")

ggplot(pca_scores_clean, aes(x = PC2)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 30) +
  geom_density(color = "red") +
  labs(title = "Distribution of PC2 (Checking for Normality)", x = "PC2", y = "Density")

# Q-Q plot for the first two principal components
qqnorm(pca_scores_clean$PC1)
qqline(pca_scores_clean$PC1, col = "red")
qqnorm(pca_scores_clean$PC2)
qqline(pca_scores_clean$PC2, col = "red")

#hz_test <- mvn(bank_clean_num, mvnTest = "hz")
#print(hz_test$multivariateNormality)
#royston_test <- mvn(bank_clean_num, mvnTest = "royston")
#print(royston_test$multivariateNormality)

# lab - 4
# Load necessary libraries
library(ggplot2)
library(dendextend)

# Load the dataset
wine_data <- read.csv("Wine_data.csv")
wine_data <- wine_data[,c(-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33)]

# Standardize the features (ignore 'Type' column)
wine_features <- scale(wine_data[, -1])

# Check for missing or invalid values in the dataset
any(is.na(wine_features))  # Check for NAs
any(is.nan(wine_features))  # Check for NaNs
any(is.infinite(wine_features))  # Check for Inf values

# Impute missing values (e.g., using the mean of the column)
wine_features_clean <- wine_features
wine_features_clean[is.na(wine_features_clean)] <- apply(wine_features, 2, mean, na.rm = TRUE)
any(is.na(wine_features_clean))

# Hierarchical clustering with complete linkage
hc_complete <- hclust(dist(wine_features_clean), method = "complete")
hc_single <- hclust(dist(wine_features_clean), method = "single")

# Plot the dendrograms for complete and single linkage
#par(mfrow = c(1, 2))  # Plot side by side

# Complete linkage dendrogram
plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "", cex = 0.6)

# Single linkage dendrogram
plot(hc_single, main = "Single Linkage", xlab = "", sub = "", cex = 0.6)

# Set K = 3 and run K-means with multiple initial configurations
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(wine_features_clean, centers = 3, nstart = 10)

# Print cluster assignments and centroids
kmeans_result$cluster  # Cluster assignments
kmeans_result$centers  # Centroids for each cluster

# Function to calculate total within-cluster scatter
total_within_cluster_scatter <- function(data, cluster_assignments) {
  sum(sapply(unique(cluster_assignments), function(cluster) {
    cluster_points <- data[cluster_assignments == cluster, ]
    centroid <- colMeans(cluster_points)
    sum(rowSums((cluster_points - centroid)^2))
  }))
}

# Calculate for K-means
within_cluster_scatter_kmeans <- total_within_cluster_scatter(wine_features_clean, kmeans_result$cluster)

# For hierarchical clustering (cut tree to get 3 clusters)
hc_complete_cut <- cutree(hc_complete, k = 3)
within_cluster_scatter_hc_complete <- total_within_cluster_scatter(wine_features_clean, hc_complete_cut)

hc_single_cut <- cutree(hc_single, k = 3)

# Modified total_within_cluster_scatter function
total_within_cluster_scatter_single <- function(data, cluster_labels) {
  unique_clusters <- unique(cluster_labels)
  total_scatter <- 0
  
  for (cluster in unique_clusters) {
    cluster_points <- data[cluster_labels == cluster, , drop = FALSE]  # Ensures cluster_points is a matrix
    
    if (nrow(cluster_points) > 1) {
      # Compute within-cluster scatter for clusters with more than 1 point
      cluster_center <- colMeans(cluster_points)
      scatter <- sum(rowSums((cluster_points - cluster_center) ^ 2))
    } else {
      # For clusters with only one point, scatter is 0
      scatter <- 0
    }
    
    total_scatter <- total_scatter + scatter
  }
  
  return(total_scatter)
}
# Use the modified function
within_cluster_scatter_hc_single <- total_within_cluster_scatter_single(wine_features_clean, hc_single_cut)

# clusters
kmeans_result$cluster
hc_complete_cut
hc_single_cut

# Print the results
within_cluster_scatter_kmeans
within_cluster_scatter_hc_complete
within_cluster_scatter_hc_single

# Cross-tabulate the actual Type vs. the cluster assignments from K-means
table(wine_data$Type, kmeans_result$cluster)

# Cross-tabulate the actual Type vs. the cluster assignments from hierarchical clustering
table(wine_data$Type, hc_complete_cut)
table(wine_data$Type, hc_single_cut)

##(2) BY ME
crime <- read.csv("CrimesOnWomenData.csv")
crime_num <- crime[,-1]
crime_num <- scale(crime_num)

# Check for missing or invalid values in the dataset
any(is.na(crime_num))  # Check for NAs
any(is.nan(crime_num))  # Check for NaNs
any(is.infinite(crime_num))  # Check for Inf values
## no NAs, NANs, Inf values in matrix

# Hierarchical clustering with complete linkage
hc_complete <- hclust(dist(crime_num), method = "complete")
hc_single <- hclust(dist(crime_num), method = "single")

# Complete linkage dendrogram
plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "", cex = 0.6)

# Single linkage dendrogram
plot(hc_single, main = "Single Linkage", xlab = "", sub = "", cex = 0.6)

# Set K = 3 and run K-means with multiple initial configurations
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(crime_num, centers = 3, nstart = 10)

# Print cluster assignments and centroids
kmeans_result$cluster  # Cluster assignments
kmeans_result$centers  # Centroids for each cluster

# Function to calculate total within-cluster scatter
total_within_cluster_scatter <- function(data, cluster_assignments) {
  sum(sapply(unique(cluster_assignments), function(cluster) {
    cluster_points <- data[cluster_assignments == cluster, ]
    centroid <- colMeans(cluster_points)
    sum(rowSums((cluster_points - centroid)^2))
  }))
}

# Calculate for K-means
within_cluster_scatter_kmeans <- total_within_cluster_scatter(wine_features_clean, kmeans_result$cluster)

# For hierarchical clustering (cut tree to get 3 clusters)
hc_complete_cut <- cutree(hc_complete, k = 3)
within_cluster_scatter_hc_complete <- total_within_cluster_scatter(wine_features_clean, hc_complete_cut)

hc_single_cut <- cutree(hc_single, k = 3)

# Modified total_within_cluster_scatter function
total_within_cluster_scatter_single <- function(data, cluster_labels) {
  unique_clusters <- unique(cluster_labels)
  total_scatter <- 0
  
  for (cluster in unique_clusters) {
    cluster_points <- data[cluster_labels == cluster, , drop = FALSE]  # Ensures cluster_points is a matrix
    
    if (nrow(cluster_points) > 1) {
      # Compute within-cluster scatter for clusters with more than 1 point
      cluster_center <- colMeans(cluster_points)
      scatter <- sum(rowSums((cluster_points - cluster_center) ^ 2))
    } else {
      # For clusters with only one point, scatter is 0
      scatter <- 0
    }
    
    total_scatter <- total_scatter + scatter
  }
  
  return(total_scatter)
}
# Use the modified function
within_cluster_scatter_hc_single <- total_within_cluster_scatter_single(wine_features_clean, hc_single_cut)

# clusters
kmeans_result$cluster
hc_complete_cut
hc_single_cut

# Print the results
within_cluster_scatter_kmeans
within_cluster_scatter_hc_complete
within_cluster_scatter_hc_single

# Cross-tabulate the actual Type vs. the cluster assignments from K-means
table(wine_data$Type, kmeans_result$cluster)

# Cross-tabulate the actual Type vs. the cluster assignments from hierarchical clustering
table(wine_data$Type, hc_complete_cut)
table(wine_data$Type, hc_single_cut)

##(2) BY CHATGPT
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(cluster)  # For hierarchical clustering
library(factoextra)  # For visualizing clusters and dendrograms

# Load the dataset
crime_data <- read.csv("CrimesOnWomenData.csv")

# Check the structure of the dataset
str(crime_data)

# Clean and preprocess the data if necessary (e.g., handling missing values)
crime_data_clean <- na.omit(crime_data)

# Example for year 2001 (repeat for other years)
crime_data_2001 <- crime_data_clean %>% filter(Year == 2001) %>% select(-Year, -State)

# Calculate the distance matrix
dist_matrix <- dist(crime_data_2001)

# Perform hierarchical clustering (average linkage)
hc_avg <- hclust(dist_matrix, method = "average")

# Perform hierarchical clustering (single linkage)
hc_single <- hclust(dist_matrix, method = "single")

# Plot dendrogram for average linkage
fviz_dend(hc_avg, main = "Dendrogram - Average Linkage (2001)")

# Plot dendrogram for single linkage
fviz_dend(hc_single, main = "Dendrogram - Single Linkage (2001)")

# Perform K-means clustering with K = 5 for 2001
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(crime_data_2001, centers = 5, nstart = 25)

# Visualize K-means clusters
fviz_cluster(kmeans_result, data = crime_data_2001, main = "K-means Clustering (K=5) for 2001")

# Function to calculate within-cluster scatter
within_cluster_scatter <- function(data, cluster_labels) {
  total_within_cluster_scatter <- 0
  unique_clusters <- unique(cluster_labels)
  
  for (cluster in unique_clusters) {
    cluster_points <- data[cluster_labels == cluster, ]
    cluster_center <- colMeans(cluster_points)
    scatter <- sum(rowSums((cluster_points - cluster_center) ^ 2))
    total_within_cluster_scatter <- total_within_cluster_scatter + scatter
  }
  
  return(total_within_cluster_scatter)
}

# Calculate within-cluster scatter for hierarchical clustering (average linkage)
hc_avg_cut <- cutree(hc_avg, k = 5)
within_scatter_hc_avg <- within_cluster_scatter(crime_data_2001, hc_avg_cut)

# Calculate within-cluster scatter for K-means clustering
within_scatter_kmeans <- within_cluster_scatter(crime_data_2001, kmeans_result$cluster)

# Print the results
print(paste("Within-cluster scatter (Hierarchical - Average Linkage):", within_scatter_hc_avg))
print(paste("Within-cluster scatter (K-means):", within_scatter_kmeans))

# Function to calculate between-cluster scatter
between_cluster_scatter <- function(data, cluster_labels) {
  overall_mean <- colMeans(data)
  unique_clusters <- unique(cluster_labels)
  total_between_cluster_scatter <- 0
  
  for (cluster in unique_clusters) {
    cluster_points <- data[cluster_labels == cluster, ]
    cluster_center <- colMeans(cluster_points)
    scatter <- nrow(cluster_points) * sum((cluster_center - overall_mean) ^ 2)
    total_between_cluster_scatter <- total_between_cluster_scatter + scatter
  }
  
  return(total_between_cluster_scatter)
}

# Calculate between-cluster scatter for hierarchical clustering (average linkage)
between_scatter_hc_avg <- between_cluster_scatter(crime_data_2001, hc_avg_cut)

# Calculate between-cluster scatter for K-means clustering
between_scatter_kmeans <- between_cluster_scatter(crime_data_2001, kmeans_result$cluster)

# Print the results
print(paste("Between-cluster scatter (Hierarchical - Average Linkage):", between_scatter_hc_avg))
print(paste("Between-cluster scatter (K-means):", between_scatter_kmeans))





