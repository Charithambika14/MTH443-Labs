### LAB-6

# Load libraries
library(dplyr)
library(ggplot2)
library(MASS) # For kde2d
library(stats) # For density estimation

# Load data
data <- read.csv("CHD.csv")

# Impute missing values with group averages
data <- data %>%
  group_by(TenYearCHD) %>%
  mutate(
    totChol = ifelse(is.na(totChol), mean(totChol, na.rm = TRUE), totChol),
    sysBP = ifelse(is.na(sysBP), mean(sysBP, na.rm = TRUE), sysBP),
    diaBP = ifelse(is.na(diaBP), mean(diaBP, na.rm = TRUE), diaBP)
  ) %>%
  ungroup()

# Split data by TenYearCHD group
group_0 <- data %>% filter(TenYearCHD == 0)
group_1 <- data %>% filter(TenYearCHD == 1)

# Define kernels and variables for density estimation
kernels <- c("gaussian", "epanechnikov", "rectangular", "triangular")
variables <- c("totChol", "sysBP", "diaBP")

# Part (A) KDE for each variable and kernel type
for (var in variables) {
  for (kernel in kernels) {
    # Density estimation for each group
    density_0 <- density(group_0[[var]], kernel = kernel, na.rm = TRUE)
    density_1 <- density(group_1[[var]], kernel = kernel, na.rm = TRUE)
    
    # Plot densities
    plot(density_0, col = "blue", main = paste("Density of", var, "using", kernel, "kernel"),
         xlab = var, ylab = "Density", ylim = range(density_0$y, density_1$y))
    lines(density_1, col = "red")
    legend("topright", legend = c("Group 0", "Group 1"), col = c("blue", "red"), lty = 1)
  }
}

# Part (C) Tail probabilities
# Define threshold levels (e.g., 90th and 95th percentiles)
thresholds <- c(0.90, 0.95)
tail_probs_0 <- list()
tail_probs_1 <- list()

for (var in variables) {
  for (threshold in thresholds) {
    thresh_val <- quantile(data[[var]], probs = threshold, na.rm = TRUE)
    tail_probs_0[[paste(var, threshold, sep = "_")]] <- mean(group_0[[var]] > thresh_val, na.rm = TRUE)
    tail_probs_1[[paste(var, threshold, sep = "_")]] <- mean(group_1[[var]] > thresh_val, na.rm = TRUE)
  }
}

# Print tail probabilities for comparison
print("Tail probabilities for Group 0:")
print(tail_probs_0)
print("Tail probabilities for Group 1:")
print(tail_probs_1)

# Part (D) Joint density estimation for sysBP and diaBP
# KDE for joint density of sysBP and diaBP
x_grid <- seq(min(data$sysBP, na.rm = TRUE), max(data$sysBP, na.rm = TRUE), length.out = 100)
y_grid <- seq(min(data$diaBP, na.rm = TRUE), max(data$diaBP, na.rm = TRUE), length.out = 100)

kde_joint_0 <- kde2d(group_0$sysBP, group_0$diaBP, n = 100, lims = c(range(x_grid), range(y_grid)))
kde_joint_1 <- kde2d(group_1$sysBP, group_1$diaBP, n = 100, lims = c(range(x_grid), range(y_grid)))

# Plot 2D joint density
filled.contour(x_grid, y_grid, kde_joint_0$z, color.palette = colorRampPalette(c("pink", "blue")),
               plot.title = title(main = "Joint Density of sysBP and diaBP for Group 0", xlab = "sysBP", ylab = "diaBP"),xlim = c(84,200),ylim = c(50,120))

filled.contour(x_grid, y_grid, kde_joint_1$z, color.palette = colorRampPalette(c("lightgreen", "red")),
               plot.title = title(main = "Joint Density of sysBP and diaBP for Group 1", xlab = "sysBP", ylab = "diaBP"))

