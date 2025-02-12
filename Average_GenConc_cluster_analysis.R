# Load necessary libraries
library(dplyr)
library(dendextend)
library(colorspace)
library(factoextra)

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Clear environment
rm(list = ls())

# Read in and tidy data ----
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

# Read in and preprocess the data
train <- read.csv("train_data_stream_id.csv")

data <- train %>%
  dplyr::select("Stream_ID", "P", "snow_cover", "precip", "NOx", "rocks_volcanic", "basin_slope")

# Scale the selected numerical columns (excluding Stream_ID) and ensure correct column names
scaled_data <- data %>%
  mutate(across(-Stream_ID, ~ as.numeric(scale(.))))

# Set seed for reproducibility
set.seed(123)

# Perform silhouette method to determine optimal clusters
p2 <- fviz_nbclust(scaled_data %>% select(-Stream_ID), kmeans, method= "silhouette", k.max = 20)
p2

kmeans_result <- kmeans(scaled_data %>% select(-Stream_ID), iter.max = 50, nstart = 50, centers = 5)

# Add cluster assignments to the reg data
data <- data %>%
  mutate(cluster = as.factor(kmeans_result$cluster))

scaled_data <- scaled_data %>%
  mutate(cluster = as.factor(kmeans_result$cluster))

# Reshape data to long format for ggplot
long_data <- scaled_data %>%
  pivot_longer(-c(Stream_ID, cluster), names_to = "Driver", values_to = "Value")

# Create box plots with facet_wrap
ggplot(long_data, aes(x = Driver, y = Value, fill = cluster)) +
  geom_boxplot() +
  facet_wrap(~cluster, scales = "free") +
  theme_minimal() +
  labs(x = NULL, y = "Scaled Value") +
  theme(legend.position = "none")

# silhouette plot with squared euclidean distance by cluster

