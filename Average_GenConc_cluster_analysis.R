# Load necessary libraries
library(dplyr)
library(dendextend)
library(colorspace)
library(factoextra)

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Clear environment
rm(list = ls())

# Load data -- change this to trained data from RF Model2
data <- read.csv("unique_stream_ids_average_5_years.csv") %>%
  dplyr::select("Stream_ID", "P", "rocks_volcanic", "snow_cover", "basin_slope")

# Set row names for dendrogram labeling
rownames(data) <- data$Stream_ID  # Ensure Stream_ID is used as labels

# Remove Stream_ID column for clustering
numerical_data <- data %>%
  select(-Stream_ID)

# Scale the data



## Keira's Code----
weights_clust <- numerical_data 

set.seed(123)

p2 <- fviz_nbclust(weights_clust, kmeans, method= "silhouette", k.max = 20)
p2

kmeans_cluster <- kmeans(weights_clust, iter.max = 50, nstart = 50, centers = 2)

data$clust <- kmeans_cluster$cluster

# facet_wrap top 5 drivers with distributions for each cluster -- box plots

# silhouette plot with squared euclidean distance by cluster

