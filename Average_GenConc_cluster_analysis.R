# Load necessary libraries
library(dplyr)
library(dendextend)
library(colorspace)
library(factoextra)

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Clear environment
rm(list = ls())

load("GenConc_Average_train_noWeathering.RData")

# Load data -- change this to trained data from RF Model2
data <- train %>%
  dplyr::select("P", "snow_cover", "precip", "NOx", "rocks_volcanic", "basin_slope")

# Scale the data



## Keira's Code----
cluster_input <- scaled_data 

set.seed(123)

p2 <- fviz_nbclust(weights_clust, kmeans, method= "silhouette", k.max = 20)
p2

kmeans_cluster <- kmeans(weights_clust, iter.max = 50, nstart = 50, centers = 2)

data$clust <- kmeans_cluster$cluster

# facet_wrap top 5 drivers with distributions for each cluster -- box plots

# silhouette plot with squared euclidean distance by cluster

