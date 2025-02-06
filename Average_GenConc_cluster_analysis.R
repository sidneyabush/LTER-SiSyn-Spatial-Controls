# Load necessary libraries
library(dplyr)
library(dendextend)
library(colorspace)

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Load data
data <- read.csv("unique_stream_ids_average_5_years.csv") %>%
  dplyr::select("Stream_ID", "P", "rocks_volcanic", "silicate_weathering", "snow_cover", "basin_slope")

# Set row names for dendrogram labeling
rownames(data) <- data$Stream_ID  # Ensure Stream_ID is used as labels

# Remove Stream_ID column for clustering
numerical_data <- data %>%
  select(-Stream_ID)

dist_matrix <- dist(numerical_data, method = "minkowski", p = 3)

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "average")

# Convert hclust to dendrogram
dend <- as.dendrogram(hc)

# ---- Ensure Y-Axis Uses Stream_IDs ----
# labels(dend) <- data$Stream_ID[hc$order]  # Ensuring correct order of labels
labels(dend) <- rep("", length(labels(dend)))  # Remove Stream_ID labels

# ---- Identify Feature Importance for Each Split ----
merge_matrix <- hc$merge
num_nodes <- nrow(merge_matrix)

# Initialize vector to store labels for splits
split_labels <- rep("", num_nodes)

# Iterate through each split to determine the most important variable
for (i in 1:num_nodes) {
  left_cluster <- merge_matrix[i, 1]
  right_cluster <- merge_matrix[i, 2]
  
  # Extract cluster data
  if (left_cluster < 0) {
    left_data <- numerical_data[abs(left_cluster), , drop = FALSE]
  } else {
    left_data <- colMeans(numerical_data[hc$order[1:left_cluster], , drop = FALSE])
  }
  
  if (right_cluster < 0) {
    right_data <- numerical_data[abs(right_cluster), , drop = FALSE]
  } else {
    right_data <- colMeans(numerical_data[hc$order[1:right_cluster], , drop = FALSE])
  }
  
  # Compute differences between merging clusters
  diff_vector <- abs(left_data - right_data)
  
  # Identify the variable with the largest difference
  split_labels[i] <- names(which.max(diff_vector))
}

# Get unique variables responsible for splits
unique_split_vars <- unique(split_labels)

# Assign colors to each split variable
split_colors <- rainbow(length(unique_split_vars))
split_var_colors <- setNames(split_colors, unique_split_vars)

# Define custom colors for branches (adjust as needed)
custom_colors <- c("#D55E00", "#56B4E9", "#009E73", "#F0E442", "#E69F00") 

dend_colored <- color_branches(dend, k = length(unique(split_labels)), col = custom_colors) %>%
  assign_values_to_branches_edgePar(edgePar = "lwd", value = 2)  # Increase line thickness

# ---- Save the Dendrogram as a High-Resolution PNG ----
png("dendrogram.png", width = 8, height = 10, res = 300, units = "in")  # Adjusted to remove white space

# # ---- Rotate the Dendrogram (Horizontal Orientation) ----
# par(mar = c(8, 18, 2, 2))  # Reduced left margin slightly to fit labels

# ---- Plot the Dendrogram Without White Space ----
plot(dend_colored, 
     horiz = TRUE, 
     main = "Dendrogram of Stream Clusters", 
     xlab = "Height", 
     ylab = "",  # Remove Y-axis label
     yaxt = "n",  # Suppress Y-axis tick labels
     xlim = c(0, max(hc$height) * 1.05))

# ---- Add Legend AFTER Plotting to Avoid "plot.new" Error ----
legend("bottomleft",  # Change position as needed
       legend = unique(split_labels), 
       col = custom_colors,  
       lwd = 3,  
       cex = 1.5,  # Make legend text larger
       #title = "Splitting Variables",
       bg = "white",  
       box.lwd = 0)

dev.off()  # Finish saving


# ## Keira's Code----
# weights_clust <- scaled_data
# 
# #kmeans using 6 clusters
# set.seed(123)
# 
# # p1 <- fviz_nbclust(weights_clust, kmeans, method="wss", k.max = 20)
# 
# p2 <- fviz_nbclust(weights_clust, kmeans, method="silhouette", k.max = 20)
# p2
# 
# # pdf("Cluster_Metrics.pdf", width = 7, height = 8)
# 
# # ggarrange(p1, p2, nrow=2, align = "v")
# 
# # dev.off()
# 
# kmeans_cluster <- kmeans(weights_clust, iter.max=50, nstart=50, centers = 3)
# 
# data$clust <- kmeans_cluster$cluster
# 
# # facet_wrap top 5 drivers with distributions for each cluster
# # sillhoutte plot with squared eucldian distance by cluster
# 
