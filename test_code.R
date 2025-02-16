# Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, factoextra, cluster, colorspace, scales, fastshap)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"

# Load required data and model from the RF script
load("FNConc_Yearly_rf_model2_full.RData")
load("FNConc_Yearly_kept_drivers_full.RData")
load("FNConc_Yearly_full.RData")
load("FNConc_Yearly_full_stream_ids.RData")

# -------------------------------
# Clustering: Use only a subset of variables
# -------------------------------
# Define variables to use for clustering
cluster_vars <- c("elevation", "basin_slope", "P", "rocks_volcanic", "evapotrans")

# Create a dataset for clustering from kept_drivers
cluster_data <- kept_drivers %>% select(all_of(cluster_vars))

# Scale the clustering variables (each feature standardized independently)
scaled_cluster_data <- cluster_data %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))

# Set seed for reproducibility
set.seed(123)

# (Optional) Determine optimal clusters using silhouette method on scaled_cluster_data
p2 <- fviz_nbclust(scaled_cluster_data, kmeans, method = "silhouette", k.max = 20)
print(p2)

# Perform k-means clustering on the scaled cluster data
kmeans_result <- kmeans(scaled_cluster_data, centers = 3)

# Attach cluster assignments to the full kept_drivers dataset (so full variables are preserved)
kept_drivers$cluster <- as.factor(kmeans_result$cluster)

# Also, create a "full scaled" dataset for SHAP plotting (using ALL variables in kept_drivers)
# Exclude the cluster column, scale all numeric columns, then reattach cluster.
full_scaled <- kept_drivers %>% select(-cluster) %>% 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))
full_scaled <- as.data.frame(full_scaled)
full_scaled$cluster <- kept_drivers$cluster

# -------------------------------
# SHAP Value Generation (using full kept_drivers)
# -------------------------------
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  custom_predict <- function(object, newdata) {
    newdata <- as.data.frame(newdata)
    predict(object, newdata = newdata)
  }
  shap_values <- fastshap::explain(
    object = model,
    X = kept_drivers %>% select(-cluster),  # Use full kept_drivers (all variables) for SHAP
    pred_wrapper = custom_predict,
    nsim = sample_size
  )
  return(shap_values)
}

# Generate global SHAP values using the full kept_drivers (with all variables)
shap_values <- generate_shap_values(rf_model2, kept_drivers, sample_size = 30)

# Determine global min and max from the full_scaled dataset (all predictors)
global_min <- min(full_scaled %>% select(-cluster), na.rm = TRUE)
global_max <- max(full_scaled %>% select(-cluster), na.rm = TRUE)

# -------------------------------
# Define Color Palette for Clusters
# -------------------------------
# Base colors for clusters 1, 2, and 3:
base_colors <- c("1" = "#E69F00", "2" = "#56B4E9", "3" = "#009E73")
# Create a light-to-dark gradient for each cluster using colorspace:
cluster_colors <- lapply(base_colors, function(col) {
  c(lighten(col, 0.4), col, darken(col, 0.4))
})
# cluster_colors is a list with names "1", "2", "3" and each is a vector: [light, base, dark]

# -------------------------------
# Function: Overall Feature Importance Plot per Cluster
# -------------------------------
generate_feature_importance_plot <- function(cluster_id, shap_values, full_scaled, output_dir) {
  # Subset SHAP values for the cluster using row indices from full_scaled:
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  shap_cluster <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE]
  
  overall_feature_importance <- shap_cluster %>%
    summarise(across(everything(), ~ mean(abs(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))
  
  # Use base color for this cluster:
  cluster_base_color <- base_colors[[as.character(cluster_id)]]
  
  importance_plot_path <- sprintf("%s/SHAP_FNConc_Ave_Cluster_%s_Variable_Importance.pdf", output_dir, cluster_id)
  pdf(importance_plot_path, width = 10, height = 8)
  importance_plot <- ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = cluster_base_color) +
    coord_flip() +
    labs(x = "Feature", y = "Mean Absolute SHAP Value", 
         title = paste("FNConc Yearly - Feature Importance for Cluster", cluster_id)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  print(importance_plot)
  dev.off()
}

# -------------------------------
# Function: SHAP Dot Plot per Cluster with Ordered Y-axis
# -------------------------------
generate_shap_dot_plot <- function(cluster_id, shap_values, full_scaled, output_dir, global_min, global_max) {
  # Subset the full_scaled data for the current cluster (all variables)
  cluster_data <- full_scaled %>%
    filter(cluster == cluster_id) %>%
    select(-cluster)
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  # Reshape cluster data to long format
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  # Get row indices for the cluster from full_scaled
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  shap_values_df <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  # Convert SHAP values to long format and join with cluster data
  shap_long <- shap_values_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  # Order y-axis: highest absolute mean SHAP at the top
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  # Get cluster-specific color gradient from our palette
  cluster_palette <- cluster_colors[[as.character(cluster_id)]]
  cluster_light <- cluster_palette[1]   # Light version
  cluster_dark  <- cluster_palette[3]   # Dark version
  
  dot_plot_path <- sprintf("%s/SHAP_FNConc_Ave_Cluster_%s_Dot_Plot.pdf", output_dir, cluster_id)
  pdf(dot_plot_path, width = 9, height = 8)
  dot_plot <- ggplot(shap_long, aes(x = shap_value, y = feature, color = feature_value)) +
    geom_point(alpha = 0.6) +
    scale_color_gradient(low = cluster_light, high = cluster_dark, name = "Scaled Value",
                         limits = c(global_min, global_max)) +
    labs(x = "SHAP Value", y = "Feature", title = paste("SHAP Dot Plot for Cluster", cluster_id)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey1") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  print(dot_plot)
  dev.off()
}

# -------------------------------
# Generate Plots for Each Cluster
# -------------------------------
unique_clusters <- unique(full_scaled$cluster)

# Create overall feature importance plots for each cluster
lapply(unique_clusters, generate_feature_importance_plot, 
       shap_values = shap_values, full_scaled = full_scaled, output_dir = output_dir)

# Create SHAP dot plots for each cluster (with ordered y-axis)
lapply(unique_clusters, generate_shap_dot_plot, 
       shap_values = shap_values, full_scaled = full_scaled,
       output_dir = output_dir, global_min = global_min, global_max = global_max)
