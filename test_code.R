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
# Data Preparation & Clustering
# -------------------------------
# Select features from kept_drivers
data <- kept_drivers %>% 
  dplyr::select("elevation", "basin_slope", "P", "rocks_volcanic", "evapotrans")

# Scale the selected numerical columns (each feature is standardized independently)
scaled_drivers <- data %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))

# Set seed for reproducibility
set.seed(123)

# (Optional) Use silhouette method to check optimal clusters
p2 <- fviz_nbclust(scaled_drivers, kmeans, method = "silhouette", k.max = 20)
print(p2)

# Perform k-means clustering on the scaled data
kmeans_result <- kmeans(scaled_drivers, centers = 3)

# Attach cluster assignments to both datasets
kept_drivers$cluster <- as.factor(kmeans_result$cluster)
scaled_drivers <- as.data.frame(scaled_drivers)
scaled_drivers$cluster <- kept_drivers$cluster

# -------------------------------
# Generate SHAP Values
# -------------------------------
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  custom_predict <- function(object, newdata) {
    newdata <- as.data.frame(newdata)
    predict(object, newdata = newdata)
  }
  shap_values <- fastshap::explain(
    object = model,
    X = kept_drivers %>% select(-cluster),  # Exclude cluster column
    pred_wrapper = custom_predict,
    nsim = sample_size
  )
  return(shap_values)
}

# Compute global SHAP values on the full dataset
shap_values <- generate_shap_values(rf_model2, kept_drivers, sample_size = 30)

# Determine global min and max of scaled feature values (for consistent color scaling)
global_min <- min(scaled_drivers %>% select(-cluster), na.rm = TRUE)
global_max <- max(scaled_drivers %>% select(-cluster), na.rm = TRUE)

# -------------------------------
# Define Color Palette
# -------------------------------
# Use your base colors for clusters 1, 2, and 3.
# Then create a gradient from light to dark for each cluster.
# Here we use lighten() and darken() from colorspace.
base_colors <- c("1" = "#E69F00", "2" = "#56B4E9", "3" = "#009E73")
cluster_colors <- lapply(base_colors, function(col) {
  c(lighten(col, 0.4), col, darken(col, 0.4))
})
# Now, cluster_colors is a list with names "1", "2", "3",
# each containing a vector: [light, base, dark].

# -------------------------------
# Function: Overall Feature Importance Plot per Cluster
# -------------------------------
generate_feature_importance_plot <- function(cluster_id, shap_values, scaled_drivers, output_dir) {
  # Subset SHAP values for the cluster based on row indices:
  cluster_indices <- which(scaled_drivers$cluster == cluster_id)
  shap_cluster <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE]
  
  # Compute mean absolute SHAP value per feature for this cluster:
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
generate_shap_dot_plot <- function(cluster_id, shap_values, scaled_drivers, output_dir, global_min, global_max) {
  # Subset data for the current cluster (remove cluster column)
  cluster_data <- scaled_drivers %>%
    filter(cluster == cluster_id) %>%
    select(-cluster)
  cluster_data$id <- seq_len(nrow(cluster_data))  # Unique IDs
  
  # Reshape cluster data to long format
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  # Get indices for this cluster
  cluster_indices <- which(scaled_drivers$cluster == cluster_id)
  shap_values_df <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  # Convert SHAP values to long format and join with feature values
  shap_long <- shap_values_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  # Compute ordering: mean absolute SHAP per feature for this cluster
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  
  # Reorder the feature factor so that the highest mean absolute SHAP is at the top
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  # Get cluster-specific color gradient
  cluster_palette <- cluster_colors[[as.character(cluster_id)]]
  cluster_light <- cluster_palette[1]  # Light shade (low end)
  cluster_dark  <- cluster_palette[3]  # Dark shade (high end)
  
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
unique_clusters <- unique(scaled_drivers$cluster)

# Create feature importance plots for each cluster
lapply(unique_clusters, generate_feature_importance_plot, shap_values = shap_values, 
       scaled_drivers = scaled_drivers, output_dir = output_dir)

# Create SHAP dot plots for each cluster
lapply(unique_clusters, generate_shap_dot_plot, shap_values = shap_values, scaled_drivers = scaled_drivers,
       output_dir = output_dir, global_min = global_min, global_max = global_max)
