# Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, randomForest, tibble, viridis, fastshap, colorspace)

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

# Scale the dataset before clustering
scaled_drivers <- scale(kept_drivers)  # Standardize features

# Perform silhouette method to determine optimal clusters
p2 <- fviz_nbclust(scaled_drivers, kmeans, method= "silhouette", k.max = 20)
print(p2)

kmeans_result <- kmeans(scaled_drivers, centers = 3)  # Perform clustering

# Attach cluster assignments to the dataset
kept_drivers$cluster <- as.factor(kmeans_result$cluster)  
scaled_drivers <- as.data.frame(scaled_drivers)  # Convert to data frame
scaled_drivers$cluster <- kept_drivers$cluster  # Attach cluster info

# Function to generate SHAP values
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

# Generate SHAP values
shap_values <- generate_shap_values(rf_model2, kept_drivers, sample_size = 30)

# Define a light-to-dark color palette using colorspace
# Cluster 1: "#E69F00" (orange), Cluster 2: "#56B4E9" (blue), Cluster 3: "#009E73" (green)
cluster_colors <- list(
  "1" = c(lighten("#E69F00", 0.4), "#E69F00", darken("#E69F00", 0.4)),
  "2" = c(lighten("#56B4E9", 0.4), "#56B4E9", darken("#56B4E9", 0.4)),
  "3" = c(lighten("#009E73", 0.4), "#009E73", darken("#009E73", 0.4))
)

# Function to create feature importance bar plots for each cluster
generate_feature_importance_plot <- function(cluster_id, shap_values, output_dir) {
  # Compute overall feature importance (mean absolute SHAP value)
  overall_feature_importance <- shap_values %>%
    as.data.frame() %>%
    summarise(across(everything(), ~ mean(abs(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))
  
  # Get cluster color
  cluster_base_color <- cluster_colors[[as.character(cluster_id)]][2]  # Use middle/base color
  
  # Generate bar plot for feature importance
  importance_plot_path <- sprintf("%s/SHAP_FNConc_Ave_Cluster_%s_Variable_Importance.pdf", output_dir, cluster_id)
  pdf(importance_plot_path, width = 10, height = 8)
  importance_plot <- ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = cluster_base_color) +
    coord_flip() +
    labs(x = "Feature", y = "Mean Absolute SHAP Value", 
         title = paste("FNConc Yearly - Feature Importance for Cluster", cluster_id)) +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  print(importance_plot)
  dev.off()
}

# Function to create SHAP dot plots per cluster with ordered y-axis
generate_shap_dot_plot <- function(cluster_id, shap_values, scaled_drivers, output_dir) {
  # Subset data for the current cluster
  cluster_data <- scaled_drivers %>%
    filter(cluster == cluster_id) %>%
    select(-cluster)
  
  cluster_data$id <- seq_len(nrow(cluster_data))  # Assign unique IDs
  
  # Reshape data to long format
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  # Get the indices of rows in scaled_drivers that belong to this cluster
  cluster_indices <- which(scaled_drivers$cluster == cluster_id)
  
  # Subset SHAP values for this cluster and reset ID
  shap_values_df <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  # Convert SHAP values to long format and join with feature values
  shap_long <- shap_values_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  # Calculate mean absolute SHAP values to order the y-axis
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  
  # Reorder feature factor for the plot
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  # Get cluster colors
  cluster_palette <- cluster_colors[[as.character(cluster_id)]]
  cluster_light <- cluster_palette[1]  # Light version (low end)
  cluster_dark  <- cluster_palette[3]  # Dark version (high end)
  
  # Generate SHAP dot plot
  dot_plot_path <- sprintf("%s/SHAP_FNConc_Ave_Cluster_%s_Dot_Plot.pdf", output_dir, cluster_id)
  pdf(dot_plot_path, width = 9, height = 8)
  dot_plot <- ggplot(shap_long, aes(x = shap_value, y = feature, color = feature_value)) +
    geom_point(alpha = 0.6) +
    scale_color_gradient(low = cluster_light, high = cluster_dark, name = "Scaled Value") +
    labs(x = "SHAP Value", y = "Feature", title = paste("SHAP Dot Plot for Cluster", cluster_id)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey1") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  print(dot_plot)
  dev.off()
}

# Generate both SHAP dot plots and feature importance plots for each cluster
unique_clusters <- unique(scaled_drivers$cluster)

# Create feature importance plots
lapply(unique_clusters, generate_feature_importance_plot, shap_values = shap_values, output_dir = output_dir)

# Create SHAP dot plots
lapply(unique_clusters, generate_shap_dot_plot, shap_values = shap_values, scaled_drivers = scaled_drivers, output_dir = output_dir)
