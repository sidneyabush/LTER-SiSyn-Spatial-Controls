# -------------------------------
# 1. Load Packages & Set Up Environment
# -------------------------------
librarian::shelf(iml, ggplot2, dplyr, tidyr, factoextra, cluster, colorspace, scales, fastshap)

rm(list = ls())  # Clear environment

setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"

# -------------------------------
# 2. Load Data & Model
# -------------------------------
load("FNConc_Yearly_rf_model2_full.RData")
load("FNConc_Yearly_kept_drivers_full.RData")
load("FNConc_Yearly_full.RData")
load("FNConc_Yearly_full_stream_ids.RData")

# -------------------------------
# 3. SHAP Value Generation (Before Clustering!)
# -------------------------------
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  # Extract only predictor variables (remove cluster)
  predictors <- all.vars(model$terms)[-1]  # Extract predictor names
  X_input <- kept_drivers %>% select(all_of(predictors))  # Keep only predictors
  
  # Custom predict function for formula-based models
  custom_predict <- function(object, newdata) {
    newdata <- as.data.frame(newdata)  # Ensure data frame format
    colnames(newdata) <- predictors  # Match column names exactly
    predict(object, newdata = newdata, type = "response")  # Ensure numeric output
  }
  
  # Compute SHAP values
  shap_values <- fastshap::explain(
    object = model,
    X = X_input,  # Ensure only predictor variables are used
    pred_wrapper = custom_predict,
    nsim = sample_size
  )
  
  return(shap_values)
}

# Generate global SHAP values BEFORE adding cluster assignments
shap_values <- generate_shap_values(rf_model2, kept_drivers, sample_size = 30)

# -------------------------------
# 4. Clustering (AFTER SHAP)
# -------------------------------
# Define variables to use for clustering
cluster_vars <- c("elevation", "basin_slope", "P", "rocks_volcanic", "evapotrans")

# Create a dataset for clustering from kept_drivers
cluster_data <- kept_drivers %>% select(all_of(cluster_vars))

# Scale clustering variables
scaled_cluster_data <- cluster_data %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))

set.seed(123)  # Ensure reproducibility

# Determine optimal clusters (optional)
p2 <- fviz_nbclust(scaled_cluster_data, kmeans, method = "silhouette", k.max = 20)
print(p2)

# Perform k-means clustering
kmeans_result <- kmeans(scaled_cluster_data, iter.max = 50, nstart = 50, centers = 3)

# NOW attach cluster assignments to kept_drivers (AFTER SHAP)
kept_drivers$cluster <- as.factor(kmeans_result$cluster)

# -------------------------------
# 5. Prepare Data for Visualization (Scaled for Display)
# -------------------------------
full_scaled <- kept_drivers %>%
  select(-cluster) %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>%
  as.data.frame()

# Reattach cluster for visualization (but NOT for SHAP!)
full_scaled$cluster <- kept_drivers$cluster

# Determine global min/max for SHAP dot plot scaling
global_min <- min(full_scaled %>% select(-cluster), na.rm = TRUE)
global_max <- max(full_scaled %>% select(-cluster), na.rm = TRUE)

# -------------------------------
# 6. Define Color Palette for Clusters
# -------------------------------
base_colors <- c("1" = "#E69F00", "2" = "#56B4E9", "3" = "#009E73")
cluster_colors <- lapply(base_colors, function(col) {
  c(lighten(col, 0.4), col, darken(col, 0.4))
})

# -------------------------------
# 7. SHAP Feature Importance Plot per Cluster
# -------------------------------
generate_feature_importance_plot <- function(cluster_id, shap_values, full_scaled, output_dir) {
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  shap_cluster <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE]
  
  overall_feature_importance <- shap_cluster %>%
    summarise(across(everything(), ~ mean(abs(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))
  
  cluster_base_color <- base_colors[[as.character(cluster_id)]]
  
  importance_plot_path <- sprintf("%s/SHAP_FNConc_Ave_Cluster_%s_Variable_Importance.pdf", output_dir, cluster_id)
  pdf(importance_plot_path, width = 10, height = 8)
  ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = cluster_base_color) +
    coord_flip() +
    labs(title = paste("Feature Importance for Cluster", cluster_id)) +
    theme_classic()
  dev.off()
}

# -------------------------------
# 8. SHAP Dot Plot per Cluster
# -------------------------------
generate_shap_dot_plot <- function(cluster_id, shap_values, full_scaled, output_dir, global_min, global_max) {
  cluster_data <- full_scaled %>% filter(cluster == cluster_id) %>% select(-cluster)
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  shap_values_df <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  shap_long <- shap_values_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  cluster_palette <- cluster_colors[[as.character(cluster_id)]]
  cluster_light <- cluster_palette[1]
  cluster_dark  <- cluster_palette[3]
  
  dot_plot_path <- sprintf("%s/SHAP_FNConc_Ave_Cluster_%s_Dot_Plot.pdf", output_dir, cluster_id)
  pdf(dot_plot_path, width = 9, height = 8)
  ggplot(shap_long, aes(x = shap_value, y = feature, color = feature_value)) +
    geom_point(alpha = 0.6) +
    scale_color_gradient(low = cluster_light, high = cluster_dark, name = "Scaled Value",
                         limits = c(global_min, global_max)) +
    labs(title = paste("SHAP Dot Plot for Cluster", cluster_id)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey1") +
    theme_minimal()
  dev.off()
}

# -------------------------------
# 9. Generate Plots for Each Cluster
# -------------------------------
unique_clusters <- unique(full_scaled$cluster)

lapply(unique_clusters, generate_feature_importance_plot, shap_values, full_scaled, output_dir)
lapply(unique_clusters, generate_shap_dot_plot, shap_values, full_scaled, output_dir, global_min, global_max)
