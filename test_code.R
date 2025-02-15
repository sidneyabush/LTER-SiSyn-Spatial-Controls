# Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, randomForest, tibble, viridis, fastshap)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Load required data and model from the RF script
load("FNConc_Yearly_rf_model2_full.RData")
load("FNConc_Yearly_kept_drivers_full.RData")
load("FNConc_Yearly_full.RData")
load("FNConc_Yearly_full_stream_ids.RData")

# Set global seed and output directory
set.seed(123)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"

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

# Determine global min and max of scaled feature values across all clusters
global_min <- min(scaled_drivers %>% select(-cluster), na.rm = TRUE)
global_max <- max(scaled_drivers %>% select(-cluster), na.rm = TRUE)

# Function to create SHAP plots per cluster with a fixed color scale
create_shap_plots_for_cluster <- function(cluster_id, shap_values, scaled_drivers, output_dir, global_min, global_max) {
  # Subset data for the current cluster
  cluster_data <- scaled_drivers %>%
    filter(cluster == cluster_id) %>%
    select(-cluster)  # Remove cluster column
  
  cluster_data$id <- seq_len(nrow(cluster_data))  # Assign unique IDs
  
  # Reshape data to long format
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  # Get the indices of rows in kept_drivers that belong to this cluster
  cluster_indices <- which(scaled_drivers$cluster == cluster_id)
  
  # Subset SHAP values for this cluster
  shap_values_df <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))  # Reset ID
  
  # Convert SHAP values to long format
  shap_long <- shap_values_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  # Generate SHAP dot plot for the cluster with fixed color scale
  pdf(sprintf("%s/SHAP_Cluster_%s_Dot_Plot.pdf", output_dir, cluster_id), width = 9, height = 8)
  dot_plot <- ggplot(shap_long, aes(x = shap_value, y = feature, color = feature_value)) +
    geom_point(alpha = 0.6) +
    scale_color_gradient(low = "lightsteelblue", high = "steelblue4", name = "Scaled Value",
                         limits = c(global_min, global_max)) +  # Fixed global limits
    labs(x = "SHAP Value", y = "Feature", title = paste("SHAP Dot Plot for Cluster", cluster_id)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey1") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 14), 
          axis.title.x = element_text(size = 14), 
          plot.title = element_text(size = 16, face = "bold"))
  print(dot_plot)
  dev.off()
}

# Generate SHAP plots for each cluster using the fixed global color scale
lapply(unique(kept_drivers$cluster), create_shap_plots_for_cluster, 
       shap_values = shap_values, scaled_drivers = scaled_drivers, 
       output_dir = output_dir, global_min = global_min, global_max = global_max)

# # Generate SHAP plots for each cluster
# unique_clusters <- unique(kept_drivers$cluster)
# lapply(unique_clusters, create_shap_plots_for_cluster, shap_values = shap_values, scaled_drivers = scaled_drivers, output_dir = output_dir)
