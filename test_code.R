# Load necessary libraries
librarian::shelf(ggplot2, dplyr, tidyr, factoextra, cluster, colorspace, scales, fastshap, stringr)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"

# Load required data and model
load("FNConc_Yearly_rf_model2_full.RData")
load("FNConc_Yearly_kept_drivers_full.RData")

# Use kept_drivers without FNConc
data <- kept_drivers

# Scale the selected numerical columns 
scaled_data <- data %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))

# Set seed for reproducibility
set.seed(123)

# Perform k-means clustering
kmeans_result <- kmeans(scaled_data, iter.max = 50, nstart = 50, centers = 3)

# Add cluster assignments to kept_drivers
final_data <- data %>%
  mutate(cluster = as.factor(kmeans_result$cluster))

# Define color palette for clusters
cb_palette <- c("#E69F00", "#56B4E9", "#009E73")  # Adjust as needed

# Define the SHAP function with a custom predict function
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  custom_predict <- function(object, newdata) {
    newdata <- as.data.frame(newdata)
    
    # Print model class and data structure for debugging
    print(paste("Model class: ", class(object)))
    print("New data structure:")
    print(str(newdata))
    
    # Check if the model is formula-based and handle accordingly
    if ("randomForest.formula" %in% class(object)) {
      print("Using formula-based randomForest")
      return(predict(object, newdata = newdata, type = "response"))
    } else if ("randomForest" %in% class(object)) {
      print("Using regular randomForest")
      return(predict(object, newdata = newdata, type = "response"))
    } else {
      stop("Model type not supported by custom_predict function")
    }
  }
  
  # Use fastshap to generate SHAP values
  print("Generating SHAP values...")
  shap_values <- fastshap::explain(
    object = model,
    X = kept_drivers,
    pred_wrapper = custom_predict,
    nsim = sample_size
  )
  
  return(shap_values)
}

# Function to generate SHAP plots per cluster
generate_shap_plots_for_cluster <- function(cluster_id, model, kept_drivers, scaled_data, output_dir, sample_size = 30) {
  cluster_data <- kept_drivers %>% filter(cluster == cluster_id) %>% select(-cluster)
  shap_values <- generate_shap_values(model, cluster_data, sample_size)
  
  # Compute feature importance
  overall_feature_importance <- shap_values %>%
    as.data.frame() %>%
    summarise(across(everything(), ~ mean(abs(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))
  
  # Define feature order
  feature_order <- overall_feature_importance$feature
  
  # Save feature importance plot
  importance_plot <- ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = cb_palette[as.numeric(cluster_id)]) +
    coord_flip() +
    labs(x = "Feature", y = "Mean Absolute SHAP Value", title = paste("Feature Importance for Cluster", cluster_id)) +
    theme_classic()
  ggsave(sprintf("%s/SHAP_Cluster_%s_Importance.pdf", output_dir, cluster_id), plot = importance_plot)
  
  # Create a long format for SHAP values for dot plot
  shap_long <- shap_values %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "shap_value") %>%
    mutate(feature = factor(feature, levels = rev(feature_order)))  # Reversing the order to match feature importance plot
  
  # Generate dot plot with scaled feature values for color
  dot_plot <- ggplot(shap_long, aes(x = shap_value, y = feature, color = scaled_value)) +
    geom_point(alpha = 0.6) +
    scale_color_gradient(low = cluster_light, high = cluster_dark, name = "Scaled Feature Value") +
    labs(title = paste("SHAP Dot Plot for Cluster", cluster_id), x = "SHAP Value", y = "Feature") +
    theme_classic() +
    # Add a vertical dashed dark gray line at a specific SHAP value (for example, at x = 0)
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray", size = 1)
  
  ggsave(sprintf("%s/SHAP_Cluster_%s_Dot_Plot.pdf", output_dir, cluster_id), plot = dot_plot)
}

# Generate SHAP plots for each cluster
unique_clusters <- unique(final_data$cluster)
lapply(unique_clusters, generate_shap_plots_for_cluster, model = rf_model2, kept_drivers = final_data, scaled_data = scaled_data, output_dir = output_dir)
