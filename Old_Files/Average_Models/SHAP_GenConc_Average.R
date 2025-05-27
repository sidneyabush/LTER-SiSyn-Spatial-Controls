# Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, randomForest, tibble, viridis)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Load required data and model from the RF script
load("GenConc_Average_rf_model2_full.RData")
load("GenConc_Average_kept_drivers_full.RData")
load("GenConc_Average_full.RData")

# Set global seed and output directory
set.seed(123)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Average_Model/GenConc"

# Function to create SHAP values
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  # Define a custom prediction function
  custom_predict <- function(object, newdata) {
    newdata <- as.data.frame(newdata)
    predict(object, newdata = newdata)
  }
  
  # Compute SHAP values using fastshap
  shap_values <- fastshap::explain(
    object = model,
    X = kept_drivers,
    pred_wrapper = custom_predict,
    nsim = sample_size
  )
  
  return(shap_values)
}

# Generate SHAP values
shap_values <- generate_shap_values(rf_model2, kept_drivers, sample_size = 30)

# Function to create overall SHAP plots for the full model
create_all_shapley_plots <- function(shap_values, output_file) {
  # Calculate overall feature importance
  overall_feature_importance <- shap_values %>%
    as.data.frame() %>%
    summarise(across(everything(), ~ mean(abs(.)))) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))
  
  # Create the feature importance plot
  pdf(output_file, width = 9, height = 8)
  overall_importance_plot <- ggplot(overall_feature_importance, aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(x = "Feature", y = "Mean Absolute SHAP Value", title = "GenConc Average - Overall Feature Importance") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 14),  # Increase Y-axis (drivers) text size
      axis.title.x = element_text(size = 14),  # Increase X-axis title size
      plot.title = element_text(size = 16, face = "bold")  # Increase title size
    )
  print(overall_importance_plot)
  dev.off()
}


# Output full SHAP plots
output_file <- sprintf("%s/SHAP_GenConc_Ave_Overall_Variable_Importance.pdf", output_dir)
create_all_shapley_plots(shap_values, output_file)


# Function to create SHAP dot plot showing positive/negative influence
create_all_shapley_dot_plot <- function(shap_values, output_file) {
  # Convert SHAP values to a data frame
  shap_df <- as.data.frame(shap_values)
  
  # Calculate mean and absolute mean SHAP values for each feature
  feature_importance <- shap_df %>%
    summarise(across(everything(), list(mean = mean, abs_mean = ~ mean(abs(.))))) %>%
    pivot_longer(cols = everything(), names_to = c("feature", ".value"), names_sep = "_") %>%
    arrange(desc(abs_mean))  # Order by absolute importance
  
  # Create the SHAP dot plot
  pdf(output_file, width = 9, height = 8)
  shap_dot_plot <- ggplot(feature_importance, aes(x = reorder(feature, abs_mean), y = mean)) +
    geom_point(aes(color = mean), size = 4) +  # Color indicates directionality
    geom_segment(aes(x = feature, xend = feature, y = 0, yend = mean), color = "gray") + # Line to zero
    scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +  # Red = negative, Blue = positive
    coord_flip() +
    labs(x = "Feature", y = "Mean SHAP Value", title = "GenConc Average - SHAP Variable Importance") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  print(shap_dot_plot)
  dev.off()
}

# Output SHAP dot plot
output_file <- sprintf("%s/SHAP_GenConc_Ave_Overall_Variable_Importance_DotPlot.pdf", output_dir)
create_all_shapley_dot_plot(shap_values, output_file)

