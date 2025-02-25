# Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, randomForest, tibble, viridis, RColorBrewer)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Load required data and model from the RF script
load("FNYield_Yearly_rf_model2_full.RData")
load("FNYield_Yearly_kept_drivers_full.RData")
load("FNYield_Yearly_full.RData")
load("FNYield_Yearly_full_stream_ids.RData")

# Already ran SHAP values:
load("FNYield_Yearly_shap_values.RData")

# Set global seed and output directory
set.seed(123)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"

# # Function to create SHAP values, uncomment to run new data
# generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
#   # Define a custom prediction function
#   custom_predict <- function(object, newdata) {
#     newdata <- as.data.frame(newdata)
#     predict(object, newdata = newdata)
#   }
#   
#   # Compute SHAP values using fastshap
#   shap_values <- fastshap::explain(
#     object = model,
#     X = kept_drivers,
#     pred_wrapper = custom_predict,
#     nsim = sample_size
#   )
#   
#   return(shap_values)
# }
# 
# # Generate SHAP values
# shap_values <- generate_shap_values(rf_model2, kept_drivers, sample_size = 30)

# Save SHAP values for future use
save(shap_values, file = "FNYield_Yearly_shap_values.RData")

create_shap_plots <- function(shap_values, kept_drivers, output_dir) {
  # Compute overall feature importance (mean absolute SHAP value)
  overall_feature_importance <- shap_values %>%
    as.data.frame() %>%
    summarise(across(everything(), ~ mean(abs(.)))) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))
  
  # Feature importance bar plot
  pdf(sprintf("%s/SHAP_FNYield_Ave_Overall_Variable_Importance.pdf", output_dir), width = 9, height = 8)
  overall_importance_plot <- ggplot(overall_feature_importance, 
                                    aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(x = "Feature", y = "Mean Absolute SHAP Value", 
         title = "FNYield Yearly - Overall Feature Importance") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 14), 
          axis.title.x = element_text(size = 14), 
          plot.title = element_text(size = 16, face = "bold"))
  print(overall_importance_plot)  
  dev.off()
  
  # Scale kept_drivers using base R's scale() on a numeric matrix
  kept_drivers_scaled <- kept_drivers %>%
    mutate(across(everything(), ~ rescale(., to = c(0,1))))
  
  # Add id and pivot to long format
  kept_drivers_with_id <- kept_drivers_scaled %>%
    mutate(id = seq_len(nrow(.))) %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  # Prepare SHAP values (without scaling them)
  shap_values_df <- as.data.frame(shap_values) %>%
    mutate(id = seq_len(nrow(shap_values)))
  
  shap_long <- shap_values_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(kept_drivers_with_id, by = c("id", "feature")) %>%
    mutate(feature = factor(feature, levels = rev(overall_feature_importance$feature)))
  
  # SHAP Dot Plot with a monochromatic gradient from lightsteelblue to steelblue
  pdf(sprintf("%s/SHAP_FNYield_Ave_Dot_Plot.pdf", output_dir), width = 9, height = 8)
  dot_plot <- ggplot(shap_long, aes(x = shap_value, 
                                    y = feature, 
                                    color = feature_value)) +
    geom_point(alpha = 0.6) +
    scale_color_gradientn(
      colors = rev(brewer.pal(5, "RdYlBu")),  # Reverse to go Red → Yellow → Blue
      name = NULL) +
    labs(x = "SHAP Value", y = "Feature", 
         title = "FNYield Yearly - Overall Feature Importance") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey1") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 14), 
          axis.title.x = element_text(size = 14), 
          plot.title = element_text(size = 16, face = "bold"))
  print(dot_plot)  
  dev.off()
}

# Create SHAP plots
create_shap_plots(shap_values, kept_drivers, output_dir)
