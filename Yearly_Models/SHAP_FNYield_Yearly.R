###############################################################################
# COMPLETE WORKFLOW: FNYield Cluster Plotting 
###############################################################################

# Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, 
                 randomForest, tibble, viridis, RColorBrewer)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Load required data and model
load("FNYield_Yearly_rf_model2_full.RData")
load("FNYield_Yearly_kept_drivers_full.RData")
load("FNYield_Yearly_full.RData")
load("FNYield_Yearly_full_stream_ids.RData")

# Load precomputed SHAP values
load("FNYield_Yearly_shap_values.RData")

# Set global seed and output directory
set.seed(123)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"


# Generate plots comparing predicted vs observed ----
lm_plot_FNYield <- plot(rf_model2$predicted, drivers_numeric$FNYield, pch = 16, cex = 1.5,
                       xlab = "Predicted", ylab = "Observed", main = NULL,
                       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5) +
  abline(a = 0, b = 1, col = "#6699CC", lwd = 3, lty = 2) +
  theme(text = element_text(size = 40), face = "bold")
legend("topleft", bty = "n", cex = 1.5, legend = paste("R2 =", format(mean(rf_model2$rsq), digits = 3)))
legend("bottomright", bty = "n", cex = 1.5, legend = paste("MSE =", format(mean(rf_model2$mse), digits = 3)))


create_shap_plots <- function(shap_values, kept_drivers, output_dir) {
  # Compute overall feature importance (mean absolute SHAP value)
  overall_feature_importance <- shap_values %>%
    as.data.frame() %>%
    summarise(across(everything(), ~ mean(abs(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))
  
  # Recode feature names for overall importance (and thus determine order)
  overall_feature_importance <- overall_feature_importance %>%
    mutate(feature = recode(feature,
                            "elevation" = "Elevation",
                            "basin_slope" = "Basin Slope",
                            "P" = "P",
                            "rocks_volcanic" = "Volcanic Rock",
                            "evapotrans" = "ET",
                            "land_urban_and_built_up_land" = "Land: Urban & Built-Up"))
  
  # Overall Feature Importance Bar Plot
  overall_importance_plot_FNYield <- ggplot(overall_feature_importance, 
                                           aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(x = NULL, y = "Mean Absolute SHAP Value", 
         title = NULL) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text       = element_text(size = 14), 
      axis.title.x       = element_text(size = 14), 
      plot.title         = element_text(size = 16, face = "bold")
    )
  
  # Save the overall bar plot as PNG
  ggsave(
    filename = sprintf("%s/SHAP_FNYield_Ave_Overall_Variable_Importance.png", output_dir),
    plot     = overall_importance_plot_FNYield,
    width    = 9,
    height   = 8,
    dpi      = 300
  )
  
  # Scale kept_drivers using rescale()
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
    left_join(kept_drivers_with_id, by = c("id", "feature"))
  
  # Recode feature names for display in the dot plot
  shap_long <- shap_long %>%
    mutate(feature = recode(feature,
                            "elevation" = "Elevation",
                            "basin_slope" = "Basin Slope",
                            "P" = "P",
                            "rocks_volcanic" = "Volcanic Rock",
                            "evapotrans" = "ET",
                            "land_urban_and_built_up_land" = "Land: Urban & Built-Up"))
  
  # Set factor levels for the dot plot based on overall feature importance order
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  # SHAP Dot Plot with a monochromatic gradient from lightsteelblue to steelblue
  dot_plot_FNYield <- ggplot(shap_long, aes(x = shap_value, 
                                           y = feature, 
                                           fill = feature_value)) +
    geom_point(alpha = 0.6, size = 3, shape = 21, stroke = 0.1, color = "black") +
    scale_fill_gradientn(
      colors = c("white", "gray", "black"),  
      name   = "Feature Value"
    ) +
    labs(x = "SHAP Value", y = NULL, title = NULL) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    theme_classic() +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      legend.key.size = unit(1.5, "lines")
    )
  
  # Save the dot plot as PNG
  ggsave(
    filename = sprintf("%s/SHAP_FNYield_Ave_Dot_Plot.png", output_dir),
    plot     = dot_plot_FNYield,
    width    = 9,
    height   = 8,
    dpi      = 300
  )
}

# Create SHAP plots
create_shap_plots(shap_values, kept_drivers, output_dir)
