###############################################################################
# COMPLETE WORKFLOW: Create Grid of LM and SHAP Dot Plots and Overall Bar Plots
# WITH LETTERS ON BOTH EXPORTED PLOTS
###############################################################################

# Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, 
                 randomForest, tibble, viridis, RColorBrewer, patchwork, scales)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# ------------------------- FNConc Data (Concentration) -------------------------
# Load required FNConc data and model
load("FNConc_Yearly_rf_model2_full_new.RData")
rf_model2_FNConc <- rf_model2
load("FNConc_Yearly_kept_drivers__full_new.RData")
kept_drivers_FNConc <- kept_drivers
load("FNConc_Yearly_full_new.RData")
drivers_numeric_FNConc <- drivers_numeric
load("FNConc_Yearly_full_stream_ids_full_new.RData")  # if needed
load("FNConc_Yearly_shap_values_new.RData")
shap_values_FNConc <- shap_values_FNConc

# ------------------------- FNYield Data (Yield) -------------------------
# Load required FNYield data and model
load("FNYield_Yearly_rf_model2_full_new.RData")
rf_model2_FNYield <- rf_model2
load("FNYield_Yearly_kept_drivers_full_new.RData")
kept_drivers_FNYield <- kept_drivers
load("FNYield_Yearly_full_new.RData")
drivers_numeric_FNYield <- drivers_numeric
load("FNYield_Yearly_full_stream_ids_new.RData")  # if needed
load("FNYield_Yearly_shap_values_new.RData")
shap_values_FNYield <- shap_values_FNYield

# Set global seed and output directory
set.seed(123)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNConc"

###############################################################################
# Create ggplot LM Plots for predicted vs. observed for Concentration and Yield
###############################################################################

# ----- For FNConc (Concentration) -----
df_FNConc <- data.frame(
  predicted = rf_model2_FNConc$predicted,
  observed  = drivers_numeric_FNConc$FNConc  # adjust if needed
)

# Compute OOB R² and RMSE for FNConc
oob_r2_FNConc <- mean(rf_model2_FNConc$rsq)
r2_val_FNConc   <- format(oob_r2_FNConc, digits = 3)
rmse_val_FNConc <- format(sqrt(mean(rf_model2_FNConc$mse)), digits = 3)

lm_plot_FNConc <- ggplot(df_FNConc, aes(x = predicted, y = observed)) +
  geom_point(shape = 16, size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "#6699CC", 
              linetype = "dashed", size = 1.2) +
  labs(x = "Predicted", y = "Observed", title = "Concentration") +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  ) +
  # Annotate with OOB R² on the top left
  annotate("text", x = max(df_FNConc$predicted)*0.2, 
           y = max(df_FNConc$observed)*0.95, 
           label = paste("R² =", r2_val_FNConc), 
           hjust = 0.5, size = 5) +
  # Annotate with RMSE on the bottom right
  annotate("text", x = max(df_FNConc$predicted)*0.95, 
           y = min(df_FNConc$observed)*1.05, 
           label = paste("RMSE =", rmse_val_FNConc), 
           hjust = 1, size = 5)

# ----- For FNYield (Yield) -----
df_FNYield <- data.frame(
  predicted = rf_model2_FNYield$predicted,
  observed  = drivers_numeric_FNYield$FNYield  # adjust if needed
)

# Compute OOB R² and RMSE for FNYield
oob_r2_FNYield <- mean(rf_model2_FNYield$rsq)
r2_val_FNYield   <- format(oob_r2_FNYield, digits = 3)
rmse_val_FNYield <- format(sqrt(mean(rf_model2_FNYield$mse)), digits = 3)

lm_plot_FNYield <- ggplot(df_FNYield, aes(x = predicted, y = observed)) +
  geom_point(shape = 16, size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "#6699CC", 
              linetype = "dashed", size = 1.2) +
  labs(x = "Predicted", y = NULL, title = "Yield") +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  ) +
  annotate("text", x = max(df_FNYield$predicted)*0.2, 
           y = max(df_FNYield$observed)*0.95, 
           label = paste("R² =", r2_val_FNYield), 
           hjust = 0.5, size = 5) +
  annotate("text", x = max(df_FNYield$predicted)*0.95, 
           y = min(df_FNYield$observed)*1.05, 
           label = paste("RMSE =", rmse_val_FNYield), 
           hjust = 1, size = 5)



###############################################################################
# Create SHAP Plots Function (for both Concentration and Yield)
###############################################################################
compute_mean_absolute_shap <- function(shap_values) {
  # Step 1: Convert SHAP values to a data frame and calculate absolute values
  df_abs <- as.data.frame(shap_values) %>% 
    mutate(across(everything(), abs))
  
  # Optional: Print or inspect df_abs to verify absolute values
  # print(head(df_abs))
  
  # Step 2: Compute the mean of the absolute values for each feature
  mean_abs <- df_abs %>% 
    summarise(across(everything(), ~ mean(., na.rm = TRUE)))
  
  # Step 3: Reshape the data to long format and arrange in descending order of importance
  result <- mean_abs %>% 
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance))
  
  return(result)
}

# Compute and print SHAP importances for FNConc and FNYield
overall_feature_importance_FNConc <- compute_mean_absolute_shap(shap_values_FNConc)
overall_feature_importance_FNYield <- compute_mean_absolute_shap(shap_values_FNYield)

print("Overall Feature Importance for FNConc:")
print(overall_feature_importance_FNConc)
print("Overall Feature Importance for FNYield:")
print(overall_feature_importance_FNYield)

# Compute the mean of the absolute values for each column
mean_abs <- apply(abs(shap_values_FNConc), 2, mean, na.rm = TRUE)
print(mean_abs)

create_shap_plots <- function(shap_values, kept_drivers, output_dir) {
  # Compute overall feature importance (mean absolute SHAP)
  shap_values_abs <- as.data.frame(shap_values) %>% mutate(across(everything(), abs))
  overall_feature_importance <- shap_values_abs %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    pivot_longer(cols = everything(), names_to = "feature", values_to = "importance") %>%
    arrange(desc(importance)) %>%
    mutate(feature = recode(feature,
                            "elevation" = "Elevation",
                            "basin_slope" = "Basin Slope",
                            "P" = "P",
                            "rocks_volcanic" = "Volcanic Rock",
                            "evapotrans" = "ET",
                            "land_urban_and_built_up_land" = "Land: Urban & Built-Up",
                            "temp" = "Temperature",
                            "land_shrubland_grassland" = "Land: Shrubland & Grassland",
                            "land_forest_all" = "Land: Forest",
                            "land_cropland" = "Land: Cropland",
                            "npp" = "NPP",
                            "rocks_sedimentary" = "Sedimentary Rock",
                            "rocks_plutonic" = "Plutonic Rock",
                            "NOx" = "NOx",
                            "rocks_metamorphic" = "Metamorphic Rock",
                            "rocks_carbonate_evaporite" = "Carbonite & Evaporite Rock",
                            "permafrost" = "Permafrost",
                            "precip" = "Precipitation"))
  
  print("Computed Overall Feature Importance (Descending Order):")
  print(overall_feature_importance)
  
  overall_importance_plot <- ggplot(overall_feature_importance, 
                                    aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "#5F7F94") +
    coord_flip() +
    labs(x = NULL, y = "Mean Absolute SHAP Value") +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y      = element_text(size = 14),
      axis.title.x     = element_text(size = 14),
      plot.title       = element_text(size = 14)
    )
  
  # Save overall plot if desired
  ggsave(
    filename = sprintf("%s/SHAP_FNConc_Ave_Overall_Variable_Importance.png", output_dir),
    plot     = overall_importance_plot,
    width    = 9,
    height   = 8,
    dpi      = 300
  )
  
  # Scale kept_drivers using rescale()
  kept_drivers_scaled <- kept_drivers %>%
    mutate(across(everything(), ~ rescale(., to = c(0,1))))
  
  kept_drivers_with_id <- kept_drivers_scaled %>%
    mutate(id = seq_len(nrow(.))) %>%
    pivot_longer(-id, names_to = "feature", values_to = "feature_value")
  
  shap_values_df <- as.data.frame(shap_values) %>%
    mutate(id = seq_len(nrow(shap_values)))
  
  shap_long <- shap_values_df %>%
    pivot_longer(-id, names_to = "feature", values_to = "shap_value") %>%
    left_join(kept_drivers_with_id, by = c("id", "feature")) %>%
    mutate(feature = recode(feature,
                            "elevation" = "Elevation",
                            "basin_slope" = "Basin Slope",
                            "P" = "P",
                            "rocks_volcanic" = "Volcanic Rock",
                            "evapotrans" = "ET",
                            "land_urban_and_built_up_land" = "Land: Urban & Built-Up",
                            "temp" = "Temperature",
                            "land_shrubland_grassland" = "Land: Shrubland & Grassland",
                            "land_forest_all" = "Land: Forest",
                            "land_cropland" = "Land: Cropland",
                            "npp" = "NPP",
                            "rocks_sedimentary" = "Sedimentary Rock",
                            "rocks_plutonic" = "Plutonic Rock",
                            "NOx" = "NOx",
                            "rocks_metamorphic" = "Metamorphic Rock",
                            "rocks_carbonate_evaporite" = "Carbonite & Evaporite Rock",
                            "permafrost" = "Permafrost",
                            "precip"= "Precipitation"))
  
  # Set factor levels for the dot plot based on overall importance order
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  print("Assigned Factor Levels to shap_long$feature:")
  print(levels(shap_long$feature))  # Debugging check
  
  dot_plot <- ggplot(shap_long, aes(x = shap_value, 
                                    y = feature, 
                                    fill = feature_value)) +
    geom_point(alpha = 0.6, size = 3, shape = 21, stroke = 0.1, color = "black") +
    scale_fill_gradientn(
      colors = c("white", "gray", "black"),
      name   = "Feature Value"
    ) +
    labs(x = "SHAP Value", y = NULL) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    theme_classic(base_size = 14) +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      axis.text  = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.key.size = unit(1, "lines"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(list(overall = overall_importance_plot, dot = dot_plot))
}

# Create SHAP plots for FNConc (Concentration)
fnconc_shap <- create_shap_plots(shap_values_FNConc, kept_drivers_FNConc, output_dir)
overall_plot_FNConc <- fnconc_shap$overall + labs(title = "Concentration") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
dot_plot_FNConc <- fnconc_shap$dot + labs(title = "Concentration") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))

# Create SHAP plots for FNYield (Yield)
fnyield_shap <- create_shap_plots(shap_values_FNYield, kept_drivers_FNYield, output_dir)
overall_plot_FNYield <- fnyield_shap$overall + labs(title = "Yield") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
dot_plot_FNYield <- fnyield_shap$dot + labs(title = "Yield") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))

###############################################################################
# Combine LM and Dot Plots into a Grid with a Combined Legend for Dot Plots
###############################################################################
library(cowplot)

##############################
# 1. Remove Legends from Dot Plots
##############################
dot_plot_FNConc_no_legend  <- dot_plot_FNConc  + theme(legend.position = "none")
dot_plot_FNYield_no_legend <- dot_plot_FNYield + theme(legend.position = "none")

##############################
# 2. Extract Shared Legend
##############################
legend_dot <- get_legend(
  dot_plot_FNYield + theme(legend.position = "right")
)

##############################
# 3. Label Each Plot
##############################
plotA <- lm_plot_FNConc  + ggtitle("A") + theme(plot.title=element_text(hjust=0,size=14))
plotB <- lm_plot_FNYield + ggtitle("B") + theme(plot.title=element_text(hjust=0,size=14))
plotC <- dot_plot_FNConc_no_legend  + ggtitle("C") + theme(plot.title=element_text(hjust=-1,size=14))
plotD <- dot_plot_FNYield_no_legend + ggtitle("D") + theme(plot.title=element_text(hjust=-1,size=14))

##############################
# 4. Build the Top Row (2×3) 
#    Column 1: A, Column 2: B, Column 3: blank
##############################
top_row_3col <- plot_grid(
  plotA,    # top-left
  plotB,    # top-middle
  NULL,     # top-right cell is blank
  ncol       = 3,
  align      = "hv",
  axis       = "tblr",
  rel_widths = c(1, 1, 0.3)  # 3 columns: left=1, middle=1, right=0.3
)

##############################
# 5. Build the Bottom Row (2×3)
#    Column 1: C, Column 2: D, Column 3: legend
##############################
bottom_row_3col <- plot_grid(
  plotC,           # bottom-left
  plotD,           # bottom-middle
  legend_dot,      # bottom-right
  ncol       = 3,
  align      = "hv",
  axis       = "tblr",
  rel_widths = c(1, 1, 0.3)  # same column widths => B & D line up
)

##############################
# 6. Stack the Two Rows
##############################
final_grid <- plot_grid(
  top_row_3col,
  bottom_row_3col,
  ncol = 1,
  align = "v",
  axis  = "lr"
)

##############################
# 7. Print or Save
##############################
print(final_grid)
ggsave(
  filename = "Final_Grid_FNConc_FNYield.png",
  plot     = final_grid,
  width    = 12,
  height   = 12,
  dpi      = 300,
  path     = output_dir
)


###############################################################################
# Combine Overall (Mean Absolute SHAP) Bar Plots into a Grid
###############################################################################
final_overall_grid <- overall_plot_FNConc | overall_plot_FNYield

# Add letters (A, B) to each bar plot
final_overall_grid_labeled <- final_overall_grid + plot_annotation(tag_levels = "A")

# Save the labeled overall grid
ggsave(filename = "Final_Overall_FNConc_FNYield.png", 
       plot = final_overall_grid_labeled,
       width = 11, height = 6, dpi = 300, path = output_dir)

