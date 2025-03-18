###############################################################################
# COMPLETE SCRIPT: Centered Column Titles & Matching Column Widths
#                  with Separate X-Axis Scaling for Conc vs. Yield
###############################################################################

## 0. Load Pre-saved Workflow Objects
load("FNConc_HierClust_Workflow_Objects.RData")
load("FNYield_HierClust_Workflow_Objects.RData")

## 0.1 (Optional) Redefine plot_mean_abs_shap() to include y_limit
plot_mean_abs_shap <- function(cluster_id, shap_values, full_scaled, y_limit = 1.3) {
  cluster_indices <- which(full_scaled$final_cluster == cluster_id)
  shap_cluster    <- shap_values[cluster_indices, , drop = FALSE]
  mean_abs_shap   <- colMeans(abs(shap_cluster), na.rm = TRUE)
  
  df_shap <- data.frame(
    feature          = names(mean_abs_shap),
    mean_abs_shapval = as.numeric(mean_abs_shap)
  ) %>%
    arrange(desc(mean_abs_shapval)) %>%
    filter(!grepl("rock", feature, ignore.case = TRUE))
  
  df_shap$feature <- recode(df_shap$feature,
                            "NOx"    = "Nitrate",
                            "P"      = "Phosphorous",
                            "precip" = "Precip",
                            "temp"   = "Temperature",
                            "snow_cover" = "Snow Cover",
                            "npp"    = "NPP",
                            "evapotrans" = "ET",
                            "greenup_day" = "Greenup Day",
                            "permafrost"  = "Permafrost",
                            "elevation"   = "Elevation",
                            "basin_slope" = "Basin Slope",
                            "FNConc"      = "DSi Concentration")
  
  ggplot(df_shap, aes(x = reorder(feature, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(stat = "identity", fill = my_cluster_colors[[as.character(cluster_id)]], alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(limits = c(0, y_limit)) +
    labs(x = NULL, y = "Mean Absolute SHAP Value", title = NULL) +
    theme_classic(base_size = 14) +
    theme(
      plot.title  = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    )
}

## 1. Load Packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)   # For +, /, wrap_plots(), plot_layout(), wrap_elements()
library(grid)        # For textGrob()

# Set your working directory & output directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model"

unique_clusters <- levels(full_scaled$final_cluster)

###############################################################################
# 1. Updated generate_shap_dot_plot_obj() for separate x-axis scaling
###############################################################################
generate_shap_dot_plot_obj <- function(
    cluster_name,
    shap_values,       # shap_values_FNConc or shap_values_FNYield
    full_scaled,       # data with columns scaled to [0..1]
    shap_min,          # min SHAP for that dataset (conc or yield)
    shap_max           # max SHAP for that dataset (conc or yield)
) {
  # Identify rows for the given cluster
  cluster_indices <- which(full_scaled$final_cluster == cluster_name)
  
  # Subset scaled data for coloring (already in [0..1])
  cluster_data <- full_scaled[cluster_indices, , drop = FALSE] %>%
    dplyr::select(where(is.numeric))
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  # Pivot to long for 'feature_value' from scaled data
  cluster_long <- cluster_data %>%
    tidyr::pivot_longer(
      cols      = -id,
      names_to  = "feature",
      values_to = "feature_value"
    )
  
  # Subset SHAP values for this cluster
  shap_df <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  # Pivot SHAP to long & join
  shap_long <- shap_df %>%
    tidyr::pivot_longer(
      cols      = -id,
      names_to  = "feature",
      values_to = "shap_value"
    ) %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  # Remove 'rock' features if desired
  shap_long <- shap_long %>% filter(!grepl("rock", feature, ignore.case = TRUE))
  
  # Order features by mean absolute SHAP
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  # Recode feature names (optional)
  shap_long$feature <- recode(
    shap_long$feature,
    "FNYield"  = "DSi Yield",
    "FNConc"   = "DSi Concentration",
    "NOx"      = "Nitrate",
    "P"        = "Phosphorous",
    "precip"   = "Precip",
    "temp"     = "Temperature",
    "snow_cover" = "Snow Cover",
    "npp"      = "NPP",
    "evapotrans" = "ET",
    "greenup_day" = "Greenup Day",
    "permafrost"  = "Permafrost",
    "elevation"   = "Elevation",
    "basin_slope" = "Basin Slope"
    # etc. if needed
  )
  
  ggplot(shap_long, aes(
    x    = shap_value,      # SHAP on x-axis
    y    = feature,
    fill = feature_value    # scaled data in [0..1]
  )) +
    geom_point(alpha = 0.6, size = 3, shape = 21, stroke = 0.1, color = "black") +
    # Fill color scale from 0..1
    scale_fill_gradientn(
      colors = c("white", "gray", "black"),
      name   = NULL,
      limits = c(0, 1)  # your scaled data is in [0..1]
    ) +
    labs(x = "SHAP Value", y = NULL, title = NULL) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    scale_x_continuous(limits = c(shap_min, shap_max)) +
    theme_classic() +
    theme(
      axis.title      = element_text(size = 14, face = "bold"),
      axis.text       = element_text(size = 12),
      legend.text     = element_text(size = 12),
      legend.title    = element_text(size = 14),
      legend.key.size = unit(1.5, "lines"),
      plot.title      = element_blank()
    )
}

###############################################################################
# 2. SHAP Bar Plots (3 columns × 5 rows) + Column Titles
###############################################################################
plot_list_bars_conc <- lapply(unique_clusters, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNConc, full_scaled, y_limit = 1.3)
})
plot_list_bars_yield <- lapply(unique_clusters, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNYield, full_scaled, y_limit = 1300)
})

# For each row, combine 3 subplots horizontally with equal widths
rows_list <- lapply(seq_along(unique_clusters), function(i) {
  p1 <- cluster_boxplots[[i]]
  p2 <- plot_list_bars_conc[[i]]
  p3 <- plot_list_bars_yield[[i]]
  
  # Remove x-axis from each subplot if not the bottom row
  if (i < length(unique_clusters)) {
    p1 <- p1 + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
    p2 <- p2 + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
    p3 <- p3 + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  # Combine horizontally with equal widths
  (p1 + p2 + p3) + plot_layout(ncol = 3, widths = c(1, 1, 1))
})

# Stack all row-plots vertically with ncol=1
bar_plots_combined <- wrap_plots(rows_list, ncol = 1)

# Create a title row for the bar plots
title1 <- wrap_elements(
  full = textGrob("All Variables", x = 0.5, hjust = 0.5,
                  gp = gpar(fontsize = 16, fontface = "bold"))
)
title2 <- wrap_elements(
  full = textGrob("Concentration", x = 0.5, hjust = 0.5,
                  gp = gpar(fontsize = 16, fontface = "bold"))
)
title3 <- wrap_elements(
  full = textGrob("Yield", x = 0.5, hjust = 0.5,
                  gp = gpar(fontsize = 16, fontface = "bold"))
)
title_row <- (title1 + title2 + title3) + plot_layout(ncol = 3, widths = c(1,1,1))

# Combine the title row with the bar plot rows vertically
final_grid_bar <- title_row / bar_plots_combined +
  plot_layout(heights = c(0.8, 10))  # Adjust to move titles further down

ggsave(
  filename = "Combined_Grid_BarPlots.png",
  plot     = final_grid_bar,
  width    = 16,
  height   = 18,
  dpi      = 300,
  path     = output_dir
)
print(final_grid_bar)

###############################################################################
# 3. SHAP Dot Plots (2 columns × 5 rows) + Column Titles
###############################################################################
# Compute min/max SHAP for each dataset separately
global_shap_min_conc <- min(shap_values_FNConc, na.rm = TRUE)
global_shap_max_conc <- max(shap_values_FNConc, na.rm = TRUE)

global_shap_min_yield <- min(shap_values_FNYield, na.rm = TRUE)
global_shap_max_yield <- max(shap_values_FNYield, na.rm = TRUE)

# Generate dot plots for FNConc (Concentrations)
dot_plots_conc <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(
    cl,
    shap_values_FNConc,
    full_scaled,
    shap_min  = global_shap_min_conc,
    shap_max  = global_shap_max_conc
  )
})

# Generate dot plots for FNYield (Yields)
dot_plots_yield <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(
    cl,
    shap_values_FNYield,
    full_scaled,
    shap_min  = global_shap_min_yield,
    shap_max  = global_shap_max_yield
  )
})

# For each row, combine 2 subplots horizontally with equal widths
rows_dot <- lapply(seq_along(unique_clusters), function(i) {
  p_conc  <- dot_plots_conc[[i]]
  p_yield <- dot_plots_yield[[i]]
  
  if (i < length(unique_clusters)) {
    p_conc  <- p_conc  + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
    p_yield <- p_yield + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  (p_conc + p_yield) + plot_layout(ncol = 2, widths = c(1,1))
})

# Stack all row-plots vertically with ncol=1
dot_plots_combined <- wrap_plots(rows_dot, ncol = 1)

# Create a title row for the dot plots
dot_title1 <- wrap_elements(
  full = textGrob("Concentration", x = 0.5, hjust = 0.5,
                  gp = gpar(fontsize = 16, fontface = "bold"))
)
dot_title2 <- wrap_elements(
  full = textGrob("Yield", x = 0.5, hjust = 0.5,
                  gp = gpar(fontsize = 16, fontface = "bold"))
)
dot_title_row <- (dot_title1 + dot_title2) + plot_layout(ncol = 2, widths = c(1,1))

# Combine the title row with the dot plot rows
final_grid_dot <- dot_title_row / dot_plots_combined +
  plot_layout(heights = c(0.8, 10))

ggsave(
  filename = "Combined_Grid_DotPlots.png",
  plot     = final_grid_dot,
  width    = 16,
  height   = 18,
  dpi      = 300,
  path     = output_dir
)
print(final_grid_dot)
