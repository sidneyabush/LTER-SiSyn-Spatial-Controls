###############################################################################
# COMPLETE REVISED WORKFLOW: Combine FNConc & FNYield Plots WITH COLUMN TITLES
###############################################################################

## 0. Load Pre-saved Workflow Objects
# These files should contain:
#   - full_scaled
#   - cluster_boxplots
#   - shap_values_FNConc
#   - shap_values_FNYield
#   - generate_shap_dot_plot_obj() (function)
#   - global_shap_min, global_shap_max (optional for dot plots)
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
    # Optionally remove 'rock' features if you like
    filter(!grepl("rock", feature, ignore.case = TRUE))
  
  # Optional recoding of feature names
  df_shap$feature <- recode(
    df_shap$feature,
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
    "FNConc"      = "DSi Concentration"  # yields remain "FNYield"
  )
  
  ggplot(df_shap, aes(x = reorder(feature, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(
      stat  = "identity",
      fill  = my_cluster_colors[[as.character(cluster_id)]],
      alpha = 0.8
    ) +
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

## 1. Load Packages (if not already loaded)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)   # For wrap_plots(), plot_annotation()

# Set your working directory & output directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model"

# Unique clusters (Volcanic, Sedimentary, etc.)
unique_clusters <- levels(full_scaled$final_cluster)

###############################################################################
# 2. Create a Grid for SHAP Bar Plots (3 columns × 5 rows)
###############################################################################
# 2.1 Create separate bar plots for concentrations & yields (with different y_limits)
plot_list_bars_conc <- lapply(unique_clusters, function(cl) {
  # e.g. y_limit=1.3 for FNConc
  plot_mean_abs_shap(cl, shap_values_FNConc, full_scaled, y_limit = 1.3)
})
plot_list_bars_yield <- lapply(unique_clusters, function(cl) {
  # e.g. y_limit=1300 for FNYield
  plot_mean_abs_shap(cl, shap_values_FNYield, full_scaled, y_limit = 1300)
})

# 2.2 Build each row: [ driver box plot | bar_conc | bar_yield ]
rows_list <- lapply(seq_along(unique_clusters), function(i) {
  
  # Subplots for this row
  p1 <- cluster_boxplots[[i]]    # "All Variables"
  p2 <- plot_list_bars_conc[[i]] # "Concentration"
  p3 <- plot_list_bars_yield[[i]]# "Yield"
  
  # If it's the TOP row (i == 1), add column titles
  if (i == 1) {
    p1 <- p1 + ggtitle("All Variables")
    p2 <- p2 + ggtitle("Concentration")
    p3 <- p3 + ggtitle("Yield")
  }
  
  # If it's NOT the last row => remove x-axis from each subplot
  if (i < length(unique_clusters)) {
    p1 <- p1 + theme(axis.title.x = element_blank(),
                     axis.text.x  = element_blank(),
                     axis.ticks.x = element_blank())
    p2 <- p2 + theme(axis.title.x = element_blank(),
                     axis.text.x  = element_blank(),
                     axis.ticks.x = element_blank())
    p3 <- p3 + theme(axis.title.x = element_blank(),
                     axis.text.x  = element_blank(),
                     axis.ticks.x = element_blank())
  }
  
  wrap_plots(p1, p2, p3, ncol = 3)
})

# 2.3 Combine rows vertically
final_grid_bar <- wrap_plots(rows_list, ncol = 1)

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
# 3. Create a Grid for SHAP Dot Plots (2 columns × 5 rows)
###############################################################################
# 3.1 Compute global min & max for dot plots (if you want the same across all)
global_shap_min <- min(full_scaled %>% dplyr::select(where(is.numeric)), na.rm = TRUE)
global_shap_max <- max(full_scaled %>% dplyr::select(where(is.numeric)), na.rm = TRUE)

# 3.2 Generate dot plots for FNConc vs. FNYield
dot_plots_conc <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(cl, shap_values_FNConc, full_scaled, global_shap_min, global_shap_max)
})
dot_plots_yield <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(cl, shap_values_FNYield, full_scaled, global_shap_min, global_shap_max)
})

# 3.3 Build each row: [ dot_conc | dot_yield ]
rows_dot <- lapply(seq_along(unique_clusters), function(i) {
  
  p_conc  <- dot_plots_conc[[i]]   # "Concentration"
  p_yield <- dot_plots_yield[[i]]  # "Yield"
  
  # If it's the TOP row (i == 1), add column titles
  if (i == 1) {
    p_conc  <- p_conc  + ggtitle("Concentration")
    p_yield <- p_yield + ggtitle("Yield")
  }
  
  # If it's NOT the last row => remove x-axis from each subplot
  if (i < length(unique_clusters)) {
    p_conc <- p_conc + theme(axis.title.x = element_blank(),
                             axis.text.x  = element_blank(),
                             axis.ticks.x = element_blank())
    p_yield <- p_yield + theme(axis.title.x = element_blank(),
                               axis.text.x  = element_blank(),
                               axis.ticks.x = element_blank())
  }
  
  wrap_plots(p_conc, p_yield, ncol = 2)
})

# 3.4 Combine rows vertically
final_grid_dot <- wrap_plots(rows_dot, ncol = 1)

ggsave(
  filename = "Combined_Grid_DotPlots.png",
  plot     = final_grid_dot,
  width    = 16,
  height   = 18,
  dpi      = 300,
  path     = output_dir
)
print(final_grid_dot)
