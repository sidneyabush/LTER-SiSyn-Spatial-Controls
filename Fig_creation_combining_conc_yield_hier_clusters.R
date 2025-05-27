###############################################################################
# COMPLETE SCRIPT: Single Y-Axis Label, Mean SHAP Plot X-Axis Label Only on Bottom Row,
#                 Land Recoding, Separate X-Axis Scaling for Dot Plots,
#                 Combined Legend for Dot Plots, Patchwork Layout
###############################################################################

## 0. Load Pre-saved Workflow Objects
load("FNConc_HierClust_Workflow_Objects.RData")
load("FNYield_HierClust_Workflow_Objects.RData")

my_cluster_colors <- c(
  "Volcanic"            = "#AC7B32",  
  "Sedimentary"         = "#579C8E",  
  "Mixed Sedimentary"   = "#89C8A0",
  "Plutonic"            = "#8D9A40",
  "Metamorphic"         = "#C26F86",  
  "Carbonate Evaporite" = "#5E88B0"   
)

# my_cluster_colors_lighter <- sapply(my_cluster_colors, function(x) lighten(x, amount = 0.3))

## 0.1 Redefine plot_mean_abs_shap() with recoding (including land types)
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
  
  # Recoding including land types
  df_shap$feature <- recode(
    df_shap$feature,
    "FNYield" = "DSi Yield",
    "FNConc"  = "DSi Concentration",
    "NOx" = "Nitrate",
    "P" = "P",
    "precip" = "Precip",
    "temp" = "Temperature",
    "snow_cover" = "Snow Cover",
    "npp" = "NPP",
    "evapotrans" = "ET",
    "greenup_day" = "Greenup Day",
    "permafrost" = "Permafrost",
    "elevation" = "Elevation",
    "RBFI" = "Flashiness Index",
    "basin_slope" = "Basin Slope",
    "rocks_volcanic" = "Rock: Volcanic",
    "rocks_sedimentary" = "Rock: Sedimentary",
    "rocks_carbonate_evaporite" = "Rock: Carbonate Evaporite",
    "rocks_metamorphic" = "Rock: Metamorphic",
    "rocks_plutonic" = "Rock: Plutonic",
    "land_Bare" = "Land: Bare", 
    "land_Cropland" = "Land: Cropland", 
    "land_Forest" = "Land: Forest",
    "land_Grassland_Shrubland" = "Land: Grassland & Shrubland", 
    "land_Ice_Snow" = "Land: Ice & Snow", 
    "land_Impervious" = "Land: Impervious", 
    "land_Salt_Water" = "Land: Salt Water",
    "land_Tidal_Wetland" = "Land: Tidal Wetland", 
    "land_Water" = "Land: Water Body", 
    "land_Wetland_Marsh" = "Land: Wetland Marsh"
  )
  
  ggplot(df_shap, aes(x = reorder(feature, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(stat = "identity", fill = my_cluster_colors[[as.character(cluster_id)]], alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(limits = c(0, y_limit)) +
    labs(x = NULL, y = NULL, title = NULL) +  # We'll add the x-label selectively
    theme_classic(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    )
}

## 1. Load Packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)   # For wrap_plots(), plot_layout(), wrap_elements()
library(grid)        # For textGrob()

# Set your working directory & output directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures"

unique_clusters <- levels(full_scaled$final_cluster)

###############################################################################
# 2. Dot-Plot Function with recoding for land types + single fill scale
###############################################################################
generate_shap_dot_plot_obj <- function(
    cluster_name,
    shap_values,       # Either shap_values_FNConc or shap_values_FNYield
    full_scaled,       # Data with columns scaled to [0,1]
    shap_min,          # min SHAP for that dataset
    shap_max           # max SHAP for that dataset
) {
  cluster_indices <- which(full_scaled$final_cluster == cluster_name)
  
  # Subset scaled data for coloring
  cluster_data <- full_scaled[cluster_indices, , drop = FALSE] %>%
    dplyr::select(where(is.numeric))
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  cluster_long <- cluster_data %>%
    tidyr::pivot_longer(
      cols = -id,
      names_to = "feature",
      values_to = "feature_value"
    )
  
  shap_df <- as.data.frame(shap_values)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  shap_long <- shap_df %>%
    tidyr::pivot_longer(
      cols = -id,
      names_to = "feature",
      values_to = "shap_value"
    ) %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  shap_long <- shap_long %>% filter(!grepl("rock", feature, ignore.case = TRUE))
  
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  # Recoding including land types
  shap_long$feature <- recode(
    shap_long$feature,
    "FNYield"     = "DSi Yield",
    "FNConc"      = "DSi Concentration",
    "NOx" = "Nitrate",
    "P" = "P",
    "precip" = "Precip",
    "temp" = "Temperature",
    "snow_cover" = "Snow Cover",
    "npp" = "NPP",
    "evapotrans" = "ET",
    "greenup_day" = "Greenup Day",
    "permafrost" = "Permafrost",
    "elevation" = "Elevation",
    "RBFI" = "Flashiness Index",
    "basin_slope" = "Basin Slope",
    "rocks_volcanic" = "Rock: Volcanic",
    "rocks_sedimentary" = "Rock: Sedimentary",
    "rocks_carbonate_evaporite" = "Rock: Carbonate Evaporite",
    "rocks_metamorphic" = "Rock: Metamorphic",
    "rocks_plutonic" = "Rock: Plutonic",
    "land_Bare" = "Land: Bare", 
    "land_Cropland" = "Land: Cropland", 
    "land_Forest" = "Land: Forest",
    "land_Grassland_Shrubland" = "Land: Grassland & Shrubland", 
    "land_Ice_Snow" = "Land: Ice & Snow", 
    "land_Impervious" = "Land: Impervious", 
    "land_Salt_Water" = "Land: Salt Water",
    "land_Tidal_Wetland" = "Land: Tidal Wetland", 
    "land_Water" = "Land: Water Body", 
    "land_Wetland_Marsh" = "Land: Wetland Marsh"
  )
  
  ggplot(shap_long, aes(
    x    = shap_value,
    y    = feature,
    fill = feature_value
  )) +
    geom_point(alpha = 0.6, size = 3, shape = 21, stroke = 0.1, color = "black") +
    scale_fill_gradientn(
      colors = c("white", "gray", "black"),
      name   = "Scaled Value",
      limits = c(0, 1)
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
# 3. SHAP Bar Plots (3 columns × 5 rows) + Column Titles, Single Y-Axis
###############################################################################
plot_list_bars_conc <- lapply(unique_clusters, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNConc, full_scaled, y_limit = 0.7)
})
plot_list_bars_yield <- lapply(unique_clusters, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNYield, full_scaled, y_limit = 850)
})

rows_list <- lapply(seq_along(unique_clusters), function(i) {
  # cluster_boxplots[[i]] is the "All Variables" subplot
  p_all_vars <- cluster_boxplots[[i]] + labs(y = NULL)
  
  p_conc  <- plot_list_bars_conc[[i]]
  p_yield <- plot_list_bars_yield[[i]]
  
  # If not the last row => remove x-axis from these 3 subplots
  if (i < length(unique_clusters)) {
    p_all_vars <- p_all_vars + theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
    p_conc <- p_conc + theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
    p_yield <- p_yield + theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
  } else {
    # LAST row => we want an x-axis label on the bar subplots for Mean SHAP
    p_conc  <- p_conc  + labs(y = "Mean Absolute SHAP Value")
    p_yield <- p_yield + labs(y = "Mean Absolute SHAP Value")
  }
  
  (p_all_vars + p_conc + p_yield) + plot_layout(ncol = 3, widths = c(1,1,1))
})

bar_plots_combined <- wrap_plots(rows_list, ncol = 1)

# Column titles
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

# Single y-axis label
y_axis_label <- wrap_elements(
  full = textGrob("Scaled Value", rot = 90, gp = gpar(fontsize = 16, fontface = "bold"))
)

bar_plots_with_title <- title_row / bar_plots_combined + plot_layout(heights = c(0.8, 10))

# Make the left column narrower so the label is closer
final_grid_bar <- (y_axis_label | bar_plots_with_title) +
  plot_layout(widths = c(0.06, 1))

ggsave(
  filename = "Fig4_Combined_Grid_BarPlots.png",
  plot = final_grid_bar,
  width = 22,
  height = 25,
  dpi = 300,
  path = output_dir
)
print(final_grid_bar)

###############################################################################
# 4. SHAP Dot Plots (2 columns × 5 rows) + Column Titles, with Combined Legend
###############################################################################
global_shap_min_conc <- min(shap_values_FNConc, na.rm = TRUE)
global_shap_max_conc <- max(shap_values_FNConc, na.rm = TRUE)
global_shap_min_yield <- min(shap_values_FNYield, na.rm = TRUE)
global_shap_max_yield <- max(shap_values_FNYield, na.rm = TRUE)

dot_plots_conc <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(
    cluster_name = cl,
    shap_values  = shap_values_FNConc,
    full_scaled  = full_scaled,
    shap_min     = global_shap_min_conc,
    shap_max     = global_shap_max_conc
  )
})
dot_plots_yield <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(
    cluster_name = cl,
    shap_values  = shap_values_FNYield,
    full_scaled  = full_scaled,
    shap_min     = global_shap_min_yield,
    shap_max     = global_shap_max_yield
  )
})

rows_dot <- lapply(seq_along(unique_clusters), function(i) {
  p_conc  <- dot_plots_conc[[i]]
  p_yield <- dot_plots_yield[[i]]
  
  if (i < length(unique_clusters)) {
    p_conc  <- p_conc  + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
    p_yield <- p_yield + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  (p_conc + p_yield) + plot_layout(ncol = 2, widths = c(1,1))
})

dot_plots_combined <- wrap_plots(rows_dot, ncol = 1)

dot_title1 <- wrap_elements(
  full = textGrob("Concentration", x = 0.5, hjust = 0.5,
                  gp = gpar(fontsize = 16, fontface = "bold"))
)
dot_title2 <- wrap_elements(
  full = textGrob("Yield", x = 0.5, hjust = 0.5,
                  gp = gpar(fontsize = 16, fontface = "bold"))
)
dot_title_row <- (dot_title1 + dot_title2) + plot_layout(ncol = 2, widths = c(1,1))

final_grid_dot <- dot_title_row / dot_plots_combined +
  plot_layout(heights = c(0.8, 10), guides = "collect")

# ggsave(
#   filename = "Combined_Grid_DotPlots.png",
#   plot = final_grid_dot,
#   width = 20,
#   height = 24,
#   dpi = 300,
#   path = output_dir
# )
# print(final_grid_dot)
