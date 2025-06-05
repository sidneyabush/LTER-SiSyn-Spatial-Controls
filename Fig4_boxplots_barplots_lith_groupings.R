###############################################################################
# COMPLETE SCRIPT: Single Y-Axis Label, Mean SHAP Plot X-Axis Label Only on Bottom Row,
#                 Land Recoding, Separate X-Axis Scaling for Dot Plots,
#                 Combined Legend for Dot Plots, Patchwork Layout
#                 (jitter removed, lighter boxplot fills, subtle group shading;
#                  each row now has a vertical cluster‐name label on the left)
###############################################################################

## 0. Load Pre‐saved Workflow Objects and Rebuild "All Variables" Boxplots
final_models_dir <- "Final_Models"

# Load FNConc objects (full_scaled, shap_values_FNConc, drivers_numeric_consolidated_lith_FNConc)
load(file.path(final_models_dir, "FNConc_HierClust_Workflow_Objects.RData"))
#   → Loads: full_scaled, shap_values_FNConc, drivers_numeric_consolidated_lith_FNConc

# Load FNYield objects (shap_values_FNYield, drivers_numeric_consolidated_lith_FNYield)
load(file.path(final_models_dir, "FNYield_HierClust_Workflow_Objects.RData"))
#   → Loads: shap_values_FNYield, drivers_numeric_consolidated_lith_FNYield

# Define cluster‐color palette
my_cluster_colors <- c(
  "Volcanic"            = "#AC7B32",
  "Sedimentary"         = "#579C8E",
  "Mixed Sedimentary"   = "#89C8A0",
  "Plutonic"            = "#8D9A40",
  "Metamorphic"         = "#C26F86",
  "Carbonate Evaporite" = "#5E88B0"
)

# Determine the unique cluster levels (from FNConc's full_scaled)
unique_clusters <- levels(full_scaled$final_cluster)
# e.g. c("Volcanic","Sedimentary","Mixed Sedimentary","Plutonic","Metamorphic","Carbonate Evaporite")

# ------------------------------------------------------------------------------
# 0.1 Define manual ordering & renaming of all numeric features
#     (for the boxplots and bar plots)
# ------------------------------------------------------------------------------

# 1) “Old” column names in the order you want them to appear:
var_order <- c(
  "NOx",
  "P",
  "npp",
  "evapotrans",
  "greenup_day",
  
  "precip",
  "temp",
  "snow_cover",
  "permafrost",
  
  "elevation",
  "basin_slope",
  
  "RBI",
  "recession_slope",
  
  "land_Bare",
  "land_Cropland",
  "land_Forest",
  "land_Grassland_Shrubland",
  "land_Ice_Snow",
  "land_Impervious",
  "land_Salt_Water",
  "land_Tidal_Wetland",
  "land_Water",
  "land_Wetland_Marsh"
)

# 2) “Pretty” labels (same length/order as var_order):
var_labels <- c(
  "NOx",
  "P",
  "NPP",
  "ET",
  "Greenup Day", # productivity
  
  "Precip",
  "Temp",
  "Snow Cover",
  "Permafrost", # climate
  
  "Elevation",
  "Basin Slope", # topo
  
  "Flashiness Index (RBI)", 
  "Recession Slope", # Q
  
  "Land: Bare",
  "Land: Cropland",
  "Land: Forest",
  "Land: Grass & Shrub",
  "Land: Ice & Snow",
  "Land: Impervious",
  "Land: Salt Water",
  "Land: Tidal Wetland",
  "Land: Water Body",
  "Land: Wetland Marsh"
)

# 3) Named vector to recode “old” → “pretty”:
recode_map_box <- setNames(var_labels, var_order)

# 4) Precompute numeric positions for each group’s shading in the discrete scale:
prod_start <- which(var_labels == "NOx") - 0.5
prod_end   <- which(var_labels == "Greenup Day") + 0.5

clim_start <- which(var_labels == "Precip") - 0.5
clim_end   <- which(var_labels == "Permafrost") + 0.5

topo_start <- which(var_labels == "Elevation") - 0.5
topo_end   <- which(var_labels == "Basin Slope") + 0.5

disc_start <- which(var_labels == "Flashiness Index (RBI)") - 0.5
disc_end   <- which(var_labels == "Recession Slope") + 0.5

land_start_index <- which(var_labels == "Land: Bare") - 0.5
land_end_index   <- which(var_labels == "Land: Wetland Marsh") + 0.5

# Define shading fills and label colors:
prod_fill   <- "white"
prod_text   <- "black"

clim_fill  <- "#f7f7f7"    # very light gray
clim_text  <- "#404040"    # dark gray

topo_fill  <- "#e0e0e0"    # light gray
topo_text  <- "#404040"

disc_fill  <- "#d3d3d3"    # medium-light gray
disc_text  <- "#404040"

lulc_fill   <- "#f0f0f0"   # a slightly darker pale gray
lulc_text   <- "black"

###############################################################################
# 0.2 Create "All Variables" boxplot for each cluster 
#     (jitter removed, lighter boxplot fills, subtle group shading;
#      “LULC” block is now light gray, no border)
###############################################################################
cluster_boxplots <- lapply(unique_clusters, function(cl) {
  df_long <- full_scaled %>%
    dplyr::filter(final_cluster == cl) %>%
    # Keep only numeric columns that are in var_order
    dplyr::select(all_of(var_order)) %>%
    tidyr::pivot_longer(
      cols      = everything(),
      names_to  = "feature",
      values_to = "scaled_value"
    ) %>%
    # Recode “feature” to pretty labels and set factor levels
    dplyr::mutate(
      feature = dplyr::recode(feature, !!!recode_map_box),
      feature = factor(feature, levels = var_labels)
    )
  
  cluster_col <- my_cluster_colors[[cl]]
  # Lighter fill for boxplot so medians stand out clearly:
  box_fill  <- adjustcolor(cluster_col, alpha.f = 0.3)
  box_color <- cluster_col
  
  ggplot(df_long, aes(x = feature, y = scaled_value)) +
    # 1) Label “Productivity” (no background rectangle)
    annotate(
      "text",
      x = (which(var_labels == "NOx") + which(var_labels == "Greenup Day")) / 2,
      y = Inf, label = "Productivity",
      color = prod_text, fontface = "bold",
      vjust = 2, size = 3.5, inherit.aes = FALSE
    ) +
    # 2) Shaded background for Climate block (very light gray)
    annotate(
      "rect",
      xmin = clim_start, xmax = clim_end,
      ymin = -Inf, ymax = Inf,
      fill = clim_fill, color = NA,
      inherit.aes = FALSE
    ) +
    annotate(
      "text",
      x = (which(var_labels == "Precip") + which(var_labels == "Permafrost")) / 2,
      y = Inf, label = "Climate",
      color = clim_text, fontface = "bold",
      vjust = 2, size = 3.5, inherit.aes = FALSE
    ) +
    # 3) Shaded background for Topo block (light gray)
    annotate(
      "rect",
      xmin = topo_start, xmax = topo_end,
      ymin = -Inf, ymax = Inf,
      fill = topo_fill, color = NA,
      inherit.aes = FALSE
    ) +
    annotate(
      "text",
      x = (which(var_labels == "Elevation") + which(var_labels == "Basin Slope")) / 2,
      y = Inf, label = "Topo",
      color = topo_text, fontface = "bold",
      vjust = 2, size = 3.5, inherit.aes = FALSE
    ) +
    # 4) Shaded background for Discharge (Q) block (very light, alpha 0.3)
    annotate(
      "rect",
      xmin = disc_start, xmax = disc_end,
      ymin = -Inf, ymax = Inf,
      fill = disc_fill, alpha = 0.3, color = NA,
      inherit.aes = FALSE
    ) +
    annotate(
      "text",
      x = (which(var_labels == "Flashiness Index (RBI)") + which(var_labels == "Recession Slope")) / 2,
      y = Inf, label = "Q",
      color = disc_text, fontface = "bold",
      vjust = 2, size = 3.5, inherit.aes = FALSE
    ) +
    # 5) Shaded background for LULC block (light gray, alpha 0.3, no border)
    annotate(
      "rect",
      xmin = land_start_index, xmax = land_end_index,
      ymin = -Inf, ymax = Inf,
      fill = lulc_fill, color = NA, alpha = 0.3,
      inherit.aes = FALSE
    ) +
    annotate(
      "text",
      x = (which(var_labels == "Land: Bare") + which(var_labels == "Land: Wetland Marsh")) / 2,
      y = Inf, label = "LULC",
      color = lulc_text, fontface = "bold",
      vjust = 2, size = 3.5, inherit.aes = FALSE
    ) +
    # 6) Actual boxplot with lighter fill so median is clearly visible
    geom_boxplot(
      outlier.shape = NA,
      fill  = box_fill,
      color = box_color,
      width = 0.7
    ) +
    labs(x = NULL, y = "Scaled Value") +
    # 7) Force discrete x‐axis ordering
    scale_x_discrete(limits = var_labels) +
    theme_classic(base_size = 10) +
    theme(
      axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "none"
    )
})

###############################################################################
# 0.3 Define plot_mean_abs_shap() with recoding & ordering for bar plots
###############################################################################
plot_mean_abs_shap <- function(cluster_id, shap_values, full_scaled, y_limit = 1.3) {
  cluster_indices <- which(full_scaled$final_cluster == cluster_id)
  shap_cluster    <- shap_values[cluster_indices, , drop = FALSE]
  mean_abs_shap   <- colMeans(abs(shap_cluster), na.rm = TRUE)
  
  df_shap <- data.frame(
    feature          = names(mean_abs_shap),
    mean_abs_shapval = as.numeric(mean_abs_shap),
    stringsAsFactors = FALSE
  ) %>%
    # Drop any “rocks” features
    dplyr::filter(!grepl("rocks", feature, ignore.case = TRUE)) %>%
    # Recode to pretty labels
    dplyr::mutate(
      feature = dplyr::recode(
        feature,
        "FNConc" = "DSi Concentration",
        "FNYield" = "DSi Yield",
        !!!recode_map_box
      )
    ) %>%
    # Use the same ordering: DSi Concentration, DSi Yield, var_labels
    dplyr::mutate(
      feature = factor(feature, levels = c("DSi Concentration", "DSi Yield", var_labels))
    ) %>%
    dplyr::arrange(feature, desc(mean_abs_shapval))
  
  df_shap <- df_shap[!is.na(df_shap$feature), ]
  
  ggplot(df_shap, aes(x = reorder(feature, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(
      stat = "identity",
      fill  = my_cluster_colors[[as.character(cluster_id)]],
      alpha = 0.8,
      width = 0.7
    ) +
    coord_flip() +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_classic(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    )
}

###############################################################################
# 1. Load Necessary Packages
###############################################################################
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)   # For wrap_plots(), plot_layout(), wrap_elements()
library(grid)        # For textGrob()

# Set working directory & output directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Figures"

###############################################################################
# 2. SHAP Bar Plots (3 columns × N rows) + Column Titles, Single Y-Axis Label,
#    with an extra “Cluster Label” column on the left.
###############################################################################
plot_list_bars_conc <- lapply(unique_clusters, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNConc, full_scaled, y_limit = 0.7)
})
plot_list_bars_yield <- lapply(unique_clusters, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNYield, full_scaled, y_limit = 850)
})

rows_list <- lapply(seq_along(unique_clusters), function(i) {
  cl            <- unique_clusters[i]
  p_label       <- wrap_elements(
    full = textGrob(cl, rot = 90, x = 0.5, hjust = 0.5,
                    gp = gpar(fontsize = 14))
  )
  p_all_vars    <- cluster_boxplots[[i]] + labs(y = NULL)
  p_conc        <- plot_list_bars_conc[[i]]
  p_yield       <- plot_list_bars_yield[[i]]
  
  # Suppress x‐axis on non‐final rows
  if (i < length(unique_clusters)) {
    p_all_vars <- p_all_vars + theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
    p_conc     <- p_conc     + theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
    p_yield    <- p_yield    + theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
  } else {
    # Last row => add y‐axis labels for Mean Absolute SHAP
    p_conc     <- p_conc     + labs(y = "Mean Absolute SHAP Value")
    p_yield    <- p_yield    + labs(y = "Mean Absolute SHAP Value")
  }
  
  # Arrange: [Label] | [All Vars] | [Concentration] | [Yield]
  (p_label | p_all_vars | p_conc | p_yield) +
    plot_layout(ncol = 4, widths = c(0.15, 1, 1, 1))
})

bar_plots_combined <- wrap_plots(rows_list, ncol = 1)

# Create column titles (skip the “Label” column, so these occupy 3 wide columns)
title_cluster <- wrap_elements(
  full = textGrob("", x = 0.5, hjust = 0.5)  # blank placeholder
)
title_all_vars <- wrap_elements(
  full = textGrob("All Variables", x = 0.5, hjust = 0.5,
                  gp = gpar(fontsize = 16, fontface = "bold"))
)
title_conc <- wrap_elements(
  full = textGrob("Concentration", x = 0.5, hjust = 0.5,
                  gp = gpar(fontsize = 16, fontface = "bold"))
)
title_yield <- wrap_elements(
  full = textGrob("Yield", x = 0.5, hjust = 0.5,
                  gp = gpar(fontsize = 16, fontface = "bold"))
)

# Combine titles with matching widths (4 columns total)
title_row <- (title_cluster | title_all_vars | title_conc | title_yield) +
  plot_layout(ncol = 4, widths = c(0.15, 1, 1, 1))

# Single y-axis label on the far left
y_axis_label <- wrap_elements(
  full = textGrob("Scaled Value", rot = 90,
                  gp = gpar(fontsize = 16, fontface = "bold"))
)

# Stack titles on top of the combined rows
bar_plots_with_title <- title_row / bar_plots_combined +
  plot_layout(heights = c(0.6, 10))

# Put the "Scaled Value" label on the far left outside everything
final_grid_bar <- (y_axis_label | bar_plots_with_title) +
  plot_layout(widths = c(0.06, 0.94))

ggsave(
  filename = "Fig4_Combined_Grid_BarPlots.png",
  plot     = final_grid_bar,
  width    = 22,
  height   = 25,
  dpi      = 300,
  path     = output_dir
)
print(final_grid_bar)
