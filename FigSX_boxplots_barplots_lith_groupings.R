###############################################################################
# COMPLETE SCRIPT: Single Y-Axis Label, Mean SHAP Plot X-Axis Label Only on Bottom Row,
#                 Land Recoding, Separate X-Axis Scaling for Dot Plots,
#                 Combined Legend for Dot Plots, Patchwork Layout
#                 (jitter removed, lighter boxplot fills, subtle group shading;
#                  cluster label in its own left‐hand panel, shifted inward;
#                  center‐aligned column titles, larger fonts for publication)
###############################################################################

## 1. Load Necessary Packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)   # For wrap_plots(), plot_layout(), wrap_elements()
library(grid)        # For textGrob()

## 2. Set Working & Output Directories
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "Final_Figures"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## 3. Load Pre‐saved Workflow Objects
final_models_dir <- "Final_Models"
# Load FNConc objects (full_scaled, shap_values_FNConc, drivers_numeric_consolidated_lith_FNConc)
load(file.path(final_models_dir, "FNConc_HierClust_Workflow_Objects.RData"))
#   → Loads: full_scaled, shap_values_FNConc, drivers_numeric_consolidated_lith_FNConc
# Load FNYield objects (shap_values_FNYield, drivers_numeric_consolidated_lith_FNYield)
load(file.path(final_models_dir, "FNYield_HierClust_Workflow_Objects.RData"))
#   → Loads: shap_values_FNYield, drivers_numeric_consolidated_lith_FNYield

## 4. Define Cluster‐Color Palette
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
# e.g.: c("Volcanic","Sedimentary","Mixed Sedimentary","Plutonic","Metamorphic","Carbonate Evaporite")

## 5. Define Manual Ordering & Renaming of All Numeric Features
# 5.1 "Old" column names in the order to appear:
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
# 5.2 "Pretty" labels (same length/order as var_order):
var_labels <- c(
  "NOx",
  "P",
  "NPP",
  "ET",
  "Greenup Day",   # Productivity
  "Precip",
  "Temp",
  "Snow Cover",
  "Permafrost",    # Climate
  "Elevation",
  "Basin Slope",   # Topo
  "Flashiness (RBI)",
  "Recession Curve Slope", # Q
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
# 5.3 Named vector for recoding all "old" → "pretty"
recode_map_box <- setNames(var_labels, var_order)

## 6. Precompute Numeric Positions for Each Group's Shading
prod_start <- which(var_labels == "NOx") - 0.5
prod_end   <- which(var_labels == "Greenup Day") + 0.5
clim_start <- which(var_labels == "Precip") - 0.5
clim_end   <- which(var_labels == "Permafrost") + 0.5
topo_start <- which(var_labels == "Elevation") - 0.5
topo_end   <- which(var_labels == "Basin Slope") + 0.5
disc_start <- which(var_labels == "Flashiness (RBI)") - 0.5
disc_end   <- which(var_labels == "Recession Curve Slope") + 0.5
land_start_index <- which(var_labels == "Land: Bare") - 0.5
land_end_index   <- which(var_labels == "Land: Wetland Marsh") + 0.5

# 6.1 Define shading fills and label colors (unchanged)
prod_fill   <- "white"
prod_text   <- "black"
clim_fill   <- "#f7f7f7"   # very light gray
clim_text   <- "#404040"   # darker gray
topo_fill   <- "#e0e0e0"   # light gray
topo_text   <- "#404040"
disc_fill   <- "#d3d3d3"   # medium-light gray
disc_text   <- "#404040"
lulc_fill   <- "#f0f0f0"   # slightly darker pale gray
lulc_text   <- "black"

## 7. Define plot_mean_abs_shap() with Recoding & Ordering for Bar Plots
plot_mean_abs_shap <- function(cluster_id, shap_values, full_scaled, y_limit = 1.3) {
  cluster_indices <- which(full_scaled$final_cluster == cluster_id)
  shap_cluster    <- shap_values[cluster_indices, , drop = FALSE]
  mean_abs_shap   <- colMeans(abs(shap_cluster), na.rm = TRUE)
  
  df_shap <- data.frame(
    feature          = names(mean_abs_shap),
    mean_abs_shapval = as.numeric(mean_abs_shap),
    stringsAsFactors = FALSE
  ) %>%
    # Drop any "rocks" features
    dplyr::filter(!grepl("rocks", feature, ignore.case = TRUE)) %>%
    # Recode to "pretty" labels
    dplyr::mutate(
      feature = dplyr::recode(
        feature,
        "FNConc"  = "DSi Concentration",
        "FNYield" = "DSi Yield",
        !!!recode_map_box
      )
    ) %>%
    # Use ordering: DSi Concentration, DSi Yield, then var_labels
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
    theme_classic(base_size = 28) +  # ↑ base_size = 28 for larger fonts
    theme(
      axis.text.y = element_text(size = 28),   # ↑ 28 for y‐text
      axis.text.x = element_text(size = 28),   # ↑ 28 for x‐text
      axis.title.x = element_text(size = 30),  # ↑ 30 for axis titles (if ever used)
      axis.title.y = element_text(size = 30)
    )
}

## 8. Create a "Cluster Label" Plot (Text Only)
cluster_label_plot <- function(cluster_name) {
  ggplot() +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label    = cluster_name,
      angle    = 90,
      fontface = "plain",
      size     = 10,       # Keep cluster‐label text at 10
      color    = "#404040"
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void()
}

## 9. Build the "All Variables" Boxplot for Each Cluster
cluster_boxplots <- lapply(unique_clusters, function(cl) {
  df_long <- full_scaled %>%
    dplyr::filter(final_cluster == cl) %>%
    # Keep only numeric columns in var_order
    dplyr::select(all_of(var_order)) %>%
    tidyr::pivot_longer(
      cols      = everything(),
      names_to  = "feature",
      values_to = "scaled_value"
    ) %>%
    # Recode to "pretty" labels and set factor levels
    dplyr::mutate(
      feature = dplyr::recode(feature, !!!recode_map_box),
      feature = factor(feature, levels = var_labels)
    )
  
  cluster_col <- my_cluster_colors[[cl]]
  # Lighter fill so medians remain visible
  box_fill  <- adjustcolor(cluster_col, alpha.f = 0.3)
  box_color <- cluster_col
  
  ggplot(df_long, aes(x = feature, y = scaled_value)) +
    ## 1) "Productivity" (just like dot version, size = 6)
    annotate(
      "text",
      x = (which(var_labels == "NOx") + which(var_labels == "Greenup Day")) / 2,
      y = Inf, label = "Productivity",
      color    = prod_text,
      fontface = "plain", 
      vjust    = 2,
      size     = 6,       # unchanged from dot version
      inherit.aes = FALSE
    ) +
    ## 2) "Climate" (shaded rect + text size = 6)
    annotate(
      "rect",
      xmin = clim_start, xmax = clim_end,
      ymin = -Inf, ymax = Inf,
      fill  = clim_fill,
      color = NA,
      inherit.aes = FALSE
    ) +
    annotate(
      "text",
      x = (which(var_labels == "Precip") + which(var_labels == "Permafrost")) / 2,
      y = Inf, label = "Climate",
      color    = clim_text,
      fontface = "plain",
      vjust    = 2,
      size     = 6,       # unchanged from dot version
      inherit.aes = FALSE
    ) +
    ## 3) "Topo" (shaded rect + text size = 6)
    annotate(
      "rect",
      xmin = topo_start, xmax = topo_end,
      ymin = -Inf, ymax = Inf,
      fill  = topo_fill,
      color = NA,
      inherit.aes = FALSE
    ) +
    annotate(
      "text",
      x = (which(var_labels == "Elevation") + which(var_labels == "Basin Slope")) / 2,
      y = Inf, label = "Topo",
      color    = topo_text,
      fontface = "plain",
      vjust    = 2,
      size     = 6,       # unchanged from dot version
      inherit.aes = FALSE
    ) +
    ## 4) "Q" (shaded rect + text size = 6)
    annotate(
      "rect",
      xmin = disc_start, xmax = disc_end,
      ymin = -Inf, ymax = Inf,
      fill  = disc_fill,
      alpha = 0.3,
      color = NA,
      inherit.aes = FALSE
    ) +
    annotate(
      "text",
      x = (which(var_labels == "Flashiness (RBI)") + which(var_labels == "Recession Curve Slope")) / 2,
      y = Inf, label = "Q",
      color    = disc_text,
      fontface = "plain",
      vjust    = 2,
      size     = 6,       # unchanged from dot version
      inherit.aes = FALSE
    ) +
    ## 5) "LULC" (shaded rect + text size = 6)
    annotate(
      "rect",
      xmin = land_start_index, xmax = land_end_index,
      ymin = -Inf, ymax = Inf,
      fill  = lulc_fill,
      alpha = 0.3,
      color = NA,
      inherit.aes = FALSE
    ) +
    annotate(
      "text",
      x = (which(var_labels == "Land: Bare") + which(var_labels == "Land: Wetland Marsh")) / 2,
      y = Inf, label = "LULC",
      color    = lulc_text,
      fontface = "plain",
      vjust    = 2,
      size     = 6,       # unchanged from dot version
      inherit.aes = FALSE
    ) +
    ## 6) The actual boxplot (lighter fill)
    geom_boxplot(
      outlier.shape = NA,
      fill  = box_fill,
      color = box_color,
      width = 0.7
    ) +
    labs(x = NULL, y = "Scaled Value") +
    # 7) Force discrete x-axis ordering, shift categories right by exactly 1 unit
    scale_x_discrete(
      limits = var_labels,
      expand = expansion(add = c(1, 0))
    ) +
    coord_cartesian(clip = "off") +   # allow drawing outside panel area
    theme_classic(base_size = 28) +   # ↑ base_size = 28 for larger fonts
    theme(
      plot.margin    = ggplot2::margin(t = 20, r = 5, b = 5, l = 50), 
      axis.text.x     = element_text(                                   # smaller for boxplot x‐axis
        angle = 90, 
        vjust = 0.5, 
        hjust = 1, 
        size  = 20  # ↑ 20 (smaller than 28)
      ),
      axis.text.y     = element_text(size = 28),                        # ↑ 28
      axis.title.y    = element_text(size = 28),                        # ↑ 28
      axis.title.x    = element_text(size = 28),                        # ↑ 28
      legend.position = "none"
    )
})

## 10. Build Concentration & Yield Bar‐Plot Lists
plot_list_bars_conc <- lapply(unique_clusters, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNConc, full_scaled, y_limit = 0.7)
})
plot_list_bars_yield <- lapply(unique_clusters, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNYield, full_scaled, y_limit = 850)
})

## 11. Combine Each Row as FOUR Panels: 
##     [ (1) cluster_label_plot | (2) boxplot | (3) concentration barplot | (4) yield barplot ]
rows_list <- lapply(seq_along(unique_clusters), function(i) {
  # (1) Text‐only cluster label:
  p_label <- cluster_label_plot(unique_clusters[i])
  
  # (2) "All Variables" boxplot:
  p_all_vars <- cluster_boxplots[[i]] + labs(y = NULL)
  
  # (3) Concentration barplot:
  p_conc  <- plot_list_bars_conc[[i]]
  
  # (4) Yield barplot:
  p_yield <- plot_list_bars_yield[[i]]
  
  # If not the last row, suppress x-axis on data panels:
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
    # Bottom row: add "Mean Absolute SHAP Value" label with larger font
    p_conc  <- p_conc  + labs(y = "Mean Absolute SHAP Value") + 
      theme(axis.title.y = element_text(size = 28))
    p_yield <- p_yield + labs(y = "Mean Absolute SHAP Value") + 
      theme(axis.title.y = element_text(size = 28))
  }
  
  # Place four panels side by side, with a narrower first column (0.05)
  (p_label | p_all_vars | p_conc | p_yield) +
    plot_layout(ncol = 4, widths = c(0.05, 0.35, 0.30, 0.30))
})

# Stack all rows
bar_plots_combined <- wrap_plots(rows_list, ncol = 1)

## 12. Build Column Titles + "Scaled Value" Y‐axis Label
# Title for the blank first column
blank_panel <- wrap_elements(
  full = textGrob("", x = 0.5, hjust = 0.5)
)

# Titles for columns 2–4, centered inside their row (y = 0.5)
title_all_vars <- wrap_elements(
  full = textGrob(
    "All Variables", 
    x = 0.6, y = 0.5, 
    hjust = 0.5, vjust = 0.5,
    gp = gpar(fontsize = 28, fontface = "plain")
  )
)
title_conc <- wrap_elements(
  full = textGrob(
    "Concentration", 
    x = 0.6, y = 0.5, 
    hjust = 0.5, vjust = 0.5,
    gp = gpar(fontsize = 28, fontface = "plain")
  )
)
title_yield <- wrap_elements(
  full = textGrob(
    "Yield", 
    x = 0.7, y = 0.5, 
    hjust = 0.5, vjust = 0.5,
    gp = gpar(fontsize = 28, fontface = "plain")
  )
)

# Combine into a single title row, with very narrow first column:
title_row <- (blank_panel + title_all_vars + title_conc + title_yield) +
  plot_layout(ncol = 4, widths = c(0.0000001, 0.35, 0.30, 0.30))

# "Scaled Value" label on the far left, moved in close to the plots
y_axis_label <- wrap_elements(
  full = textGrob(
    "Scaled Value", 
    x = 0.5, 
    rot = 90,
    gp = gpar(fontsize = 28, fontface = "plain")
  )
)

# Stack the title row above the combined bar plots, and move them together.
#   – We give the title row a small height (0.2), so it sits close to its plots.
#   – We move the entire left column (“Scaled Value”) in close by using widths = c(0.03, 0.97).
dot_plots_with_title <- title_row / bar_plots_combined +
  plot_layout(heights = c(0.2, 10))

final_grid_bar <- (y_axis_label | dot_plots_with_title) +
  plot_layout(widths = c(0.03, 0.97))

## 13. Save & Print Final Figure
ggsave(
  filename = "FigSX_Combined_Grid_BarPlots.png",
  plot     = final_grid_bar,
  width    = 28,   # slightly wider to accommodate larger fonts
  height   = 30,
  dpi      = 300,
  path     = output_dir
)
print(final_grid_bar)
