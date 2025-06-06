###############################################################################
# ADAPTED SCRIPT: Single Y-Axis Label, SHAP Dot Plot for Concentration & Yield,
#                 Land Recoding, Separate X-Axis Scaling for Dot Plots,
#                 One Shared Legend, Patchwork Layout
#                 (jittered points with dark‐gray outline, subtle group shading;
#                  cluster label in its own left‐hand panel, shifted inward;
#                  FEATURES ORDERED BY DESCENDING MEAN |SHAP| FOR Y‐AXIS)
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
load(file.path(final_models_dir, "FNConc_HierClust_Workflow_Objects.RData"))
load(file.path(final_models_dir, "FNYield_HierClust_Workflow_Objects.RData"))

## 3.1 Compute global min/max of all scaled feature values (for color scale)
all_scaled_vals <- full_scaled %>%
  dplyr::select(all_of(var_order)) %>%
  unlist(use.names = FALSE)
global_scaled_min <- min(all_scaled_vals, na.rm = TRUE)
global_scaled_max <- max(all_scaled_vals, na.rm = TRUE)

## 3.2 Compute global min/max of SHAP values for Conc and Yield separately
#    so that dot-plot x-axes are aligned across clusters
global_shap_min_conc  <- min(shap_values_FNConc, na.rm = TRUE)
global_shap_max_conc  <- max(shap_values_FNConc, na.rm = TRUE)
global_shap_min_yield <- min(shap_values_FNYield, na.rm = TRUE)
global_shap_max_yield <- max(shap_values_FNYield, na.rm = TRUE)

## 4. Define Cluster‐Color Palette
my_cluster_colors <- c(
  "Volcanic"            = "#AC7B32",
  "Sedimentary"         = "#579C8E",
  "Mixed Sedimentary"   = "#89C8A0",
  "Plutonic"            = "#8D9A40",
  "Metamorphic"         = "#C26F86",
  "Carbonate Evaporite" = "#5E88B0"
)
unique_clusters <- levels(full_scaled$final_cluster)

## 5. Define Manual Ordering & Renaming of All Numeric Features
var_order <- c(
  "NOx", "P", "npp", "evapotrans", "greenup_day", "precip", "temp",
  "snow_cover", "permafrost", "elevation", "basin_slope", "RBI",
  "recession_slope", "land_Bare", "land_Cropland", "land_Forest",
  "land_Grassland_Shrubland", "land_Ice_Snow", "land_Impervious",
  "land_Salt_Water", "land_Tidal_Wetland", "land_Water",
  "land_Wetland_Marsh"
)
var_labels <- c(
  "NOx", "P", "NPP", "ET", "Greenup Day", "Precip", "Temp",
  "Snow Cover", "Permafrost", "Elevation", "Basin Slope",
  "Flashiness (RBI)", "Recession Slope", "Land: Bare", "Land: Cropland",
  "Land: Forest", "Land: Grass & Shrub", "Land: Ice & Snow",
  "Land: Impervious", "Land: Salt Water", "Land: Tidal Wetland",
  "Land: Water Body", "Land: Wetland Marsh"
)
recode_map_box <- setNames(var_labels, var_order)

## 6. Precompute Numeric Positions for Each Group's Shading
prod_start <- which(var_labels == "NOx") - 0.5
prod_end   <- which(var_labels == "Greenup Day") + 0.5
clim_start <- which(var_labels == "Precip") - 0.5
clim_end   <- which(var_labels == "Permafrost") + 0.5
topo_start <- which(var_labels == "Elevation") - 0.5
topo_end   <- which(var_labels == "Basin Slope") + 0.5
disc_start <- which(var_labels == "Flashiness (RBI)") - 0.5
disc_end   <- which(var_labels == "Recession Slope") + 0.5
land_start_index <- which(var_labels == "Land: Bare") - 0.5
land_end_index   <- which(var_labels == "Land: Wetland Marsh") + 0.5

prod_fill   <- "white"
prod_text   <- "black"
clim_fill   <- "#f7f7f7"
clim_text   <- "#404040"
topo_fill   <- "#e0e0e0"
topo_text   <- "#404040"
disc_fill   <- "#d3d3d3"
disc_text   <- "#404040"
lulc_fill   <- "#f0f0f0"
lulc_text   <- "black"

## 7. Define plot_shap_dot()
#    - Orders features by descending MEAN |SHAP| for this cluster
#    - Uses a fixed x-axis range (passed via arguments)
#    - Points filled by scaled feature_value, outlined dark gray
#    - Vertical dashed line at x = 0
plot_shap_dot <- function(cluster_id, shap_values, full_scaled,
                          global_shap_min, global_shap_max) {
  idx      <- which(full_scaled$final_cluster == cluster_id)
  shap_cl  <- shap_values[idx, , drop = FALSE]
  
  shap_long <- as.data.frame(shap_cl) %>%
    mutate(id = row_number()) %>%
    pivot_longer(
      cols      = -id,
      names_to  = "feature",
      values_to = "shap_value"
    )
  feat_long <- full_scaled[idx, var_order, drop = FALSE] %>%
    mutate(id = row_number()) %>%
    pivot_longer(
      cols      = -id,
      names_to  = "feature",
      values_to = "feature_value"
    )
  
  df_shap_long <- left_join(shap_long, feat_long, by = c("id", "feature")) %>%
    filter(!grepl("rocks", feature, ignore.case = TRUE)) %>%
    mutate(
      feature = recode(
        feature,
        "FNConc"  = "DSi Concentration",
        "FNYield" = "DSi Yield",
        !!!recode_map_box
      )
    ) %>%
    filter(!is.na(feature))
  
  # Order from most important (top) to least (bottom)
  feature_order <- df_shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap)) %>%
    pull(feature)
  
  df_shap_long <- df_shap_long %>%
    mutate(feature = factor(feature, levels = rev(feature_order))) # REVERSED HERE
  
  # Make a big, horizontal legend
  ggplot(df_shap_long, aes(x = shap_value, y = feature)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_jitter(
      aes(fill = feature_value),
      shape = 21,
      color = "darkgray",
      height = 0.2,
      size   = 2.7,
      alpha  = 0.9
    ) +
    scale_fill_gradient(
      low    = "white",
      high   = "black",
      limits = c(global_scaled_min, global_scaled_max),
      name   = "Scaled Value",
      guide = guide_colourbar(
        barheight = unit(1.3, "cm"),
        barwidth  = unit(20, "lines"),
        title.position = "top",
        title.theme    = element_text(size = 30, face = "plain", hjust = 0.5),
        label.theme    = element_text(size = 28)
      )
    ) +
    scale_x_continuous(
      limits = c(global_shap_min, global_shap_max)
    ) +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_classic(base_size = 28) +
    theme(
      axis.text.y       = element_text(size = 20),  # smaller for dot plots
      axis.text.x       = element_text(size = 28),
      axis.title.x      = element_text(size = 30),
      axis.title.y      = element_text(size = 30),
      legend.position   = "bottom",
      legend.title      = element_text(size = 30, face = "plain"),
      legend.text       = element_text(size = 28),
      legend.key.height = unit(1.3, "cm"),
      legend.key.width  = unit(6, "cm"),
      legend.justification = "center"
    )
}

## 8. Create a "Cluster Label" Plot (Text Only)
cluster_label_plot <- function(cluster_name) {
  ggplot() +
    annotate(
      "text",
      x = 0.8, y = 0.5,
      label    = cluster_name,
      angle    = 90,
      fontface = "plain",
      size     = 10,  # unchanged
      color    = "#404040"
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void()
}

## 9. Build the "All Variables" Boxplot for Each Cluster
cluster_boxplots <- lapply(unique_clusters, function(cl) {
  df_long <- full_scaled %>%
    filter(final_cluster == cl) %>%
    select(all_of(var_order)) %>%
    pivot_longer(
      cols      = everything(),
      names_to  = "feature",
      values_to = "scaled_value"
    ) %>%
    mutate(
      feature = recode(feature, !!!recode_map_box),
      feature = factor(feature, levels = var_labels)
    )
  
  cluster_col <- my_cluster_colors[[cl]]
  box_fill  <- adjustcolor(cluster_col, alpha.f = 0.3)
  box_color <- cluster_col
  
  ggplot(df_long, aes(x = feature, y = scaled_value)) +
    annotate(
      "text",
      x = (which(var_labels == "NOx") + which(var_labels == "Greenup Day")) / 2,
      y = Inf, label = "Productivity",
      color    = prod_text,
      fontface = "plain",
      vjust    = 2,
      size     = 6,  # unchanged
      inherit.aes = FALSE
    ) +
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
      size     = 6,  # unchanged
      inherit.aes = FALSE
    ) +
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
      size     = 6,  # unchanged
      inherit.aes = FALSE
    ) +
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
      x = (which(var_labels == "Flashiness (RBI)") + which(var_labels == "Recession Slope")) / 2,
      y = Inf, label = "Q",
      color    = disc_text,
      fontface = "plain",
      vjust    = 2,
      size     = 6,  # unchanged
      inherit.aes = FALSE
    ) +
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
      size     = 6,  # unchanged
      inherit.aes = FALSE
    ) +
    geom_boxplot(
      outlier.shape = NA,
      fill  = box_fill,
      color = box_color,
      width = 0.7
    ) +
    labs(x = NULL, y = "Scaled Value") +
    scale_x_discrete(
      limits = var_labels,
      expand = expansion(add = c(1, 0))
    ) +
    coord_cartesian(clip = "off") +
    theme_classic(base_size = 28) +
    theme(
      plot.margin    = ggplot2::margin(t = 20, r = 5, b = 5, l = 50),
      axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20), # smaller for boxplot x-axis
      axis.text.y     = element_text(size = 28),
      axis.title.y    = element_text(size = 28),
      axis.title.x    = element_text(size = 28),
      legend.position = "none"
    )
})

## 10. Build Concentration & Yield Dot‐Plot Lists
plot_list_dots_conc <- lapply(unique_clusters, function(cl) {
  plot_shap_dot(
    cl,
    shap_values_FNConc,
    full_scaled,
    global_shap_min = global_shap_min_conc,
    global_shap_max = global_shap_max_conc
  )
})
plot_list_dots_yield <- lapply(unique_clusters, function(cl) {
  plot_shap_dot(
    cl,
    shap_values_FNYield,
    full_scaled,
    global_shap_min = global_shap_min_yield,
    global_shap_max = global_shap_max_yield
  )
})

## 11. Combine Each Row and Collect One Shared Legend
rows_list <- lapply(seq_along(unique_clusters), function(i) {
  p_label    <- cluster_label_plot(unique_clusters[i])
  p_all_vars <- cluster_boxplots[[i]] + labs(y = NULL)
  p_conc     <- plot_list_dots_conc[[i]]
  p_yield    <- plot_list_dots_yield[[i]]
  
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
    p_conc  <- p_conc  + labs(x = "SHAP Value") + 
      theme(axis.title.x = element_text(size = 28))
    p_yield <- p_yield + labs(x = "SHAP Value") + 
      theme(axis.title.x = element_text(size = 28))
  }
  
  (p_label | p_all_vars | p_conc | p_yield) +
    plot_layout(ncol = 4, widths = c(0.05, 0.35, 0.30, 0.30))
})

# Stack all rows, collect a single legend on the bottom
dot_plots_combined <- wrap_plots(rows_list, ncol = 1, guides = "collect") & 
  theme(legend.position   = "bottom",
        legend.direction  = "horizontal",
        legend.title      = element_text(size = 30, face = "plain"),
        legend.text       = element_text(size = 28),
        legend.key.height = unit(1.3, "cm"),
        legend.key.width  = unit(6, "cm"),
        legend.justification = "center"
  )

## 12. Build Column Titles + "Scaled Value" Y-axis Label
blank_panel    <- wrap_elements(full = textGrob("", x = 0.5, hjust = 0.5))
title_all_vars <- wrap_elements(
  full = textGrob(
    "All Variables", 
    x = 0.6, y = 0.5,           # Move closer to plot (y close to 1 = closer to top, but <1 or it clips)
    hjust = 0.6, vjust = 1,
    gp = gpar(fontsize = 28, fontface = "plain")
  )
)
title_conc     <- wrap_elements(
  full = textGrob(
    "Concentration", 
    x = 0.6, y = 0.5,
    hjust = 0.2, vjust = 1,
    gp = gpar(fontsize = 28, fontface = "plain")
  )
)
title_yield    <- wrap_elements(
  full = textGrob(
    "Yield", 
    x = 0.5, y = 0.5,
    hjust = -0.8, vjust = 1,
    gp = gpar(fontsize = 30, fontface = "plain")
  )
)

title_row <- (blank_panel + title_all_vars + title_conc + title_yield) +
  plot_layout(ncol = 4, widths = c(0.0000001, 0.35, 0.30, 0.30))

y_axis_label <- wrap_elements(
  full = textGrob("Scaled Value", rot = 90,
                  gp = gpar(fontsize = 28, fontface = "plain"))
)

# Reduce height of title row to bring it closer to the plots
dot_plots_with_title <- title_row / dot_plots_combined +
  plot_layout(heights = c(0.2, 10))  # was c(1.0, 10)

final_grid_dot <- (y_axis_label | dot_plots_with_title) +
  plot_layout(widths = c(0.03, 0.97))


## 13. Save & Print Final Figure
ggsave(
  filename = "FigSX_Combined_Grid_DotPlots.png",
  plot     = final_grid_dot,
  width    = 28,
  height   = 32,
  dpi      = 300,
  path     = output_dir
)
