###############################################################################
# COMPLETE WORKFLOW: FNYield Cluster Plotting with Manually Assigned Clusters
# (Single Global Scaling for All Data)
###############################################################################

## 1. Load Packages & Clear Environment
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)   # For wrap_plots(), plot_annotation()
library(fastshap)
library(RColorBrewer)
library(grid)        # For textGrob() if needed
library(colorspace)
library(cluster)     # silhouette()
library(factoextra)  # fviz_silhouette()

# Set working directory and output directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Figures/Yearly_Model/FNYield"

## 2. Load Data & Model
load("FNYield_Yearly_rf_model2_full_new.RData")
load("FNYield_Yearly_kept_drivers_full_new.RData")
load("FNYield_Yearly_full_new.RData")
load("FNYield_Yearly_full_stream_ids_new.RData")

# Load precomputed SHAP values
load("FNYield_Yearly_shap_values_new.RData")

drivers_full <- read.csv("All_Drivers_Harmonized_Yearly_FNConc_FNYield_5_years.csv")

# Join 'major_rock' and 'major_land' onto 'drivers_df'
drivers_combined <- drivers_df %>%
  left_join(
    drivers_full %>% dplyr::select(Stream_ID, Year, major_rock, major_land),
    by = c("Stream_ID", "Year")
  )

###############################################################################
# 3. Consolidate Lithology Categories & Manually Assign Clusters
###############################################################################
# We define a single threshold of 70 on the *original* rocks_sedimentary values.
drivers_numeric_consolidated_lith <- drivers_combined %>%
  filter(trimws(major_rock) != "") %>%
  mutate(
    consolidated_rock = case_when(
      major_rock %in% c(
        "volcanic", 
        "volcanic; sedimentary; carbonate_evaporite",
        "volcanic; carbonate_evaporite", 
        "volcanic; plutonic", 
        "volcanic; plutonic; metamorphic"
      ) ~ "volcanic",
      major_rock %in% c(
        "sedimentary", 
        "sedimentary; carbonate_evaporite", 
        "sedimentary; plutonic; carbonate_evaporite; metamorphic",
        "sedimentary; metamorphic"
      ) ~ "sedimentary",
      major_rock %in% c(
        "metamorphic", 
        "plutonic", 
        "plutonic; metamorphic", 
        "carbonate_evaporite; metamorphic"
      ) ~ "metamorphic/ plutonic",
      major_rock == "carbonate_evaporite" ~ "carbonate/ evaporite"
    )
  ) %>%
  mutate(
    consolidated_rock = factor(
      consolidated_rock,
      levels = c("volcanic", "sedimentary", "metamorphic/ plutonic", "carbonate/ evaporite")
    )
  ) %>%
  mutate(cluster = as.numeric(consolidated_rock)) %>%
  mutate(
    sedimentary_cluster = case_when(
      consolidated_rock == "sedimentary" & rocks_sedimentary >= 70 ~ "Sedimentary",
      consolidated_rock == "sedimentary" & rocks_sedimentary < 70 ~ "Mixed Sedimentary"
    )
  )

# Quick plot to check lithology distribution vs. FNYield
ggplot(drivers_numeric_consolidated_lith, aes(x = consolidated_rock, y = FNYield)) +
  geom_boxplot() +
  labs(x = "Lithology Category", y = "DSi Yield") +
  theme_classic()

###############################################################################
# 4. Prepare Data for Further Analysis (Single Global Scaling)
###############################################################################
# Identify numeric columns to scale (exclude 'cluster' if numeric)
numeric_cols <- setdiff(
  names(dplyr::select(drivers_numeric_consolidated_lith, where(is.numeric))),
  "cluster"
)

# 1. Create final_cluster
# 2. Scale all numeric columns across entire dataset (no group_by)
scaled_data <- drivers_numeric_consolidated_lith %>%
  mutate(final_cluster = if_else(
    consolidated_rock == "sedimentary",
    sedimentary_cluster,
    as.character(cluster)
  )) %>%
  mutate(
    across(
      all_of(numeric_cols),
      ~ scales::rescale(.x, na.rm = TRUE)  # global min->0, max->1
    )
  )

###############################################################################
# 6. Create Long-format Data for Box Plots (Using final_cluster)
###############################################################################
long_data <- scaled_data %>%
  dplyr::select(
    -major_rock, -consolidated_rock, -major_land,
    -Stream_ID, -Year, -sedimentary_cluster
  ) %>%
  pivot_longer(-final_cluster, names_to = "Driver", values_to = "Value") %>%
  mutate(
    Driver = factor(
      Driver,
      levels = c(
        "FNYield",
        "elevation", "basin_slope",
        "NOx", "P", "npp", "greenup_day", "evapotrans",
        "precip", "temp", "snow_cover", "permafrost",
        "rocks_volcanic", "rocks_sedimentary", "rocks_carbonate_evaporite",
        "rocks_metamorphic", "rocks_plutonic",
        "land_tundra", "land_barren_or_sparsely_vegetated", "land_cropland",
        "land_shrubland_grassland", "land_urban_and_built_up_land",
        "land_wetland", "land_forest_all"
      )
    ),
    Driver = recode(
      Driver,
      "FNYield" = "DSi Yield",
      "NOx" = "Nitrate",
      "P" = "Phosphorous",
      "precip" = "Precip",
      "temp" = "Temperature",
      "snow_cover" = "Snow Cover",
      "npp" = "NPP",
      "evapotrans" = "ET",
      "greenup_day" = "Greenup Day",
      "permafrost" = "Permafrost",
      "elevation" = "Elevation",
      "basin_slope" = "Basin Slope",
      "rocks_volcanic" = "Rock: Volcanic",
      "rocks_sedimentary" = "Rock: Sedimentary",
      "rocks_carbonate_evaporite" = "Rock: Carbonate & Evaporite",
      "rocks_metamorphic" = "Rock: Metamorphic",
      "rocks_plutonic" = "Rock: Plutonic",
      "land_tundra" = "Land: Tundra",
      "land_barren_or_sparsely_vegetated" = "Land: Barren & Sparsely Vegetated",
      "land_cropland" = "Land: Cropland",
      "land_shrubland_grassland" = "Land: Shrubland & Grassland",
      "land_urban_and_built_up_land" = "Land: Urban & Built-up",
      "land_wetland" = "Land: Wetland",
      "land_forest_all" = "Land: Forest"
    )
  )

###############################################################################
# 7. Define Cluster Colors
###############################################################################
my_cluster_colors <- c(
  "Sedimentary"       = "#579C8E",  
  "Mixed Sedimentary" = "#89C8A0",
  "1"                 = "#AC7B32",  
  "3"                 = "#C26F86",  
  "4"                 = "#5E88B0"   
)
my_cluster_colors_lighter <- sapply(my_cluster_colors, function(x) lighten(x, amount = 0.3))

###############################################################################
# 8. Generate Box Plots for Each Cluster (Drivers) using final_cluster
###############################################################################
unique_clusters <- sort(unique(long_data$final_cluster))
cluster_boxplots <- lapply(unique_clusters, function(cl) {
  p <- long_data %>%
    filter(final_cluster == cl) %>%
    ggplot(aes(x = Driver, y = Value, fill = final_cluster)) +
    geom_boxplot() +
    scale_fill_manual(values = my_cluster_colors_lighter, guide = "none") +
    scale_y_continuous(limits = c(0, 1)) +  # Force axis 0..1
    labs(
      x = NULL,
      y = NULL,
      title = paste("Cluster", cl)
    ) +
    theme_classic() +
    theme(
      plot.title  = element_text(size = 14, hjust = 0.5),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
      axis.text.y = element_text(size = 14)
    )
  # Remove x-axis labels for all but the last plot
  if (cl != tail(unique_clusters, 1)) {
    p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  return(p)
})

###############################################################################
# 9. Prepare Data for SHAP Dot Plots (Global Scaling)
###############################################################################
# We'll do the same global scaling logic for SHAP analysis.
# 1) Add final_cluster
# 2) Rescale numeric columns globally
# 3) Drop non-numeric columns

full_scaled <- drivers_numeric_consolidated_lith %>%
  mutate(final_cluster = if_else(
    consolidated_rock == "sedimentary",
    sedimentary_cluster,
    as.character(cluster)
  )) %>%
  mutate(
    across(
      all_of(numeric_cols),
      ~ scales::rescale(.x, na.rm = TRUE)
    )
  ) %>%
  dplyr::select(
    -major_rock, -consolidated_rock, -major_land,
    -Stream_ID, -Year, -sedimentary_cluster
  ) %>%
  as.data.frame()

# Compute global min & max from scaled numeric columns (excluding final_cluster)
global_min <- min(full_scaled %>% dplyr::select(-final_cluster), na.rm = TRUE)
global_max <- max(full_scaled %>% dplyr::select(-final_cluster), na.rm = TRUE)

###############################################################################
# 10. Define SHAP Dot Plot Function (Using final_cluster)
###############################################################################
generate_shap_dot_plot_obj <- function(cluster_id, shap_values_FNYield, full_scaled,
                                       global_shap_min, global_shap_max) {
  cluster_indices <- which(full_scaled$final_cluster == cluster_id)
  
  # Subset numeric features (excluding final_cluster)
  cluster_data <- full_scaled[cluster_indices, , drop = FALSE] %>%
    dplyr::select(-final_cluster)
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  # Pivot longer for feature values
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  # Subset SHAP values for this cluster
  shap_values_FNYield_df <- as.data.frame(shap_values_FNYield)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  # Pivot SHAP to long & join
  shap_long <- shap_values_FNYield_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  # Filter out 'rock' features if desired
  shap_long <- shap_long %>% 
    filter(!grepl("rock", feature, ignore.case = TRUE))
  
  # Order features by mean absolute SHAP
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  # Recode feature names
  shap_long$feature <- recode(
    shap_long$feature,
    "FNYield" = "DSi Yield",
    "NOx" = "Nitrate",
    "P" = "Phosphorous",
    "precip" = "Precip",
    "temp" = "Temperature",
    "snow_cover" = "Snow Cover",
    "npp" = "NPP",
    "evapotrans" = "ET",
    "greenup_day" = "Greenup Day",
    "permafrost" = "Permafrost",
    "elevation" = "Elevation",
    "basin_slope" = "Basin Slope",
    "land_tundra" = "Land: Tundra",
    "land_barren_or_sparsely_vegetated" = "Land: Barren & Sparsely Vegetated",
    "land_cropland" = "Land: Cropland",
    "land_shrubland_grassland" = "Land: Shrubland & Grassland",
    "land_urban_and_built_up_land" = "Land: Urban & Built-up",
    "land_wetland" = "Land: Wetland",
    "land_forest_all" = "Land: Forest"
  )
  
  # Build dot plot
  dot_plot <- ggplot(shap_long, aes(x = shap_value, y = feature, fill = feature_value)) +
    geom_point(alpha = 0.6, size = 3, shape = 21, stroke = 0.1, color = "black") +
    scale_fill_gradientn(
      colors = c("white", "gray", "black"),
      name   = NULL,
      limits = c(global_min, global_max)
    ) +
    labs(x = "SHAP Value", y = NULL) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    scale_x_continuous(limits = c(global_shap_min, global_shap_max)) +
    theme_classic() +
    theme(
      axis.title     = element_text(size = 14, face = "bold"),
      axis.text      = element_text(size = 12),
      legend.text    = element_text(size = 12),
      legend.title   = element_text(size = 14),
      legend.key.size = unit(1.5, "lines")
    )
  
  return(dot_plot)
}

###############################################################################
# 11. Generate Dot Plots for Each final_cluster
###############################################################################
global_shap_min <- min(shap_values_FNYield, na.rm = TRUE)
global_shap_max <- max(shap_values_FNYield, na.rm = TRUE)

unique_clusters <- sort(unique(full_scaled$final_cluster))
dot_plots <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(cl, shap_values_FNYield, full_scaled,
                             global_shap_min, global_shap_max)
})

# Remove x-axis labels for all but last
for(i in seq_along(dot_plots)) {
  if(i < length(dot_plots)) {
    dot_plots[[i]] <- dot_plots[[i]] + 
      theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
}

###############################################################################
# 12. Combine Box Plots & SHAP Dot Plots
###############################################################################
left_col <- wrap_plots(cluster_boxplots, ncol = 1) & labs(y = "Scaled Value")
right_col <- wrap_plots(dot_plots, ncol = 1)

final_combined_plot <- left_col | right_col +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 30, face = "bold"),
      plot.tag.position = "topleft"
    )
  )

ggsave(
  filename = "Combined_Cluster_Boxplot_and_SHAP_DotPlots.png",
  plot = final_combined_plot,
  width = 16,
  height = 18,
  dpi = 300,
  path = output_dir
)
print(final_combined_plot)

###############################################################################
# 13. Box Plot of FNYield by Manually Assigned Cluster (Using final_cluster)
###############################################################################
df <- data.frame(
  Stream_ID     = drivers_full$Stream_ID,
  Year          = drivers_full$Year,
  FNYield       = drivers_full$FNYield,
  final_cluster = scaled_data$final_cluster
)
write.csv(
  df,
  file = file.path(output_dir, "FNYield_Stream_ID_Year_Cluster.csv"),
  row.names = FALSE
)

p <- ggplot(df, aes(x = factor(final_cluster), y = FNYield, fill = factor(final_cluster))) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  geom_jitter(aes(color = factor(final_cluster)), width = 0.3, alpha = 0.4, size = 2) +
  scale_fill_manual(values = my_cluster_colors_lighter) +
  scale_color_manual(values = my_cluster_colors) +
  labs(
    x = "Cluster",
    y = expression(DSi~Yield~(kg~m^{-2}~y^{-1}))
  ) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

print(p)
ggsave(
  "FNYield_Yearly_Clusters.png",
  p,
  width = 8,
  height = 5,
  dpi = 300,
  path = output_dir
)

###############################################################################
# 14. Silhouette Plot for Manually Assigned FNYield Clusters (Optional)
###############################################################################
sil_obj <- silhouette(
  as.numeric(scaled_data$cluster),
  dist(
    scaled_data %>%
      dplyr::select(rocks_volcanic, rocks_sedimentary, rocks_carbonate_evaporite,
             rocks_metamorphic, rocks_plutonic)
  )
)
mean_sil_value <- mean(sil_obj[, "sil_width"])

p_sil <- fviz_silhouette(
  sil_obj,
  label   = FALSE,
  palette = unname(my_cluster_colors)
) +
  geom_hline(
    yintercept = mean_sil_value,
    linetype   = "dashed",
    color      = "gray4"
  ) +
  annotate(
    "text",
    x     = nrow(sil_obj) * 0.8,
    y     = mean_sil_value,
    label = paste("Mean =", round(mean_sil_value, 2)),
    color = "gray4",
    vjust = -0.5
  ) +
  labs(x = "Sites", y = "Silhouette Width") +
  theme_classic(base_size = 16) +
  theme(
    legend.title = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

print(p_sil)
ggsave(
  filename = "FNYield_Yearly_Silhouette.png",
  plot = p_sil,
  width = 8,
  height = 5,
  dpi = 300,
  path = output_dir
)

###############################################################################
# 15. Mean Absolute SHAP Bar Plots (Using final_cluster)
###############################################################################
plot_mean_abs_shap <- function(cluster_id, shap_values_FNYield, full_scaled) {
  cluster_indices <- which(full_scaled$final_cluster == cluster_id)
  shap_cluster    <- shap_values_FNYield[cluster_indices, , drop = FALSE]
  
  mean_abs_shap <- colMeans(abs(shap_cluster), na.rm = TRUE)
  df_shap <- data.frame(
    feature          = names(mean_abs_shap),
    mean_abs_shapval = as.numeric(mean_abs_shap)
  ) %>%
    arrange(desc(mean_abs_shapval)) %>%
    filter(!grepl("rock", feature, ignore.case = TRUE))
  
  df_shap$feature <- recode(
    df_shap$feature,
    "NOx" = "Nitrate",
    "P"   = "Phosphorous",
    "precip" = "Precip",
    "temp"   = "Temperature",
    "snow_cover" = "Snow Cover",
    "npp"    = "NPP",
    "evapotrans" = "ET",
    "greenup_day" = "Greenup Day",
    "permafrost"  = "Permafrost",
    "elevation"   = "Elevation",
    "basin_slope" = "Basin Slope",
    "FNConc"      = "DSi Concentration",
    "rocks_volcanic" = "Rock: Volcanic",
    "rocks_sedimentary" = "Rock: Sedimentary",
    "rocks_carbonate_evaporite" = "Rock: Carbonate & Evaporite",
    "rocks_metamorphic" = "Rock: Metamorphic",
    "rocks_plutonic"    = "Rock: Plutonic",
    "land_tundra" = "Land: Tundra",
    "land_barren_or_sparsely_vegetated" = "Land: Barren & Sparsely Vegetated",
    "land_cropland" = "Land: Cropland",
    "land_shrubland_grassland" = "Land: Shrubland & Grassland",
    "land_urban_and_built_up_land" = "Land: Urban & Built-up",
    "land_wetland" = "Land: Wetland",
    "land_forest_all" = "Land: Forest"
  )
  
  ggplot(df_shap, aes(x = reorder(feature, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(
      stat  = "identity",
      fill  = my_cluster_colors[[as.character(cluster_id)]],
      alpha = 0.8
    ) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 6500)) +
    labs(x = NULL, y = "Mean Absolute SHAP Value", title = paste("Cluster", cluster_id)) +
    theme_classic(base_size = 14) +
    theme(
      plot.title  = element_text(size = 14, hjust = 0.5),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    )
}

unique_clusters <- sort(unique(full_scaled$final_cluster))
plot_list <- lapply(unique_clusters, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNYield, full_scaled)
})

ncol <- 2
nrow <- ceiling(length(unique_clusters)/ncol)
final_shap_grid <- wrap_plots(plot_list, ncol = ncol, nrow = nrow)
print(final_shap_grid)

ggsave(
  filename = "MeanAbsSHAP_Grid.png",
  plot = final_shap_grid,
  width = 12,
  height = 9,
  dpi = 300,
  path = output_dir
)
