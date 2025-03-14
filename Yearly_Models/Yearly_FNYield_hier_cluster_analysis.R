###############################################################################
# COMPLETE WORKFLOW: FNYield Cluster Plotting with Manually Assigned Clusters
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

# Create a new factor variable 'consolidated_rock' using the major_rock column
drivers_numeric_consolidated_lith <- drivers_combined %>%
  # Keep only rows that have a valid major_rock entry
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
  # Manually assign clusters based on the factor order:
  # volcanic=1, sedimentary=2, metamorphic/ plutonic=3, carbonate/ evaporite=4
  mutate(cluster = as.numeric(consolidated_rock))

# Quick plot to check lithology distribution vs. FNYield
ggplot(drivers_numeric_consolidated_lith, aes(x = consolidated_rock, y = FNYield)) +
  geom_boxplot() +
  labs(x = "Lithology Category", y = "DSi Yield") +
  theme_classic()

###############################################################################
# 4. Prepare Data for Further Analysis
###############################################################################
# We want to scale numeric variables (between 0 and 1), but keep the cluster column unscaled.

numeric_cols <- setdiff(
  names(dplyr::select(drivers_numeric_consolidated_lith, where(is.numeric))),
  "cluster"
)

scaled_data <- drivers_numeric_consolidated_lith %>%
  # Temporarily store the cluster
  mutate(cluster_unscaled = cluster) %>% 
  # Scale only the numeric columns except 'cluster'
  mutate(across(all_of(numeric_cols), ~ scales::rescale(.))) %>%
  # Restore the original cluster values
  mutate(cluster = cluster_unscaled) %>%
  dplyr::select(-cluster_unscaled)


###############################################################################
# 5. Plot Distributions for Rock Type Percentages by Cluster
###############################################################################

# If you have rock percentage columns (e.g., rocks_volcanic, etc.), 
# you can see how they distribute across your 4 manual clusters:

rock_cluster_df <- drivers_numeric_consolidated_lith %>%
  dplyr::select(cluster, starts_with("rocks_"))

rock_long <- rock_cluster_df %>%
  pivot_longer(
    cols = starts_with("rocks_"),
    names_to = "Rock_Type",
    values_to = "Percentage"
  )

ggplot(rock_long, aes(x = Percentage)) +
  geom_histogram(binwidth = 5, color = "black", fill = "skyblue", alpha = 0.7) +
  facet_grid(Rock_Type ~ cluster, scales = "free_x") +
  labs(
    title = "Distribution of Rock Type Percentages by Cluster",
    x = "Percentage",
    y = "Count"
  ) +
  theme_classic()

###############################################################################
# 6. Create Long-format Data for Box Plots of Drivers
###############################################################################

# Remove non-numeric columns before pivoting to long format
long_data <- scaled_data %>%
  dplyr::select(
    -major_rock,
    -consolidated_rock,
    -major_land,
    -Stream_ID,
    -Year
  ) %>%
  pivot_longer(-cluster, names_to = "Driver", values_to = "Value") %>%
  mutate(
    # Control the order of drivers on the x-axis
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
    # Rename for prettier plotting
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
  "1" = "#AC7B32",  # volcanic
  "2" = "#579C8E",  # sedimentary
  "3" = "#C26F86",  # metamorphic/plutonic
  "4" = "#5E88B0"   # carbonate/evaporite
)

my_cluster_colors_lighter <- sapply(my_cluster_colors, function(x) lighten(x, amount = 0.3))

###############################################################################
# 8. Generate Box Plots for Each Cluster (Drivers)
###############################################################################

unique_clusters <- sort(unique(long_data$cluster))

cluster_boxplots <- lapply(unique_clusters, function(cl) {
  p <- long_data %>%
    filter(cluster == cl) %>%
    ggplot(aes(x = Driver, y = Value, fill = factor(cluster))) +
    geom_boxplot() +
    scale_fill_manual(values = my_cluster_colors_lighter, guide = "none") +
    scale_y_continuous(limits = c(0, 1)) +
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
# 9. Prepare Data for SHAP Dot Plots
###############################################################################
# We'll again scale numeric columns but keep the cluster as is.

numeric_cols <- setdiff(
  names(dplyr::select(drivers_numeric_consolidated_lith, where(is.numeric))),
  "cluster"
)

full_scaled <- drivers_numeric_consolidated_lith %>%
  # Temporarily store cluster
  mutate(cluster_unscaled = cluster) %>%
  # Scale only the numeric columns except "cluster"
  mutate(across(all_of(numeric_cols), ~ scales::rescale(.))) %>%
  # Restore the original cluster values
  mutate(cluster = cluster_unscaled) %>%
  dplyr::select(-cluster_unscaled, -major_rock, -consolidated_rock, -major_land, -Stream_ID, -Year) %>%
  as.data.frame()


# Global min/max for feature_value in the SHAP dot plots
global_min <- min(full_scaled %>% dplyr::select(-cluster), na.rm = TRUE)
global_max <- max(full_scaled %>% dplyr::select(-cluster), na.rm = TRUE)

###############################################################################
# 10. Define SHAP Dot Plot Function
###############################################################################

generate_shap_dot_plot_obj <- function(cluster_id, shap_values_FNYield, full_scaled,
                                       global_shap_min, global_shap_max) {
  # Identify rows for the given cluster
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  
  # Extract numeric features, remove 'cluster' from the data
  cluster_data <- full_scaled[cluster_indices, , drop = FALSE] %>% dplyr::select(-cluster)
  cluster_data$id <- seq_len(nrow(cluster_data))
  
  # Pivot to long for the feature values
  cluster_long <- cluster_data %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "feature_value")
  
  # Grab the SHAP values for the same cluster rows
  shap_values_FNYield_df <- as.data.frame(shap_values_FNYield)[cluster_indices, , drop = FALSE] %>%
    mutate(id = seq_len(nrow(.)))
  
  # Pivot SHAP to long and join with the feature values
  shap_long <- shap_values_FNYield_df %>%
    pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
    left_join(cluster_long, by = c("id", "feature"))
  
  # Filter out any rows with "rock" in the feature name (case-insensitive)
  shap_long <- shap_long %>% 
    filter(!grepl("rock", feature, ignore.case = TRUE))
  
  # Order features by mean absolute SHAP
  overall_feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap))
  shap_long$feature <- factor(shap_long$feature, levels = rev(overall_feature_importance$feature))
  
  # Recode feature names for nicer plotting
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
  
  # Build the dot plot
  dot_plot <- ggplot(shap_long, aes(x = shap_value, y = feature, fill = feature_value)) +
    geom_point(alpha = 0.6, size = 3, shape = 21, stroke = 0.1, color = "black") +
    scale_fill_gradientn(
      colors = c("white", "gray", "black"),
      name = NULL,
      limits = c(global_min, global_max)
    ) +
    labs(x = "SHAP Value", y = NULL, title = NULL) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    scale_x_continuous(limits = c(global_shap_min, global_shap_max)) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.key.size = unit(1.5, "lines")
    )
  return(dot_plot)
}

###############################################################################
# 11. Set Global SHAP Limits & Generate Dot Plots for Each Cluster
###############################################################################

global_shap_min <- min(shap_values_FNYield, na.rm = TRUE)
global_shap_max <- max(shap_values_FNYield, na.rm = TRUE)

unique_clusters <- sort(unique(scaled_data$cluster))
dot_plots <- lapply(unique_clusters, function(cl) {
  generate_shap_dot_plot_obj(cl, shap_values_FNYield, full_scaled,
                             global_shap_min, global_shap_max)
})

# Remove x-axis labels for all but the last dot plot
for(i in seq_along(dot_plots)) {
  if(i < length(dot_plots)) {
    dot_plots[[i]] <- dot_plots[[i]] + 
      theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
}

###############################################################################
# 12. Combine Plots in patchwork
###############################################################################

left_col <- wrap_plots(cluster_boxplots, ncol = 1) & labs(y = "Scaled Value")
right_col <- wrap_plots(dot_plots, ncol = 1)

final_combined_plot <- left_col | right_col
final_combined_plot <- final_combined_plot +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    title = NULL,
    caption = NULL,
    theme = theme(
      plot.tag = element_text(size = 30, face = "bold"),
      plot.tag.position = "topleft"
    )
  )

## Save & print
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
# 13. Box Plot of FNYield by Manually Assigned Cluster
###############################################################################

df <- data.frame(
  Stream_ID = drivers_full$Stream_ID,
  Year      = drivers_full$Year,
  FNYield   = drivers_full$FNYield,
  cluster   = scaled_data$cluster
)

write.csv(
  df,
  file = file.path(output_dir, "FNYield_Stream_ID_Year_Cluster.csv"),
  row.names = FALSE
)

p <- ggplot(df, aes(x = factor(cluster), y = FNYield, fill = factor(cluster))) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  geom_jitter(aes(color = factor(cluster)), width = 0.3, alpha = 0.4, size = 2) +
  scale_fill_manual(values = my_cluster_colors_lighter) +
  scale_color_manual(values = my_cluster_colors) +
  labs(
    title = NULL,
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
# Even though these are "supervised" clusters, you can still examine silhouette.

sil_obj <- silhouette(
  as.numeric(scaled_data$cluster),
  dist(
    scaled_data %>%
      dplyr::select(
        rocks_volcanic,
        rocks_sedimentary,
        rocks_carbonate_evaporite,
        rocks_metamorphic,
        rocks_plutonic
      )
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
  labs(
    title = NULL,
    x     = "Sites",
    y     = "Silhouette Width"
  ) +
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
# 15. Mean Absolute SHAP Bar Plots
###############################################################################

plot_mean_abs_shap <- function(cluster_id, shap_values_FNYield, full_scaled) {
  # Identify which rows belong to the cluster
  cluster_indices <- which(full_scaled$cluster == cluster_id)
  shap_cluster    <- shap_values_FNYield[cluster_indices, , drop = FALSE]
  
  # Compute mean absolute SHAP
  mean_abs_shap <- colMeans(abs(shap_cluster), na.rm = TRUE)
  df_shap <- data.frame(
    feature          = names(mean_abs_shap),
    mean_abs_shapval = as.numeric(mean_abs_shap)
  ) %>%
    arrange(desc(mean_abs_shapval)) %>%
    filter(!grepl("rock", feature, ignore.case = TRUE))
  
  
  # Recode feature names for the plot
  df_shap$feature <- recode(
    df_shap$feature,
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
    "FNConc" = "DSi Concentration",
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
  
  # Plot the bar chart
  ggplot(df_shap, aes(x = reorder(feature, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(
      stat  = "identity",
      fill  = my_cluster_colors[[as.character(cluster_id)]],
      alpha = 0.8
    ) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 6500)) +
    labs(
      x = NULL,
      y = "Mean Absolute SHAP Value",
      title = paste("Cluster", cluster_id)
    ) +
    theme_classic(base_size = 14) +
    theme(
      plot.title  = element_text(size = 14, hjust = 0.5),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    )
}

# Generate a bar plot for each cluster
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
