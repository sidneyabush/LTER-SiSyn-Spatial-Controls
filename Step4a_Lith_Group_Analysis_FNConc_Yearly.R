###############################################################################
# COMPLETE WORKFLOW: FNConc Cluster Plotting with Faceted Box & Dot Plots,
# Silhouette, & SHAP Bar‐Plot Grid
###############################################################################

## 1. Load Packages & Clear Environment
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)    # For wrap_plots()
library(fastshap)
library(RColorBrewer)
library(grid)         # For textGrob(), if needed
library(colorspace)
library(cluster)      # silhouette()
library(factoextra)   # fviz_silhouette()
library(forcats)      # fct_recode()

## 2. (Again) Clear environment just to be sure
rm(list = ls())

## 3. Set working and output directories
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir       <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Figures"
final_models_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"

###############################################################################
# 4. Load Data & Model (from Final_Models)
###############################################################################
load(file.path(final_models_dir, "FNConc_Yearly_rf_model2.RData"))
rf_model2_FNConc <- rf_model2

load(file.path(final_models_dir, "FNConc_Yearly_kept_drivers.RData"))
kept_drivers_FNConc <- kept_drivers

# (Optional) numeric data if needed:
# load(file.path(final_models_dir, "FNConc_Yearly_numeric.RData"))
# drivers_numeric_FNConc <- drivers_numeric

load(file.path(final_models_dir, "FNConc_Yearly_stream_ids.RData"))
drivers_df <- drivers_df

# Load precomputed SHAP values
load(file.path(final_models_dir, "FNConc_Yearly_shap_values_new.RData"))
shap_values_FNConc <- shap_values_FNConc

drivers_full <- read.csv("harmonization_files/All_Drivers_Harmonized_Yearly_FNConc_FNYield_5_years.csv")

drivers_combined <- drivers_df %>%
  inner_join(
    drivers_full %>% 
      dplyr::select(Stream_ID, Year, major_rock, major_land),
    by = c("Stream_ID", "Year")
  ) %>%
  filter(!is.na(major_rock))

###############################################################################
# 5. Consolidate Lithology Categories & Manually Assign Clusters
###############################################################################
drivers_numeric_consolidated_lith <- drivers_combined %>%
  # Remove rows with missing, blank, or "0" in major_rock
  filter(!is.na(major_rock) & trimws(major_rock) != "" & major_rock != "0") %>%
  mutate(
    # Group string combos into categories
    consolidated_rock = case_when(
      major_rock %in% c("volcanic", "volcanic; plutonic") ~ "Volcanic",
      major_rock %in% c(
        "sedimentary",
        "volcanic; sedimentary; carbonate_evaporite",
        "sedimentary; carbonate_evaporite",
        "sedimentary; plutonic; carbonate_evaporite; metamorphic",
        "sedimentary; metamorphic"
      ) ~ "Sedimentary",
      major_rock %in% c("plutonic", "plutonic; metamorphic", "volcanic; plutonic; metamorphic") ~ "Plutonic",
      major_rock %in% c("metamorphic", "carbonate_evaporite; metamorphic") ~ "Metamorphic",
      major_rock %in% c("carbonate_evaporite", "volcanic; carbonate_evaporite") ~ "Carbonate Evaporite"
    )
  ) %>%
  mutate(
    # If Sedimentary and ≥70% sed rocks → "Sedimentary"; else "Mixed Sedimentary"
    final_cluster = case_when(
      consolidated_rock == "Sedimentary" & rocks_sedimentary >= 70 ~ "Sedimentary",
      consolidated_rock == "Sedimentary" & rocks_sedimentary < 70  ~ "Mixed Sedimentary",
      TRUE ~ consolidated_rock
    )
  ) %>%
  # Manually order clusters
  mutate(
    final_cluster = factor(
      final_cluster, 
      levels = c(
        "Volcanic", "Sedimentary", "Mixed Sedimentary",
        "Plutonic", "Metamorphic", "Carbonate Evaporite"
      )
    )
  )

drivers_numeric_consolidated_lith <- as_tibble(drivers_numeric_consolidated_lith)

# Summary table (fixed count syntax)
row_counts <- drivers_numeric_consolidated_lith %>%
  count(final_cluster) %>%
  rename(total_rows = n)

stream_counts <- drivers_numeric_consolidated_lith %>%
  group_by(final_cluster) %>%
  summarise(unique_stream_ids = n_distinct(Stream_ID), .groups = "drop")

summary_table <- left_join(row_counts, stream_counts, by = "final_cluster")
print(summary_table)

###############################################################################
# 6. Prepare Data for Further Analysis (Global Scaling)
###############################################################################
numeric_cols <- setdiff(
  names(select(drivers_numeric_consolidated_lith, where(is.numeric))),
  "cluster"
)

scaled_data <- drivers_numeric_consolidated_lith %>%
  mutate(
    across(
      all_of(numeric_cols),
      ~ scales::rescale(.x, na.rm = TRUE)
    )
  )

###############################################################################
# 7. Define Cluster Colors (Using New Naming & Order)
###############################################################################
my_cluster_colors <- c(
  "Volcanic"            = "#AC7B32",
  "Sedimentary"         = "#579C8E",
  "Mixed Sedimentary"   = "#89C8A0",
  "Plutonic"            = "#8D9A40",
  "Metamorphic"         = "#C26F86",
  "Carbonate Evaporite" = "#5E88B0"
)

my_cluster_colors_lighter <- sapply(my_cluster_colors, function(x) lighten(x, amount = 0.3))

###############################################################################
# 8. Create Long-format Data for Box Plots 
#    (exclude FNConc & anything starting with "rocks")
#    BUT recode "evapotrans" → "ET" *before* factoring
###############################################################################
long_data <- scaled_data %>%
  select(-major_rock, -consolidated_rock, -major_land, -Stream_ID, -Year) %>%
  pivot_longer(
    cols      = -final_cluster,
    names_to  = "Driver",
    values_to = "Value"
  ) %>%
  # Drop FNConc and all "rocks_..." columns
  filter(
    Driver != "FNConc",
    !grepl("^rocks", Driver, ignore.case = TRUE)
  ) %>%
  # FIRST: recode the raw "evapotrans" text to "ET"
  mutate(
    Driver = ifelse(Driver == "evapotrans", "ET", Driver)
  ) %>%
  # THEN turn Driver into a factor that actually includes "ET"
  mutate(
    Driver = factor(
      Driver,
      levels = c(
        "NOx", "P", "precip", "temp", "snow_cover", "npp", "ET",        # <-- "ET" here
        "greenup_day", "permafrost", "elevation", "RBI", "basin_slope",
        "recession_slope",
        "land_Bare", "land_Cropland", "land_Forest",
        "land_Grassland_Shrubland", "land_Ice_Snow", "land_Impervious",
        "land_Salt_Water", "land_Tidal_Wetland", "land_Water", "land_Wetland_Marsh"
      )
    )
  ) %>%
  # Finally, recode the other names to human‐readable text
  mutate(
    Driver = fct_recode(
      Driver,
      "Nitrate"                     = "NOx",
      "P"                           = "P",
      "Precip"                      = "precip",
      "Temperature"                 = "temp",
      "Snow Cover"                  = "snow_cover",
      "NPP"                         = "npp",
      # "ET" is already ET, so no need to recode that one
      "Greenup Day"                 = "greenup_day",
      "Permafrost"                  = "permafrost",
      "Elevation"                   = "elevation",
      "Flashiness Index"            = "RBI",
      "Basin Slope"                 = "basin_slope",
      "Recession Curve Slope"       = "recession_slope",
      "Land: Bare"                  = "land_Bare", 
      "Land: Cropland"              = "land_Cropland", 
      "Land: Forest"                = "land_Forest",
      "Land: Grassland & Shrubland" = "land_Grassland_Shrubland", 
      "Land: Ice & Snow"            = "land_Ice_Snow", 
      "Land: Impervious"            = "land_Impervious", 
      "Land: Salt Water"            = "land_Salt_Water",
      "Land: Tidal Wetland"         = "land_Tidal_Wetland", 
      "Land: Water Body"            = "land_Water", 
      "Land: Wetland Marsh"         = "land_Wetland_Marsh"
    )
  )

unique_clusters <- levels(long_data$final_cluster)

###############################################################################
# 9. Generate Individual Box Plots (for reference if needed)
###############################################################################
cluster_boxplots <- lapply(unique_clusters, function(cl) {
  p <- long_data %>%
    filter(final_cluster == cl) %>%
    ggplot(aes(x = Driver, y = Value, fill = final_cluster)) +
    geom_boxplot() +
    scale_fill_manual(values = my_cluster_colors_lighter, guide = "none") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_classic() +
    theme(
      plot.title = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
      axis.text.y = element_text(size = 14)
    )
  if (cl != tail(unique_clusters, 1)) {
    p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  return(p)
})

###############################################################################
# 10. Box Plot of FNConc by Manually Assigned Cluster (Unscaled FNConc)
###############################################################################
df_unscaled <- drivers_numeric_consolidated_lith %>%
  dplyr::select(Stream_ID, Year, FNConc, final_cluster)

write.csv(
  df_unscaled,
  file = file.path(output_dir, "FNConc_Stream_ID_Year_Cluster.csv"),
  row.names = FALSE
)

p_FNConc <- ggplot(df_unscaled, aes(x = final_cluster, y = FNConc, fill = final_cluster)) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  geom_jitter(aes(color = final_cluster), width = 0.3, alpha = 0.4, size = 2) +
  scale_fill_manual(values = my_cluster_colors_lighter) +
  scale_color_manual(values = my_cluster_colors) +
  labs(x = NULL, y = expression(DSi~Concentration~(mg~L^{-1}))) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

###############################################################################
# 11. Silhouette Plot with Factoextra (Remove x-axis elements)
###############################################################################
sil_obj <- silhouette(
  as.numeric(scaled_data$final_cluster),
  dist(scaled_data %>% dplyr::select(
    rocks_volcanic, rocks_sedimentary,
    rocks_carbonate_evaporite, rocks_metamorphic,
    rocks_plutonic
  ))
)
mean_sil_value <- mean(sil_obj[, "sil_width"], na.rm = TRUE)

p_sil <- fviz_silhouette(
  sil_obj,
  label   = FALSE,
  palette = c(
    "#AC7B32",
    "#579C8E",
    "#89C8A0",
    "#8D9A40",
    "#C26F86",
    "#5E88B0"
  )
) +
  guides(color = "none") +
  scale_fill_manual(
    name   = "Cluster",
    values = c(
      "1" = "#AC7B32", "2" = "#579C8E", "3" = "#89C8A0",
      "4" = "#8D9A40", "5" = "#C26F86", "6" = "#5E88B0"
    ),
    labels = c(
      "1" = "Volcanic", "2" = "Sedimentary", "3" = "Mixed Sedimentary",
      "4" = "Plutonic", "5" = "Metamorphic", "6" = "Carbonate Evaporite"
    )
  ) +
  geom_hline(yintercept = mean_sil_value, linetype = "dashed", color = "gray4") +
  annotate(
    "text",
    x = nrow(sil_obj) * 0.8,
    y = mean_sil_value,
    label = paste("Mean =", round(mean_sil_value, 2)),
    color = "gray4",
    vjust = -0.5
  ) +
  labs(
    x = NULL,
    y = "Silhouette Width",
    title = NULL,
    subtitle = NULL
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    axis.title.x  = element_blank(),
    legend.title  = element_blank(),
    plot.title    = element_blank(),
    plot.subtitle = element_blank()
  )

print(p_sil)

###############################################################################
# 12. Define function plot_mean_abs_shap for SHAP bar plots
###############################################################################
plot_mean_abs_shap <- function(cluster_id, shap_values_FNConc, full_scaled) {
  cluster_indices <- which(full_scaled$final_cluster == cluster_id)
  shap_cluster    <- shap_values_FNConc[cluster_indices, , drop = FALSE]
  
  mean_abs_shap <- colMeans(abs(shap_cluster), na.rm = TRUE)
  
  df_shap <- data.frame(
    feature          = names(mean_abs_shap),
    mean_abs_shapval = as.numeric(mean_abs_shap),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(mean_abs_shapval)) %>%
    filter(
      feature != "FNConc",
      !grepl("^rocks", feature, ignore.case = TRUE)
    )
  
  feature_recode_map <- c(
    "NOx" = "Nitrate", "P" = "P", "precip" = "Precip", "temp" = "Temperature",
    "snow_cover" = "Snow Cover", "npp" = "NPP", "evapotrans" = "ET",
    "greenup_day" = "Greenup Day", "permafrost" = "Permafrost",
    "elevation" = "Elevation", "RBI" = "Flashiness Index",
    "basin_slope" = "Basin Slope", "recession_slope" = "Recession Curve Slope",
    "land_Bare" = "Land: Bare", "land_Cropland" = "Land: Cropland",
    "land_Forest" = "Land: Forest", "land_Grassland_Shrubland" = "Land: Grassland & Shrubland",
    "land_Ice_Snow" = "Land: Ice & Snow", "land_Impervious" = "Land: Impervious",
    "land_Salt_Water" = "Land: Salt Water", "land_Tidal_Wetland" = "Land: Tidal Wetland",
    "land_Water" = "Land: Water Body", "land_Wetland_Marsh" = "Land: Wetland Marsh"
  )
  
  df_shap <- df_shap %>%
    mutate(
      feature_recoded = ifelse(
        feature %in% names(feature_recode_map),
        feature_recode_map[feature],
        feature
      ),
      feature_recoded = factor(feature_recoded, levels = feature_recoded)
    )
  
  ggplot(df_shap, aes(x = reorder(feature_recoded, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(stat = "identity", fill = my_cluster_colors[[as.character(cluster_id)]], alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1.3)) +
    labs(x = NULL, y = "Mean Absolute SHAP Value", title = NULL) +
    theme_classic(base_size = 14) +
    theme(
      plot.title  = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    )
}

###############################################################################
# 13. SHAP Dot Plots: Compute faceted data for all clusters
###############################################################################
full_scaled <- scaled_data

global_min <- min(full_scaled %>% dplyr::select(where(is.numeric)), na.rm = TRUE)
global_max <- max(full_scaled %>% dplyr::select(where(is.numeric)), na.rm = TRUE)

# 13a. Identify features present in both shap and scaled_data
shap_feats   <- colnames(shap_values_FNConc)
scaled_feats <- colnames(full_scaled)[sapply(full_scaled, is.numeric)]
common_feats <- intersect(shap_feats, scaled_feats)

# 13b. Build a combined data frame of numeric feature values
cluster_data_all <- full_scaled[, common_feats, drop = FALSE] %>%
  as.data.frame() %>%
  mutate(id = seq_len(nrow(.)))

cluster_long_all <- cluster_data_all %>%
  pivot_longer(
    cols      = -id,
    names_to  = "feature",
    values_to = "feature_value"
  )

# 13c. Build a combined data frame of SHAP values
shap_df_all <- as.data.frame(shap_values_FNConc)[, common_feats, drop = FALSE] %>%
  mutate(id = seq_len(nrow(.)))

shap_long_all <- shap_df_all %>%
  pivot_longer(
    cols      = -id,
    names_to  = "feature",
    values_to = "shap_value"
  ) %>%
  left_join(cluster_long_all, by = c("id", "feature"))

# 13d. Drop "FNConc" but keep all rock columns
shap_long_all <- shap_long_all %>%
  filter(feature != "FNConc")

# 13e. Recode feature names for plotting
feature_recode_map <- c(
  "NOx"                      = "Nitrate",
  "P"                        = "P",
  "precip"                   = "Precip",
  "temp"                     = "Temperature",
  "snow_cover"               = "Snow Cover",
  "npp"                      = "NPP",
  "evapotrans"               = "ET",
  "greenup_day"              = "Greenup Day",
  "permafrost"               = "Permafrost",
  "elevation"                = "Elevation",
  "RBI"                      = "Flashiness Index",
  "basin_slope"              = "Basin Slope",
  "recession_slope"          = "Recession Curve Slope",
  "land_Bare"                = "Land: Bare",
  "land_Cropland"            = "Land: Cropland",
  "land_Forest"              = "Land: Forest",
  "land_Grassland_Shrubland" = "Land: Grassland & Shrubland",
  "land_Ice_Snow"            = "Land: Ice & Snow",
  "land_Impervious"          = "Land: Impervious",
  "land_Salt_Water"          = "Land: Salt Water",
  "land_Tidal_Wetland"       = "Land: Tidal Wetland",
  "land_Water"               = "Land: Water Body",
  "land_Wetland_Marsh"       = "Land: Wetland Marsh"
)

shap_long_all <- shap_long_all %>%
  mutate(
    feature_recoded = ifelse(
      feature %in% names(feature_recode_map),
      feature_recode_map[feature],
      feature
    )
  ) %>%
  mutate(
    feature_recoded = factor(feature_recoded, levels = unique(feature_recoded))
  )

# 13f. Attach the cluster label to each row by ID
shap_long_all$final_cluster <- full_scaled$final_cluster[shap_long_all$id]

###############################################################################
# 14. FACETED BOX‐PLOT: all clusters in one plot, strip labels on the left
###############################################################################
p_box_all <- ggplot(long_data, aes(x = Driver, y = Value, fill = final_cluster)) +
  geom_boxplot() +
  scale_fill_manual(values = my_cluster_colors_lighter, guide = "none") +
  facet_grid(final_cluster ~ ., switch = "y", scales = "free_y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 14) +
  theme(
    strip.placement       = "outside",
    strip.text.y.left     = element_text(angle = 0, size = 14, face = "bold", vjust = 0.5),
    panel.spacing.y       = unit(0.5, "lines"),
    strip.background      = element_blank(),
    strip.text.x          = element_blank(),
    axis.text.x           = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
    axis.text.x.top       = element_blank(),
    axis.ticks.x          = element_blank()
  ) +
  # Hide x‐axis tick marks for all but the bottom facet:
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Add back bottom‐row x-axis driver names using geom_text
bottom_clust <- unique_clusters[length(unique_clusters)]
p_box_all <- p_box_all +
  geom_text(
    data = long_data %>% filter(final_cluster == bottom_clust),
    aes(x = Driver, y = 0, label = Driver),
    angle = 90, hjust = 1, vjust = 0.5, size = 3
  ) +
  labs(x = NULL)

###############################################################################
# 15. FACETED DOT‐PLOT: all clusters in one plot, strip labels on the left
###############################################################################
p_dot_all <- ggplot(shap_long_all, aes(x = shap_value, y = feature_recoded, fill = feature_value)) +
  geom_point(alpha = 0.6, size = 2, shape = 21, color = "black") +
  scale_fill_gradientn(
    colors = c("white", "gray", "black"),
    name   = NULL,
    limits = c(global_min, global_max)
  ) +
  facet_grid(final_cluster ~ ., switch = "y", scales = "free_y") +
  labs(x = "SHAP Value", y = NULL) +
  theme_classic(base_size = 14) +
  theme(
    strip.placement       = "outside",
    strip.text.y.left     = element_text(angle = 0, size = 14, face = "bold", vjust = 0.5),
    panel.spacing.y       = unit(0.5, "lines"),
    strip.background      = element_blank(),
    strip.text.x          = element_blank(),
    axis.text.x           = element_blank(),
    axis.ticks.x          = element_blank(),
    axis.text.y           = element_text(size = 12)
  ) +
  # Re‐enable x‐axis ticks only for the bottom facet
  theme(
    axis.text.x  = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold")
  )

###############################################################################
# 16. Combine Faceted Box & Dot Plots Side‐by‐Side
###############################################################################
final_combined_plot <- p_box_all | p_dot_all

###############################################################################
# 17. Save Combined Faceted Figure
###############################################################################
ggsave(
  filename = "Fig4_FNConc_Cluster_Boxplot_SHAP_DotPlots_faceted.png",
  plot     = final_combined_plot,
  width    = 16,
  height   = 18,
  dpi      = 300,
  path     = output_dir
)
print(final_combined_plot)

###############################################################################
# 18. Save Individual Figures (Unchanged)
###############################################################################
ggsave(
  filename = "Fig5_FNConc_Yearly_Clusters.png",
  plot     = p_FNConc,
  width    = 8,
  height   = 5,
  dpi      = 300,
  path     = output_dir
)
print(p_FNConc)

ggsave(
  filename = "FigSX_FNConc_Sil.png",
  plot     = p_sil,
  width    = 10,
  height   = 6,
  dpi      = 300,
  path     = output_dir
)
print(p_sil)

###############################################################################
# 19. Create and save SHAP bar‐plot grid
###############################################################################
unique_clusters_for_shap <- levels(full_scaled$final_cluster)
plot_list_bars <- lapply(unique_clusters_for_shap, function(cl) {
  plot_mean_abs_shap(cl, shap_values_FNConc, full_scaled)
})

ggsave(
  filename = "FigSX_FNConc_MeanAbsSHAP_Grid.png",
  plot     = wrap_plots(plot_list_bars, ncol = 2),
  width    = 12,
  height   = 9,
  dpi      = 300,
  path     = output_dir
)
print(wrap_plots(plot_list_bars, ncol = 2))

###############################################################################
# 20. Save workspace objects if needed
###############################################################################
save(
  full_scaled,
  cluster_boxplots,
  shap_values_FNConc,
  global_min,
  global_max,
  plot_mean_abs_shap,
  file = "FNConc_HierClust_Workflow_Objects.RData"
)
