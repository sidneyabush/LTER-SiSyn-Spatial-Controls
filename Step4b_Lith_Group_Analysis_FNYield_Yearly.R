###############################################################################
# STREAMLINED WORKFLOW: FNYield Clusters → Unscaled Box‐Plot, Silhouette,
#                       & SHAP Bar‐Plot Grid (with Titles and Bottom‐Row X‐Axis)
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

## 2. Clear environment again to be safe
rm(list = ls())

## 3. Set working and output directories
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir       <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Figures"
final_models_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Models"

###############################################################################
# 4. Load Data & Model (from Final_Models)
###############################################################################
load(file.path(final_models_dir, "FNYield_Yearly_rf_model2.RData"))
rf_model2_FNYield <- rf_model2

load(file.path(final_models_dir, "FNYield_Yearly_kept_drivers.RData"))
kept_drivers_FNYield <- kept_drivers

# (Optional) numeric data if needed:
# load(file.path(final_models_dir, "FNYield_Yearly_numeric.RData"))
# drivers_numeric_FNYield <- drivers_numeric

load(file.path(final_models_dir, "FNYield_Yearly_stream_ids.RData"))
drivers_df <- drivers_df

# Load precomputed SHAP values
load(file.path(final_models_dir, "FNYield_Yearly_shap_values_new.RData"))
shap_values_FNYield <- shap_values_FNYield

drivers_full <- read.csv("harmonization_files/All_Drivers_Harmonized_Yearly_FNConc_FNYield_5_years.csv")

drivers_combined <- drivers_df %>%
  inner_join(
    drivers_full %>% dplyr::select(Stream_ID, Year, major_rock, major_land),
    by = c("Stream_ID", "Year")
  ) %>%
  filter(!is.na(major_rock))

###############################################################################
# 5. Consolidate Lithology Categories & Manually Assign Clusters
###############################################################################
drivers_numeric_consolidated_lith <- drivers_combined %>%
  filter(!is.na(major_rock) & trimws(major_rock) != "" & major_rock != "0") %>%
  mutate(
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
      major_rock %in% c("carbonate_evaporite", "volcanic; carbonate_evaporite") ~ "Carbonate Evaporite",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    final_cluster = case_when(
      consolidated_rock == "Sedimentary" & rocks_sedimentary >= 70 ~ "Sedimentary",
      consolidated_rock == "Sedimentary" & rocks_sedimentary < 70  ~ "Mixed Sedimentary",
      TRUE ~ consolidated_rock
    )
  ) %>%
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

# (Optional) Print a small summary table of counts per cluster
row_counts <- drivers_numeric_consolidated_lith %>%
  dplyr::count(final_cluster) %>%
  dplyr::rename(total_rows = n)

stream_counts <- drivers_numeric_consolidated_lith %>%
  dplyr::group_by(final_cluster) %>%
  dplyr::summarise(unique_stream_ids = dplyr::n_distinct(Stream_ID), .groups = "drop")

summary_table <- dplyr::left_join(row_counts, stream_counts, by = "final_cluster")
print(summary_table)

###############################################################################
# 6. Prepare Data for Further Analysis (Global Scaling)
###############################################################################
numeric_cols <- setdiff(
  names(dplyr::select(drivers_numeric_consolidated_lith, where(is.numeric))),
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
# 8. Box‐Plot of Unscaled FNYield by Cluster
###############################################################################
df_unscaled <- drivers_numeric_consolidated_lith %>%
  dplyr::select(Stream_ID, Year, FNYield, final_cluster)

# Save the CSV of unscaled data (optional)
write.csv(
  df_unscaled,
  file = file.path(output_dir, "FNYield_Stream_ID_Year_Cluster.csv"),
  row.names = FALSE
)

p_FNYield <- ggplot(df_unscaled, aes(x = final_cluster, y = FNYield, fill = final_cluster)) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  geom_jitter(aes(color = final_cluster), width = 0.3, alpha = 0.4, size = 2) +
  scale_fill_manual(values = my_cluster_colors_lighter) +
  scale_color_manual(values = my_cluster_colors) +
  labs(x = NULL, y = expression(FNYield~(mg~L^{-1}))) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = "Fig6_FNYield_Yearly_Clusters.png",
  plot     = p_FNYield,
  width    = 8,
  height   = 5,
  dpi      = 300,
  path     = output_dir
)
print(p_FNYield)

###############################################################################
# 9. Silhouette Plot with factoextra
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
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    axis.title.x  = element_blank(),
    legend.title  = element_blank(),
    plot.title    = element_blank(),
    plot.subtitle = element_blank()
  )

ggsave(
  filename = "FigSX_FNYield_Sil.png",
  plot     = p_sil,
  width    = 8,
  height   = 6,
  dpi      = 300,
  path     = output_dir
)
print(p_sil)

###############################################################################
# 10. Define function plot_mean_abs_shap() for SHAP bar plots
#     Each panel has a title (cluster name). We will suppress x‐axis titles
#     on all but the bottom row of the grid.
###############################################################################
plot_mean_abs_shap <- function(cluster_id, shap_values_FNYield, full_scaled) {
  # Subset SHAP rows for this cluster
  cluster_indices <- which(full_scaled$final_cluster == cluster_id)
  shap_cluster    <- shap_values_FNYield[cluster_indices, , drop = FALSE]
  
  # Compute mean(|SHAP|) per feature
  mean_abs_shap <- colMeans(abs(shap_cluster), na.rm = TRUE)
  
  # Build a data frame of feature names + their mean(|SHAP|)
  df_shap <- data.frame(
    feature          = names(mean_abs_shap),
    mean_abs_shapval = as.numeric(mean_abs_shap),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(mean_abs_shapval)) %>%
    # (No need to drop a “target” column since shap_values come only from predictors)
    filter(
      !grepl("^rocks", feature, ignore.case = TRUE)
    )
  
  # Recode feature names to human‐readable labels
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
  
  df_shap <- df_shap %>%
    dplyr::mutate(
      feature_recoded = ifelse(
        feature %in% names(feature_recode_map),
        feature_recode_map[feature],
        feature
      ),
      feature_recoded = factor(feature_recoded, levels = feature_recoded)
    )
  
  # Draw the horizontal bar‐plot with the cluster name as the panel title
  ggplot(df_shap, aes(x = reorder(feature_recoded, mean_abs_shapval), y = mean_abs_shapval)) +
    geom_bar(stat = "identity", fill = my_cluster_colors[[as.character(cluster_id)]], alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1000)) +
    labs(
      x     = NULL,
      y     = "Mean Absolute SHAP Value",
      title = cluster_id
    ) +
    theme_classic(base_size = 14) +
    theme(
      plot.title  = element_text(size = 14, hjust = 0.5),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    )
}

###############################################################################
# 11. Create & Save SHAP Bar‐Plot Grid (with titles, and x‐axis on bottom row only)
###############################################################################
full_scaled <- scaled_data
unique_clusters_for_shap <- levels(full_scaled$final_cluster)

# We will arrange the grid in 2 columns
ncol_bars    <- 2
n_total      <- length(unique_clusters_for_shap)
bottom_index <- seq(n_total - ncol_bars + 1, n_total)  # indices of bottom‐row panels

plot_list_bars <- lapply(seq_along(unique_clusters_for_shap), function(i) {
  cl <- unique_clusters_for_shap[i]
  p  <- plot_mean_abs_shap(cl, shap_values_FNYield, full_scaled)
  
  # If this panel is NOT in the bottom row, suppress its x‐axis title
  if (!(i %in% bottom_index)) {
    p <- p + theme(axis.title.x = element_blank())
  }
  return(p)
})

barplot_grid <- wrap_plots(plot_list_bars, ncol = ncol_bars)

ggsave(
  filename = "FigSX_FNYield_MeanAbsSHAP_Grid_withTitles.png",
  plot     = barplot_grid,
  width    = 12,
  height   = 9,
  dpi      = 300,
  path     = output_dir
)
print(barplot_grid)

