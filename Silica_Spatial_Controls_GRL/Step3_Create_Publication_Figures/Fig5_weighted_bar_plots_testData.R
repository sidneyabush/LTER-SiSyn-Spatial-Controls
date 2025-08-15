# #############################################################################
# Fig 5: weighted lithology SHAP bars 
# #############################################################################
# Required inputs:
#   1) <fm>/FNConc_Yearly_shap_values_recent30_split.RData
#   2) <fm>/FNYield_Yearly_shap_values_recent30_split.RData
#   3) <drv_dir>/AllDrivers_recent30_split.csv
#
# Outputs created:
#   A) <output_dir>/Fig5_Lithology_Stacked_SHAP_WeightedValues_split.png

rm(list = ls())
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

librarian::shelf(ggplot2, dplyr, tidyr, patchwork, colorspace, scales, quiet = TRUE)

# #############################################################################
# 1. Paths & output
# #############################################################################
fm <- "Final_Models"
recent30_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files/AllDrivers_recent30_split.csv"
output_dir <- "Final_Figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# #############################################################################
# 2. Load SHAP values (recent30)
# #############################################################################
load(file.path(fm, "FNConc_Yearly_shap_values_recent30_split.RData"))   
load(file.path(fm, "FNYield_Yearly_shap_values_recent30_split.RData"))  

# #############################################################################
# 3. Read recent30 split
# #############################################################################
recent30 <- read.csv(recent30_path, stringsAsFactors = FALSE)

# #############################################################################
# 4. Recreate final_cluster 
# #############################################################################
site_clusters <- recent30 %>%
  distinct(Stream_ID, major_rock, rocks_volcanic, rocks_sedimentary,
           rocks_carbonate_evaporite, rocks_metamorphic, rocks_plutonic) %>%
  mutate(
    consolidated_rock = case_when(
      major_rock %in% c("volcanic", "volcanic; plutonic") ~ "Volcanic",
      major_rock %in% c("sedimentary", "sedimentary; metamorphic",
                        "sedimentary; carbonate_evaporite",
                        "volcanic; sedimentary; carbonate_evaporite",
                        "sedimentary; plutonic; carbonate_evaporite; metamorphic") ~ "Sedimentary",
      major_rock %in% c("plutonic", "plutonic; metamorphic", "volcanic; plutonic; metamorphic") ~ "Plutonic",
      major_rock %in% c("metamorphic", "carbonate_evaporite; metamorphic") ~ "Metamorphic",
      major_rock %in% c("carbonate_evaporite", "volcanic; carbonate_evaporite") ~ "Carbonate Evaporite",
      TRUE ~ NA_character_
    ),
    final_cluster = case_when(
      consolidated_rock == "Sedimentary" & rocks_sedimentary >= 70 ~ "Sedimentary",
      consolidated_rock == "Sedimentary" & rocks_sedimentary < 70  ~ "Mixed Sedimentary",
      TRUE ~ consolidated_rock
    )
  ) %>%
  select(Stream_ID, final_cluster)

recent30 <- recent30 %>%
  left_join(site_clusters, by = "Stream_ID")

# 5. Sanity check: row counts
if (nrow(recent30) != nrow(shap_values_FNConc) || nrow(recent30) != nrow(shap_values_FNYield)) {
  warning("Row count mismatch between recent30 and SHAP objects.")
}

# #############################################################################
# 6. Build scaled driver tables
# #############################################################################
drivers_FNConc_scaled <- recent30 %>%
  mutate(
    P = log10(P)
  ) %>%
  mutate(across(where(is.numeric), ~ scales::rescale(., to = c(0, 1))))

drivers_FNYield_scaled <- recent30 %>%
  mutate(
    NOx = log10(NOx),
    P   = log10(P)
  ) %>%
  mutate(across(where(is.numeric), ~ scales::rescale(., to = c(0, 1))))

# #############################################################################
# 7. Pretty feature recoding (exclude rock predictors intentionally here)
# #############################################################################
recode_map <- setNames(
  c("Log(N)","Log(P)","NPP","ET","Green-up day","Precip","Temp","Snow cover","Permafrost probability",
    "Elevation","Basin slope","RBI","RCS",
    "Bare land cover","Cropland cover","Forest cover","Grass & shrub cover",
    "Ice & snow cover","Impervious cover","Saltwater cover","Tidal wetland cover",
    "Open-water cover","Wetland cover","Volcanic rock","Sedimentary rock",
    "Carbonate-evaporite rock","Metamorphic rock","Plutonic rock"),
  
  c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
    "snow_cover","permafrost","elevation","basin_slope","RBI",
    "recession_slope","land_Bare","land_Cropland","land_Forest",
    "land_Grassland_Shrubland","land_Ice_Snow","land_Impervious",
    "land_Salt_Water","land_Tidal_Wetland","land_Water","land_Wetland_Marsh",
    "rocks_volcanic","rocks_sedimentary","rocks_carbonate_evaporite",
    "rocks_metamorphic","rocks_plutonic")
)
# #############################################################################
# 8. Cluster levels & colors
# #############################################################################
cluster_levels <- c(
  "Volcanic",
  "Sedimentary",
  "Mixed Sedimentary",
  "Plutonic",
  "Metamorphic",
  "Carbonate Evaporite"
)
base_colors <- c("#AC7B32", "#579C8E", "#89C8A0", "#8D9A40", "#C26F86", "#5E88B0")
my_cluster_colors <- setNames(lighten(base_colors, amount = 0.05), cluster_levels)

# #############################################################################
# 9. Concentration: overall driver importance (exclude rock features)
# #############################################################################
conc_driver_importance <- as.data.frame(shap_values_FNConc) %>%
  select(-any_of("final_cluster")) %>%
  select(-matches("^rocks_", ignore.case = TRUE)) %>%
  summarise(across(everything(), ~ mean(abs(.x), na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "driver_mean_abs_shap")

# #############################################################################
# 10. Concentration: lithology-weighted SHAP
# #############################################################################
conc_shap_litho <- as.data.frame(shap_values_FNConc) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
  left_join(
    drivers_FNConc_scaled %>% mutate(id = row_number()) %>% select(id, final_cluster),
    by = "id"
  ) %>%
  filter(!grepl("^rocks_", feature, ignore.case = TRUE)) %>%
  group_by(feature, final_cluster) %>%
  summarize(litho_mean_abs = mean(abs(shap_value), na.rm = TRUE), .groups = "drop") %>%
  left_join(conc_driver_importance, by = "feature") %>%
  group_by(feature) %>%
  mutate(
    proportion     = litho_mean_abs / sum(litho_mean_abs),
    weighted_value = proportion * driver_mean_abs_shap
  ) %>%
  ungroup() %>%
  mutate(
    feature       = recode(feature, !!!recode_map),
    final_cluster = factor(final_cluster, levels = cluster_levels)
  )

feature_order_conc <- conc_shap_litho %>%
  distinct(feature, driver_mean_abs_shap) %>%
  arrange(driver_mean_abs_shap) %>%
  pull(feature)
conc_shap_litho$feature <- factor(conc_shap_litho$feature, levels = feature_order_conc)

# #############################################################################
# 11. Yield: overall driver importance
# #############################################################################
yield_driver_importance <- as.data.frame(shap_values_FNYield) %>%
  select(-any_of("final_cluster")) %>%
  select(-matches("^rocks_", ignore.case = TRUE)) %>%
  summarise(across(everything(), ~ mean(abs(.x), na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "driver_mean_abs_shap")

# #############################################################################
# 12. Yield: lithology-weighted SHAP
# #############################################################################
yield_shap_litho <- as.data.frame(shap_values_FNYield) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
  left_join(
    drivers_FNYield_scaled %>% mutate(id = row_number()) %>% select(id, final_cluster),
    by = "id"
  ) %>%
  filter(!grepl("^rocks_", feature, ignore.case = TRUE)) %>%
  group_by(feature, final_cluster) %>%
  summarize(litho_mean_abs = mean(abs(shap_value), na.rm = TRUE), .groups = "drop") %>%
  left_join(yield_driver_importance, by = "feature") %>%
  group_by(feature) %>%
  mutate(
    proportion     = litho_mean_abs / sum(litho_mean_abs),
    weighted_value = proportion * driver_mean_abs_shap
  ) %>%
  ungroup() %>%
  mutate(
    feature       = recode(feature, !!!recode_map),
    final_cluster = factor(final_cluster, levels = cluster_levels)
  )

feature_order_yield <- yield_shap_litho %>%
  distinct(feature, driver_mean_abs_shap) %>%
  arrange(driver_mean_abs_shap) %>%
  pull(feature)
yield_shap_litho$feature <- factor(yield_shap_litho$feature, levels = feature_order_yield)

# #############################################################################
# 13. Plot 
# #############################################################################
conc_litho_bar <- ggplot(conc_shap_litho,
                         aes(x = weighted_value, y = feature, fill = final_cluster)) +
  geom_col(position = position_stack(reverse = TRUE), color = "black", size = 0.3) +
  scale_fill_manual(
    name   = "Lithology",
    values = my_cluster_colors,
    breaks = cluster_levels,
    limits = cluster_levels
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    x     = "Weighted Mean Absolute SHAP Value",
    y     = NULL,
    title = "Concentration",
    tag   = "a)"
  ) +
  theme_classic(base_size = 22) +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 24),
    plot.tag         = element_text(size = 30, hjust = 0, vjust = 1),
    axis.text        = element_text(size = 22),
    axis.title.x     = element_text(size = 22),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 22)
  )

yield_litho_bar <- ggplot(yield_shap_litho,
                          aes(x = weighted_value, y = feature, fill = final_cluster)) +
  geom_col(position = position_stack(reverse = TRUE), color = "black", size = 0.3) +
  scale_fill_manual(
    name   = "Lithology",
    values = my_cluster_colors,
    breaks = cluster_levels,
    limits = cluster_levels
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    x     = "Weighted Mean Absolute SHAP Value",
    y     = NULL,
    title = "Yield",
    tag   = "b)"
  ) +
  theme_classic(base_size = 22) +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 24),
    plot.tag         = element_text(size = 30, hjust = 0, vjust = 1),
    axis.text        = element_text(size = 22),
    axis.title.x     = element_text(size = 22),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 22)
  )

# #############################################################################
# 14. Combine & save
# #############################################################################
fig_litho_shap <- conc_litho_bar + yield_litho_bar +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  file.path(output_dir, "Fig5_Lithology_Stacked_SHAP_WeightedValues_split.png"),
  fig_litho_shap,
  width  = 20,
  height = 10,
  dpi    = 300,
  bg     = "white"
)
