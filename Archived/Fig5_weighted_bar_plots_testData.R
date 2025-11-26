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
recent30_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files/inputs/AllDrivers_recent30_split.csv"
output_dir_png <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Spatial_controls_GRL/GRL_Materials/Final_Figures/PNG"
output_dir_pdf <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Spatial_controls_GRL/GRL_Materials/Final_Figures/PDF"

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
  left_join(site_clusters, by = "Stream_ID") %>%
  filter(!is.na(final_cluster))

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
# 10. Concentration: lithology-specific SHAP with variability
# #############################################################################
conc_shap_litho <- as.data.frame(shap_values_FNConc) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
  left_join(
    drivers_FNConc_scaled %>% mutate(id = row_number()) %>% select(id, final_cluster),
    by = "id"
  ) %>%
  filter(!grepl("^rocks_", feature, ignore.case = TRUE)) %>%
  filter(!is.na(final_cluster)) %>%
  mutate(abs_shap = abs(shap_value)) %>%
  group_by(feature, final_cluster) %>%
  summarize(
    litho_mean_abs = mean(abs_shap, na.rm = TRUE),
    litho_se       = sd(abs_shap, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  left_join(conc_driver_importance, by = "feature") %>%
  mutate(
    feature       = recode(feature, !!!recode_map),
    final_cluster = factor(final_cluster, levels = cluster_levels)
  )

feature_order_conc <- conc_shap_litho %>%
  distinct(feature, driver_mean_abs_shap) %>%
  arrange(desc(driver_mean_abs_shap)) %>%
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
# 12. Yield: lithology-specific SHAP with variability
# #############################################################################
yield_shap_litho <- as.data.frame(shap_values_FNYield) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "feature", values_to = "shap_value") %>%
  left_join(
    drivers_FNYield_scaled %>% mutate(id = row_number()) %>% select(id, final_cluster),
    by = "id"
  ) %>%
  filter(!grepl("^rocks_", feature, ignore.case = TRUE)) %>%
  filter(!is.na(final_cluster)) %>%
  mutate(abs_shap = abs(shap_value)) %>%
  group_by(feature, final_cluster) %>%
  summarize(
    litho_mean_abs = mean(abs_shap, na.rm = TRUE),
    litho_se       = sd(abs_shap, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  left_join(yield_driver_importance, by = "feature") %>%
  mutate(
    feature       = recode(feature, !!!recode_map),
    final_cluster = factor(final_cluster, levels = cluster_levels)
  )

feature_order_yield <- yield_shap_litho %>%
  distinct(feature, driver_mean_abs_shap) %>%
  arrange(desc(driver_mean_abs_shap)) %>%
  pull(feature)
yield_shap_litho$feature <- factor(yield_shap_litho$feature, levels = feature_order_yield)

# #############################################################################
# 13. Plot - Faceted by Driver (across-lithology comparison with variability)
# #############################################################################
# For each driver, show bars for each lithology with error bars (SE)
# This allows comparison of how each driver varies across lithologies

# Concentration: 9 retained drivers (excluding lithology) - in order from most to least important
conc_retained_order <- c("Elevation", "Basin slope", "RCS", "Open-water cover",
                         "Log(P)", "RBI", "Log(N)", "Impervious cover", "NPP")

# Yield: 10 retained drivers (excluding lithology) - in order from most to least important
yield_retained_order <- c("ET", "Log(N)", "Temp", "RCS", "Elevation", "RBI",
                          "Impervious cover", "Wetland cover", "Basin slope", "Log(P)")

conc_shap_plot <- conc_shap_litho %>%
  filter(feature %in% conc_retained_order) %>%
  mutate(feature = factor(feature, levels = conc_retained_order))

yield_shap_plot <- yield_shap_litho %>%
  filter(feature %in% yield_retained_order) %>%
  mutate(feature = factor(feature, levels = yield_retained_order))

# Calculate x-axis limits with padding to ensure error bars don't get cut off
conc_max <- max(conc_shap_plot$litho_mean_abs + conc_shap_plot$litho_se, na.rm = TRUE) * 1.1
yield_max <- max(yield_shap_plot$litho_mean_abs + yield_shap_plot$litho_se, na.rm = TRUE) * 1.1

# Create concentration plot with facet_grid (lithology labels on top)
conc_plot <- ggplot(conc_shap_plot, aes(x = litho_mean_abs, y = feature, fill = final_cluster)) +
  geom_col(color = "black", size = 0.3) +
  geom_errorbar(aes(xmin = litho_mean_abs - litho_se,
                    xmax = litho_mean_abs + litho_se),
                width = 0.3, size = 0.4) +
  facet_grid(. ~ final_cluster, scales = "free_x") +
  scale_fill_manual(values = my_cluster_colors, limits = cluster_levels) +
  scale_x_continuous(limits = c(0, conc_max), expand = c(0, 0)) +
  scale_y_discrete(limits = rev) +
  labs(x = NULL, y = NULL, tag = "a) Concentration") +
  theme_classic(base_size = 26) +
  theme(
    plot.tag = element_text(hjust = 0, size = 26),
    plot.tag.position = c(-0.05, 1.02),
    plot.margin = ggplot2::margin(t = 10, r = 15, b = 5, l = 70, unit = "pt"),
    strip.text.x = element_text(size = 22),
    strip.background = element_rect(fill = "white", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    legend.position = "none",
    panel.spacing = unit(0.3, "lines")
  )

# Create yield plot with facet_grid (lithology labels on top, x-axis shown)
yield_plot <- ggplot(yield_shap_plot, aes(x = litho_mean_abs, y = feature, fill = final_cluster)) +
  geom_col(color = "black", size = 0.3) +
  geom_errorbar(aes(xmin = litho_mean_abs - litho_se,
                    xmax = litho_mean_abs + litho_se),
                width = 0.3, size = 0.4) +
  facet_grid(. ~ final_cluster, scales = "free_x") +
  scale_fill_manual(values = my_cluster_colors, limits = cluster_levels) +
  scale_x_continuous(limits = c(0, yield_max), expand = c(0, 0)) +
  scale_y_discrete(limits = rev) +
  labs(x = "Mean Absolute Value of SHAP", y = NULL, tag = "b) Yield") +
  theme_classic(base_size = 26) +
  theme(
    plot.tag = element_text(hjust = 0, size = 26),
    plot.tag.position = c(-0.05, 1.02),
    plot.margin = ggplot2::margin(t = 10, r = 15, b = 5, l = 70, unit = "pt"),
    strip.text.x = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.title.x = element_text(size = 22),
    legend.position = "none",
    panel.spacing = unit(0.3, "lines")
  )

# Combine vertically (concentration on top, yield on bottom)
fig_litho_shap <- conc_plot / yield_plot

# Save as PNG for viewing
ggsave(
  file.path(output_dir_png, "Fig5_Lithology_Faceted_SHAP_Within_Lithology.png"),
  fig_litho_shap,
  width  = 23,
  height = 12,
  dpi    = 300,
  bg     = "white"
)

# Save as PDF for publication
ggsave(
  file.path(output_dir_pdf, "Fig5_Lithology_Faceted_SHAP_Within_Lithology.pdf"),
  fig_litho_shap,
  width  = 23,
  height = 12,
  device = "pdf",
  bg     = "white"
)
