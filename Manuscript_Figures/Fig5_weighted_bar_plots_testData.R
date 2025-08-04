# ----- Fig 5: weighted lithology SHAP bars for Concentration & Yield -----

# 0. Packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(colorspace)

# 1. Paths & output
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
fm <- "Final_Models"
output_dir <- "Final_Figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 2. Load SHAP data (recent30)
load(file.path(fm, "FNConc_Yearly_shap_values_recent30.RData"))   # defines shap_values_FNConc
load(file.path(fm, "FNYield_Yearly_shap_values_recent30.RData"))  # defines shap_values_FNYield

# ---- prep for step 12: get scaled recent30 driver data + clusters ----

# 0. Packages (if not already loaded)
library(dplyr)
library(tidyr)
library(scales)       # for rescale
# (ggplot2 / patchwork etc. used later in plotting section)

# 1. Paths
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
fm <- "Final_Models"
output_dir <- "Final_Figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 2. Load SHAP (recent30)
load(file.path(fm, "FNConc_Yearly_shap_values_recent30.RData"))   # shap_values_FNConc
load(file.path(fm, "FNYield_Yearly_shap_values_recent30.RData"))  # shap_values_FNYield

# 3. Load hierarchical clustering workflow to get full_scaled (for cluster labels)
load(file.path(fm, "FNConc_HierClust_Workflow_Objects.RData"))    # expects object full_scaled
# If the yield side has a separate clustering object and differs, load that too:
# load(file.path(fm, "FNYield_HierClust_Workflow_Objects.RData"))

if (!exists("full_scaled") || is.null(full_scaled$final_cluster)) {
  stop("full_scaled$final_cluster not found; need the hierarchical clustering object with cluster labels.")
}

# 4. Prepare recent30 driver numeric data
# === YOU MUST PROVIDE these objects ===
# e.g., they might come from your existing recent30 split pipeline; placeholder names:
# drivers_numeric_recent30_FNConc
# drivers_numeric_recent30_FNYield

if (!exists("drivers_numeric_recent30_FNConc") || !exists("drivers_numeric_recent30_FNYield")) {
  stop("Please supply 'drivers_numeric_recent30_FNConc' and 'drivers_numeric_recent30_FNYield' (raw numeric driver tables for recent30).")
}

# 5. Apply log transform + rescale as in your original logic
drivers_FNConc_scaled <- drivers_numeric_recent30_FNConc %>%
  mutate(
    # NOx = log10(NOx),  # intentionally omitted for concentration
    P = log10(P)
  ) %>%
  mutate(across(everything(), ~ scales::rescale(., to = c(0, 1))))

drivers_FNYield_scaled <- drivers_numeric_recent30_FNYield %>%
  mutate(
    NOx = log10(NOx),
    P   = log10(P)
  ) %>%
  mutate(across(everything(), ~ scales::rescale(., to = c(0, 1))))

# 6. Attach cluster labels: subset full_scaled$final_cluster to the recent30 rows
# This assumes rownames or an ID allows mapping. Prefer explicit matching if possible.
# If the driver tables and full_scaled share rownames:
if (!is.null(rownames(drivers_FNConc_scaled)) &&
    all(rownames(drivers_FNConc_scaled) %in% rownames(full_scaled))) {
  drivers_FNConc_scaled$final_cluster <- full_scaled[rownames(drivers_FNConc_scaled), "final_cluster"]
} else {
  warning("Row names of drivers_FNConc_scaled do not align with full_scaled. You need to supply a matching key to map clusters.")
  # Example alternative (if you have an 'id' column in both):
  # drivers_FNConc_scaled <- drivers_FNConc_scaled %>%
  #   left_join(full_scaled %>% select(final_cluster) %>% mutate(id = rownames(.)), by = "id")
}

if (!is.null(rownames(drivers_FNYield_scaled)) &&
    all(rownames(drivers_FNYield_scaled) %in% rownames(full_scaled))) {
  drivers_FNYield_scaled$final_cluster <- full_scaled[rownames(drivers_FNYield_scaled), "final_cluster"]
} else {
  warning("Row names of drivers_FNYield_scaled do not align with full_scaled. You need to supply a matching key to map clusters.")
}

# At this point the objects drivers_FNConc_scaled and drivers_FNYield_scaled
# have the required final_cluster column and can be fed into the step 12 plotting code.


# 3. Expectation: drivers_*_scaled exist in workspace with a column final_cluster.
#    If not, stop with message.
if (!exists("drivers_FNConc_scaled") || !"final_cluster" %in% colnames(drivers_FNConc_scaled)) {
  stop("drivers_FNConc_scaled with final_cluster is required in the environment.")
}
if (!exists("drivers_FNYield_scaled") || !"final_cluster" %in% colnames(drivers_FNYield_scaled)) {
  stop("drivers_FNYield_scaled with final_cluster is required in the environment.")
}

# 4. Define recoding map (for pretty feature labels)
recode_map_box <- setNames(
  c("N", "P", "NPP", "ET", "Greenup Day", "Precip", "Temp",
    "Snow Cover", "Permafrost", "Elevation", "Basin Slope",
    "Flashiness (RBI)", "Recession Curve Slope", "Land: Bare", "Land: Cropland",
    "Land: Forest", "Land: Grass & Shrub", "Land: Ice & Snow",
    "Land: Impervious", "Land: Salt Water", "Land: Tidal Wetland",
    "Land: Water Body", "Land: Wetland Marsh"),
  c("NOx", "P", "npp", "evapotrans", "greenup_day", "precip", "temp",
    "snow_cover", "permafrost", "elevation", "basin_slope", "RBI",
    "recession_slope", "land_Bare", "land_Cropland", "land_Forest",
    "land_Grassland_Shrubland", "land_Ice_Snow", "land_Impervious",
    "land_Salt_Water", "land_Tidal_Wetland", "land_Water",
    "land_Wetland_Marsh")
)

# 5. Lithology ordering & colors
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

# 6. Concentration: driver importance (mean absolute SHAP, excluding rocks_)
conc_driver_importance <- as.data.frame(shap_values_FNConc) %>%
  select(-any_of("final_cluster")) %>%
  select(-matches("^rocks_", ignore.case = TRUE)) %>%
  summarise(across(everything(), ~ mean(abs(.x), na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "feature",
    values_to = "driver_mean_abs_shap"
  )

# 7. Concentration: lithology-specific weighted contributions
conc_shap_litho <- as.data.frame(shap_values_FNConc) %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = -id,
    names_to = "feature",
    values_to = "shap_value"
  ) %>%
  left_join(
    drivers_FNConc_scaled %>%
      mutate(id = row_number()) %>%
      select(id, final_cluster),
    by = "id"
  ) %>%
  filter(!grepl("^rocks_", feature, ignore.case = TRUE)) %>%
  group_by(feature, final_cluster) %>%
  summarize(
    litho_mean_abs = mean(abs(shap_value), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(conc_driver_importance, by = "feature") %>%
  group_by(feature) %>%
  mutate(
    proportion     = litho_mean_abs / sum(litho_mean_abs),
    weighted_value = proportion * driver_mean_abs_shap
  ) %>%
  ungroup() %>%
  mutate(
    feature       = recode(feature, !!!recode_map_box),
    final_cluster = factor(final_cluster, levels = cluster_levels)
  )

feature_order_conc <- conc_shap_litho %>%
  distinct(feature, driver_mean_abs_shap) %>%
  arrange(driver_mean_abs_shap) %>%
  pull(feature)
conc_shap_litho$feature <- factor(conc_shap_litho$feature, levels = feature_order_conc)

# 8. Yield: driver importance
yield_driver_importance <- as.data.frame(shap_values_FNYield) %>%
  select(-any_of("final_cluster")) %>%
  select(-matches("^rocks_", ignore.case = TRUE)) %>%
  summarise(across(everything(), ~ mean(abs(.x), na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "feature",
    values_to = "driver_mean_abs_shap"
  )

# 9. Yield: lithology-weighted
yield_shap_litho <- as.data.frame(shap_values_FNYield) %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = -id,
    names_to = "feature",
    values_to = "shap_value"
  ) %>%
  left_join(
    drivers_FNYield_scaled %>%
      mutate(id = row_number()) %>%
      select(id, final_cluster),
    by = "id"
  ) %>%
  filter(!grepl("^rocks_", feature, ignore.case = TRUE)) %>%
  group_by(feature, final_cluster) %>%
  summarize(
    litho_mean_abs = mean(abs(shap_value), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(yield_driver_importance, by = "feature") %>%
  group_by(feature) %>%
  mutate(
    proportion     = litho_mean_abs / sum(litho_mean_abs),
    weighted_value = proportion * driver_mean_abs_shap
  ) %>%
  ungroup() %>%
  mutate(
    feature       = recode(feature, !!!recode_map_box),
    final_cluster = factor(final_cluster, levels = cluster_levels)
  )

feature_order_yield <- yield_shap_litho %>%
  distinct(feature, driver_mean_abs_shap) %>%
  arrange(driver_mean_abs_shap) %>%
  pull(feature)
yield_shap_litho$feature <- factor(yield_shap_litho$feature, levels = feature_order_yield)

# 10. Plot panels
conc_litho_bar <- ggplot(conc_shap_litho,
                         aes(x = weighted_value, y = feature, fill = final_cluster)) +
  geom_col(position = position_stack(reverse = TRUE)) +
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
    tag   = "A"
  ) +
  theme_classic(base_size = 22) +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 24),
    plot.tag         = element_text(size = 24, hjust = 0, vjust = 1),
    axis.text        = element_text(size = 22),
    axis.title.x     = element_text(size = 22),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 22)
  )

yield_litho_bar <- ggplot(yield_shap_litho,
                          aes(x = weighted_value, y = feature, fill = final_cluster)) +
  geom_col(position = position_stack(reverse = TRUE)) +
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
    tag   = "B"
  ) +
  theme_classic(base_size = 22) +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 24),
    plot.tag         = element_text(size = 24, hjust = 0, vjust = 1),
    axis.text        = element_text(size = 22),
    axis.title.x     = element_text(size = 22),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 22)
  )

# 11. Combine & save
fig_litho_shap <- conc_litho_bar + yield_litho_bar +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  file.path(output_dir, "Fig5_Lithology_Stacked_SHAP_WeightedValues.png"),
  fig_litho_shap,
  width  = 20,
  height = 10,
  dpi    = 300,
  bg     = "white"
)
