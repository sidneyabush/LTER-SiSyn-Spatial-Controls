###############################################################################
# COMBINED WORKFLOW: FNConc & FNYield Clusters → CSV Exports, RData Saves,
#                   Box‐Plot & Silhouette
#
#   - Exports unscaled data CSVs for both FNConc and FNYield
#   - Saves workflow objects for FNConc and FNYield into "Final_Models/"
#   - Builds one silhouette plot (identical clusters)
#   - Stacks FNConc (top) / FNYield (bottom) box‐plots, with x‐axis only on bottom
###############################################################################

## 1. Load Packages & Clear Environment
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)    # For wrapping/stacking plots
library(cluster)      # silhouette()
library(factoextra)   # fviz_silhouette()
library(colorspace)   # lighten()
library(forcats)      # fct_recode() if needed later

## 2. Set Working & Output Directories (change paths as needed)
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir       <- "Final_Figures"
final_models_dir <- "Final_Models"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(final_models_dir, showWarnings = FALSE, recursive = TRUE)

## 3. Define Cluster‐Color Palettes (shared by both FNConc & FNYield)
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
# A. PROCESS FNConc DATA → CSV, Save FNConc Workflow Objects, p_FNConc (box‐plot),
#                          and p_sil (silhouette)
###############################################################################

## A1. Load FNConc RF Model & Drivers‐of‐Interest Data
load(file.path(final_models_dir, "FNConc_Yearly_rf_model2.RData"))       # loads rf_model2
rf_model2_FNConc       <- rf_model2

load(file.path(final_models_dir, "FNConc_Yearly_kept_drivers.RData"))     # loads kept_drivers
kept_drivers_FNConc    <- kept_drivers

load(file.path(final_models_dir, "FNConc_Yearly_stream_ids.RData"))       # loads drivers_df
drivers_df_FNConc      <- drivers_df

load(file.path(final_models_dir, "FNConc_Yearly_shap_values_new.RData"))  # loads shap_values_FNConc
shap_values_FNConc     <- shap_values_FNConc

## A2. Load the full harmonized driver file (for lithology + land‐cover columns)
drivers_full <- read.csv("harmonization_files/All_Drivers_Harmonized_Yearly_FNConc_FNYield_5_years.csv")

## A3. Join drivers_df_FNConc to drivers_full → filter out missing lithology
drivers_combined_FNConc <- drivers_df_FNConc %>%
  dplyr::inner_join(
    drivers_full %>% dplyr::select(Stream_ID, Year, major_rock, major_land),
    by = c("Stream_ID", "Year")
  ) %>%
  dplyr::filter(
    !is.na(major_rock) &
      trimws(major_rock) != "" &
      major_rock != "0"
  )

## A4. Consolidate Lithology Categories & Assign “final_cluster”
drivers_numeric_consolidated_lith_FNConc <- drivers_combined_FNConc %>%
  dplyr::mutate(
    consolidated_rock = dplyr::case_when(
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
  dplyr::mutate(
    final_cluster = dplyr::case_when(
      consolidated_rock == "Sedimentary" & rocks_sedimentary >= 70 ~ "Sedimentary",
      consolidated_rock == "Sedimentary" & rocks_sedimentary < 70  ~ "Mixed Sedimentary",
      TRUE ~ consolidated_rock
    )
  ) %>%
  dplyr::mutate(
    final_cluster = factor(
      final_cluster,
      levels = c(
        "Volcanic", "Sedimentary", "Mixed Sedimentary",
        "Plutonic", "Metamorphic", "Carbonate Evaporite"
      )
    )
  ) %>%
  as_tibble()

## A5. Export Unscaled FNConc Data as CSV
df_unscaled_FNConc <- drivers_numeric_consolidated_lith_FNConc %>%
  dplyr::select(Stream_ID, Year, FNConc, final_cluster)

write.csv(
  df_unscaled_FNConc,
  file = file.path(output_dir, "FNConc_Stream_ID_Year_Cluster.csv"),
  row.names = FALSE
)

## A6. Prepare “scaled_data” for Silhouette (use FNConc‐based clusters)
numeric_cols_FNConc <- setdiff(
  names(dplyr::select(drivers_numeric_consolidated_lith_FNConc, where(is.numeric))),
  "cluster"
)

scaled_data <- drivers_numeric_consolidated_lith_FNConc %>%
  dplyr::mutate(
    dplyr::across(
      all_of(numeric_cols_FNConc),
      ~ scales::rescale(.x, na.rm = TRUE)
    )
  )

## A7. Build FNConc Box‐Plot (unscaled FNConc by final_cluster)
p_FNConc <- ggplot(df_unscaled_FNConc,
                   aes(x = final_cluster, y = FNConc, fill = final_cluster)) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  geom_jitter(aes(color = final_cluster), width = 0.3, alpha = 0.4, size = 2) +
  scale_fill_manual(values = my_cluster_colors_lighter) +
  scale_color_manual(values = my_cluster_colors) +
  labs(x = NULL, y = expression(Concentration~(mg~L^{-1}))) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    axis.text.x     = element_blank(),   # remove x‐axis text on top plot
    axis.ticks.x    = element_blank()    # remove x‐axis ticks on top plot
  )

## A8. Build Silhouette Plot (once, since clusters same for FNConc & FNYield)
sil_obj <- silhouette(
  as.numeric(scaled_data$final_cluster),
  dist(dplyr::select(scaled_data,
                     rocks_volcanic, rocks_sedimentary,
                     rocks_carbonate_evaporite, rocks_metamorphic,
                     rocks_plutonic))
)
mean_sil_value <- mean(sil_obj[, "sil_width"], na.rm = TRUE)

p_sil <- fviz_silhouette(
  sil_obj,
  label   = FALSE,
  palette = c(
    "#AC7B32", "#579C8E", "#89C8A0",
    "#8D9A40", "#C26F86", "#5E88B0"
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
  labs(x = NULL, y = "Silhouette Width") +
  theme_classic(base_size = 14) +
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

## A9. Assign and Save FNConc Workflow Objects into Final_Models/
full_scaled <- scaled_data

save(
  full_scaled,
  shap_values_FNConc,
  drivers_numeric_consolidated_lith_FNConc,
  file = file.path(final_models_dir, "FNConc_HierClust_Workflow_Objects.RData")
)


###############################################################################
# B. PROCESS FNYield DATA → CSV, Save FNYield Workflow Objects, and p_FNYield (box‐plot)
###############################################################################

## B1. Load FNYield RF Model & Drivers‐of‐Interest Data
load(file.path(final_models_dir, "FNYield_Yearly_rf_model2.RData"))      # loads rf_model2
rf_model2_FNYield      <- rf_model2

load(file.path(final_models_dir, "FNYield_Yearly_kept_drivers.RData"))   # loads kept_drivers
kept_drivers_FNYield   <- kept_drivers

load(file.path(final_models_dir, "FNYield_Yearly_stream_ids.RData"))     # loads drivers_df
drivers_df_FNYield     <- drivers_df

load(file.path(final_models_dir, "FNYield_Yearly_shap_values_new.RData")) # loads shap_values_FNYield
shap_values_FNYield    <- shap_values_FNYield

## B2. Join drivers_df_FNYield to drivers_full → filter out missing lithology
drivers_combined_FNYield <- drivers_df_FNYield %>%
  dplyr::inner_join(
    drivers_full %>% dplyr::select(Stream_ID, Year, major_rock, major_land),
    by = c("Stream_ID", "Year")
  ) %>%
  dplyr::filter(
    !is.na(major_rock) &
      trimws(major_rock) != "" &
      major_rock != "0"
  )

## B3. Consolidate Lithology Categories & Assign “final_cluster”
drivers_numeric_consolidated_lith_FNYield <- drivers_combined_FNYield %>%
  dplyr::mutate(
    consolidated_rock = dplyr::case_when(
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
  dplyr::mutate(
    final_cluster = dplyr::case_when(
      consolidated_rock == "Sedimentary" & rocks_sedimentary >= 70 ~ "Sedimentary",
      consolidated_rock == "Sedimentary" & rocks_sedimentary < 70  ~ "Mixed Sedimentary",
      TRUE ~ consolidated_rock
    )
  ) %>%
  dplyr::mutate(
    final_cluster = factor(
      final_cluster,
      levels = c(
        "Volcanic", "Sedimentary", "Mixed Sedimentary",
        "Plutonic", "Metamorphic", "Carbonate Evaporite"
      )
    )
  ) %>%
  as_tibble()

## B4. Export Unscaled FNYield Data as CSV
df_unscaled_FNYield <- drivers_numeric_consolidated_lith_FNYield %>%
  dplyr::select(Stream_ID, Year, FNYield, final_cluster)

write.csv(
  df_unscaled_FNYield,
  file = file.path(output_dir, "FNYield_Stream_ID_Year_Cluster.csv"),
  row.names = FALSE
)

## B5. Build FNYield Box‐Plot (unscaled FNYield by final_cluster)
p_FNYield <- ggplot(df_unscaled_FNYield,
                    aes(x = final_cluster, y = FNYield, fill = final_cluster)) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  geom_jitter(aes(color = final_cluster), width = 0.3, alpha = 0.4, size = 2) +
  scale_fill_manual(values = my_cluster_colors_lighter) +
  scale_color_manual(values = my_cluster_colors) +
  labs(x = NULL, y = expression(Yield~(kg~km^{-2}~yr^{-1}))) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 45, hjust = 1)  # x‐axis only on bottom plot
  )

## B6. Save FNYield Workflow Objects into Final_Models/
save(
  shap_values_FNYield,
  drivers_numeric_consolidated_lith_FNYield,
  file = file.path(final_models_dir, "FNYield_HierClust_Workflow_Objects.RData")
)


###############################################################################
# C. COMBINE & SAVE “FNConc over FNYield” for Box‐Plots, and SAVE SILHOUETTE
###############################################################################

## C1. Combine Box‐Plots: FNConc (top) / FNYield (bottom), with x‐axis only on bottom
combined_conc <- p_FNConc / p_FNYield +
  plot_annotation(tag_levels = "A") & 
  theme(
    plot.tag         = element_text(size = 16),  # <-- make tags smaller
    plot.tag.position = c(0.02, 0.98)             # optional: tweak tag position if desired
  )

ggsave(
  filename = "Fig5_Concentration_FNConc_FNYield_Combined.png",
  plot     = combined_conc,
  width    = 8,
  height   = 10,       # 8" wide × 10" tall
  dpi      = 300,
  path     = output_dir
)


## C2. Save Single Silhouette Plot
ggsave(
  filename = "FigSX_Silhouette_FNConc_FNYield.png",
  plot     = p_sil,
  width    = 8,
  height   = 6,        # adjust as needed
  dpi      = 300,
  path     = output_dir
)

## C3. (Optional) Print to screen if you want to inspect
print(combined_conc)
print(p_sil)
