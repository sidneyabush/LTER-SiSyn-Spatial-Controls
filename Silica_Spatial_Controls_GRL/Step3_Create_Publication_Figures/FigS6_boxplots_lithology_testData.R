# #############################################################################
# Fig S6: Faceted 3x2 Boxplots showing driver distribution
# #############################################################################
# Required inputs:
#   1) <drv_dir>/AllDrivers_recent30_split.csv
#
# Outputs created:
#   A) <out_dir>/FigS6_Boxplots_lithology_split.png
rm(list = ls())
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

librarian::shelf(ggplot2, dplyr, tidyr, scales, colorspace, quiet = TRUE)

# #############################################################################
# 1. Load & prep data
# #############################################################################

recent30 <- read.csv(
  "harmonization_files/AllDrivers_recent30_split.csv",
  stringsAsFactors = FALSE
)

site_clusters <- recent30 %>%
  distinct(
    Stream_ID,
    major_rock,
    rocks_volcanic,
    rocks_sedimentary,
    rocks_carbonate_evaporite,
    rocks_metamorphic,
    rocks_plutonic
  ) %>%
  mutate(
    consolidated_rock = case_when(
      major_rock %in% c("volcanic","volcanic; plutonic")                       ~ "Volcanic",
      major_rock %in% c(
        "sedimentary","sedimentary; metamorphic",
        "sedimentary; carbonate_evaporite",
        "volcanic; sedimentary; carbonate_evaporite",
        "sedimentary; plutonic; carbonate_evaporite; metamorphic"
      )                                                                        ~ "Sedimentary",
      major_rock %in% c("plutonic","plutonic; metamorphic","volcanic; plutonic; metamorphic") ~ "Plutonic",
      major_rock %in% c("metamorphic","carbonate_evaporite; metamorphic")      ~ "Metamorphic",
      major_rock %in% c("carbonate_evaporite","volcanic; carbonate_evaporite") ~ "Carbonate-evaporite",
      TRUE                                                                     ~ NA_character_
    ),
    final_cluster = case_when(
      consolidated_rock == "Sedimentary" & rocks_sedimentary >= 70 ~ "Sedimentary",
      consolidated_rock == "Sedimentary" & rocks_sedimentary <  70 ~ "Mixed Sedimentary",
      TRUE                                                           ~ consolidated_rock
    )
  ) %>%
  select(Stream_ID, final_cluster)

df <- recent30 %>%
  left_join(site_clusters, by = "Stream_ID") %>%
  mutate(
    NOx = log10(NOx),
    P   = log10(P)
  ) %>%
  mutate(across(where(is.numeric), ~ rescale(.x, to = c(0,1))))

# #############################################################################
# 2. Shared recode map (all features)
# #############################################################################
recode_map <- setNames(
  c(
    "Log(N)", "Log(P)", "NPP", "ET", "Green-up day", "Precip", "Temp",
    "Snow cover", "Permafrost probability", "Elevation", "Basin slope", "RBI", "RCS",
    "Bare land cover", "Cropland cover", "Forest cover", "Grass & shrub cover",
    "Ice & snow cover", "Impervious cover", "Saltwater cover", "Tidal wetland cover",
    "Open-water cover", "Wetland cover", "Volcanic rock", "Sedimentary rock",
    "Carbonate-evaporite rock", "Metamorphic rock", "Plutonic rock"
  ),
  c(
    "NOx", "P", "npp", "evapotrans", "greenup_day", "precip", "temp",
    "snow_cover", "permafrost", "elevation", "basin_slope", "RBI", "recession_slope",
    "land_Bare", "land_Cropland", "land_Forest", "land_Grassland_Shrubland",
    "land_Ice_Snow", "land_Impervious", "land_Salt_Water",
    "land_Tidal_Wetland", "land_Water", "land_Wetland_Marsh",
    "rocks_volcanic", "rocks_sedimentary", "rocks_carbonate_evaporite",
    "rocks_metamorphic", "rocks_plutonic"
  )
)
# #############################################################################
# 3. Select only non-rock features for panels 1–23
# #############################################################################
feature_vars   <- names(recode_map)[1:23]
feature_labels <- unname(recode_map)[1:23]

# #############################################################################
# 4. Melt to long & recode (drop rock columns)
# #############################################################################
df_long <- df %>%
  select(final_cluster, all_of(feature_vars)) %>%
  pivot_longer(
    cols      = -final_cluster,
    names_to  = "feature",
    values_to = "scaled_value"
  ) %>%
  mutate(
    feature       = recode(feature, !!!recode_map[feature_vars]),
    feature       = factor(feature, levels = feature_labels),
    final_cluster = factor(
      final_cluster,
      levels = c(
        "Volcanic","Sedimentary","Mixed Sedimentary",
        "Plutonic","Metamorphic","Carbonate-evaporite"
      )
    )
  )

# #############################################################################
# 5. Precompute shading spans
# #############################################################################
span       <- function(lbl) which(feature_labels == lbl)
prod_range <- c(span("Log(N)") - .5, span("Green-up day") + .5)
clim_range <- c(span("Precip") - .5, span("Permafrost probability") + .5)
topo_range <- c(span("Elevation") - .5, span("Basin slope") + .5)
disc_range <- c(span("RBI") - .5, span("RCS") + .5)
lulc_range <- c(span("Bare land cover") - .5, span("Wetland cover") + .5)

# #############################################################################
# 6. Build the plot
# #############################################################################
p <- ggplot(df_long, aes(x = feature, y = scaled_value)) +
  # shaded background regions
  annotate("rect", xmin = prod_range[1], xmax = prod_range[2],
           ymin = -Inf, ymax = Inf, fill = "#F0F0F0", inherit.aes = FALSE) +
  annotate("rect", xmin = clim_range[1], xmax = clim_range[2],
           ymin = -Inf, ymax = Inf, fill = "#FFFFFF", inherit.aes = FALSE) +
  annotate("rect", xmin = topo_range[1], xmax = topo_range[2],
           ymin = -Inf, ymax = Inf, fill = "#E5E5E5", alpha = 0.9, inherit.aes = FALSE) +
  annotate("rect", xmin = disc_range[1], xmax = disc_range[2],
           ymin = -Inf, ymax = Inf, fill = "#E5E5E5", alpha = 0.5, inherit.aes = FALSE) +
  annotate("rect", xmin = lulc_range[1], xmax = lulc_range[2],
           ymin = -Inf, ymax = Inf, fill = "#FFFFFF", inherit.aes = FALSE) +
  # category labels
  annotate("text", x = mean(prod_range), y = 1.05, label = "Productivity", size = 4, vjust = 0) +
  annotate("text", x = mean(clim_range), y = 1.05, label = "Climate",      size = 4, vjust = 0) +
  annotate("text", x = mean(topo_range), y = 1.05, label = "Topo",         size = 4, vjust = 0) +
  annotate("text", x = mean(disc_range), y = 1.05, label = "Q",            size = 4, vjust = 0) +
  annotate("text", x = mean(lulc_range), y = 1.05, label = "LULC",         size = 4, vjust = 0) +
  # boxplots
  geom_boxplot(
    outlier.shape = NA,
    width         = 0.7,
    aes(fill       = final_cluster),
    color         = "black",
    alpha         = 0.6
  ) +
  # color scale, axes, theme
  scale_fill_manual(
    values = setNames(
      lighten(
        c("#AC7B32","#579C8E","#89C8A0","#8D9A40","#C26F86","#5E88B0"),
        .05
      ),
      levels(df_long$final_cluster)
    )
  ) +
  scale_x_discrete(expand = expansion(add = c(1, 0))) +
  labs(x = NULL, y = "Scaled Value") +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x     = element_text(angle = 90, vjust = .5, hjust = 1, size = 14),
    axis.title.y    = element_text(size = 14),
    legend.position = "none"
  ) +
  facet_wrap(~ final_cluster, ncol = 3)

# #############################################################################
# 7. Save the figure
# #############################################################################
ggsave(
  filename = "Final_Figures/FigS6_Boxplots_lithology_split.png",
  plot     = p,
  width    = 15,
  height   = 10,
  dpi      = 300
)
