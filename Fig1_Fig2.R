library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(patchwork)
library(scales)
library(colorspace)
library(ggrepel)
library(ggspatial)
library(sf)
library(ggpubr)
library(cowplot)

# --------------------------------------------------
# 1) Data Preparation
# --------------------------------------------------
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")
rm(list = ls())

record_length <- 5

drivers_df_uncleaned <- read.csv(
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length)
) %>% distinct(Stream_ID, .keep_all = TRUE)

drivers_df_final_sites <- read.csv(
  sprintf("All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv", record_length)
) %>% distinct(Stream_ID, .keep_all = TRUE)

drivers_df_filtered <- drivers_df_final_sites %>%
  left_join(drivers_df_uncleaned, by = "Stream_ID") %>%
  select(-ends_with(".y"), -contains("Gen"), -contains("Flux")) %>%
  select(
    -Year.x, -X, -DecYear, -num_days, -drainSqKm, -chemical,
    -Stream_Name, -LTER, -contains("Coord"), -cycle0, -prop_area,
    -silicate_weathering, -contains("elevation_"), -contains("basin_slope_"),
    -contains("rocks"), -contains("land_"), -contains("permafrost_"),
    -Use_WRTDS
  ) %>%
  rename_with(~ str_remove(., "\\.x$"), ends_with(".x"))

drivers_df_filtered$Name <- factor(
  drivers_df_filtered$Name,
  levels = c(
    "Tropical", "Humid Subtropical", "Humid Temperate",
    "Humid Continental", "Mediterranean", "Semi-Arid",
    "Arid", "Subarctic"
  )
)

cbPalette_named <- c(
  "Tropical"          = "#145A32",
  "Humid Subtropical" = "#3F7E5B",
  "Humid Temperate"   = "#A2BFA1",
  "Humid Continental" = "#1F78B4",
  "Mediterranean"     = "#35878F",
  "Semi-Arid"         = "#D4A017",
  "Arid"              = "#C55A11",
  "Subarctic"         = "#A75078"
)

cbPalette_lighter  <- lighten(cbPalette_named, amount = 0.3)
cbPalette_lighter2 <- lighten(cbPalette_named, amount = 0.4)

drivers_df_filtered <- st_as_sf(
  drivers_df_filtered,
  coords = c("Longitude", "Latitude"),
  crs    = 4326
) %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2]
  )

# --------------------------------------------------
# 2) Global Map (Panel A)
# --------------------------------------------------
world <- map_data("world")

global_map <- ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill  = "lightgray",
    color = "white"
  ) +
  geom_point(
    data  = drivers_df_filtered,
    aes(x = Longitude, y = Latitude, fill = Name),
    shape = 21, size = 2, alpha = 0.8, stroke = 0.1, color = "gray2"
  ) +
  scale_fill_manual(
    values = cbPalette_lighter,
    guide  = guide_legend(override.aes = list(size = 5))
  ) +
  coord_sf(crs = st_crs(3857), expand = FALSE) +
  theme_minimal() +
  labs(fill = NULL) +
  theme(
    panel.background     = element_rect(fill = "white", color = NA),
    plot.background      = element_rect(fill = "white", color = NA),
    panel.grid           = element_blank(),
    axis.title           = element_blank(),
    axis.text            = element_blank(),
    axis.ticks           = element_blank(),
    legend.text          = element_text(size = 10),
    legend.position      = c(0.05, 0.65),
    legend.justification = c("left", "top")
  )

# --------------------------------------------------
# 3) Regional Insets
# --------------------------------------------------
create_regional_map <- function(xlim, ylim) {
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "lightgray", color = "white") +
    geom_point(data = drivers_df_filtered,
               aes(x = Longitude, y = Latitude, fill = Name),
               shape = 21, size = 2, alpha = 0.8, stroke = 0.1, color = "gray2") +
    scale_fill_manual(values = cbPalette_lighter, guide = "none") +
    coord_sf(xlim = xlim, ylim = ylim, crs = st_crs(3857), expand = FALSE) +
    theme_classic() +
    theme(
      legend.position   = "none",
      panel.background  = element_rect(fill = "white", color = NA),
      panel.border      = element_rect(color = "black", fill = NA, size = 1),
      plot.background   = element_rect(fill = "transparent", color = NA),
      panel.grid        = element_blank(),
      axis.text         = element_blank(),
      axis.ticks        = element_blank(),
      axis.title        = element_blank()
    )
}

uk_map          <- create_regional_map(c(-10, 2),   c(49, 61))
scandinavia_map <- create_regional_map(c(4, 30),    c(50, 72))
australia_map   <- create_regional_map(c(135, 155), c(-40, -28))

# --------------------------------------------------
# 4) Overlay Insets
# --------------------------------------------------
final_map <- ggdraw(global_map) +
  draw_plot(uk_map,          x = 0.345, y = 0.447, width = 0.25, height = 0.25) +
  draw_plot(scandinavia_map, x = 0.524, y = 0.47,  width = 0.25, height = 0.25) +
  draw_plot(australia_map,   x = 0.56,  y = 0.12,  width = 0.25, height = 0.25) +
  draw_line(x = c(0.419,0.48), y = c(0.68,0.71), color = "black", size = 0.7) +
  draw_line(x = c(0.54,0.58),  y = c(0.72,0.66), color = "black", size = 0.7) +
  draw_line(x = c(0.788,0.85), y = c(0.26,0.32), color = "black", size = 0.7)

# --------------------------------------------------
# 5) Boxplots (Panels B & C)
# --------------------------------------------------
p1 <- ggplot(drivers_df_filtered, aes(x = FNConc, y = Name)) +
  geom_boxplot(aes(fill = Name), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(color = Name), width = 0.1, size = 2) +
  scale_fill_manual(values = cbPalette_lighter2) +
  scale_color_manual(values = cbPalette_lighter) +
  labs(x = expression("Concentration (mg L"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(drivers_df_filtered$Name))) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

p2 <- ggplot(drivers_df_filtered, aes(x = FNYield, y = Name)) +
  geom_boxplot(aes(fill = Name), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(color = Name), width = 0.1, size = 2) +
  scale_fill_manual(values = cbPalette_lighter2) +
  scale_color_manual(values = cbPalette_lighter) +
  labs(x = expression("Yield (kg km"^-2*" year"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(drivers_df_filtered$Name))) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(
    legend.position = "none",
    axis.text.y     = element_blank(),
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

p1_labeled <- p1 + labs(tag = "B") + theme(plot.tag = element_text(size = 16, hjust = 0, vjust = 1))
p2_labeled <- p2 + labs(tag = "C") + theme(plot.tag = element_text(size = 16, hjust = 0, vjust = 1))
final_boxplots <- p1_labeled + p2_labeled

# --------------------------------------------------
# 6) Label Map as "A" & Combine into Fig.1
# --------------------------------------------------
p_map_labeled <- final_map +
  labs(tag = "A") +
  theme(
    plot.tag          = element_text(size = 16, hjust = 0, vjust = 1, face = "plain"),
    plot.tag.position = c(0.02, 0.98)
  )

combined_figure <- ggarrange(
  p_map_labeled,
  final_boxplots,
  ncol    = 1,
  nrow    = 2,
  heights = c(3, 2.5),
  align   = "v",
  labels  = NULL
)

# --------------------------------------------------
# 7) Precip vs Temp (Figure 2)
# --------------------------------------------------
df_lter <- drivers_df_filtered %>%
  mutate(LTER_extracted = str_extract(Stream_ID, "^[^_]+")) %>%
  group_by(LTER_extracted, Name) %>%
  summarise(
    Temperature   = mean(temp, na.rm = TRUE),
    Precipitation = mean(precip * 365, na.rm = TRUE),
    .groups       = "drop"
  )

p_temp_precip <- ggplot(df_lter, aes(x = Temperature, y = Precipitation, fill = Name, label = LTER_extracted)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray2", size = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray2", size = 0.6) +
  geom_point(shape = 21, size = 5, stroke = 0.1, color = "gray2", alpha = 0.8) +
  geom_text_repel(size = 5, max.overlaps = Inf, point.padding = unit(1, "lines")) +
  scale_fill_manual(values = cbPalette_lighter) +
  labs(
    x = expression("Temperature ("*degree*C*")"),
    y = expression("Precipitation (mm)")
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = c(0.9, 0.55),
    legend.title    = element_blank()
  )

# --------------------------------------------------
# 8) Export Figures 1 & 2
# --------------------------------------------------
output_dir <- file.path("..", "Final_Figures")

ggsave(
  filename = file.path(output_dir, "Fig1_map_and_boxplots.png"),
  plot     = combined_figure,
  width    = 8,
  height   = 8.5,
  dpi      = 300
)

ggsave(
  filename = file.path(output_dir, "Fig2_temp_vs_precip.png"),
  plot     = p_temp_precip,
  width    = 10,
  height   = 9.5,
  dpi      = 300
)
