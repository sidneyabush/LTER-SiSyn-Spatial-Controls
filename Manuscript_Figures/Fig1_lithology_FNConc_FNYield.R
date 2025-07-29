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
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years.csv", record_length)
) %>% distinct(Stream_ID, .keep_all = TRUE)

# Read all yearly FNConc/FNYield rows (one per year per site)
drivers_df_final_sites <- read.csv(
  sprintf("All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv", record_length)
)

drivers_df_filtered <- drivers_df_final_sites %>%
  left_join(drivers_df_uncleaned, by = "Stream_ID") %>%
  select(-ends_with(".y"), -contains("Gen"), -contains("Flux")) %>%
  # select(
  #   -X, -DecYear, -num_days, -drainSqKm, -chemical,
  #   -Stream_Name, -LTER, -contains("Coord"), -cycle0, -prop_area,
  #   -silicate_weathering, -contains("elevation_"), -contains("basin_slope_"),
  #   -contains("rocks"), -contains("land_"), -contains("permafrost_"),
  #   -Use_WRTDS
  # ) %>%
  rename_with(~ str_remove(., "\\.x$"), ends_with(".x"))

# Convert to sf for mapping, Need to add back in coordinates (pull from reference table)
drivers_sf <- st_as_sf(
  drivers_df_filtered,
  coords = c("Longitude", "Latitude"),
  crs    = 4326
) %>% mutate(
  Longitude = st_coordinates(.)[,1],
  Latitude  = st_coordinates(.)[,2]
)

# --------------------------------------------------
# 2) Load clustering results & merge rock clusters
# --------------------------------------------------
load("../Final_Models/FNConc_HierClust_Workflow_Objects.RData")
sites_with_clusters <- drivers_sf %>%
  st_drop_geometry() %>%
  inner_join(full_scaled %>% select(Stream_ID, final_cluster), by = "Stream_ID")

# Define rock cluster palette
my_cluster_colors <- c(
  "Volcanic"            = "#AC7B32",
  "Sedimentary"         = "#579C8E",
  "Mixed Sedimentary"   = "#89C8A0",
  "Plutonic"            = "#8D9A40",
  "Metamorphic"         = "#C26F86",
  "Carbonate Evaporite" = "#5E88B0"
)

# --------------------------------------------------
# 3) Global Map (Panel A)
# --------------------------------------------------
world <- map_data("world")

global_base <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  coord_sf(crs = st_crs(3857), expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background     = element_rect(fill = "white", color = NA),
    plot.background      = element_rect(fill = "white", color = NA),
    panel.grid           = element_blank(),
    axis.title           = element_blank(),
    axis.text            = element_blank(),
    axis.ticks           = element_blank(),
    legend.position      = c(0.05, 0.65),
    legend.justification = c("left","top"),
    legend.text          = element_text(size = 10),
    legend.title         = element_blank()
  )

global_map <- global_base +
  geom_point(data = sites_with_clusters,
             aes(x = Longitude, y = Latitude, fill = final_cluster),
             shape = 21, size = 2, alpha = 0.5, stroke = 0.1, color = "gray20") +
  scale_fill_manual(values = my_cluster_colors,
                    guide = guide_legend(override.aes = list(size = 4, alpha = 1)))

# --------------------------------------------------
# 4) Regional Insets
# --------------------------------------------------
create_regional_map <- function(xlim, ylim, data_df) {
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "lightgray", color = "white") +
    geom_point(data = data_df,
               aes(x = Longitude, y = Latitude, fill = final_cluster),
               shape = 21, size = 2, alpha = 0.4, stroke = 0.1, color = "gray20") +
    scale_fill_manual(values = my_cluster_colors, guide = "none") +
    coord_sf(xlim = xlim, ylim = ylim, crs = st_crs(3857), expand = FALSE) +
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.border     = element_rect(color = "black", fill = NA, size = 0.5))
}

# Define inset extents
uk_xlim          <- c(-10, 2);   uk_ylim <- c(49, 61)
scandinavia_xlim <- c(4, 30);    scandinavia_ylim <- c(50, 72)
australia_xlim   <- c(135,155);  australia_ylim   <- c(-40,-28)

# Subset for insets
df_uk <- filter(sites_with_clusters, Longitude >= uk_xlim[1], Longitude <= uk_xlim[2], Latitude >= uk_ylim[1], Latitude <= uk_ylim[2])
df_scandinavia <- filter(sites_with_clusters, Longitude >= scandinavia_xlim[1], Longitude <= scandinavia_xlim[2], Latitude >= scandinavia_ylim[1], Latitude <= scandinavia_ylim[2])
df_australia <- filter(sites_with_clusters, Longitude >= australia_xlim[1], Longitude <= australia_xlim[2], Latitude >= australia_ylim[1], Latitude <= australia_ylim[2])

uk_inset <- create_regional_map(uk_xlim, uk_ylim, df_uk)
scandinavia_inset <- create_regional_map(scandinavia_xlim, scandinavia_ylim, df_scandinavia)
australia_inset <- create_regional_map(australia_xlim, australia_ylim, df_australia)

final_map <- ggdraw(global_map) +
  draw_plot(uk_inset, x = 0.345, y = 0.447, width = 0.25, height = 0.25) +
  draw_plot(scandinavia_inset, x = 0.524, y = 0.47, width = 0.25, height = 0.25) +
  draw_plot(australia_inset, x = 0.56, y = 0.12, width = 0.25, height = 0.25) +
  draw_line(x = c(0.419,0.48), y = c(0.68,0.71), color = "black", size = 0.7) +
  draw_line(x = c(0.54,0.58), y = c(0.72,0.66), color = "black", size = 0.7) +
  draw_line(x = c(0.788,0.85), y = c(0.26,0.32), color = "black", size = 0.7)

p_map_labeled <- final_map +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 16, hjust = 0, vjust = 1, face = "plain"),
        plot.tag.position = c(0.02, 0.98))

# --------------------------------------------------
# 5) Boxplots (Panels B & C) plotting average by site
# --------------------------------------------------
site_summary <- sites_with_clusters %>%
  group_by(Stream_ID, final_cluster) %>%
  summarise(
    FNConc  = mean(FNConc,  na.rm = TRUE),
    FNYield = mean(FNYield, na.rm = TRUE),
    .groups = "drop"
  )

site_summary$final_cluster <- factor(site_summary$final_cluster, levels = names(my_cluster_colors))

p1 <- ggplot(site_summary, aes(x = FNConc, y = final_cluster)) +
  geom_boxplot(aes(fill = final_cluster), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(fill = final_cluster, color = final_cluster), width = 0.1, size = 2.2, stroke = 0.1, shape = 21, alpha = 0.6) +
  scale_fill_manual(values = my_cluster_colors) +
  scale_color_manual(values = my_cluster_colors) +
  labs(x = expression("Concentration (mg L"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(site_summary$final_cluster))) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(site_summary, aes(x = FNYield, y = final_cluster)) +
  geom_boxplot(aes(fill = final_cluster), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(fill = final_cluster, color = final_cluster), width = 0.1, size = 2.2, stroke = 0.1, shape = 21, alpha = 0.6) +
  scale_fill_manual(values = my_cluster_colors) +
  scale_color_manual(values = my_cluster_colors) +
  labs(x = expression("Yield (kg km"^-2*" year"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(site_summary$final_cluster))) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

p1_labeled <- p1 + labs(tag = "B") + theme(plot.tag = element_text(size = 16, hjust = 0))
p2_labeled <- p2 + labs(tag = "C") + theme(plot.tag = element_text(size = 16, hjust = 0))
final_boxplots <- p1_labeled + p2_labeled

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
# 6) Export Figures
# --------------------------------------------------
output_dir <- file.path("..", "Final_Figures")

ggsave(file.path(output_dir, "Fig1_map_and_boxplots.png"), combined_figure,
       width=8, height=8.5, dpi=300)
