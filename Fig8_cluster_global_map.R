# --------------------------------------------------
# World Map for Clusters with Inlay Maps for Both Panels
# --------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(sf)
library(ggspatial)
library(cowplot)  # For overlaying inlay maps

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(maps)

# Define discrete color palettes for clusters
my_conc_cluster_colors <- c(
  "1" = "#88A2DC",  # Muted Blue (instead of bold primary blue)
  "2" = "#E69F00",  # Warm Muted Orange
  "3" = "#6BAE75"
)

my_yield_cluster_colors <- c(
  "1" = "#AC7B32",  # Rich Ochre (Warm Earthy Brown-Gold)
  "2" = "#579C8E",  # Muted Teal (Cool & fresh)
  "3" = "#C26F86",  # Dusty Rose (Soft but warm)
  "4" = "#5E88B0"   # Soft Steel Blue (Cool contrast)
)

# 1. Read in the cluster data
FNConc_clusters <- read.csv("FNConc_Stream_ID_Year_Cluster.csv")
FNYield_clusters <- read.csv("FNYield_Stream_ID_Year_Cluster.csv")

# Helper function to calculate the mode (first in case of ties)
calc_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 2. Merge clusters with drivers_df to get lat/long values and compute modal cluster per site

# For FNConc: merge and compute modal cluster per stream_id
df_FNConc <- drivers_df_filtered %>%
  inner_join(FNConc_clusters, by = c("Stream_ID")) %>% 
  group_by(Stream_ID) %>%
  summarize(
    modal_cluster = calc_mode(cluster),
    lat = first(Latitude),
    long = first(Longitude),
    .groups = "drop"
  ) %>%
  mutate(modal_cluster = factor(modal_cluster))

# For FNYield: merge and compute modal cluster per stream_id
df_FNYield <- drivers_df_filtered %>%
  inner_join(FNYield_clusters, by = c("Stream_ID")) %>% 
  group_by(Stream_ID) %>%
  summarize(
    modal_cluster = calc_mode(cluster),
    lat = first(Latitude),
    long = first(Longitude),
    .groups = "drop"
  ) %>%
  mutate(modal_cluster = factor(modal_cluster))


# 1. Get the world data
world_data <- map_data("world")

# 2. Create a base world map with your desired styling
world_map_base <- ggplot() +
  geom_polygon(
    data = world_data,
    aes(x = long, y = lat, group = group),
    fill = "lightgray",
    color = "white"
  ) +
  coord_sf(
    crs = st_crs(3857), 
    expand = FALSE
  ) +
  theme_minimal() +
  labs(fill = NULL) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.05, 0.65),
    legend.justification = c("left", "top")
  )

# 3. Prepare your cluster data (assuming these objects already exist)
# For FNConc:
df_FNConc <- drivers_df_filtered %>%
  inner_join(FNConc_clusters, by = c("Stream_ID")) %>% 
  group_by(Stream_ID) %>%
  summarize(
    modal_cluster = calc_mode(cluster),
    lat = first(Latitude),
    long = first(Longitude),
    .groups = "drop"
  ) %>%
  mutate(modal_cluster = factor(modal_cluster))

# For FNYield:
df_FNYield <- drivers_df_filtered %>%
  inner_join(FNYield_clusters, by = c("Stream_ID")) %>% 
  group_by(Stream_ID) %>%
  summarize(
    modal_cluster = calc_mode(cluster),
    lat = first(Latitude),
    long = first(Longitude),
    .groups = "drop"
  ) %>%
  mutate(modal_cluster = factor(modal_cluster))

# Define discrete color palettes for clusters
my_conc_cluster_colors <- c(
  "1" = "#88A2DC",  # Muted Blue (instead of bold primary blue)
  "2" = "#E69F00",  # Warm Muted Orange
  "3" = "#6BAE75"
)

my_yield_cluster_colors <- c(
  "1" = "#AC7B32",  # Rich Ochre (Warm Earthy Brown-Gold)
  "2" = "#579C8E",  # Muted Teal (Cool & fresh)
  "3" = "#C26F86",  # Dusty Rose (Soft but warm)
  "4" = "#5E88B0"   # Soft Steel Blue (Cool contrast)
)
p_FNConc_map <- world_map_base +
  geom_point(
    data = df_FNConc,
    aes(x = long, y = lat, fill = modal_cluster),
    shape = 21,
    size = 2,
    alpha = 0.8,
    stroke = 0.1,
    color = "gray2"
  ) +
  scale_fill_manual(
    values = my_conc_cluster_colors,
    guide = guide_legend(override.aes = list(size = 5))
  ) +
  labs(title = NULL,
       x = "Longitude", y = "Latitude",
       tag = "A") +
  theme(
    plot.tag = element_text(size = 16, hjust = 0, vjust = 1),
    legend.position = c(0.05, 0.65),
    legend.justification = c("left", "top"),
    legend.title = element_blank()
  )

# 5. Create Panel B: Global map for FNYield modal clusters
p_FNYield_map <- world_map_base +
  geom_point(
    data = df_FNYield,
    aes(x = long, y = lat, fill = modal_cluster),
    shape = 21,
    size = 2,
    alpha = 0.8,
    stroke = 0.1,
    color = "gray2"
  ) +
  scale_fill_manual(
    values = my_yield_cluster_colors,
    guide = guide_legend(override.aes = list(size = 5))
  ) +
  labs(title = NULL,
       x = "Longitude", y = "Latitude",
       tag = "B") +
  theme(
    plot.tag = element_text(size = 16, hjust = 0, vjust = 1),
    legend.position = c(0.05, 0.65),
    legend.justification = c("left", "top"),
    legend.title = element_blank()
  )

# 6. Create Regional Inlay Maps with a helper function
create_regional_map <- function(xlim, ylim, data, fill_colors) {
  ggplot() +
    geom_polygon(
      data = world_data,
      aes(x = long, y = lat, group = group),
      fill = "lightgray", color = "white"
    ) +
    geom_point(
      data = data,
      aes(x = long, y = lat, fill = modal_cluster),
      shape = 21, size = 2, alpha = 0.8, stroke = 0.1, color = "gray2"
    ) +
    scale_fill_manual(values = fill_colors) +
    coord_sf(
      xlim = xlim,
      ylim = ylim,
      crs = st_crs(3857),
      expand = FALSE
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
}

# Define bounding boxes for the inlay maps
uk_xlim <- c(-10, 2)
uk_ylim <- c(49, 61)
scandinavia_xlim <- c(4, 30)
scandinavia_ylim <- c(50, 72)
australia_xlim <- c(135, 155)
australia_ylim <- c(-40, -28)

# 7. Create inlay maps for each panel
# For FNConc:
uk_inlay_FNConc <- create_regional_map(uk_xlim, uk_ylim, df_FNConc, my_conc_cluster_colors)
scandinavia_inlay_FNConc <- create_regional_map(scandinavia_xlim, scandinavia_ylim, df_FNConc, my_conc_cluster_colors)
australia_inlay_FNConc <- create_regional_map(australia_xlim, australia_ylim, df_FNConc, my_conc_cluster_colors)

# For FNYield:
uk_inlay_FNYield <- create_regional_map(uk_xlim, uk_ylim, df_FNYield, my_yield_cluster_colors)
scandinavia_inlay_FNYield <- create_regional_map(scandinavia_xlim, scandinavia_ylim, df_FNYield, my_yield_cluster_colors)
australia_inlay_FNYield <- create_regional_map(australia_xlim, australia_ylim, df_FNYield, my_yield_cluster_colors)

# 8. Overlay the Regional Inlay Maps onto the Global Maps using cowplot
final_FNConc_map <- ggdraw(p_FNConc_map) +
  draw_plot(uk_inlay_FNConc, x = 0.35, y = 0.44, width = 0.2, height = 0.2) +
  draw_plot(scandinavia_inlay_FNConc, x = 0.55, y = 0.45, width = 0.2, height = 0.2) +
  draw_plot(australia_inlay_FNConc, x = 0.6, y = 0.25, width = 0.2, height = 0.2) +
  # Optional connecting lines
  draw_line(x = c(0.42, 0.48), y = c(0.63, 0.66), color = "black", linewidth = 1) +
  draw_line(x = c(0.56, 0.65), y = c(0.68, 0.64), color = "black", linewidth = 1) +
  draw_line(x = c(0.7899, 0.86), y = c(0.32, 0.375), color = "black", linewidth = 1)

final_FNYield_map <- ggdraw(p_FNYield_map) +
  draw_plot(uk_inlay_FNYield, x = 0.35, y = 0.44, width = 0.25, height = 0.25) +
  draw_plot(scandinavia_inlay_FNYield, x = 0.55, y = 0.45, width = 0.25, height = 0.25) +
  draw_plot(australia_inlay_FNYield, x = 0.6, y = 0.25, width = 0.25, height = 0.25) +
  # Optional connecting lines
  draw_line(x = c(0.42, 0.48), y = c(0.63, 0.66), color = "black", linewidth = 1) +
  draw_line(x = c(0.56, 0.65), y = c(0.68, 0.64), color = "black", linewidth = 1) +
  draw_line(x = c(0.7899, 0.86), y = c(0.32, 0.375), color = "black", linewidth = 1)

# 9. Combine the two panels vertically
combined_map <- ggarrange(final_FNConc_map, final_FNYield_map, 
                          ncol = 1, nrow = 2,
                          heights = c(1, 1))

# Display the combined map
print(combined_map)

# Save the final output
ggsave("global_map_clusters_with_inlays.png", 
       combined_map, width = 8, height = 8.5, dpi = 300)
