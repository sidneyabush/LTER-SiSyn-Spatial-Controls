# ================================================================
# GLOBAL MAP WITH TRANSLUCENT POINTS, INSETS (WHITE BACKGROUNDS), NO LEGEND TITLE
# ================================================================

# 1) Load required packages
library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(sf)
library(cowplot)

# 2) Set working directory (adjust path as needed)
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# 3) Read & prepare driver/location data
record_length <- 5

drivers_df_uncleaned <- read.csv(
  sprintf("harmonization_files/AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length)
) %>%
  distinct(Stream_ID, .keep_all = TRUE)

drivers_df_final_sites <- read.csv(
  sprintf("harmonization_files/All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv", record_length)
) %>%
  distinct(Stream_ID, .keep_all = TRUE)

drivers_df_filtered <- drivers_df_final_sites %>%
  left_join(drivers_df_uncleaned, by = "Stream_ID") %>%
  select(-ends_with(".y"), -contains("Gen"), -contains("Flux")) %>%
  select(-Year.x, -X, -DecYear, -num_days, -drainSqKm, -chemical,
         -Stream_Name, -LTER, -contains("Coord"), -cycle0, -prop_area,
         -contains("elevation_"), -contains("basin_slope_"), -contains("major"),
         -contains("permafrost_"), -Use_WRTDS) %>%
  rename_with(~ str_remove(., "\\.x$"), ends_with(".x"))

drivers_sf <- st_as_sf(
  drivers_df_filtered,
  coords = c("Longitude", "Latitude"),
  crs = 4326
) %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()

# 4) Load clustering results (full_scaled contains Stream_ID + final_cluster)
load("Final_Models/FNConc_HierClust_Workflow_Objects.RData")
# → `full_scaled` has columns: Stream_ID, final_cluster, etc.

# 5) Merge drivers with final_cluster
sites_with_clusters <- drivers_sf %>%
  inner_join(
    full_scaled %>% select(Stream_ID, final_cluster),
    by = "Stream_ID"
  )

# 6) Define color palette for lithology clusters
my_cluster_colors <- c(
  "Volcanic"            = "#AC7B32",
  "Sedimentary"         = "#579C8E",
  "Mixed Sedimentary"   = "#89C8A0",
  "Plutonic"            = "#8D9A40",
  "Metamorphic"         = "#C26F86",
  "Carbonate Evaporite" = "#5E88B0"
)

# 7) Get world polygon data
world_data <- map_data("world")

# 8) Base global map (light gray land, white oceans)
global_base <- ggplot() +
  geom_polygon(
    data = world_data,
    aes(x = long, y = lat, group = group),
    fill  = "lightgray",
    color = "white"
  ) +
  coord_sf(crs = st_crs(3857), expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background    = element_rect(fill = "white", color = NA),
    plot.background     = element_rect(fill = "white", color = NA),
    panel.grid          = element_blank(),
    axis.title          = element_blank(),
    axis.text           = element_blank(),
    axis.ticks          = element_blank(),
    legend.position     = c(0.05, 0.65),
    legend.justification = c("left", "top"),
    legend.text         = element_text(size = 10),
    legend.title        = element_blank()  # remove legend title
  )

# 9) Plot all sites on global map with translucent points (alpha = 0.4)
global_map <- global_base +
  geom_point(
    data  = sites_with_clusters,
    aes(x = Longitude, y = Latitude, fill = final_cluster),
    shape = 21,
    size  = 2,
    alpha = 0.5,      # make points translucent
    stroke = 0.1,
    color  = "gray20"
  ) +
  scale_fill_manual(
    values = my_cluster_colors,
    name   = NULL,   # explicitly remove legend title
    guide  = guide_legend(override.aes = list(size = 4, alpha = 1))
  )

# 10) Helper function to create a regional inset (white background & translucent points)
create_regional_map <- function(xlim, ylim, data_df) {
  ggplot() +
    geom_polygon(
      data = world_data,
      aes(x = long, y = lat, group = group),
      fill  = "lightgray",
      color = "white"
    ) +
    geom_point(
      data  = data_df,
      aes(x = Longitude, y = Latitude, fill = final_cluster),
      shape = 21,
      size  = 2,
      alpha = 0.4,      # translucent points in inset as well
      stroke = 0.1,
      color  = "gray20"
    ) +
    scale_fill_manual(values = my_cluster_colors, guide = "none") +
    coord_sf(
      xlim   = xlim,
      ylim   = ylim,
      crs    = st_crs(3857),
      expand = FALSE
    ) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.border     = element_rect(color = "black", fill = NA, size = 0.5)
    )
}

# 11) Define bounding boxes for the three insets
uk_xlim          <- c(-10,   2)
uk_ylim          <- c(49,   61)
scandinavia_xlim <- c(4,   30)
scandinavia_ylim <- c(50,  72)
australia_xlim   <- c(135, 155)
australia_ylim   <- c(-40, -28)

# 12) Subset points for each inset
df_uk          <- sites_with_clusters %>%
  filter(Longitude >= uk_xlim[1], Longitude <= uk_xlim[2],
         Latitude  >= uk_ylim[1],  Latitude  <= uk_ylim[2])
df_scandinavia <- sites_with_clusters %>%
  filter(Longitude >= scandinavia_xlim[1], Longitude <= scandinavia_xlim[2],
         Latitude  >= scandinavia_ylim[1],  Latitude  <= scandinavia_ylim[2])
df_australia   <- sites_with_clusters %>%
  filter(Longitude >= australia_xlim[1], Longitude <= australia_xlim[2],
         Latitude  >= australia_ylim[1],  Latitude  <= australia_ylim[2])

# 13) Create inset maps (white backgrounds, translucent points, no legends)
uk_inset          <- create_regional_map(uk_xlim, uk_ylim, df_uk)
scandinavia_inset <- create_regional_map(scandinavia_xlim, scandinavia_ylim, df_scandinavia)
australia_inset   <- create_regional_map(australia_xlim, australia_ylim, df_australia)

# 14) Overlay insets onto the global map
global_with_insets <- ggdraw(global_map) +
  draw_plot(uk_inset,          x = 0.33, y = 0.45, width = 0.25, height = 0.25) +
  draw_plot(scandinavia_inset, x = 0.52, y = 0.50, width = 0.25, height = 0.25) +
  draw_plot(australia_inset,   x = 0.57, y = 0.12, width = 0.25, height = 0.25) +
  # Lines connecting insets to approximate bounding regions
  draw_line(x = c(0.415, 0.480), y = c(0.675, 0.730), color = "black", size = 0.7) +  # UK
  draw_line(x = c(0.55,  0.587), y = c(0.76,  0.70),  color = "black", size = 0.7) +  # Scandinavia
  draw_line(x = c(0.788, 0.850), y = c(0.25,  0.31),  color = "black", size = 0.7)    # Australia

# 15) Display and save
print(global_with_insets)

ggsave(
  filename = "Final_Figures/Fig6_global_map_lith.png",
  plot     = global_with_insets,
  width    = 8,
  height   = 5,
  dpi      = 300
)
