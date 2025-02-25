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

# --------------------------------------------------
# 1) Data preparation (same as your script)
# --------------------------------------------------
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
rm(list = ls())

record_length <- 5

drivers_df_uncleaned <- read.csv(sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length)) %>%
  distinct(Stream_ID, .keep_all = TRUE)

drivers_df_final_sites <- read.csv(sprintf("All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv", record_length)) %>%
  distinct(Stream_ID, .keep_all = TRUE)

drivers_df_filtered <- drivers_df_final_sites %>%
  left_join(drivers_df_uncleaned, by = "Stream_ID") %>%
  select(-ends_with(".y"), -contains("Gen"), -contains("Flux")) %>%
  select(-Year.x, -X, -DecYear, -num_days, -drainSqKm, -chemical, 
         -Stream_Name, -LTER, -contains("Coord"), -cycle0, -prop_area, 
         -contains("elevation_"), -contains("basin_slope_"), -contains("major"),
         -contains("permafrost_"), -Use_WRTDS) %>%
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
  "Tropical"           = "#009E73",
  "Humid Subtropical"  = "#F0E442",
  "Humid Temperate"    = "#56B4E9",
  "Humid Continental"  = "#0072B2",
  "Mediterranean"      = "#E69F00",
  "Semi-Arid"          = "#D55E00",
  "Arid"               = "#CC79A7",
  "Subarctic"          = "#882255"
)

cbPalette_lighter <- lighten(cbPalette_named, amount = 0.3)
cbPalette_lighter2 <- lighten(cbPalette_named, amount = 0.4)

# --------------------------------------------------
# 2) MAP (Panel A)
# --------------------------------------------------
world <- map_data("world")

p <- ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "lightgray",
    color = "white"
  ) +
  geom_point(
    data = drivers_df_filtered,
    aes(x = Longitude, y = Latitude, fill = Name),
    shape = 21,
    size = 2,
    stroke = 0.1,
    color = "gray2"
  ) +
  scale_fill_manual(
    values = cbPalette_lighter,
    guide = guide_legend(override.aes = list(size = 5))  # Increase legend point size
  ) +
  # 2a) Use coord_sf with bounding box to reduce empty space
  coord_sf(
    crs = st_crs(3857), 
    expand = FALSE
  ) +
  # 2b) Add scale and north arrow
  annotation_scale(
    location = "bl",  
    width_hint = 0.5,  
    unit_category = "metric",
    plot_unit = "km",
    pad_x = unit(11, "cm"),  
    pad_y = unit(0.1, "cm") 
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(0.7, "cm"),
    width = unit(0.5, "cm"),
    pad_x = unit(0.1, "cm"),
    pad_y = unit(0.1, "cm"),
    style = north_arrow_orienteering()
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
    legend.justification = c("left", "top"),
    plot.margin = margin(0, 0, 0, 0)
  )

# Label as Panel A
p_labeled <- p + 
  labs(tag = "A") +
  theme(
    plot.tag = element_text(size = 16, hjust = 0, vjust = 1)
  )

print(p_labeled)

# # Define the bounding box for the inset map (UK/Europe region)
# inset_xlim <- c(-15, 30)  # Longitude range for Europe
# inset_ylim <- c(35, 70)   # Latitude range for Europe
# 
# # Create an inset map (zoom into UK/Europe)
# inset_map <- ggplot() +
#   geom_polygon(
#     data = world,
#     aes(x = long, y = lat, group = group),
#     fill = "gray90", color = "white"
#   ) +
#   geom_point(
#     data = drivers_df_filtered %>% filter(Longitude > inset_xlim[1] & Longitude < inset_xlim[2],
#                                           Latitude > inset_ylim[1] & Latitude < inset_ylim[2]),
#     aes(x = Longitude, y = Latitude, fill = Name),
#     shape = 21, size = 2, stroke = 0.2, color = "black"
#   ) +
#   scale_fill_manual(values = cbPalette_lighter) +
#   coord_sf(
#     xlim = inset_xlim,
#     ylim = inset_ylim,
#     expand = FALSE
#   ) +
#   theme(legend.position = "none")+
#   theme_void()  # Remove all background elements for a clean look
# 
# # Add the inset map as an annotation to the main map
# p_labeled_inset <- p_labeled +
#   annotation_custom(
#     grob = ggplotGrob(inset_map),
#     xmin = 50, xmax = -40,  # Positioning in the blank space
#     ymin = -10, ymax = 50
#   )
# 
# print(p_labeled_inset)


# --------------------------------------------------
# 3) Boxplots (Panel B)
# --------------------------------------------------
p1 <- ggplot(drivers_df_filtered, aes(x = FNConc, y = Name)) +
  geom_boxplot(aes(fill = Name), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(color = Name), width = 0.1, size = 2) +
  scale_fill_manual(values = cbPalette_lighter2) +
  scale_color_manual(values = cbPalette_lighter) +
  labs(x = expression("Concentration (mg L"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(drivers_df_filtered$Name))) +
  theme(legend.position = "none")

p2 <- ggplot(drivers_df_filtered, aes(x = FNYield, y = Name)) +
  geom_boxplot(aes(fill = Name), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(color = Name), width = 0.1, size = 2) +
  scale_fill_manual(values = cbPalette_lighter2) +
  scale_color_manual(values = cbPalette_lighter) +
  labs(x = expression("Yield (kg km"^-2*" year"^-1*")"), y = NULL) +
  theme_classic(base_size = 14) +
  scale_y_discrete(limits = rev(levels(drivers_df_filtered$Name))) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank()
  )

# Label only the concentration plot (p1) as "B"
p1_labeled <- p1 + 
  labs(tag = "B") +
  theme(
    plot.tag = element_text(size = 16, hjust = 0, vjust = 1)
  )

p2_labeled <- p2 + 
  labs(tag = "C") +
  theme(
    plot.tag = element_text(size = 16, hjust = 0, vjust = 1)
  )

# No label for p2
final_boxplots <- p1_labeled + p2_labeled

# Add a left margin to align with map
final_boxplots_labeled <- final_boxplots +
  theme(
    # Increase left margin to ensure the y-axis text aligns with the map
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
  )

# --------------------------------------------------
# 4) Combine vertically with map bigger
# --------------------------------------------------
# Adjust margins for the map (Panel A) and the boxplots (Panel B) so they line up.
p_labeled_aligned <- p_labeled +
  theme(plot.margin = margin(t = 10, r = 5, b = 0, l = 5))  # adjust as needed

final_boxplots_labeled_aligned <- final_boxplots_labeled +
  theme(plot.margin = margin(t = 0, r = 5, b = 5, l = 5))  # same left margin

# Now, use ggarrange to stack them vertically
combined_figure <- ggarrange(
  p_labeled_aligned,
  final_boxplots_labeled_aligned,
  ncol = 1, nrow = 2,
  heights = c(3, 2.5),  # Keeps map larger, but reduces space
  align = "v",        # Ensure vertical alignment
  common.legend = FALSE, # Keeps legend only where needed
  labels = NULL,      # Avoids extra label space
  vjust = -2          # Move them closer together
)

print(combined_figure)

ggsave("combined_map_boxplots_vertical_aligned_ggarrange.png", 
       combined_figure, width = 8, height = 8.5, dpi = 300)


# --------------------------------------------------
# 5) Plot Precip vs Temp -----
# --------------------------------------------------
df_lter <- drivers_df_filtered %>%
  mutate(LTER_extracted = str_extract(Stream_ID, "^[^_]+")) %>%
  mutate(LTER_extracted = case_when(
    LTER_extracted == "Swedish Goverment" ~ "Swedish Government",
    TRUE ~ LTER_extracted
  )) %>%
  group_by(LTER_extracted, Name) %>%  # Grouping by both LTER and Climate Class
  summarise(
    Temperature   = mean(temp, na.rm = TRUE),     
    Precipitation = mean(precip, na.rm = TRUE),   
    .groups = "drop"  # Prevents ungrouping warnings
  ) %>%
  mutate(Name = factor(Name, levels = c(
    "Tropical", "Humid Subtropical", "Humid Temperate", 
    "Humid Continental", "Mediterranean", "Semi-Arid", 
    "Arid", "Subarctic"
  )))  # Ensuring all categories exist in the factor levels


df_lter$Name <- factor(
  df_lter$Name,
  levels = c(
    "Tropical", "Humid Subtropical", "Humid Temperate", 
    "Humid Continental", "Mediterranean", "Semi-Arid", 
    "Arid", "Subarctic"
  )
)


# Create Plot C: Precipitation vs. Temperature, one point per LTER
p_temp_precip <- ggplot(df_lter, aes(x = Temperature, y = Precipitation,
                                     fill = Name, label = LTER_extracted)) +
  # Add dashed reference lines at Temperature = 0 and Precipitation = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray2", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray2", linewidth = 0.6) +
  geom_point(
    shape = 21,
    size = 5,
    stroke = 0.1,
    color = "gray2"  # outlines for the points
  ) +
  geom_text_repel(size = 5, max.overlaps = Inf, point.padding = unit(1, "lines")) +
  scale_fill_manual(values = cbPalette_lighter) +  # using your original palette
  labs(
    x = expression("Temperature ("*degree*C*")"),
    y = expression("Precipitation (mm day"^-1*")"),
    color = NULL
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = c(0.9, 0.55),
    legend.title =element_blank()
  )

# Print the figure
print(p_temp_precip)

# Save the figure as a high-resolution PNG file
ggsave("precip_temp_plot.png", p_temp_precip, width = 10, height = 9.5, dpi = 300)
 