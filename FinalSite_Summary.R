# Creating summary stats and maps for Spatial Controls MS
# Read in and tidy data ----
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn") 

# Load needed libraries
librarian::shelf(dplyr, stringr, ggplot2, maps)

# Clear environment
rm(list = ls())

record_length <- 5

# Read in and ensure one row per Stream_ID for the uncleaned dataset
drivers_df_uncleaned <- read.csv(sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length)) %>%
  distinct(Stream_ID, .keep_all = TRUE)

# Read in and ensure one row per Stream_ID for the final_sites dataset
drivers_df_final_sites <- read.csv(sprintf("All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv", record_length)) %>%
  distinct(Stream_ID, .keep_all = TRUE)

drivers_df_filtered <- drivers_df_final_sites %>%
  left_join(drivers_df_uncleaned, by = "Stream_ID") %>%
  # Remove duplicate columns from the right-hand data frame
  dplyr::select(-ends_with(".y"), -contains("Gen"), -contains("Flux")) %>%
  dplyr::select(-Year.x, -X, -DecYear, -num_days, -drainSqKm, -chemical, 
                -Stream_Name, -LTER, -contains("Coord"), -cycle0, -prop_area, 
                -contains("elevation_"), -contains("basin_slope_"), -contains("major"),
                -contains("permafrost_"), -Use_WRTDS) %>%
  # Rename left-hand duplicate columns by removing the ".x" suffix
  rename_with(~ str_remove(., "\\.x$"), ends_with(".x")) 

# Create a data frame listing the min and max of each numeric column
ranges_df <- data.frame(
  variable = names(drivers_df_filtered),
  min      = sapply(drivers_df_filtered, min, na.rm = TRUE),
  max      = sapply(drivers_df_filtered, max, na.rm = TRUE)
)

# Print the table of ranges
print(ranges_df)


# Example colorblind-friendly palette (Okabe-Ito):
# You can reorder or remove colors as needed for your data.
cbPalette <- c(
  "#E69F00", # Orange
  "#56B4E9", # Sky blue
  "#009E73", # Bluish green
  "#F0E442", # Yellow
  "#0072B2", # Blue
  "#D55E00", # Vermillion
  "#CC79A7", # Reddish purple
  "#B15928"  # Gray
)

# Get world map data
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
    aes(x = Longitude, y = Latitude, color = Name),
    size = 2
  ) +
  scale_color_manual(values = cbPalette) +
  theme_minimal() +
  # Remove x and y labels in labs (or set them to NULL)
  labs(
    title = NULL,
    color = "Name"
  ) +
  # Remove axis titles, axis text, axis ticks, and grid lines
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 12), 
    panel.grid = element_blank(),
    # Set the plot background to white
    plot.background = element_rect(fill = "white", color = NA),
    # Manually adjust legend position
    legend.position   = c(0.08, 0.6),    
    legend.justification = c("left", "top")
  )
print(p)

# Export the plot as a high-resolution PNG:
ggsave("global_map_high_quality.png", plot = p, width = 10, height = 5, dpi = 300)

# Alternatively, export as a vector PDF for infinite scalability:
ggsave("global_map_high_quality.pdf", plot = p, width = 10, height = 5)
