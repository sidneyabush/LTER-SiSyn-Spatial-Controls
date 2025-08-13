# =============================================================================
# Prepare histogram input from split subsets
# =============================================================================
rm(list = ls())
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")

# Load libraries
librarian::shelf(dplyr, readr, tidyr, stringr, ggplot2)

theme_set(
  theme_bw(base_size = 20) +
    theme(
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank()
    )
)

# Read split datasets
older70     <- read_csv("AllDrivers_cc_older70.csv", show_col_types = FALSE)     %>% mutate(subset = "Training")
recent30    <- read_csv("AllDrivers_cc_recent30.csv", show_col_types = FALSE)    %>% mutate(subset = "Testing")
unseen10_df <- read_csv("AllDrivers_cc_unseen10.csv", show_col_types = FALSE)    %>% mutate(subset = "Validation")

# Combine and set order
hist_input_df <- bind_rows(older70, recent30, unseen10_df) %>%
  mutate(subset = factor(subset, levels = c("Training", "Testing", "Validation")))

# Pivot longer for plotting
hist_long <- hist_input_df %>%
  pivot_longer(cols = c(NOx, P, npp, evapotrans, greenup_day, precip, temp,
                        snow_cover, permafrost, elevation, basin_slope,
                        RBI, recession_slope,
                        starts_with("land_"), starts_with("rocks_")),
               names_to = "driver", values_to = "value") %>%
  mutate(value = if_else(driver %in% c("NOx", "P"), log10(value), value))

# 5) Updated recode map
recode_map <- setNames(
  # pretty labels
  c(
    "Log(N)", "Log(P)", "NPP", "ET", "Green-up day", "Precip", "Temp",
    "Snow cover", "Permafrost probability", "Elevation", "Basin slope",
    "RBI", "RCS",
    "Bare land cover", "Cropland cover", "Forest cover", "Grass & shrub cover",
    "Ice & snow cover", "Impervious cover", "Saltwater cover",
    "Tidal wetland cover", "Open-water cover", "Wetland cover",
    "Volcanic rock", "Sedimentary rock", "Carbonate-evaporite rock",
    "Metamorphic rock", "Plutonic rock"
  ),
  # original variable names
  c(
    "NOx", "P", "npp", "evapotrans", "greenup_day", "precip", "temp",
    "snow_cover", "permafrost", "elevation", "basin_slope",
    "RBI", "recession_slope",
    "land_Bare", "land_Cropland", "land_Forest", "land_Grassland_Shrubland",
    "land_Ice_Snow", "land_Impervious", "land_Salt_Water",
    "land_Tidal_Wetland", "land_Water", "land_Wetland_Marsh",
    "rocks_volcanic", "rocks_sedimentary", "rocks_carbonate_evaporite",
    "rocks_metamorphic", "rocks_plutonic"
  )
)

# 6) Driver ordering for facets
driver_order <- unname(recode_map)


hist_long <- hist_long %>%
  mutate(
    driver = recode(driver, !!!recode_map),
    driver = factor(driver, levels = driver_order)
  )

# Create the plot object
p <- ggplot(hist_long, aes(x = value, fill = subset)) +
  geom_histogram(position = "identity", alpha = 0.8, bins = 30) +
  facet_wrap(~ driver, scales = "free", ncol = 4) +
  scale_fill_manual(
    values = c(
      "Training"            = "gray70",  
      "Testing"             = "#b9d7ef", 
      "Validation"    = "#525693"
    ),
    guide = guide_legend(override.aes = list(alpha = 1))
  ) +
  labs(x = "Value", y = "Count", fill = NULL) +
  theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(1.5, "lines"),   
      legend.text = element_text(size = 18),  
      legend.title = element_blank(),
      strip.background = element_blank())

# Save to file
ggsave(
  filename = "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Figures/FigS2_histograms.png",
  plot     = p,
  width    = 16,
  height   = 18,
  dpi      = 300
)

# =============================================================================
# Save correlation plots for each subset (using driver order & recode map)
# =============================================================================

library(corrplot)

# Define variable subset to match the histogram drivers
drivers_to_use <- names(recode_map)

save_subset_corrplot <- function(df, label) {
  # Filter & transform to numeric matrix
  numeric_df <- df %>%
    select(all_of(drivers_to_use)) %>%
    mutate(across(c(NOx, P), log10)) %>%
    rename_with(~ recode_map[.x]) %>%
    select(all_of(driver_order))
  
  cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs")
  
  # sanitize label for filename
  label_file <- tolower(gsub("[- ]", "_", label))
  
  png(
    filename = file.path(
      "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/Final_Figures",
      sprintf("FigS3_corrplot_%s.png", label_file)
    ),
    width  = 10, 
    height = 10, 
    units = "in", 
    res = 300
  )
  par(mar = c(1, 1, 1, 1))  # tightened margins
  
  corrplot(
    cor_matrix,
    type         = "lower",
    pch.col      = "black",
    tl.col       = "black",
    tl.cex       = 1.0,
    diag         = FALSE,
    na.label     = "X",
    na.label.col = "grey70",
    addgrid.col  = "grey90"
  )
  
  dev.off()
}


# Call for each subset
# save_subset_corrplot(older70,     "Training")
save_subset_corrplot(recent30,    "Testing")
# save_subset_corrplot(unseen10_df, "Cross_Validation")
