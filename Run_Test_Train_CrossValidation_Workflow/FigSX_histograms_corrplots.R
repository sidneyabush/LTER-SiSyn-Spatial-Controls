# =============================================================================
# Prepare histogram input from split subsets
# =============================================================================
rm(list = ls())
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")

# Load libraries
librarian::shelf(dplyr, readr, tidyr, stringr)

# Read split datasets
older70     <- read_csv("AllDrivers_cc_older70.csv", show_col_types = FALSE)     %>% mutate(subset = "older70")
recent30    <- read_csv("AllDrivers_cc_recent30.csv", show_col_types = FALSE)    %>% mutate(subset = "recent30")
unseen10_df <- read_csv("AllDrivers_cc_unseen10.csv", show_col_types = FALSE)    %>% mutate(subset = "unseen10")

# Combine
hist_input_df <- bind_rows(older70, recent30, unseen10_df)

# Pivot longer for plotting
hist_long <- hist_input_df %>%
  pivot_longer(cols = c(NOx, P, npp, evapotrans, greenup_day, precip, temp,
                        snow_cover, permafrost, elevation, basin_slope,
                        RBI, recession_slope,
                        starts_with("land_"), starts_with("rocks_")),
               names_to = "driver", values_to = "value") %>%
  mutate(value = if_else(driver %in% c("NOx", "P"), log10(value), value))


# Create the plot object
p <- ggplot(hist_long, aes(x = value, fill = subset)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~ driver, scales = "free", ncol = 4) +
  scale_fill_manual(values = c(
    older70   = "#E41A1C",
    recent30  = "#377EB8",
    unseen10  = "#4DAF4A"
  )) +
  labs(x = "Value", y = "Count", fill = "Subset") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Save to file
ggsave("Final_Figures/overlapping_histograms_by_subset.png", plot = p,
       width = 16, height = 12, dpi = 300)
