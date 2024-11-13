# Calculating Weathering Rates: ANNUAL

# Clear environment
rm(list = ls())

# Required libraries
library(dplyr)
library(tidyr)

# Read in and tidy data ----
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

drivers_df <- read.csv("AllDrivers_Harmonized_20241112_WRTDS_MD_KG_NP_GenConc_Annual.csv") %>%
  #distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  dplyr::rename(drainage_area = drainSqKm,
                # snow_cover = prop_area,
                green_up_day = greenup_day,
                max_daylength = Max_Daylength)

# Load and merge mapped lithologies
mapped_lithologies <- read.csv("mapped_lithologies.csv")
drivers_df <- merge(drivers_df, mapped_lithologies, by = "major_rock", all.x = TRUE)

# Filter out rows with NA in `major_rock`
drivers_df <- drivers_df %>%
  filter(!is.na(mapped_lithology))

# Constants for weathering calculations
seconds_per_year <- 31536000  
kg_per_m3 <- 1000  
km2_to_m2 <- 10^6  

# Calculate annual runoff per site and year
drivers_df <- drivers_df %>%
  group_by(Stream_ID, Year) %>%
  mutate(runoff = (med_q * seconds_per_year * kg_per_m3) / (drainage_area * km2_to_m2)) %>%
  ungroup()

# Convert `temp` to Kelvin for each row
drivers_df <- drivers_df %>%
  mutate(temp_K = temp + 273.15)  # Add new column `temp_K`

# Constants for the weathering function
R <- 8.314  

# Define lithology parameters
lithology_params <- data.frame(
  mapped_lithology = c("su", "vb", "pb", "py", "va", "vi", "ss", "pi", "sm", "mt", "pa"),
  b = c(0.003364, 0.007015, 0.007015, 0.0061, 0.002455, 0.007015, 0.005341, 0.007015, 0.012481, 0.007626, 0.005095),
  sp = c(1, 1, 1, 1, 1, 1, 0.64, 0.58, 0.24, 0.25, 0.58),
  sa = c(60, 50, 50, 46, 60, 50, 60, 60, 60, 60, 60)
)

# Update function to calculate weathering for each year
calculate_weathering <- function(q, lithologies, temp_kelvin) {
  
  lithologies_list <- strsplit(lithologies, ",\\s*")[[1]]  
  
  weathering_values <- numeric(length(lithologies_list))
  
  for (i in seq_along(lithologies_list)) {
    lith <- lithologies_list[i]
    
    params <- lithology_params %>%
      filter(mapped_lithology == lith)
    
    if (nrow(params) == 0) stop(paste("Lithology not found for", lith))
    
    b <- params$b
    sp <- params$sp
    sa <- params$sa
    
    weathering_values[i] <- q * b * (sp * exp(((1000 * sa) / R) * ((1 / 284.2) - (1 / temp_kelvin))))
  }
  
  return(mean(weathering_values))
}

# Apply the weathering calculation per site per year
drivers_df <- drivers_df %>%
  group_by(Stream_ID, Year) %>%
  mutate(silicate_weathering = mapply(calculate_weathering, runoff, mapped_lithology, temp_K)) %>%
  ungroup()

# Export the dataframe to a CSV file
write.csv(drivers_df, "AllDrivers_Harmonized_20241112_WRTDS_MD_KG_NP_GenConc_silicate_weathering_Annual.csv", row.names = FALSE)
