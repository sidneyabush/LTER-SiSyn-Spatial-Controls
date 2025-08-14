# Clear environment
rm(list = ls())

# Required libraries
library(dplyr)

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Read in and tidy data
drivers_df <- read.csv("AllDrivers_Harmonized_Annual_with_KG_DA_rawNP_Conc_20241106.csv") %>%
  # Retain distinct Stream_ID and Year
  distinct(Stream_ID, Year, .keep_all = TRUE)
  # Rename specific columns
  # dplyr::rename(
  #   drainage_area = drainSqKm,
  #   snow_cover = prop_area,
  #   green_up_day = cycle0,
  #   max_daylength = Max_Daylength

# Export unique major rock types to confirm mappings
unique_major_rock <- unique(drivers_df$major_rock)
write.csv(unique_major_rock, "unique_major_rock.csv", row.names = FALSE)

# Import mapped lithologies and merge with drivers_df
mapped_lithologies <- read.csv("mapped_lithologies.csv")
drivers_df <- merge(drivers_df, mapped_lithologies, by = "major_rock", all.x = TRUE) %>%
  filter(!is.na(mapped_lithology))

# Constants for weathering calculations
seconds_per_year <- 31536000  # Number of seconds in a year
kg_per_m3 <- 1000  # Density of water in kg/m^3
km2_to_m2 <- 10^6  # Conversion factor from km^2 to m^2

# Convert temp from Celsius to Kelvin and calculate runoff annually
drivers_df <- drivers_df %>%
  mutate(
    runoff = (med_q * seconds_per_year * kg_per_m3) / (drainage_area * km2_to_m2),
    temp_K = temp + 273.15  # Add new column 'temp_K'
  )

# Constants for gas and lithology parameters
R <- 8.314 # gas constant in J/(mol*K)

lithology_params <- data.frame(
  mapped_lithology = c("su", "vb", "pb", "py", "va", "vi", "ss", "pi", "sm", "mt", "pa"),
  b = c(0.003364, 0.007015, 0.007015, 0.0061, 0.002455, 0.007015, 0.005341, 0.007015, 0.012481, 0.007626, 0.005095),
  sp = c(1, 1, 1, 1, 1, 1, 0.64, 0.58, 0.24, 0.25, 0.58),
  sa = c(60, 50, 50, 46, 60, 50, 60, 60, 60, 60, 60)
)

# Weathering calculation function for multiple lithologies per year
calculate_weathering <- function(q, lithologies, temp_kelvin) {
  
  lithologies_list <- strsplit(lithologies, ",\\s*")[[1]]
  
  weathering_values <- numeric(length(lithologies_list))
  
  for (i in seq_along(lithologies_list)) {
    lith <- lithologies_list[i]
    
    params <- lithology_params %>%
      filter(mapped_lithology == lith)
    
    if (nrow(params) == 0) stop(paste("Lithology not found in the table for", lith))
    
    b <- params$b
    sp <- params$sp
    sa <- params$sa
    
    # Calculate weathering rate without carbon parameters
    weathering_values[i] <- q * b * (sp * exp(((1000 * sa) / R) * ((1 / 284.2) - (1 / temp_kelvin))))
  }
  
  return(mean(weathering_values))
}

# Apply the weathering consumption calculation across the entire dataframe grouped by Stream_ID and Year
drivers_df <- drivers_df %>%
  group_by(Stream_ID, Year) %>%
  mutate(
    silicate_weathering = mapply(calculate_weathering, runoff, mapped_lithology, temp_K)
  ) %>%
  ungroup()

# Export the updated dataframe
write.csv(drivers_df, "AllDrivers_Harmonized_20241107_WRTDS_MD_KG_rawNP_Conc_silicate_weathering_annual.csv", row.names = FALSE)
