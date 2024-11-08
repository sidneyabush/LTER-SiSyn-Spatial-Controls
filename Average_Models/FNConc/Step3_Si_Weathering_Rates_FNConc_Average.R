# Calculating Weathering Rates: AVERAGE

# Clear environment
rm(list = ls())

# Required libraries
library(dplyr)

# Read in and tidy data ----
# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

drivers_df <- read.csv("AllDrivers_Harmonized_20241108_WRTDS_MD_KG_NP_FNConc_Average.csv") %>%
  distinct(Stream_ID, .keep_all = TRUE) %>%
  # Rename specific columns
  dplyr::rename(drainage_area = drainSqKm,
                snow_cover = prop_area,
                green_up_day = cycle0,
                max_daylength = Max_Daylength)

unique_major_rock <- unique(drivers_df$major_rock)

# Export the unique values as a CSV file
write.csv(unique_major_rock, "unique_major_rock.csv", row.names = FALSE)

# import mapped lithologies and merge with drivers_df
mapped_lithologies <- read.csv("mapped_lithologies.csv")

# Merge the mapped lithologies with drivers_df by the "major_rock" column
drivers_df <- merge(drivers_df, mapped_lithologies, by = "major_rock", all.x = TRUE)

# Filter out rows with NA in the "major_rock" column
drivers_df <- drivers_df %>%
  filter(!is.na(mapped_lithology))

# Calculate runoff in kg/yr/m2 per equation: 
# Constants
seconds_per_year <- 31536000  # Number of seconds in a year
kg_per_m3 <- 1000  # Density of water in kg/m^3
km2_to_m2 <- 10^6  # Conversion factor from km^2 to m^2

# Calculate runoff for each row in drivers_df
drivers_df <- drivers_df %>%
  mutate(runoff = (med_q * seconds_per_year * kg_per_m3) / (drainage_area * km2_to_m2))

# Convert the "temp" column from degrees Celsius to Kelvin
drivers_df <- drivers_df %>%
  mutate(temp_K = temp + 273.15)  # Add a new column 'temp_K'

# Constants
R <- 8.314 # gas constant in J/(mol*K)

# Parameters for each lithology type (Table S1), without carbon (cp, ca, cc)
lithology_params <- data.frame(
  mapped_lithology = c("su", "vb", "pb", "py", "va", "vi", "ss", "pi", "sm", "mt", "pa"),
  b = c(0.003364, 0.007015, 0.007015, 0.0061, 0.002455, 0.007015, 0.005341, 0.007015, 0.012481, 0.007626, 0.005095),
  sp = c(1, 1, 1, 1, 1, 1, 0.64, 0.58, 0.24, 0.25, 0.58),
  sa = c(60, 50, 50, 46, 60, 50, 60, 60, 60, 60, 60)
)

# Function to calculate weathering consumption rate and average for multiple lithologies
calculate_weathering <- function(q, lithologies, temp_celsius) {
  
  # Split lithologies if there are multiple
  lithologies_list <- strsplit(lithologies, ",\\s*")[[1]]  # Split by commas and optional spaces
  
  # Initialize an empty vector to store weathering values for each lithology
  weathering_values <- numeric(length(lithologies_list))
  
  # Loop through each lithology in the list and calculate weathering 
  for (i in seq_along(lithologies_list)) {
    lith <- lithologies_list[i]
    
    # Look up parameters for the lithology
    params <- lithology_params %>%
      filter(mapped_lithology == lith)
    
    if (nrow(params) == 0) stop(paste("Lithology not found in the table for", lith))
    
    # Extract parameters
    b <- params$b
    sp <- params$sp
    sa <- params$sa
    
    # weathering consumption calculation without carbon parameters
    weathering_values[i] <- q * b * (sp * exp(((1000 * sa) / R) * ((1 / 284.2) - (1 / temp_celsius))))
  }
  
  # Return the average weathering consumption across all lithologies
  return(mean(weathering_values))
}

# Apply the weathering consumption calculation across the entire dataframe in one step
drivers_df <- drivers_df %>%
  mutate(silicate_weathering = mapply(calculate_weathering, runoff, mapped_lithology, temp_K))

# Export the dataframe to a CSV file
write.csv(drivers_df, "AllDrivers_Harmonized_20241108_WRTDS_MD_KG_NP_FNConc_silicate_weathering.csv", row.names = FALSE)
