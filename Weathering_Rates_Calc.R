# Calculating Weathering Rates: 

# Clear environment
rm(list = ls())

# Required libraries
library(dplyr)

# Read in and tidy data ----
# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

drivers_df <- read.csv("AllDrivers_Harmonized_20241017_WRTDS_MD_KG_NP.csv") %>%
  distinct(Stream_ID, .keep_all = TRUE)

unique_major_rock <- unique(drivers_df$major_rock)

# Export the unique values as a CSV file
write.csv(unique_major_rock, "unique_major_rock.csv", row.names = FALSE)


# Constants
R <- 8.314 # gas constant in J/(mol*K)

# Parameters for each lithology type (Table S1)
lithology_params <- data.frame(
  Lithology = c("su", "vb", "pb", "py", "va", "vi", "ss", "pi", "sm", "mt", "pa"),
  b = c(0.003364, 0.007015, 0.007015, 0.0061, 0.002455, 0.007015, 0.005341, 0.007015, 0.012481, 0.007626, 0.005095),
  sp = c(1, 1, 1, 1, 1, 1, 0.64, 0.58, 0.24, 0.25, 0.58),
  sa = c(60, 50, 50, 46, 60, 50, 60, 60, 60, 60, 60),
  cp = c(0, 0, 0, 0, 0, 0, 0.36, 0.42, 0.76, 0.75, 0.42),
  ca = c(14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14),
  cc = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
)

# Function to calculate CO2 consumption rate
calculate_CO2 <- function(q, lithology, temp_celsius) {
  params <- lithology_params %>%
    filter(Lithology == lithology)
  
  if(nrow(params) == 0) stop("Lithology not found in the table")
  
  # Convert temperature from Celsius to Kelvin
  T <- temp_celsius + 273.15
  
  b <- params$b
  sp <- params$sp
  sa <- params$sa
  cp <- params$cp
  ca <- params$ca
  cc <- params$cc
  
  # CO2 consumption calculation without s (soil shield)
  CO2 <- q * b * (sp * exp(((1000 * sa) / R) * ((1 / 284.2) - (1 / T))) +
                    cp * cc * exp(((1000 * ca) / R) * ((1 / 284.2) - (1 / T))))
  
  return(CO2)
}

# Example usage:
# Assuming you have a dataframe 'df' with columns 'Lithology', 'Runoff' (q), and 'temp' (in degrees Celsius)
df <- data.frame(
  Lithology = c("su", "vb", "ss"),
  Runoff = c(1000, 1500, 1200),  # example runoff values in kg/a/m^2
  temp = c(15, 20, 18)           # example temperature values in Celsius
)

# Apply the CO2 consumption calculation for each row
df <- df %>%
  rowwise() %>%
  mutate(CO2_consumption = calculate_CO2(Runoff, Lithology, temp))

# View results
print(df)
