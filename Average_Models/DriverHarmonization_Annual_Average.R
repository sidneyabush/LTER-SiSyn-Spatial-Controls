## ------------------------------------------------------- ##
# Silica WG - Harmonize Drivers: Annual and Average Models
## ------------------------------------------------------- ##
# Written by:
## Sidney A Bush, Keira Johnson

# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr)

# Clear environment
rm(list = ls())

## ------------------------------------------------------- ##
# Read in and Tidy Data ----
## ------------------------------------------------------- ##
## Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

## Download most up to date WRTDS outputs (annual kalman)
# Read in WRTDS input file and process date and Stream_ID
wrtds_df <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  rename(LTER = LTER.x) %>%
  select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y, -contains("date"), -contains("month"), -min_year, -max_year, -duration) %>%
  dplyr::mutate(
    DecYear = as.Date(format(date_decimal(DecYear), "%Y-%m-%d")),
    Year = format(DecYear, "%Y"),
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)  # Create Stream_ID after Stream_Name adjustment
  )

## Need to tidy the Finnish site names:
finn <- read.csv("FinnishSites.csv")

finn$Stream_ID <- paste0("Finnish Environmental Institute__", finn$Site.ID)
finn$Stream_ID2 <- paste0("Finnish Environmental Institute__", finn$Site)

for (i in 1:nrow(finn)) {
  site_id<-finn[i,3]
  row_num<-which(wrtds_df$Stream_ID==site_id)
  wrtds_df[row_num, "Stream_ID"]<-finn[i,4]
}

wrtds_df$Stream_ID <- ifelse(wrtds_df$Stream_ID=="Finnish Environmental Institute__TORNIONJ KUKKOLA 14310  ",
                           "Finnish Environmental Institute__TORNIONJ KUKKOLA 14310", wrtds_df$Stream_ID)

wrtds_df$Stream_ID <- ifelse(wrtds_df$Stream_ID=="Finnish Environmental Institute__SIMOJOKI AS. 13500      ",
                           "Finnish Environmental Institute__SIMOJOKI AS. 13500", wrtds_df$Stream_ID)

## ------------------------------------------------------- ##
      # Download Reference Table from GD for DA ----
## ------------------------------------------------------- ##
ref_table_link <- "https://docs.google.com/spreadsheets/d/11t9YYTzN_T12VAQhHuY5TpVjGS50ymNmKznJK4rKTIU/edit#gid=357814834"
ref_table_folder = drive_get(as_id(ref_table_link))
ref_table <- drive_download(ref_table_folder$drive_resource, overwrite = T)

ref_table <- readxl::read_xlsx("Site_Reference_Table.xlsx")
ref_table$Stream_ID <- paste0(ref_table$LTER, "__", ref_table$Stream_Name)
area <- ref_table[,c("drainSqKm", "Stream_ID")]

# Define renamed and old names directly
name_conversion <- data.frame(
  Stream_ID = c("Walker Branch__East Fork", "Walker Branch__West Fork"),
  Updated_StreamName = c("Walker Branch__east fork", "Walker Branch__west fork")
)

# Filter and join in one step
missing_sites <- left_join(area[area$Stream_ID %in% name_conversion$Stream_ID, ], 
                           name_conversion, by = "Stream_ID")

# Remove the unnecessary column and rename the remaining one
missing_sites <- missing_sites %>%
  dplyr::select(-Stream_ID) %>%
  dplyr::rename(Stream_ID = Updated_StreamName)

# Append the updated sites to the original dataframe
area <- bind_rows(area, missing_sites)

# Perform a left join while selecting only specific columns from area
tot <- wrtds_df %>%
  left_join(area %>% select(Stream_ID), by = "Stream_ID", relationship = "many-to-many")

## ------------------------------------------------------- ##
            # Calculate Yields ----
## ------------------------------------------------------- ##
tot <- wrtds_df %>%
  mutate(FNYield = FNFlux / drainSqKm,
         GenYield = GenFlux / drainSqKm) %>%
  select(-FNFlux, -GenFlux)

## ------------------------------------------------------- ##
        # Add in KG Classifications ----
## ------------------------------------------------------- ##
# Read in climate data produced in KoeppenGeigerClassification.R
KG <- read.csv("Koeppen_Geiger_2.csv") %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)
  )


tot <- left_join(tot, KG %>% select(-Stream_Name), by = "Stream_ID", relationship = "many-to-many")

## ------------------------------------------------------- ##
          # Import Daylength ----
## ------------------------------------------------------- ##
# Load and clean daylength data
daylen <- read.csv("Monthly_Daylength_2.csv") %>%
  select(-1)

# Define renamed and old names directly in a streamlined manner
name_conversion <- data.frame(
  Stream_Name = c("East Fork", "West Fork"),
  Updated_StreamName = c("east fork", "west fork")
)

# Calculate min and max daylength, update Stream_Name, and ensure all sites in "tot" are left-joined
daylen_range <- daylen %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(
    Min_Daylength = min(mean_daylength),
    Max_Daylength = max(mean_daylength)
  ) %>%
  left_join(name_conversion, by = "Stream_Name") %>%
  mutate(Stream_Name = coalesce(Updated_StreamName, Stream_Name)) %>%
  select(-Updated_StreamName)

# Ensure the result is left-joined to "tot"
tot <- left_join(tot, daylen_range, by = "Stream_Name", relationship = "many-to-many")

## Subset to just DSi to make merging easier from hereout
tot <- subset(tot, chemical == "DSi")

# ## ------------------------------------------------------- ##
#           # On to the Dynamic Drivers ---- 
# ## ------------------------------------------------------- ##

## ------------------------------------------------------- ##
          # Import Spatial Drivers ----
## ------------------------------------------------------- ##
# Load data and create the Stream_ID column
spatial_drivers <- read.csv("all-data_si-extract_2_20240802.csv", stringsAsFactors = FALSE) %>%
  mutate(Stream_ID = paste0(LTER, "__", Stream_Name)) 

# Define the regular expression pattern for month abbreviations
months_abb <- "_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_"

# Pull out the monthly drivers and Stream_ID column
monthly_drivers <- spatial_drivers %>%
  select(Stream_ID, matches(months_abb))

# Remove monthly drivers from spatial drivers
spatial_drivers <- spatial_drivers %>%
  select(-matches(months_abb))

# Identify columns with any numbers in the header
year_columns <- grep("[0-9]", colnames(spatial_drivers), value = TRUE)
# Identify columns without numbers, ensuring Stream_ID is included
non_year_columns <- colnames(spatial_drivers)[!colnames(spatial_drivers) %in% year_columns]

## ------------------------------------------------------- ##
                # Annual Data ----
## ------------------------------------------------------- ##
# Separate columns with numbers from those without using base R indexing
spatial_drivers_with_years <- spatial_drivers[, year_columns, drop = FALSE]  # Columns with numbers in the header
# Retain Stream_ID and columns with years in spatial_drivers_with_years
spatial_drivers_with_years <- spatial_drivers %>%
  select(Stream_ID, all_of(year_columns))

# Convert all columns except Stream_ID to numeric where possible, coercing characters to NA
spatial_drivers_with_years <- spatial_drivers_with_years %>%
  mutate(across(-Stream_ID, as.numeric))

# Reshape spatial drivers to long format
spatial_drivers_long <- spatial_drivers_with_years %>%
  pivot_longer(
    cols = -Stream_ID,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    # Conditionally extract suffix after the year only for variables that start with "snow"
    driver_type = ifelse(grepl("^snow", variable),
                         sub("^[^_]+_[0-9]{4}_", "", variable),  # Extract suffix for "snow" variables
                         sub("_[0-9]{4}.*$", "", variable)),     # Standard extraction for other variables
    year = sub(".*_([0-9]{4}).*", "\\1", variable)
  ) %>%
  select(Stream_ID, driver_type, year, value)

# Step 1: Create the unique identifier column
# Here we create a unique column by combining Stream_ID, year, and driver_type
spatial_drivers_long <- spatial_drivers_long %>%
  mutate(unique_column = paste(Stream_ID, year, driver_type, sep = "_"))

# Step 2: Remove rows with any NA values in the relevant columns
# We ensure Stream_ID, year, driver_type, unique_column, and value are all non-NA
spatial_drivers_long_clean <- spatial_drivers_long %>%
  filter(!is.na(Stream_ID) & !is.na(year) & !is.na(driver_type) & !is.na(unique_column) & !is.na(value))

# Step 3: Convert `value` to a consistent type (e.g., numeric) if applicable
# This step ensures there are no type conflicts in the value column
spatial_drivers_long_clean <- spatial_drivers_long_clean %>%
  mutate(value = as.numeric(value))

# Step 4: Pivot to wide format using the unique_column as the column headers
# Split data by driver type and pivot each separately
wide_data_list <- list()

for(driver in unique(spatial_drivers_long_clean$driver_type)) {
  # Subset the data for the current driver type
  driver_data <- spatial_drivers_long_clean %>%
    filter(driver_type == driver) %>%
    select(Stream_ID, year, value) %>%
    rename(!!driver := value)
  
  # Store each driver type as a separate wide dataframe
  wide_data_list[[driver]] <- driver_data
}

# Merge all wide dataframes by Stream_ID and year
spatial_drivers_wide <- Reduce(function(x, y) merge(x, y, by = c("Stream_ID", "year"), all = TRUE), 
                               wide_data_list)

setDT(spatial_drivers_wide)

spatial_drivers_no_years <- spatial_drivers[, !colnames(spatial_drivers) %in% year_columns, drop = FALSE]  # Columns without numbers
# Retain Stream_ID and columns without numbers in spatial_drivers_no_years
spatial_drivers_no_years <- spatial_drivers %>%
  select(Stream_ID, all_of(non_year_columns))

# Perform a left join to combine the wide annual data with the non-annual data
combined_spatial_drivers <- spatial_drivers_wide %>%
  left_join(spatial_drivers_no_years, by = "Stream_ID", relationship = "many-to-many")


setDT(combined_spatial_drivers)

# Rename `year` to `Year` in `combined_spatial_drivers` to match `tot`
combined_spatial_drivers <- combined_spatial_drivers %>%
  dplyr::mutate(Year = as.integer(year))

combined_spatial_drivers <- combined_spatial_drivers %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

setDT(tot)

final_combined_data <- tot[combined_spatial_drivers, on = .(Stream_ID, Year), nomatch = 0]

## ------------------------------------------------------- ##
            # Greenup Day ----
## ------------------------------------------------------- ##
# Define greenup cycles (excluding cycle1 as it's not needed)
greenup_cycles <- c("cycle0")

# Step 1: Standardize column names by removing "MMDD" if present
colnames(spatial_drivers) <- sub("MMDD$", "", colnames(spatial_drivers))

# Convert date columns to day-of-year values for cycle0 only
for (cycle in greenup_cycles) {
  # Identify `greenup` columns for the specified cycle
  cycle_cols <- grep(paste0("greenup_", cycle), colnames(spatial_drivers), value = TRUE)
  
  # Loop through each column in the cycle
  for (col in cycle_cols) {
    # Convert date to day-of-year if it’s in the correct format
    spatial_drivers[[paste0(col, "_doy")]] <- ifelse(
      !is.na(as.Date(spatial_drivers[[col]], format = "%Y-%m-%d")),
      yday(as.Date(spatial_drivers[[col]], format = "%Y-%m-%d")),
      NA_real_
    )
  }
}

# Step 3: Reshape `greenup` data for cycle0 only, excluding "cycle" column
greenup_long <- list()

for (cycle in greenup_cycles) {
  # Select columns starting with `greenup` and ending with `_doy` that match the current cycle
  cycle_doy_cols <- grep(paste0("greenup_", cycle, ".*_doy$"), colnames(spatial_drivers), value = TRUE)
  
  # Proceed with reshaping if there are matching columns
  if (length(cycle_doy_cols) > 0) {
    greenup_long[[cycle]] <- spatial_drivers %>%
      select(Stream_ID, all_of(cycle_doy_cols)) %>%
      pivot_longer(
        cols = all_of(cycle_doy_cols),
        names_to = "variable",
        values_to = "greenup_day"
      ) %>%
      mutate(
        year = as.integer(sub(".*_(\\d{4}).*", "\\1", variable))  # Extract year
      ) %>%
      select(Stream_ID, year, greenup_day)
  } else {
    warning(paste("No matching columns found for cycle:", cycle))
  }
}

# Combine `greenup_long` data for cycle0 only
greenup_df <- bind_rows(greenup_long$cycle0)

# Merge `greenup_df`, renaming `year` to `Year` if needed
greenup_df <- greenup_df %>%
  rename(Year = year)  # Ensure column names match

tot <- final_combined_data %>%
  left_join(greenup_df, by = c("Stream_ID", "Year"), relationship = "many-to-many")

# ## ------------------------------------------------------- ##
#           # Import WRTDS N_P Conc ---- 
# ## ------------------------------------------------------- ##
# Filter for relevant chemicals and positive GenConc values, and simplify NO3/NOx to NOx
wrtds_NP <- wrtds_df %>%
  filter(chemical %in% c("P", "NO3", "NOx"), GenConc > 0) %>%  # Removes NAs and zero values
  mutate(chemical = ifelse(chemical %in% c("NOx", "NO3"), "NOx", chemical))

# Summarize to get the median GenConc by Stream_ID, Year, and simplified chemical with NA removal
wrtds_NP_annual <- wrtds_NP %>%
  group_by(Stream_ID, Year, chemical) %>%
  summarise(
    median_Conc = median(GenConc, na.rm = TRUE),  # Ensures NAs are ignored in median calculation
    .groups = 'drop'
  )

# Reshape data to have separate columns for NOx and P
wrtds_NP_annual_wide <- wrtds_NP_annual %>%
  pivot_wider(names_from = chemical, values_from = median_Conc)

# Merge with the "tot" dataframe to add annual NOx and P data in a single row per Stream_ID and Year
tot <- tot %>%
  left_join(wrtds_NP_annual_wide, by = c("Stream_ID", "Year"))

# Count the number of years with NA values for NOx and P for each Stream_ID, showing only non-zero results
na_counts <- tot %>%
  group_by(Stream_ID) %>%
  summarise(
    na_years_NOx = sum(is.na(NOx)),
    na_years_P = sum(is.na(P)),
    .groups = "drop"
  ) %>%
  filter(na_years_NOx > 0 | na_years_P > 0)  # Keep only rows with non-zero counts

# ## ------------------------------------------------------- ##
#           # Import RAW N_P Conc ---- 
# ## ------------------------------------------------------- ##
# Read in the dataset
raw_NP <- read.csv("20241003_masterdata_chem.csv")

# Step 1: Filter for Nitrogen and Phosphorus variables with valid values, extract Year, and keep only years with >5 values
raw_NP <- raw_NP %>%
  filter(variable %in% c("SRP", "PO4", "NO3", "NOx") & value > 0) %>%
  mutate(Year = year(as.Date(date, format = "%Y-%m-%d"))) %>%
  group_by(Stream_Name, Year) %>%
  filter(n() > 5) %>%  # Keep only Stream_ID and Year combinations with more than 5 values
  ungroup()

# Step 2: Calculate annual median N and P values, removing NAs before calculating
raw_NP_avg <- raw_NP %>%
  group_by(Stream_Name, Year, variable) %>%
  summarise(annual_median_Conc = median(value, na.rm = TRUE), .groups = "drop")

# Step 3: Get unique stream-variable combinations for units
raw_NP_units <- raw_NP %>%
  select(Stream_Name, variable, units) %>%
  distinct()

# Step 4: Merge annual average values with units
raw_NP_avg <- raw_NP_avg %>%
  left_join(raw_NP_units, by = c("Stream_Name", "variable"))

# Step 5: Rename sites using a lookup table and update stream names
name_conversion <- data.frame(
  Stream_Name = c("east fork", "west fork"),
  Updated_StreamName = c("East Fork", "West Fork")
)

raw_NP_avg <- raw_NP_avg %>%
  left_join(name_conversion, by = "Stream_Name") %>%
  mutate(Stream_Name = coalesce(Updated_StreamName, Stream_Name)) %>%
  select(-Updated_StreamName)

# Step 6: Simplify variable names for NOx and SRP to NOx and P
raw_NP_avg <- raw_NP_avg %>%
  mutate(solute_simplified = ifelse(variable %in% c("NOx", "NO3"), "NOx", "P"))

# Step 7: Average values for streams that switched between NO3 and NOx reporting
raw_NP_avg_final <- raw_NP_avg %>%
  group_by(Stream_Name, Year, solute_simplified) %>%
  summarise(annual_median_Conc = median(annual_median_Conc, na.rm = TRUE), .groups = "drop")

# Step 8: Ensure unique stream-solute-year combinations
raw_NP_avg_final <- raw_NP_avg_final %>%
  distinct(Stream_Name, Year, solute_simplified, .keep_all = TRUE)

# Step 9: Filter `raw_NP_avg_final` for the "P" solute
raw_P <- raw_NP_avg_final %>%
  filter(solute_simplified == "P") %>%
  select(Stream_Name, Year, annual_median_Conc) %>%
  rename(raw_P = annual_median_Conc)

# Convert Year to numeric in tot to match the type in raw_P
tot <- tot %>%
  mutate(Year = as.numeric(Year)) %>%  # Convert Year to numeric
  # Join and replace NA values in P with raw_P values where applicable
  left_join(raw_P, by = c("Stream_Name", "Year")) %>%
  mutate(P = ifelse(is.na(P), raw_P, P)) %>%  # Replace NA values in P with raw_P values
  select(-raw_P)  # Remove the temporary raw_P column

## ------------------------------------------------------- ##
            #  Silicate Weathering ----
## ------------------------------------------------------- ##
# Read and prepare data
mapped_lithologies <- fread("mapped_lithologies.csv")
setDT(tot)  # Use tot only for the final merged dataset

# Ensure compatibility in the major_rock column for merging
tot[, major_rock := as.character(major_rock)]
mapped_lithologies[, major_rock := as.character(major_rock)]

# Perform the merge and filter out NA values in mapped_lithology
weathering <- merge(tot[, .(Stream_ID, Year, major_rock, Q, temp, drainSqKm)], 
                    mapped_lithologies[, .(major_rock, mapped_lithology)], 
                    by = "major_rock", all.x = TRUE)
weathering <- weathering[!is.na(mapped_lithology)]  # Remove rows with NA in mapped_lithology

# Constants for weathering calculations
seconds_per_year <- 31536000  
kg_per_m3 <- 1000  
km2_to_m2 <- 10^6  
R <- 8.314  

# Define lithology parameters as a data.table
lithology_params <- data.table(
  mapped_lithology = c("su", "vb", "pb", "py", "va", "vi", "ss", "pi", "sm", "mt", "pa"),
  b = c(0.003364, 0.007015, 0.007015, 0.0061, 0.002455, 0.007015, 0.005341, 0.007015, 0.012481, 0.007626, 0.005095),
  sp = c(1, 1, 1, 1, 1, 1, 0.64, 0.58, 0.24, 0.25, 0.58),
  sa = c(60, 50, 50, 46, 60, 50, 60, 60, 60, 60, 60)
)

# Convert temperature to Kelvin for calculations
weathering[, temp_K := temp + 273.15]

# Calculate runoff based on the given formula
weathering[, runoff := (Q * seconds_per_year * kg_per_m3) / (drainSqKm * km2_to_m2)]

# Define a function for vectorized calculation of weathering
calculate_weathering_vectorized <- function(lithologies, runoff, temp_k) {
  lithologies_split <- strsplit(lithologies, ",\\s*")
  weathering_results <- sapply(seq_along(lithologies_split), function(i) {
    liths <- lithologies_split[[i]]
    weathering_values <- sapply(liths, function(lith) {
      params <- lithology_params[mapped_lithology == lith]
      if (nrow(params) == 0) stop(paste("Lithology not found in the table for", lith))
      params$b * (params$sp * exp(((1000 * params$sa) / R) * ((1 / 284.2) - (1 / temp_k[i])))) * runoff[i]
    })
    mean(weathering_values, na.rm = TRUE)
  })
  return(weathering_results)
}

# Calculate silicate weathering for each row in the weathering data
weathering[, silicate_weathering := calculate_weathering_vectorized(mapped_lithology, runoff, temp_K), by = .(Stream_ID, Year)]

setDT(weathering)

# Ensure 'Stream_ID' and 'Year' are character to avoid merge issues due to type differences
tot[, Stream_ID := as.character(Stream_ID)]
tot[, Year := as.integer(Year)]
weathering[, Stream_ID := as.character(Stream_ID)]
weathering[, Year := as.integer(Year)]

# Step 1: Check for duplicates in both datasets for the join keys
weathering <- unique(weathering, by = c("Stream_ID", "Year"))
tot <- unique(tot, by = c("Stream_ID", "Year"))

# Step 2: Attempt the merge with careful memory management
tryCatch({
  tot <- merge(
    tot,
    weathering[, .(Stream_ID, Year, silicate_weathering)],
    by = c("Stream_ID", "Year"),
    all.x = TRUE,
    allow.cartesian = TRUE
  )
}, error = function(e) {
  message("Error encountered: ", e$message)
  print("Attempting alternative approach using by=.EACHI")
  
  # Retry with by=.EACHI to handle large merges with potential duplicates
  tot <<- merge(
    tot,
    weathering[, .(Stream_ID, Year, silicate_weathering)],
    by = c("Stream_ID", "Year"),
    all.x = TRUE,
    allow.cartesian = TRUE,
    by = .EACHI
  )
})

## ------------------------------------------------------- ##
            #  Gap Filling Missing Data ----
## ------------------------------------------------------- ##
# Import streams with na slopes
# Load and process Krycklan slopes
Krycklan_slopes <- transform(read.csv("Krycklan_basin_slopes.csv"), 
                             basin_slope_mean_degree = atan(gradient_pct / 100) * (180 / pi))

# Load and process US slopes
US_slopes <- read.csv("DSi_Basin_Slope_missing_sites.csv", header = FALSE)
colnames(US_slopes) <- US_slopes[1, ]
US_slopes <- US_slopes[-1, ]
US_slopes <- US_slopes %>%
  pivot_longer(
    cols = everything(),
    names_to = "Stream_Name",
    values_to = "basin_slope_mean_degree"
  ) %>%
  mutate(basin_slope_mean_degree = as.numeric(basin_slope_mean_degree))

# Upload the Stream_Name to Stream_ID key file
stream_key <- read.csv("basin_stream_id_conversions.csv", header = TRUE)

# Merge stream key with Krycklan_slopes and US_slopes to add Stream_ID
Krycklan_slopes <- left_join(Krycklan_slopes, stream_key, by = "Stream_Name") %>%
  filter(!is.na(basin_slope_mean_degree))  # Remove rows with NA values after merging with key

US_slopes <- left_join(US_slopes, stream_key, by = "Stream_Name") %>%
  filter(!is.na(basin_slope_mean_degree))  # Remove rows with NA values after merging with key

# Filter tot for rows with NA basin slope values
tot_with_na_slope <- tot %>%
  filter(is.na(basin_slope_mean_degree)) %>%
  select(Stream_ID)

# Merge tot_with_na_slope with US_slopes and Krycklan_slopes to fill missing values
tot_with_slope_filled <- tot_with_na_slope %>%
  left_join(US_slopes %>% select(Stream_ID, basin_slope_mean_degree), by = "Stream_ID") %>%
  left_join(Krycklan_slopes %>% select(Stream_ID, basin_slope_mean_degree), by = "Stream_ID", suffix = c("_US", "_Krycklan")) %>%
  mutate(
    basin_slope_mean_degree = coalesce(basin_slope_mean_degree_US, basin_slope_mean_degree_Krycklan)
  ) %>%
  select(Stream_ID, basin_slope_mean_degree)

tot_with_slope_filled <- as.data.table(tot_with_slope_filled)

# Convert tot to data.table
tot <- as.data.table(tot)

# Set keys on Stream_ID for both tables
setkey(tot, Stream_ID)
setkey(tot_with_slope_filled, Stream_ID)

# Update in-place: replace NA values in basin_slope_mean_degree only
tot[tot_with_slope_filled, basin_slope_mean_degree := i.basin_slope_mean_degree, on = .(Stream_ID)]

# Load the US elevation data without headers
US_elev <- read.csv("DSi_Basin_Elevation_missing_sites.csv", header = FALSE)

# Set the first row as column names and remove it from the data
colnames(US_elev) <- US_elev[1, ]
US_elev <- US_elev[-1, ]

# Convert the dataframe from wide to long format
US_elev <- US_elev %>%
  pivot_longer(
    cols = everything(),
    names_to = "Stream_Name",
    values_to = "elevation_mean_m"
  )

# Convert elevation_mean_m to numeric
US_elev$elevation_mean_m <- as.numeric(US_elev$elevation_mean_m)

# Merge with the stream key and remove rows with NA in elevation_mean_m after the merge
US_elev <- left_join(US_elev, stream_key, by = "Stream_Name") %>%
  filter(!is.na(elevation_mean_m))

# Filter tot for rows with NA elevation values
tot_with_na_elev <- tot %>%
  filter(is.na(elevation_mean_m)) %>%
  select(Stream_ID)

# Merge tot_with_na_elev with US_elev to fill missing elevation values
tot_with_elev_filled <- tot_with_na_elev %>%
  left_join(US_elev %>% select(Stream_ID, elevation_mean_m), by = "Stream_ID")

# Update tot with the filled elevation values
tot <- tot %>%
  left_join(tot_with_elev_filled, by = "Stream_ID", suffix = c("", "_filled"), relationship = "many-to-many") %>%
  mutate(
    elevation_mean_m = coalesce(elevation_mean_m_filled, elevation_mean_m)
  ) %>%
  select(-elevation_mean_m_filled)  # Remove Stream_Name_filled from final output

# Tidy data for export: 
tot_si <- tot %>%
  select(Stream_ID, Year, drainSqKm, ClimateZ, Name, NOx, P, precip,
         temp, Max_Daylength, num_days, max_prop_area, npp, evapotrans,
         silicate_weathering, greenup_day, contains("permafrost"),
         contains("Conc"), contains("Yield"), contains("slope"), 
         contains("elevation"), contains("q"), 
         contains("rock"), contains("land"))

## ------------------------------------------------------- ##
#  Export Annual Driver data ----
## ------------------------------------------------------- ##
# Check number of unique sites: should agree with avg
num_unique_sites <- tot_si %>% 
  summarise(num_sites = n_distinct(Stream_ID))

print(num_unique_sites)

write.csv(tot_si, "AllDrivers_Harmonized_Annual.csv")

## ------------------------------------------------------- ##
#   Calculate and Export Average Driver data ----
## ------------------------------------------------------- ##

## ------------------------------------------------------- ##
## Calculate Stats that can only be calculated for average data: 
## ------------------------------------------------------- ##
# Calculate si_stats with CV columns
si_stats <- tot_si %>%
  group_by(Stream_ID, Year) %>%
  summarise(
    across(
      c(FNConc, GenConc, GenYield, FNYield), 
      list(mean = mean, median = median, min = min, max = max), 
      .names = "{.fn}_si_{.col}"
    ),
    across(
      c(FNConc, GenConc, GenYield, FNYield), 
      ~ sd(.) / mean(.),  # Calculate CV
      .names = "CV_si_{.col}"
    ),
    .groups = "drop"  # Ungroup after summarise for a clean output
  ) 

# Calculate q_stats with CV for Q
q_stats <- tot_si %>%
  group_by(Stream_ID, Year) %>%
  summarise(
    mean_q = mean(Q), 
    med_q = median(Q), 
    min_q = min(Q), 
    max_q = max(Q), 
    q_95 = quantile(Q, 0.95), 
    q_5 = quantile(Q, 0.05),
    cv_q = sd(Q) / mean(Q),  # Calculate CV for Q
    .groups = "drop"
  )

# Combine si_stats and q_stats and calculate CVC/CVQ
tot <- si_stats %>%
  left_join(q_stats, by = c("Stream_ID", "Year")) %>%
  mutate(
    cvc_cvq = CV_si_FNConc / cv_q  # Example: Calculate CVC/CVQ for FNConc and Q
  )

tot_si <- tot %>%
  left_join(wrtds_df, by = c("Stream_ID", "Year"))


## Prepare to summarize tot_si for site average behavior: 

# Define static keywords, excluding Stream_ID (it is handled by grouping)
static_keywords <- c("drainSqKm", "ClimateZ", "Name", "Max_Daylength",
                     "slope", "elevation", "rock", "land")  # Columns containing these words are static

# Dynamically identify static and summarizable columns
static_columns <- names(tot_si)[grepl(paste(static_keywords, collapse = "|"), names(tot_si))]
columns_to_summarize <- setdiff(names(tot_si), c(static_columns, "Stream_ID"))  # Exclude Stream_ID explicitly

# Summarize data
tot_si_avg <- tot_si %>%
  group_by(Stream_ID) %>%
  summarise(
    across(
      all_of(static_columns),  # Static columns identified dynamically
      ~ first(.), 
      .names = "{.col}"  # Keep the original column name
    ),
    across(
      all_of(columns_to_summarize),  # Non-static columns
      mean, 
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Make sure stream counts match
num_unique_sites <- tot_si_avg %>% 
  summarise(num_sites = n_distinct(Stream_ID))

print(num_unique_sites)

write.csv(tot_si_avg, "AllDrivers_Harmonized_Average.csv")
