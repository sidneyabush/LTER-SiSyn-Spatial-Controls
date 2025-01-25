# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr)

# Clear environment
rm(list = ls())

## ------------------------------------------------------- ##
              # Read in and Tidy Data ----
## ------------------------------------------------------- ##
## Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

wrtds_df <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  rename(LTER = LTER.x) %>%
  filter(FNConc >= 0.5 * GenConc & FNConc <= 1.5 * GenConc) %>%
  dplyr::select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y, -contains("date"), 
                -contains("month"), -min_year, -max_year, -duration) %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    Year = floor(as.numeric(DecYear))) %>%
  dplyr:: filter(Year >= 2001 & Year <= 2024) %>%  # Filter rows with dates between 2001 and 2024
  filter(chemical == "DSi") 

gc()

num_unique_stream_ids <- wrtds_df %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

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
            # Calculate Yields ----
## ------------------------------------------------------- ##
yields <- wrtds_df %>%
  mutate(FNYield = FNFlux / drainSqKm,
         GenYield = GenFlux / drainSqKm) %>%
  dplyr::select(-FNFlux, -GenFlux)

tot <- wrtds_df %>%
  left_join(yields, by = c("Stream_ID", "Year")) %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

gc()

## ------------------------------------------------------- ##
            # Add in KG Classifications ----
## ------------------------------------------------------- ##
# Read in climate data produced in KoeppenGeigerClassification.R
KG <- read.csv("Koeppen_Geiger_2.csv")
KG$Stream_ID<-paste0(KG$LTER, "__", KG$Stream_Name)

tot <- tot %>%
  left_join(KG, by = "Stream_ID") %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

## ------------------------------------------------------- ##
              # Import Daylength ----
## ------------------------------------------------------- ##
# Load and clean daylength data
daylen <- read.csv("Monthly_Daylength_2.csv") %>%
  dplyr::select(-1)

num_unique_stream_ids <- daylen %>%
  pull(Stream_Name) %>%
  n_distinct()

print(num_unique_stream_ids)

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
  dplyr::select(-Updated_StreamName)

# Ensure the result is left-joined to "tot"
tot <- tot %>% 
  left_join(daylen_range, by = "Stream_Name") %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

num_unique_stream_ids <- tot %>%
  pull(Stream_ID) %>%
  n_distinct()

print(num_unique_stream_ids)

## We lose one stream here that we don't lose in the average dataset ---- revisit this:

## ------------------------------------------------------- ##
              # Spatial Drivers----
## ------------------------------------------------------- ##
# Define renamed and old names directly
name_conversion <- data.frame(
  Stream_ID = c("Walker Branch__East Fork", "Walker Branch__West Fork"),
  Updated_StreamName = c("Walker Branch__east fork", "Walker Branch__west fork")
)

# Read and preprocess spatial drivers
si_drivers <- read.csv("all-data_si-extract_2_202412.csv", stringsAsFactors = FALSE) %>%
  select(-contains("soil")) %>%
  # Create Stream_ID first using LTER and Stream_Name
  mutate(Stream_ID = paste0(LTER, "__", Stream_Name)) %>%
  # Incorporate site renaming
  left_join(name_conversion, by = "Stream_ID") %>%
  mutate(
    Stream_ID = coalesce(Updated_StreamName, Stream_ID)  # Replace Stream_ID with Updated_StreamName if available
  ) %>%
  select(-Updated_StreamName)  # Remove temporary renaming column

# Need to also do this for the Finnish sites in si drivers to names are all consistent: 
for (i in 1:nrow(finn)) {
  site_id<-finn[i,3]
  row_num<-which(si_drivers$Stream_ID==site_id)
  si_drivers[row_num, "Stream_ID"]<-finn[i,4]
}

si_drivers$Stream_ID <- ifelse(si_drivers$Stream_ID=="Finnish Environmental Institute__TORNIONJ KUKKOLA 14310  ",
                             "Finnish Environmental Institute__TORNIONJ KUKKOLA 14310", si_drivers$Stream_ID)

si_drivers$Stream_ID <- ifelse(si_drivers$Stream_ID=="Finnish Environmental Institute__SIMOJOKI AS. 13500      ",
                             "Finnish Environmental Institute__SIMOJOKI AS. 13500", si_drivers$Stream_ID)

# Remove rows where Stream_Name starts with "Site" and duplicates based on shapefile name
si_drivers <- si_drivers %>%
  filter(!grepl("^Site", Stream_Name)) %>%  # Remove rows starting with "Site"
  distinct(Shapefile_Name, .keep_all = TRUE)  # Remove duplicates by Shapefile_Name


## Before, using full abbrevs removed some of the spatial driver columns (e.g., "dec" in "deciduous" was causing
#  deciduous land cover to be filtered out)
months <- c("_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_")
months_cols <- si_drivers[,(colnames(si_drivers) %like% months)]

# Confirm NA replacement for permafrost columns
permafrost_cols <- grep("permafrost", colnames(si_drivers), value = TRUE)

# Replace NA values with 0 for all permafrost columns
si_drivers[, permafrost_cols] <- lapply(si_drivers[, permafrost_cols], function(x) {
  x <- as.numeric(x)  # Convert to numeric to avoid issues
  x[is.na(x)] <- 0
  return(x)
})

# Confirm updates
summary(si_drivers[, permafrost_cols])

# Parse out and clean annual data
year_cols <- si_drivers[,!(colnames(si_drivers) %in% colnames(months_cols))]
year_cols$Stream_Name <- si_drivers$Stream_Name
character_vars <- c("elevation|rock|land|soil|slope|permafrost")
year_cols <- year_cols[,!(colnames(year_cols) %like% character_vars)]
year_cols_melt <- melt(year_cols, id.vars = "Stream_Name")
year_cols_melt$variable <- as.character(year_cols_melt$variable)
year_cols_melt$year <- str_extract(year_cols_melt$variable, "(?<=_)[^_]+(?=\\MMDD$)")
year_cols_melt$year <- ifelse(is.na(year_cols_melt$year), sapply(strsplit(year_cols_melt$variable, "_"), function(x) x[2]),
                            year_cols_melt$year)

vars_annual <- c("num_days","prop_area","evapotrans","precip","temp","cycle0","cycle1","npp")
units_annual <- c("days", "prop_watershed","kg_m2","mm_day","deg_C","MMDD","MMDD","kgC_m2_year")
units_df_annual <- data.frame(vars_annual,units_annual)

colnames(units_df_annual)[1] <- "driver"

year_cols_melt$driver <- NA

for (i in 1:length(vars_annual)) {
  
  year_cols_melt$driver <- ifelse(year_cols_melt$variable %like% vars_annual[i],paste(vars_annual[i]), 
                                year_cols_melt$driver)
  
}

year_cols_melt <- year_cols_melt[,-2]

year_cols_melt <- merge(year_cols_melt, units_df_annual, by="driver")

num_unique_stream_ids <- year_cols_melt %>%
  pull(Stream_Name) %>%
  n_distinct()

print(num_unique_stream_ids)

# Parse out character data
character_cols <- si_drivers[,(colnames(si_drivers) %like% character_vars)]
character_cols$Stream_Name <- si_drivers$Stream_Name

num_unique_stream_ids <- tot %>%
  pull(Stream_Name) %>%
  n_distinct()

print(num_unique_stream_ids)


## ------------------------------------------------------- ##
    # Calculate Greenup Day ----
## ------------------------------------------------------- ##
drivers <- year_cols_melt

drivers <- subset(drivers, !drivers$driver %in% c("cycle1","num_days"))

drivers_cropped <- subset(drivers, drivers$year > 2001 & drivers$year < 2024)

# drivers_cast <- drivers_cropped %>%
#   dplyr::select(!c(units_annual,X)) %>%
#   tidyr::pivot_wider(names_from = driver, values_from=value)

drivers_cast <- drivers_cropped %>%
  # Remove duplicates based on relevant columns
  distinct(Stream_Name, year, driver, value, .keep_all = TRUE) %>%
  # Rename 'year' to 'Year'
  rename(Year = year) %>%
  # Convert 'Year' to numeric
  mutate(Year = as.numeric(Year)) %>%
  # Remove unwanted columns
  dplyr::select(!c(units_annual)) %>%
  # Pivot the data
  tidyr::pivot_wider(names_from = driver, values_from = value)

# Convert green up day to Julian Day
drivers_cast$cycle0 <- as.Date(unlist(drivers_cast$cycle0))
drivers_cast$cycle0 <- format(as.Date(drivers_cast$cycle0), "%j")

# Combine with character columns: 
all_spatial <- drivers_cast %>% 
  left_join(character_cols, by = "Stream_Name") %>%
  distinct(Stream_Name, Year, .keep_all = TRUE)

num_unique_stream_ids <- all_spatial %>%
  pull(Stream_Name) %>%
  n_distinct()

print(num_unique_stream_ids)

tot <- tot %>% 
  left_join(all_spatial, by = c("Stream_Name", "Year")) 

num_unique_stream_ids <- tot %>%
  pull(Stream_Name) %>%
  n_distinct()

print(num_unique_stream_ids)

# Combine with wrtds_df
tot <- tot %>%
  left_join(wrtds_df, by = c("Stream_Name", "Year")) %>%
  rename(Stream_ID = Stream_ID.y)

num_unique_stream_ids <- tot %>%
  pull(Stream_Name) %>%
  n_distinct()

print(num_unique_stream_ids)

# ## ------------------------------------------------------- ##
#           # Import WRTDS N_P Data ---- 
# ## ------------------------------------------------------- ##
# Filter for relevant chemicals (N and P) and simplify NO3/NOx to NOx
wrtds_NP <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  rename(LTER = LTER.x) %>%
  dplyr::select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y, -contains("date"), 
                -contains("month"), -min_year, -max_year, -duration) %>%
  dplyr::mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Year = floor(as.numeric(DecYear)) # Convert DecYear to Year
  ) %>%
  filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  filter(GenConc <= 60) %>%  # Remove rows where GenConc > 60
  filter(FNConc >= 0.5 * GenConc & FNConc <= 1.5 * GenConc)  %>%  # Remove rows where FNConc is ±50% of GenConc
  dplyr::select(-DecYear, -.groups, -LTER, -contains("FN"), -GenFlux) # Trying to tidy up this workflow by removing GenConc and GenFlux, this can be added back in in the future if GenConc/ GenFlux are desired for Yearly models.


wrtds_NP <- wrtds_NP %>%
  filter(chemical %in% c("P", "NO3", "NOx") & GenConc > 0) %>%  # Keep only positive GenConc
  mutate(
    chemical = ifelse(chemical %in% c("NOx", "NO3"), "NOx", chemical)  # Simplify to NOx
  )

# Handle duplicates by taking the median
wrtds_NP <- wrtds_NP %>%
  group_by(Stream_ID, Year, chemical) %>%  # Group by unique combinations
  summarise(
    GenConc = median(GenConc, na.rm = TRUE),  # Take the median if duplicates exist
    .groups = "drop"  # Ungroup after summarizing
  )

# Reshape data to wide format
wrtds_NP_wide <- wrtds_NP %>%
  pivot_wider(
    id_cols = c(Stream_ID, Year),  # Group by Stream_ID and Year
    names_from = chemical,         # Create separate columns for NOx and P
    values_from = GenConc,         # Populate these columns with GenConc values
    values_fill = list(GenConc = NA)  # Fill missing combinations with NA
  )

# Verify structure after reshaping
# str(wrtds_NP_wide)

gc()

setDT(wrtds_NP_wide)

# Merge with the "tot" dataframe to add annual NOx and P data
tot <- tot %>%
  inner_join(wrtds_NP_wide, by = c("Stream_ID", "Year")) %>%
  dplyr::select(-contains(".y")) 
  # %>%
  # # Rename P.x and NOx.x to P and NOx
  # rename(P = P.x, NOx = NOx.x)

num_unique_stream_ids <- tot %>%
  pull(Stream_Name) %>%
  n_distinct()

print(num_unique_stream_ids)

#str(tot)


# ## ------------------------------------------------------- ##
#           # Import RAW N_P Data ---- 
# ## ------------------------------------------------------- ##
# Read in the dataset
raw_NP <- read.csv("20241003_masterdata_chem.csv")

# Step 1: Filter, create Stream_ID, simplify solutes, and calculate median
raw_NP_median <- raw_NP %>%
  mutate(
    Year = as.integer(year(as.Date(date, format = "%Y-%m-%d"))),  # Extract Year from date
    Stream_ID = paste(LTER, Stream_Name, sep = "__"),              # Create Stream_ID by combining LTER and Stream_Name
    solute_simplified = case_when(  # Simplify solutes into NOx and P categories
      variable %in% c("NOx", "NO3") ~ "NOx",
      variable %in% c("SRP", "PO4") ~ "P",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(solute_simplified)) %>%  # Keep only relevant solutes (NOx, P)
  group_by(Stream_ID, Year, solute_simplified) %>%  # Group by Stream_ID, year, and solute
  summarise(
    median_value = median(value, na.rm = TRUE),  # Calculate median value
    .groups = "drop"  # Ungroup after summarizing
  )

# Step 2: Reshape the data to wide format
raw_NP_wide <- raw_NP_median %>%
  pivot_wider(
    names_from = solute_simplified,  # Create columns for NOx and P
    values_from = median_value,      # Populate these columns with the median values
    values_fill = list(median_value = NA)  # Fill missing combinations with NA
  )

# str(raw_NP_wide)

# ## ------------------------------------------------------- ##
#           # Match Data by Year ---- 
# ## ------------------------------------------------------- ##
# Merge raw N, P data into `tot`, matching years and replacing missing values
solutes <- c("P", "NOx")

for (solute in solutes) {
  raw_solute <- raw_NP_wide %>%
    select(Stream_ID, Year, !!sym(solute)) %>%
    rename(raw_solute = !!sym(solute))
  
  tot <- tot %>%
    mutate(Year = as.numeric(Year)) %>%  # Ensure Year is numeric for consistent merging
    left_join(raw_solute, by = c("Stream_ID", "Year")) %>%
    mutate(!!sym(solute) := ifelse(is.na(!!sym(solute)), raw_solute, !!sym(solute))) %>%
    select(-raw_solute)  # Remove the temporary column
}

# str(tot)

# # Check for duplicates in the `tot` dataframe
# duplicates_tot <- tot %>%
#   group_by(Stream_ID, Year) %>% 
#   filter(n() > 1)  # Keep only duplicated Stream_ID-Year combinations
# 
# # Print the duplicates for review
# print(duplicates_tot)

num_unique_stream_ids <- tot %>%
  pull(Stream_Name) %>%
  n_distinct()

print(num_unique_stream_ids)

## ------------------------------------------------------- ##
              #  Silicate Weathering ----
## ------------------------------------------------------- ##
# Read and prepare data
mapped_lithologies <- fread("mapped_lithologies.csv")

# Convert `tot` and `mapped_lithologies` to data.tables
setDT(tot)
setDT(mapped_lithologies)

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
weathering[, temp := as.numeric(temp)]
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

# Ensure 'Stream_ID' and 'Year' are compatible types
tot[, Stream_ID := as.character(Stream_ID)]
tot[, Year := as.integer(Year)]
weathering[, Stream_ID := as.character(Stream_ID)]
weathering[, Year := as.integer(Year)]

# Calculate silicate weathering for each row in the weathering data
weathering[, silicate_weathering := calculate_weathering_vectorized(mapped_lithology, runoff, temp_K)]

# Ensure uniqueness in the datasets
weathering <- unique(weathering, by = c("Stream_ID", "Year"))
tot <- unique(tot, by = c("Stream_ID", "Year"))

# Perform the merge correctly
tryCatch({
  tot <- merge(
    tot,
    weathering[, .(Stream_ID, Year, silicate_weathering)],
    by = c("Stream_ID", "Year"),
    all.x = TRUE,
    allow.cartesian = FALSE
  )
}, error = function(e) {
  message("Error encountered during merge: ", e$message)
})

# Clean up memory
gc()

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

# Filter rows with NA slopes from 'tot'
tot_with_na_slope <- tot %>%
  filter(is.na(basin_slope_mean_degree)) %>%
  select(Stream_ID)

# Merge 'tot_with_na_slope' with US and Krycklan slope data
tot_with_slope_filled <- tot_with_na_slope %>%
  left_join(US_slopes %>% select(Stream_ID, basin_slope_mean_degree), by = "Stream_ID") %>%
  left_join(Krycklan_slopes %>% select(Stream_ID, basin_slope_mean_degree), by = "Stream_ID", suffix = c("_US", "_Krycklan")) %>%
  mutate(
    basin_slope_mean_degree = coalesce(basin_slope_mean_degree_US, basin_slope_mean_degree_Krycklan)
  ) %>%
  select(Stream_ID, basin_slope_mean_degree)

# Convert to data.table for efficient key-based operations
tot_with_slope_filled <- as.data.table(tot_with_slope_filled)
tot <- as.data.table(tot)

# Set keys for efficient join
setkey(tot, Stream_ID)
setkey(tot_with_slope_filled, Stream_ID)

# Update 'tot' with gap-filled slopes, retaining original values where present
tot[tot_with_slope_filled, basin_slope_mean_degree := 
      ifelse(is.na(basin_slope_mean_degree), i.basin_slope_mean_degree, basin_slope_mean_degree),
    on = .(Stream_ID)]

# Check for NA values: 
# Identify unique Stream_IDs with NA values for basin_slope or elevation
na_sites <- read.csv("AllDrivers_Harmonized_Yearly_Keira.csv") %>%
  filter(is.na(basin_slope)) %>% # Filter rows where basin_slope is NA
  distinct(Stream_ID) # Get unique Stream_IDs

# Print the unique Stream_IDs with NA values
print(na_sites)

# Now do gap filling for elevation for the same sites:
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

# Combine with wrtds_df
tot <- tot %>%
  left_join(wrtds_df, by = c("Stream_Name", "Year")) 

# Identify Stream_IDs with NA in basin_slope_mean_degree
na_stream_ids <- tot %>%
  filter(is.na(basin_slope_mean_degree)) %>%
  select(Stream_ID.x)

# View the Stream_IDs with NA
print(na_stream_ids)

# Count the number of unique Stream_IDs
unique_stream_id_count <- tot %>%
  distinct(Stream_ID.x) %>%
  nrow()

# Print the count
cat("Number of unique Stream_IDs:", unique_stream_id_count, "\n")

# View the dataframe column names -- what a mess!
print(colnames(tot))

# View the cleaned dataframe
print(colnames(tot))

# Tidy data for export: 
tot_si <- tot %>%
  dplyr::select(Stream_ID.x, Year, drainSqKm.x, NOx, P, precip, Q.x,
                temp, Max_Daylength, prop_area, npp, evapotrans,
                silicate_weathering, cycle0, permafrost_mean_m, elevation_mean_m, 
                basin_slope_mean_degree, FNConc.x, FNYield, GenConc.x, GenYield, 
                contains("rocks"), contains("land_"))%>%
  dplyr::rename(Stream_ID = Stream_ID.x,
                Q = Q.x,
                FNConc = FNConc.x,
                GenConc = GenConc.x,
                snow_cover = prop_area, 
                greenup_day = cycle0,
                drainage_area = drainSqKm.x,
                elevation = elevation_mean_m,
                permafrost = permafrost_mean_m,
                basin_slope = basin_slope_mean_degree) %>%
  dplyr::mutate(permafrost = ifelse(is.na(permafrost), 0, permafrost)) 


duplicates <- tot_si %>%
  group_by(Stream_ID, Year) %>%
  filter(n() > 1)


if (nrow(duplicates) > 0) {
  print("Duplicates detected:")
  print(duplicates)
} else {
  print("No duplicates detected.")
}

tot_si <- tot_si %>%
  distinct(Stream_ID, Year, .keep_all = TRUE)

## ------------------------------------------------------- ##
          #  Export Annual Driver data ----
## ------------------------------------------------------- ##
# Check number of unique sites: should agree with avg
num_unique_sites <- tot_si %>% 
  summarise(num_sites = n_distinct(Stream_ID))

print(num_unique_sites)

write.csv(as.data.frame(tot_si), "AllDrivers_Harmonized_Yearly.csv")

gc()





