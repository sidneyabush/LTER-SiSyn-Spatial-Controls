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
  filter(chemical == "DSi") %>%
  dplyr::select(-chemical)

gc()

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

tot_yields <- wrtds_df %>%
  left_join(yields, by = c("Stream_ID", "Year")) %>%
  distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  # Remove columns with .y
  select(-contains(".y")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.x$"))

## ------------------------------------------------------- ##
# Add in KG Classifications ----
## ------------------------------------------------------- ##
# Read in climate data produced in KoeppenGeigerClassification.R
KG <- read.csv("Koeppen_Geiger_2.csv")%>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name))

KG$Stream_ID<-paste0(KG$LTER, "__", KG$Stream_Name)

tot_KG <- tot_yields %>%
  left_join(KG, by = "Stream_ID") %>%
  distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  # Remove columns with .y
  select(-contains(".y"), -X, -Use_WRTDS, -contains("coord"), -Latitude, -Longitude) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.x$"))

## ------------------------------------------------------- ##
              # Import Daylength ----
## ------------------------------------------------------- ##
# Load and clean daylength data
daylen <- read.csv("Monthly_Daylength_2.csv") %>%
  dplyr::select(-1) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name))

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
  dplyr::select(-Updated_StreamName, -contains(".x"), -contains(".y"))

# Ensure the result is left-joined to "tot"
tot_daylen <- tot_KG %>% 
  left_join(daylen_range, by = "Stream_Name") %>%
  distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  # Remove columns with .y
  select(-contains(".y"), -Min_Daylength) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.x$"))

# Capture `Stream_IDs` in the original `tot` dataframe
stream_ids_pre_spatial <- tot_daylen %>%
  distinct(Stream_ID) %>%
  pull(Stream_ID)

## ------------------------------------------------------- ##
# Spatial Drivers Processing----
## ------------------------------------------------------- ##

# Rename specific Stream_IDs
name_conversion <- data.frame(
  Stream_ID = c("Walker Branch__East Fork", "Walker Branch__West Fork"),
  Updated_StreamName = c("Walker Branch__east fork", "Walker Branch__west fork")
)

# Load and preprocess spatial drivers
si_drivers <- read.csv("all-data_si-extract_2_202412.csv", stringsAsFactors = FALSE) %>%
  select(-contains("soil")) %>%
  mutate(Stream_ID = paste0(LTER, "__", Stream_Name)) %>%
  left_join(name_conversion, by = "Stream_ID") %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      TRUE ~ Stream_Name
    ), # Update Stream_Name for consistency
    Stream_ID = coalesce(Updated_StreamName, Stream_ID) # Update Stream_ID
  ) %>%
  # Remove columns with .y
  dplyr::select(-contains(".y"), -Updated_StreamName) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.x$"))

# Standardize Finnish site names
for (i in seq_len(nrow(finn))) {
  site_id <- finn[i, 3]
  row_num <- which(si_drivers$Stream_ID == site_id)
  si_drivers[row_num, "Stream_ID"] <- finn[i, 4]
}

# Cleanup specific Finnish Stream_IDs
si_drivers <- si_drivers %>%
  mutate(
    Stream_ID = case_when(
      Stream_ID == "Finnish Environmental Institute__TORNIONJ KUKKOLA 14310  " ~ "Finnish Environmental Institute__TORNIONJ KUKKOLA 14310",
      Stream_ID == "Finnish Environmental Institute__SIMOJOKI AS. 13500      " ~ "Finnish Environmental Institute__SIMOJOKI AS. 13500",
      TRUE ~ Stream_ID
    )
  ) %>%
  filter(!grepl("^Site", Stream_Name)) %>%
  distinct(Shapefile_Name, .keep_all = TRUE)

# Replace NA values in permafrost columns with 0
permafrost_cols <- grep("permafrost", colnames(si_drivers), value = TRUE)
si_drivers[, permafrost_cols] <- lapply(si_drivers[, permafrost_cols], function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- 0
  return(x)
})

# Remove monthly columns
months <- c("_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_")
months_cols <- si_drivers %>%
  select(matches(months))
si_drivers <- si_drivers %>%
  select(-one_of(colnames(months_cols)))

## ------------------------------------------------------- ##
# Parse and Process Annual Data----
## ------------------------------------------------------- ##

# Define annual variables and their units
annual_vars <- c("num_days", "prop_area", "evapotrans", "precip", "temp", "cycle0", "cycle1", "npp")
units_annual <- c("days", "prop_watershed", "kg_m2", "mm_day", "deg_C", "MMDD", "MMDD", "kgC_m2_year")

# Check matched columns
matched_columns <- colnames(si_drivers)[grepl(paste(annual_vars, collapse = "|"), colnames(si_drivers))]
if (!"Stream_Name" %in% colnames(si_drivers)) {
  stop("Stream_Name column is missing from si_drivers!")
}

# Separate numeric columns, explicitly keeping Stream_Name
numeric_data <- si_drivers %>%
  dplyr::select(Stream_Name, all_of(matched_columns)) %>%
  dplyr::select(where(is.numeric))

numeric_data <- numeric_data %>%
  mutate(Stream_Name = si_drivers$Stream_Name)

# Pivot numeric columns
numeric_long <- numeric_data %>%
  pivot_longer(
    cols = -Stream_Name,  # Exclude Stream_Name from pivoting
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    # Extract driver and year from variable names
    driver = str_extract(variable, paste(annual_vars, collapse = "|")),
    year = str_extract(variable, "\\d{4}") %>% as.numeric()
  ) %>%
  drop_na(driver, year)  # Remove rows with missing driver or year

# Convert value column in numeric_long to character
numeric_long <- numeric_long %>%
  mutate(value = as.character(value))

character_data <- si_drivers %>%
  dplyr::select(Stream_Name, all_of(matched_columns)) %>%
  dplyr::select(where(is.character))

character_data <- character_data %>%
  mutate(Stream_Name = si_drivers$Stream_Name)

# Pivot character columns
character_long <- character_data %>%
  pivot_longer(
    cols = -Stream_Name,  # Exclude Stream_Name from pivoting
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    # Extract driver and year from variable names
    driver = str_extract(variable, paste(annual_vars, collapse = "|")),
    year = str_extract(variable, "\\d{4}") %>% as.numeric()
  ) %>%
  drop_na(driver, year)  # Remove rows with missing driver or year

# Combine numeric and character data
year_cols <- bind_rows(numeric_long, character_long)

# Add units to the drivers
units_df <- data.frame(driver = annual_vars, unit = units_annual)

year_cols <- year_cols %>%
  left_join(units_df, by = "driver") %>% # Remove columns with .y
  dplyr::select(-contains(".y")) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.x$"))

# Define columns to exclude from numeric conversion
exclude_columns <- c("LTER", "Stream_Name", "Stream_ID", "ClimateZ", "Name", "cycle0") 

# Ensure one row per Stream_ID-Year combination
wide_drivers <- year_cols %>%
  select(Stream_Name, year, driver, value) %>% # Ensure only necessary columns
  pivot_wider(
    names_from = driver,           # Each driver becomes a column
    values_from = value            # Fill columns with values from `value`
  ) %>%
  rename(Year = year)              # Rename `year` to `Year` for consistency

# Join wide-format drivers with `tot` and selectively convert to numeric
tot_spatial <- tot_daylen %>%
  left_join(wide_drivers, by = c("Stream_Name", "Year")) %>%
  distinct(Stream_Name, Year, .keep_all = TRUE) %>%
  # Remove columns with .y
  dplyr::select(-contains(".y"), -cycle1) %>%
  # Rename columns with .x by removing the suffix
  rename_with(~ str_remove(., "\\.x$")) %>%
  # Convert only the relevant columns to numeric
  mutate(across(
    .cols = setdiff(colnames(.), exclude_columns), # Exclude specified columns
    .fns = ~ suppressWarnings(as.numeric(.)) # Convert to numeric, suppress warnings for non-convertible values
  ))

tot_spatial <- tot_spatial %>%
  mutate(
    cycle0 = as.Date(cycle0, origin = "1970-01-01") %>% 
      format("%j") %>% 
      as.numeric()
  )

## ------------------------------------------------------- ##
# Merge Processed Data ----
## ------------------------------------------------------- ##

# Combine annual data with other variables
character_vars <- c("elevation", "rock", "land", "slope", "permafrost")
character_cols_full <- si_drivers %>%
  select(matches(paste(character_vars, collapse = "|")), Stream_Name)

# Pivot wider for numeric annual data
drivers <- year_cols %>%
  pivot_wider(names_from = driver, values_from = value) %>%
  left_join(character_cols_full, by = "Stream_Name") %>%
  rename(Year=year)

# Merge with `tot` and `wrtds_df`, clean up column suffixes
tot_drivers <- tot_spatial %>%
  left_join(drivers, by = c("Stream_Name", "Year")) %>%
  # Remove columns with `.y`
  select(-contains(".y")) %>%
  # Rename columns with `.x` by removing the suffix
  rename_with(~ str_remove(., "\\.x$"))

# Capture `Stream_IDs` in the original `tot` dataframe
stream_ids_post_spatial_pre_NP <- tot_drivers %>%
  distinct(Stream_ID) %>%
  pull(Stream_ID)

original_stream_ids <- tot_drivers %>%
  distinct(Stream_ID) %>%
  pull(Stream_ID)

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
  ) %>%
  group_by(Stream_ID, Year, chemical) 

# Check non-NA data availability for DSi, NOx, and P
chemical_summary <- wrtds_NP %>%
  filter(chemical %in% c("NOx", "P")) %>% # Ensure these chemicals are included
  group_by(chemical) %>%
  summarise(
    Sites_With_Data = n_distinct(Stream_ID[!is.na(GenConc)]),
    Total_Sites = n_distinct(Stream_ID),
    .groups = "drop"
  )

# Print the summary
print("Summary of sites with non-NA data for NOx, and P:")
print(chemical_summary)


# Reshape data to wide format
wrtds_NP_wide <- wrtds_NP %>%
  pivot_wider(
    id_cols = c(Stream_ID, Year),  # Group by Stream_ID and Year
    names_from = chemical,         # Create separate columns for NOx and P
    values_from = GenConc,         # Populate these columns with GenConc values
    values_fill = list(GenConc = NA)  # Fill missing combinations with NA
  )


gc()

setDT(wrtds_NP_wide)

# Merge with the "tot" dataframe to add annual NOx and P data
tot_wrtds_NP <- tot_drivers %>%
  inner_join(wrtds_NP_wide, by = c("Stream_ID", "Year")) %>%
  dplyr::select(-contains(".y")) 

WRTDS_NP_stream_ids <- tot_wrtds_NP %>%
  distinct(Stream_ID) %>%
  pull(Stream_ID)

# ## ------------------------------------------------------- ##
#           # Import RAW N_P Data ---- 
# ## ------------------------------------------------------- ##
# Read in the dataset
raw_NP <- read.csv("20241003_masterdata_chem.csv") %>%
  mutate(
    Year = as.integer(format(as.Date(date, format = "%Y-%m-%d"), "%Y"))  # Extract Year from date
  ) %>%
  filter(Year >= 2001 & Year <= 2024)  # Filter for years between 2001 and 2024

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


# Check non-NA data availability for DSi, NOx, and P
chemical_summary <- raw_NP_median %>%
  filter(solute_simplified %in% c("NOx", "P")) %>% # Ensure these chemicals are included
  group_by(solute_simplified) %>%
  summarise(
    Sites_With_Data = n_distinct(Stream_ID[!is.na(median_value)]),
    Total_Sites = n_distinct(Stream_ID),
    .groups = "drop"
  )

# Print the summary
print("Summary of sites with non-NA data for NOx, and P:")
print(chemical_summary)


# Step 2: Reshape the data to wide format
raw_NP_wide <- raw_NP_median %>%
  pivot_wider(
    names_from = solute_simplified,  # Create columns for NOx and P
    values_from = median_value,      # Populate these columns with the median values
    values_fill = list(median_value = NA)  # Fill missing combinations with NA
  )

# Capture Stream_IDs before adding raw data
stream_ids_pre_raw_data <- tot_wrtds_NP %>%
  filter(is.na(NOx) | is.na(P)) %>%  # Identify sites with missing NOx or P
  distinct(Stream_ID) %>%
  pull(Stream_ID)


# ## ------------------------------------------------------- ##
#           # Match Data by Year ---- 
# ## ------------------------------------------------------- ##
# Merge raw N, P data into `tot`, matching years and replacing missing values
solutes <- c("P", "NOx")

for (solute in solutes) {
  raw_solute <- raw_NP_wide %>%
    select(Stream_ID, Year, !!sym(solute)) %>%
    rename(raw_solute = !!sym(solute))
  
  tot_raw_NP <- tot_wrtds_NP %>%
    mutate(Year = as.numeric(Year)) %>%  # Ensure Year is numeric for consistent merging
    left_join(raw_solute, by = c("Stream_ID", "Year")) %>%
    mutate(!!sym(solute) := ifelse(is.na(!!sym(solute)), raw_solute, !!sym(solute))) %>%
    select(-raw_solute)  # Remove the temporary column
}

# Capture Stream_IDs after adding raw data
stream_ids_post_raw_data <- tot_raw_NP %>%
  filter(!is.na(NOx) | !is.na(P)) %>%  # Identify sites where missing values were filled
  distinct(Stream_ID) %>%
  pull(Stream_ID)

# Identify gap-filled Stream_IDs
gap_filled_sites <- setdiff(stream_ids_post_raw_data, stream_ids_pre_raw_data)

# Print the list of gap-filled sites
print("Gap-filled Stream_IDs:")
print(gap_filled_sites)

# Optionally, save to a file for further analysis
write.csv(data.frame(Stream_ID = gap_filled_sites), "raw_NP_gap_filled_sites.csv", row.names = FALSE)

num_unique_stream_ids <- tot_raw_NP %>%
  pull(Stream_Name) %>%
  n_distinct()

print(num_unique_stream_ids) # We should still have 384 Sites (27 sites don't have any NP data)

raw_NP_stream_ids <- tot_raw_NP %>%
  distinct(Stream_ID) %>%
  pull(Stream_ID)

# See which sites we lost due to lack of N and P data: 
no_WRTDS_NP_sites <- setdiff(original_stream_ids, WRTDS_NP_stream_ids)
print(no_WRTDS_NP_sites)

no_NP_sites <- setdiff(original_stream_ids, raw_NP_stream_ids)
print(no_NP_sites)

## ------------------------------------------------------- ##
            #  Silicate Weathering ----
## ------------------------------------------------------- ##
# Read and prepare data
mapped_lithologies <- fread("mapped_lithologies.csv")

# Convert `tot` and `mapped_lithologies` to data.tables
setDT(tot_raw_NP)
setDT(mapped_lithologies)

# Ensure compatibility in the major_rock column for merging
tot_raw_NP[, major_rock := as.character(major_rock)]
mapped_lithologies[, major_rock := as.character(major_rock)]

# Perform the merge and filter out NA values in mapped_lithology
weathering <- merge(tot_raw_NP[, .(Stream_ID, Year, major_rock, Q, temp, drainSqKm)], 
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
tot_raw_NP[, Stream_ID := as.character(Stream_ID)]
tot_raw_NP[, Year := as.integer(Year)]
weathering[, Stream_ID := as.character(Stream_ID)]
weathering[, Year := as.integer(Year)]

# Calculate silicate weathering for each row in the weathering data
weathering[, silicate_weathering := calculate_weathering_vectorized(mapped_lithology, runoff, temp_K)]

# Ensure uniqueness in the datasets
weathering <- unique(weathering, by = c("Stream_ID", "Year"))
tot_raw_NP <- unique(tot_raw_NP, by = c("Stream_ID", "Year"))

# Perform the merge correctly
tryCatch({
  tot_weathering <- merge(
    tot_raw_NP,
    weathering[, .(Stream_ID, Year, silicate_weathering)],
    by = c("Stream_ID", "Year"),
    all.x = TRUE,
    allow.cartesian = FALSE
  )
}, error = function(e) {
  message("Error encountered during merge: ", e$message)
})

num_unique_stream_ids <- tot_weathering %>%
  pull(Stream_Name) %>%
  n_distinct()

print(num_unique_stream_ids) # We should still have 384 Sites


# Clean up memory
gc()

## ------------------------------------------------------- ##
          #  Gap Filling Missing Data ----
## ------------------------------------------------------- ##
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
tot_with_na_slope <- tot_weathering %>%
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

# Manually update specific values
tot_with_slope_filled <- tot_with_slope_filled %>%
  mutate(
    basin_slope_mean_degree = case_when(
      Stream_ID == "Walker Branch__east fork" ~ 2.2124321596241265,
      Stream_ID == "Walker Branch__west fork" ~ 1.8972192246291828,       
      TRUE ~ basin_slope_mean_degree               # Retain existing values
    )
  )

# Convert to data.table for efficient key-based operations
tot_with_slope_filled <- as.data.table(tot_with_slope_filled)
tot_weathering <- as.data.table(tot_weathering)

# Set keys for efficient join
setkey(tot_weathering, Stream_ID)
setkey(tot_with_slope_filled, Stream_ID)

tot_weathering[tot_with_slope_filled, basin_slope_mean_degree := 
      ifelse(is.na(basin_slope_mean_degree), i.basin_slope_mean_degree, basin_slope_mean_degree),
    on = .(Stream_ID)]

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
tot_with_na_elev <- tot_weathering %>%
  filter(is.na(elevation_mean_m)) %>%
  select(Stream_ID)

# Merge tot_with_na_elev with US_elev to fill missing elevation values
tot_with_elev_filled <- tot_with_na_elev %>%
  left_join(US_elev %>% select(Stream_ID, elevation_mean_m), by = "Stream_ID")

# Update tot with the filled elevation values
tot_gap_filled <- tot_weathering %>%
  left_join(tot_with_elev_filled, by = "Stream_ID", suffix = c("", "_filled"), relationship = "many-to-many") %>%
  mutate(
    elevation_mean_m = coalesce(elevation_mean_m_filled, elevation_mean_m)
  ) %>%
  select(-elevation_mean_m_filled)  # Remove Stream_Name_filled from final output

# Tidy data for export: 
tot_annual <- tot_gap_filled %>%
  dplyr::select(Stream_ID, Year, drainSqKm, NOx, P, precip, Q,
                temp, Max_Daylength, prop_area, npp, evapotrans,
                silicate_weathering, cycle0, permafrost_mean_m, elevation_mean_m, 
                basin_slope_mean_degree, FNConc, FNYield, GenConc, GenYield, 
                contains("rocks"), contains("land_")) %>%
  dplyr::rename(
                elevation = elevation_mean_m,
                permafrost = permafrost_mean_m,
                greenup_day = cycle0,
                basin_slope = basin_slope_mean_degree) %>%
  dplyr::mutate(permafrost = ifelse(is.na(permafrost), 0, permafrost)) 

tot_annual <- tot_annual %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)))


# Export annual data
write.csv(as.data.frame(tot_annual), "AllDrivers_Harmonized_Yearly.csv")

# Create the tot_average dataframe
tot_average <- tot_annual %>%
  group_by(Stream_ID) %>%
  summarise(
    # Numerical variables: calculate the mean across all years
    drainSqKm = mean(drainSqKm, na.rm = TRUE),
    NOx = mean(NOx, na.rm = TRUE),
    P = mean(P, na.rm = TRUE),
    precip = mean(precip, na.rm = TRUE),
    Q = mean(Q, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    Max_Daylength = mean(Max_Daylength, na.rm = TRUE),
    prop_area = mean(prop_area, na.rm = TRUE),
    npp = mean(npp, na.rm = TRUE),
    evapotrans = mean(evapotrans, na.rm = TRUE),
    silicate_weathering = mean(silicate_weathering, na.rm = TRUE),
    greenup_day = mean(greenup_day, na.rm = TRUE),
    permafrost = mean(permafrost, na.rm = TRUE),
    elevation = mean(elevation, na.rm = TRUE),
    basin_slope = mean(basin_slope, na.rm = TRUE),
    FNConc = mean(FNConc, na.rm = TRUE),
    FNYield = mean(FNYield, na.rm = TRUE),
    GenConc = mean(GenConc, na.rm = TRUE),
    GenYield = mean(GenYield, na.rm = TRUE),
    # Categorical variables: grab the first value
    across(contains("rocks"), ~ first(.)),
    across(contains("land_"), ~ first(.))
  ) %>%
  ungroup() %>%
  # Replace NaN with NA in all columns
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))


# Export annual data
write.csv(as.data.frame(tot_annual), "AllDrivers_Harmonized_Average.csv")



