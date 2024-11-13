## ------------------------------------------------------- ##
# Silica WG - Harmonize Drivers
## ------------------------------------------------------- ##
# Written by:
## Sidney A Bush, Keira Johnson

# Purpose:
## Combine (Harmonize) KG Class, Daylength, WRTDS Chem/Q
## Calculate Discharge metrics (min, max, median, q5, q95)

## ------------------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------------------- ##
# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr)

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
  dplyr::mutate(DecYear = as.Date(format(date_decimal(DecYear), "%Y-%m-%d")),
                Year = format(DecYear, "%Y"),
                Stream_ID = paste0(LTER.x, "__", Stream_Name))


# ## Change names: This will need to be updated later with the inclusion of these 5 sites in WRTDS Outputs
# ## LATER: incorporate all hard-coded name changes into the final step of the combine drivers workflow so that
# ## harmonization scripts just harmonize over desired time frame (e.g, annual, average, etc.)
wrtds_df <- wrtds_df %>%
  mutate(Stream_ID = case_when(
    #Stream_ID=="Catalina Jemez__MG_WEIR"~"Catalina Jemez__Marshall Gulch",
    #Stream_ID=="Catalina Jemez__OR_low"~"Catalina Jemez__Oracle Ridge",
    #Stream_ID=="NWT__COMO"~"NWT__Como Creek",
    Stream_ID=="Walker Branch__East Fork"~"Walker Branch__east fork",
    Stream_ID=="Walker Branch__West Fork"~"Walker Branch__west fork",
    .default = Stream_ID
  ))

## Need to tidy the Finnish site names:
finn <- read.csv("FinnishSites.csv")

finn$Stream_ID <- paste0("Finnish Environmental Institute__", finn$Site.ID)
finn$Stream_ID2 <- paste0("Finnish Environmental Institute__", finn$Site)

for (i in 1:nrow(finn)) {
  site_id<-finn[i,3]
  row_num<-which(wrtds_df$Stream_ID==site_id)
  wrtds_df[row_num, "Stream_ID"]<-finn[i,4]
}

wrtds_df$Stream_ID<-ifelse(wrtds_df$Stream_ID=="Finnish Environmental Institute__TORNIONJ KUKKOLA 14310  ",
                           "Finnish Environmental Institute__TORNIONJ KUKKOLA 14310", wrtds_df$Stream_ID)

wrtds_df$Stream_ID<-ifelse(wrtds_df$Stream_ID=="Finnish Environmental Institute__SIMOJOKI AS. 13500      ",
                           "Finnish Environmental Institute__SIMOJOKI AS. 13500", wrtds_df$Stream_ID)


## ------------------------------------------------------- ##
# Calculate DSi Stats ----
## ------------------------------------------------------- ##
## Subset to just Dsi:
wrtds_df <- subset(wrtds_df, chemical == "DSi")

## ------------------------------------------------------- ##
# Calculate annual statistics for Flow Normalized Si Concentrations ----
## ------------------------------------------------------- ##
si_stats <- wrtds_df %>%
  #dplyr::mutate(Year = floor(DecYear)) %>%  # Convert DecYear to Year
  dplyr::group_by(Stream_ID, Year) %>%
  dplyr::summarise(
    mean_si = mean(GenConc, na.rm = TRUE),
    med_si = median(GenConc, na.rm = TRUE),
    #sd_si = sd(GenConc, na.rm = TRUE),
    min_si = min(GenConc, na.rm = TRUE),
    max_si = max(GenConc, na.rm = TRUE),
    .groups = 'drop'
  )

## ------------------------------------------------------- ##
# Calculate Q Stats ----
## ------------------------------------------------------- ##
q_stats <- wrtds_df %>%
  #dplyr::mutate(Year = floor(DecYear)) %>%  # Convert DecYear to Year
  dplyr::group_by(Stream_ID, Year) %>%
  dplyr::summarise(
    mean_q = mean(Q, na.rm = TRUE),
    med_q = median(Q, na.rm = TRUE),
    sd_q = sd(Q, na.rm = TRUE),
    min_Q = min(Q, na.rm = TRUE),
    max_Q = max(Q, na.rm = TRUE),
    q_95 = quantile(Q, 0.95, na.rm = TRUE),
    q_5 = quantile(Q, 0.05, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate coefficient of variation for each group
q_stats <- q_stats %>%
  dplyr::mutate(CV_Q = sd_q / mean_q)

cv_tot <- merge(si_stats, q_stats, by=c("Stream_ID", "Year"))

## ------------------------------------------------------- ##
# Calculate annual minimum Q ----
## ------------------------------------------------------- ##
q_min_stats <- wrtds_df %>%
  #dplyr::mutate(Year = floor(DecYear)) %>%  # Convert DecYear to Year
  dplyr::group_by(Stream_ID, Year) %>%
  dplyr::slice_min(Q, with_ties = FALSE)  # `with_ties = FALSE` to get a single minimum per year

cv_tot <- merge(cv_tot, q_min_stats, by=c("Stream_ID", "Year"))

## ------------------------------------------------------- ##
# Calculate annual maximum Q ----
## ------------------------------------------------------- ##
q_max_stats <- wrtds_df %>%
  #dplyr::mutate(Year = floor(DecYear)) %>%  # Convert DecYear to Year
  dplyr::group_by(Stream_ID, Year) %>%
  dplyr::slice_max(Q, with_ties = FALSE)  # `with_ties = FALSE` to get a single maximum per year

cv_tot <- merge(cv_tot, q_max_stats, by=c("Stream_ID", "Year"))

## ------------------------------------------------------- ##
# Add in KG Classifications ----
## ------------------------------------------------------- ##
# Read in climate data produced in KoeppenGeigerClassification.R
KG <- read.csv("Koeppen_Geiger_2.csv")
KG$Stream_ID <- paste0(KG$LTER, "__", KG$Stream_Name)

tot <- merge(cv_tot, KG, by="Stream_ID")
tot <- tot[!duplicated(tot$Stream_ID),]

## ------------------------------------------------------- ##
# Download Reference Table from GD for DA ----
## ------------------------------------------------------- ##
ref_table_link <- "https://docs.google.com/spreadsheets/d/11t9YYTzN_T12VAQhHuY5TpVjGS50ymNmKznJK4rKTIU/edit#gid=357814834"
ref_table_folder = drive_get(as_id(ref_table_link))
ref_table <- drive_download(ref_table_folder$drive_resource, overwrite = T)

ref_table <- readxl::read_xlsx("Site_Reference_Table.xlsx")
ref_table$Stream_ID <-paste0(ref_table$LTER, "__", ref_table$Stream_Name)
area <-ref_table[,c("drainSqKm", "Stream_ID")]

# Define renamed and old names directly
name_conversion <- data.frame(
  # Stream_ID = c("Catalina Jemez__MG_WEIR", "Catalina Jemez__OR_low", "NWT__COMO", "Walker Branch__East Fork", "Walker Branch__West Fork"),
  Stream_ID = c("Walker Branch__East Fork", "Walker Branch__West Fork"),
  # Updated_StreamName = c("Catalina Jemez__Marshall Gulch", "Catalina Jemez__Oracle Ridge", "NWT__Como Creek", "Walker Branch__east fork", "Walker Branch__west fork")
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

tot <- merge(tot, area, by="Stream_ID")
tot <- tot[!duplicated(tot$Stream_ID),]

## ------------------------------------------------------- ##
# Import Spatial Drivers ----
## ------------------------------------------------------- ##
spatial_drivers <- read.csv("all-data_si-extract_2_20240802.csv", stringsAsFactors = FALSE)
spatial_drivers$Stream_ID <- paste0(spatial_drivers$LTER, "__", spatial_drivers$Stream_Name)

## Before, using full abbrevs removed some of the spatial driver columns (e.g., "dec" in "deciduous" was causing
#  deciduous land cover to be filtered out)
months_abb <- c("_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_")

## Pull out the monthly drivers, plus the column that contains "Stream_ID"
monthly_drivers <- spatial_drivers[,c(361,which(colnames(spatial_drivers) %like% months_abb))]

# Remove monthly drivers from spatial drivers
spatial_drivers <- spatial_drivers[,-c(which(colnames(spatial_drivers) %like% months_abb))]

# Identify columns with any numbers in the header
year_columns <- grep("[0-9]", colnames(spatial_drivers), value = TRUE)
# Identify columns without numbers, ensuring Stream_ID is included
non_year_columns <- colnames(spatial_drivers)[!colnames(spatial_drivers) %in% year_columns]

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

spatial_drivers_no_years <- spatial_drivers[, !colnames(spatial_drivers) %in% year_columns, drop = FALSE]  # Columns without numbers
# Retain Stream_ID and columns without numbers in spatial_drivers_no_years
spatial_drivers_no_years <- spatial_drivers %>%
  select(Stream_ID, all_of(non_year_columns))

# Perform a left join to combine the wide annual data with the non-annual data
combined_spatial_drivers <- spatial_drivers_wide %>%
  left_join(spatial_drivers_no_years, by = "Stream_ID")

## ADD IN GREENUP DAY: ----

# Define greenup cycles
greenup_cycles <- c("cycle0", "cycle1")

# Step 1: Standardize column names by removing "MMDD" if present
colnames(spatial_drivers) <- sub("MMDD$", "", colnames(spatial_drivers))

# Convert date columns to day-of-year values
for (cycle in greenup_cycles) {
  # Identify `greenup` columns for each cycle
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

# Step 3: Reshape `greenup` data for each cycle
greenup_long <- list()

for (cycle in greenup_cycles) {
  # Select columns starting with `greenup` and ending with `_doy` that match the current cycle
  cycle_doy_cols <- grep(paste0("greenup_", cycle, ".*_doy$"), colnames(spatial_drivers), value = TRUE)
  
  # Print selected columns to verify the correct ones are being used
  print(paste("Selected columns for", cycle, ":"))
  print(cycle_doy_cols)
  
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
        year = as.integer(sub(".*_(\\d{4}).*", "\\1", variable)),  # Extract year
        cycle = cycle
      ) %>%
      select(Stream_ID, year, cycle, greenup_day)
  } else {
    warning(paste("No matching columns found for cycle:", cycle))
  }
}

# Combine `greenup_long` data for each cycle
greenup_df <- bind_rows(greenup_long$cycle0, greenup_long$cycle1)

## ------------------------------------------------------- ##
# Final Merge with Combined Data ----
## ------------------------------------------------------- ##

# Convert `Year` in `tot` to integer and rename if necessary
tot <- tot %>%
  mutate(Year = as.integer(Year))  # Ensure `Year` is integer

# Rename `year` to `Year` in `combined_spatial_drivers` to match `tot`
combined_spatial_drivers <- combined_spatial_drivers %>%
  dplyr::mutate(Year = as.integer(year))

# Merge `tot` with `combined_spatial_drivers`
final_combined_data <- tot %>%
  left_join(combined_spatial_drivers, by = c("Stream_ID", "Year"))

# Merge `greenup_df`, renaming `year` to `Year` if needed
greenup_df <- greenup_df %>%
  rename(Year = year)  # Ensure column names match

tot <- final_combined_data %>%
  left_join(greenup_df, by = c("Stream_ID", "Year"))

## Need to clean after all this mergin: 
# Remove columns ending with ".y" in `tot`
tot <- tot %>%
  dplyr::select(-ends_with(".y"), -ends_with(".x.x"), -ends_with(".y.x")) %>%
  dplyr::select(-44, -"cycle") %>%
  rename_with(~ str_replace(., "\\.x$", ""), ends_with(".x")) 

## Check how many unique streams we have before merging with N and P (403 streams)
num_unique_streams <- tot %>% 
  summarise(unique_streams = n_distinct(Stream_Name)) %>%
  pull(unique_streams)

print(num_unique_streams)  

## total is 395 going into N and P merge

# ## ------------------------------------------------------- ##
#           # Import WRTDS N_P Conc & Flux ---- 
# ## ------------------------------------------------------- ##
# Load N and P data -- concentrations, can be Raw or WRTDS
N_P_conc <- read.csv("Median_NP_WRTDS_GenConc_2_Annual.csv") %>%
  dplyr::select(-"X", -"chemical")

num_unique_streams <- N_P_conc %>% 
  summarise(unique_streams = n_distinct(Stream_Name)) %>%
  pull(unique_streams)

print(num_unique_streams)  

N_P_conc_wide <- N_P_conc %>%
  pivot_wider(names_from = solute_simplified, values_from = median_Conc) %>%
  mutate(across(everything(), ~ na_if(as.character(.), "NULL"))) %>%
  # Optionally, convert back to numeric where appropriate
  mutate(across(where(is.character), ~ type.convert(., as.is = TRUE)))

tot <- merge(tot, N_P_conc_wide, by = c("Stream_Name", "Year"), all.x = TRUE)

num_unique_streams <- tot %>% 
  summarise(unique_streams = n_distinct(Stream_Name)) %>%
  pull(unique_streams)

print(num_unique_streams)  

# ## ------------------------------------------------------- ##
#           # Import RAW N_P Conc ---- 
# # ## ------------------------------------------------------- ##
N_P_conc_raw <- read.csv("Median_NP_Raw_Conc_2_Annual.csv") %>%
  dplyr::select(-"X")

# Reshape data using pivot_wider
N_P_conc_raw_cast <- N_P_conc_raw %>%
  pivot_wider(names_from = solute_simplified, values_from = annual_median_Conc)

num_unique_streams <- N_P_conc_raw_cast %>% 
  summarise(unique_streams = n_distinct(Stream_Name)) %>%
  pull(unique_streams)

print(num_unique_streams)  

tot <- merge(tot, N_P_conc_raw_cast, by= c("Stream_Name", "Year"), all.x = TRUE)

## ------------------------------------------------------- ##
          # Import Daylength ----
## ------------------------------------------------------- ##
daylen<-read.csv("Monthly_Daylength_2.csv")
daylen<-daylen[,-1]

daylen_range<-daylen %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(min_day=min(mean_daylength), max_len=max(mean_daylength))

colnames(daylen_range)<-c("Stream_Name", "Min_Daylength", "Max_Daylength")
# Define renamed and old names directly
name_conversion <- data.frame(
  Stream_Name = c("MG_WEIR", "OR_low", "COMO", "East Fork", "West Fork"),
  Updated_StreamName = c("Marshall Gulch", "Oracle Ridge", "Como Creek", "east fork", "west fork")
)

# Filter, join, remove old Stream_Name, and rename in one step
missing_sites <- daylen_range %>%
  filter(Stream_Name %in% name_conversion$Stream_Name) %>%
  left_join(name_conversion, by = "Stream_Name") %>%
  mutate(Stream_Name = Updated_StreamName) %>%
  select(-Updated_StreamName)

# Append the updated sites to the original dataframe
daylen_range <- bind_rows(daylen_range, missing_sites)

tot <-merge(tot, daylen_range, by="Stream_Name")
# tot <- tot[!duplicated(tot$Stream_Name),]

write.csv(tot, "AllDrivers_Harmonized_20241112_WRTDS_MD_KG_rawNP_GenConc_Annual.csv")
# write.csv(tot, "AllDrivers_Harmonized_20241112_WRTDS_MD_KG_NP_GenConc_Annual.csv")
