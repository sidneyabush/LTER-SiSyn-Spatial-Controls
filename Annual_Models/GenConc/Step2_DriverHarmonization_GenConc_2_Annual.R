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
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate)

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

# Remove "MMDD" from column names to standardize the format
colnames(spatial_drivers_with_years) <- sub("MMDD$", "", colnames(spatial_drivers_with_years))

# Convert all columns except Stream_ID to numeric where possible, coercing characters to NA
spatial_drivers_with_years <- spatial_drivers_with_years %>%
  mutate(across(-Stream_ID, as.numeric))




# Step 1: Remove rows with NA values in relevant columns (Stream_ID, year, driver_type, or value)
spatial_drivers_long_clean <- spatial_drivers_long %>%
  filter(!is.na(Stream_ID) & !is.na(year) & !is.na(driver_type) & !is.na(value))

# Step 2: Confirm `year` is treated consistently as an integer
spatial_drivers_long_clean <- spatial_drivers_long_clean %>%
  mutate(year = as.integer(year))

# Step 3: Create the unique identifier column
spatial_drivers_long_clean <- spatial_drivers_long_clean %>%
  mutate(unique_id = paste(Stream_ID, year, driver_type, sep = "_"))

# Step 4: Confirm data structure before pivoting
print("Data structure after cleaning:")
str(spatial_drivers_long_clean)

# Step 5: Pivot to wide format using unique_id as the key
spatial_drivers_wide <- spatial_drivers_long_clean %>%
  pivot_wider(
    id_cols = c(Stream_ID, year),   # Keep Stream_ID and year as identifiers
    names_from = unique_id,         # Use unique_id for each driver_type and year combination
    values_from = value             # Fill values from the value column
  )

### Need to revisit this later







spatial_drivers_no_years <- spatial_drivers[, !colnames(spatial_drivers) %in% year_columns, drop = FALSE]  # Columns without numbers
# Retain Stream_ID and columns without numbers in spatial_drivers_no_years
spatial_drivers_no_years <- spatial_drivers %>%
  select(Stream_ID, all_of(non_year_columns))














# Combine spatial drivers and categorical variables
spatial_vars <- cbind(spatial_drivers, cat_vars)

# Define the pattern to search for elevation and basin slope columns
relevant_columns <- c("elevation", "basin")

# Use grep to locate elevation and basin slope columns
elevation_basin_cols <- grep(paste(relevant_columns, collapse = "|"), colnames(spatial_drivers))

## Now need to add in basin slope and elevation for missing sites: 

# Combine major categorical columns with elevation and basin slope
cat_vars <- spatial_drivers[, c(major_cat_vars, elevation_basin_cols)]


# Add Stream_ID back to the resulting data
cat_vars$Stream_ID <- spatial_drivers$Stream_ID

# These are the quantitative drivers
drivers_list_quant <- c("num_days", "prop_area", "precip", "evapotrans", "temp", "npp", "permafrost")

# Add in Greenup day
greenup <- c("cycle0", "cycle1")
site_mean <- list()

for (i in 1:length(drivers_list_quant)) {
  drive_cols <- grep(drivers_list_quant[i], colnames(spatial_drivers))
  one_driver <- spatial_drivers[,c(301, drive_cols)]
  site_mean[[i]] <- rowMeans(one_driver[,c(2:length(one_driver))], na.rm = TRUE)
}

mean_df <- as.data.frame(do.call(cbind, site_mean))
colnames(mean_df) <- drivers_list_quant
mean_df$Stream_ID <- spatial_drivers$Stream_ID

## Get mean greenup day
greenup_mean <- list()
stream_id <- c("Stream_ID")
for (i in 1:length(greenup)) {
  drive_cols <- grep(greenup[i], colnames(spatial_drivers))
  one_driver <- spatial_drivers[,c(301, drive_cols)]
  one_driver <- one_driver[!duplicated(one_driver$Stream_ID),]
  
  driver_melt <- melt(one_driver, id.vars=stream_id)
  
  driver_melt$doy <- yday(as.Date(driver_melt$value, "%Y-%m-%d"))
  one_driver <- dcast(driver_melt, Stream_ID~variable, value.var = "doy")
  greenup_mean[[i]] <- rowMeans(one_driver[,c(2:length(one_driver))], na.rm = TRUE)
}

green_df <- as.data.frame(do.call(cbind, greenup_mean))

colnames(green_df) <- greenup
green_df$Stream_ID <- one_driver$Stream_ID

mean_df <- merge(mean_df, green_df, by="Stream_ID", all = TRUE)
mean_df <- merge(mean_df, cat_vars, by="Stream_ID", all=TRUE)

tot <- merge(tot, mean_df, by="Stream_ID")

# unique(tot$Stream_ID)

# ## ------------------------------------------------------- ##
#           # Import WRTDS N_P Conc & Flux ---- 
# ## ------------------------------------------------------- ##
# # Load N and P data -- concentrations, can be Raw or WRTDS
# N_P_conc <- read.csv("Median_NP_WRTDS_Conc_2.csv")
# N_P_conc_cast <- dcast(N_P_conc, Stream_Name~solute_simplified, value.var = "median_Conc", fun.aggregate = mean)
# 
# tot <- merge(tot, N_P_conc_cast, by="Stream_Name")


# ## ------------------------------------------------------- ##
#           # Import RAW N_P Conc ---- 
# ## ------------------------------------------------------- ##
N_P_conc_raw <- read.csv("Median_NP_Raw_Conc_2.csv")
N_P_conc_raw_cast <- dcast(N_P_conc_raw, Stream_Name~solute_simplified,
                           value.var = "median_val", fun.aggregate = mean)

tot <- merge(tot, N_P_conc_raw_cast, by="Stream_Name")

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
tot <- tot[!duplicated(tot$Stream_Name),]

write.csv(tot, "AllDrivers_Harmonized_20241108_WRTDS_MD_KG_rawNP_GenConc.csv")
# write.csv(tot, "AllDrivers_Harmonized_20241108_WRTDS_MD_KG_NP_GenConc_Average.csv")
