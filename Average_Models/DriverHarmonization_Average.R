## ------------------------------------------------------- ##
      # Silica WG - Harmonize Drivers: Average Model
## ------------------------------------------------------- ##
# Written by:
## Sidney A Bush, Keira Johnson

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
  rename(LTER = LTER.x) %>%
  select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y, -contains("date"), -contains("month"), -min_year, -max_year, -duration) %>%
  dplyr::mutate(DecYear = as.Date(format(date_decimal(DecYear), "%Y-%m-%d")),
                Year = format(DecYear, "%Y"),
                Stream_ID = paste0(LTER, "__", Stream_Name))


wrtds_df <- wrtds_df %>%
  mutate(Stream_ID = case_when(
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
wrtds_df <- wrtds_df %>%
  left_join(area %>% select(Stream_ID), by = "Stream_ID", relationship = "many-to-many")

## ------------------------------------------------------- ##
            # Calculate Yields ----
## ------------------------------------------------------- ##
wrtds_df <- wrtds_df %>%
  mutate(FNYield = FNFlux / drainSqKm,
         GenYield = GenFlux / drainSqKm) %>%
  select(-FNFlux, -GenFlux)

## ------------------------------------------------------- ##
          # Calculate Stats ----
## ------------------------------------------------------- ##
# Calculate si_stats and q_stats individually with CV columns
# Do this just for Si for now: 
wrtds_si <- subset(wrtds_df, chemical == "DSi")

si_stats <- wrtds_si %>%
  group_by(Stream_ID) %>%
  summarise(
    across(
      c(FNConc, GenConc, GenYield, FNYield), 
      list(mean = mean, median = median, sd = sd, min = min, max = max), 
      .names = "{.fn}_si_{.col}"
    )
  ) %>%
  mutate(
    across(
      ends_with("_si_mean"), 
      ~ get(sub("mean", "sd", cur_column())) / ., 
      .names = "CV_{.col}"
    )
  )

q_stats <- wrtds_si %>%
  group_by(Stream_ID) %>%
  summarise(
    mean_q = mean(Q), 
    med_q = median(Q), 
    sd_q = sd(Q), 
    min_Q = min(Q), 
    max_Q = max(Q), 
    q_95 = quantile(Q, 0.95), 
    q_5 = quantile(Q, 0.05),
    CV_Q = sd_q / mean_q
  )

# Merge si_stats and q_stats and calculate CVC/CVQ in the same pipeline
cv_tot <- si_stats %>%
  left_join(q_stats, by = "Stream_ID") %>%
  mutate(cvc_cvq = across(starts_with("CV_si"), ~ . * CV_Q, .names = "CVC_CVQ_{.col}"))

tot <- cv_tot %>%
  left_join(wrtds_si, by = "Stream_ID")

## ------------------------------------------------------- ##
# Add in KG Classifications ----
## ------------------------------------------------------- ##
# Read in climate data produced in KoeppenGeigerClassification.R
KG <- read.csv("Koeppen_Geiger_2.csv")
KG$Stream_ID <- paste0(KG$LTER, "__", KG$Stream_Name)

tot <- left_join(cv_tot, KG, by="Stream_ID", relationship = "many-to-many")

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

# These are the categorical variables like "major_land", "major_soil", and "major_rock" and the numerical % of each
major_cat_vars <- which(colnames(spatial_drivers) %like% c("soil|land|rock"))
cat_vars <- spatial_drivers[,c(major_cat_vars)]

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
#           # Import WRTDS N_P Conc ---- 
# ## ------------------------------------------------------- ##
# Load N and P data -- concentrations, can be Raw or WRTDS
N_P_conc <- read.csv("Median_NP_WRTDS_Conc_2.csv")
N_P_conc_cast <- dcast(N_P_conc, Stream_Name~solute_simplified, value.var = "median_Conc", fun.aggregate = mean)

tot <- merge(tot, N_P_conc_cast, by="Stream_Name")

# ## ------------------------------------------------------- ##
#           # Import RAW N_P Conc ---- 
# ## ------------------------------------------------------- ##
# N_P_conc_raw <- read.csv("Median_NP_Raw_Conc_2.csv")
# N_P_conc_raw_cast <- dcast(N_P_conc_raw, Stream_Name~solute_simplified,
#                            value.var = "median_val", fun.aggregate = mean)
# 
# tot <- merge(tot, N_P_conc_raw_cast, by="Stream_Name")

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

# write.csv(tot, "AllDrivers_Harmonized_20241108_WRTDS_MD_KG_rawNP_FNConc.csv")
write.csv(tot, "AllDrivers_Harmonized_20241108_WRTDS_MD_KG_NP_FNConc_Average.csv")
