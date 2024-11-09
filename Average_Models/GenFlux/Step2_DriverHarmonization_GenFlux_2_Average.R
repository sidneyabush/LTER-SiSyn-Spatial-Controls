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

## Using Flux
si_stats <- wrtds_df %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::summarise(mean_si = mean(GenFlux), med_si=median(GenFlux), sd_si=sd(GenFlux), 
                   min_Si=min(GenFlux), max_Si=max(GenFlux))

si_stats$CV_C <- si_stats$sd_si/si_stats$mean_si

## Using Flow Normalized Si Fluxes
flux_stats <- wrtds_df %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::summarise(mean_flux = mean(FNFlux), med_flux=median(FNFlux), 
                   sd_flux=sd(FNFlux), min_flux=min(FNFlux), max_flux=max(FNFlux))

flux_stats$CV_C_flux <- flux_stats$sd_flux/flux_stats$mean_flux

# si_stats_annual <- wrtds_df %>%
#   dplyr::group_by(Stream_ID, year(as.Date(Date))) %>%
#   dplyr::summarise(an_mean_si = mean(value_mgL), an_med_si=median(value_mgL), an_sd_si=sd(value_mgL))

## ------------------------------------------------------- ##
# Calculate Q Stats ----
## ------------------------------------------------------- ##
q_stats <- wrtds_df %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::summarise(mean_q = mean(Q), med_q=median(Q), sd_q=sd(Q), min_Q=min(Q), max_Q=max(Q),
                   q_95=quantile(Q, 0.95), q_5=quantile(Q, 0.05))

q_min_stats <- wrtds_df %>%
  dplyr::group_by(Stream_ID, Year) %>%
  dplyr::slice_min(Q)

q_max_stats <- wrtds_df %>%
  dplyr::group_by(Stream_ID, Year) %>%
  dplyr::slice_max(Q)

## Can use this only when using daily data:
# q_min_stats$doy <- format(as.Date(q_min_stats$Date),"%j")
# q_max_stats$doy <- format(as.Date(q_max_stats$Date),"%j")
# 
# min_day <- q_min_stats %>%
#   dplyr::group_by(Stream_ID) %>%
#   dplyr::summarise(q_min_day=round(median(as.numeric(doy)),0))
# 
# max_day <- q_max_stats %>%
#   dplyr::group_by(Stream_ID) %>%
#   dplyr::summarise(q_max_day=round(median(as.numeric(doy)),0))

## Calculate annual stats (when not using annual WRTDS data)
# q_stats_annual <- q_df %>%
#   dplyr::group_by(Stream_ID, year(as.Date(Date))) %>%
#   dplyr::summarise(an_mean_q = mean(Q), an_med_q=median(Q), an_sd_q=sd(Q))

q_stats$CV_Q <- q_stats$sd_q/q_stats$mean_q

## ------------------------------------------------------- ##
#           Combine to get C-Q Stats ----
## ------------------------------------------------------- ##
#merge them into one
cv_tot <- merge(si_stats, q_stats, by= "Stream_ID")
cv_tot <- merge(cv_tot, flux_stats, by= "Stream_ID")
# cv_tot <- merge(cv_tot, max_day, by="Stream_ID")
# cv_tot <- merge(cv_tot, min_day, by="Stream_ID")

#calculate CVC/CVQ
cv_tot$cvc_cvq <- (cv_tot$CV_C)*(cv_tot$CV_Q)

## remove any NA values: 
cv_tot <- na.omit(cv_tot)

## This is dumb, but doing this for now to keep current workflow: 
## Split between C and Q datasets: 
#extract needed columns
chem_df <- wrtds_df[,c("LTER.x", "Stream_ID", "chemical", "DecYear","GenFlux", "FNFlux")]
q_df <- wrtds_df[,c("LTER.x", "Stream_ID", "chemical", "DecYear","Q")]

#get list of streams
streams <- unique(cv_tot$Stream_ID)

#open list to calculate slope and r2 of each stream
slope_list <- list()
rsquared_list <- list()

#calculate CQ slope and CQ r2 for Si-Q for each stream
for (i in 1:length(streams)) {
  si <- subset(chem_df, chem_df$Stream_ID == streams[i])
  si$Date <- as.Date(si$DecYear)
  q <- subset(q_df, q_df$Stream_ID == streams[i])
  q$Date <- as.Date(q$DecYear)
  # combine silica and discharge data
  tot <- merge(si, q, by="DecYear")
  # remove zero values for flow and Si GenFlux
  tot <- tot[tot$Q > 0,]
  tot <- tot[tot$GenFlux > 0,]
  # calculate CQ
  lm1 <- lm(log(GenFlux)~log(Q), tot)
  sum <- summary(lm1)
  # create list for slopes
  slope_list[[i]] <- sum$coefficients[2,1]
  # create list for r squared values
  rsquared_list[[i]] <- sum$r.squared
  
}

#put slopes into df
slope_df <- as.data.frame(do.call(rbind, slope_list))
slope_df$Stream_ID <- streams
colnames(slope_df)[1] <- "C_Q_slope"

#put r2 into df
r2_df <- as.data.frame(do.call(rbind, rsquared_list))
r2_df$Stream_ID <- streams
colnames(r2_df)[1]<-"r2"

# add slope dataframe to cv_tot dataframe
cv_tot <- merge(cv_tot, slope_df)

# cv_tot <- merge(cv_tot, si_stats_annual, by="Stream_ID")
# colnames(cv_tot)[12]<-"Year"
# colnames(q_stats_annual)[2]<-"Year"
# 
# cv_tot <- merge(cv_tot, q_stats_annual, by=c("Stream_ID", "Year"))

## ------------------------------------------------------- ##
# Add in KG Classifications ----
## ------------------------------------------------------- ##
# Read in climate data produced in KoeppenGeigerClassification.R
KG <- read.csv("Koeppen_Geiger_2.csv")
KG$Stream_ID<-paste0(KG$LTER, "__", KG$Stream_Name)

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
#           # Import WRTDS N_P Conc & Flux ---- 
# ## ------------------------------------------------------- ##
# Load N and P data -- concentrations, can be Raw or WRTDS
# N_P_conc <- read.csv("Median_NP_WRTDS_GenConc_2.csv")
# N_P_conc_cast <- dcast(N_P_conc, Stream_Name~solute_simplified, value.var = "median_Conc", fun.aggregate = mean)
# 
# tot <- merge(tot, N_P_conc_cast, by="Stream_Name")

## Load N and P data -- yields
N_P_flux <- read.csv("Median_NP_WRTDS_GenFlux_2.csv")
N_P_flux_cast <- dcast(N_P_flux, Stream_Name~solute_simplified, value.var = "median_Flux", fun.aggregate = mean)

tot <- merge(tot, N_P_flux_cast, by="Stream_Name")

# ## ------------------------------------------------------- ##
#           # Import RAW N_P Conc ---- 
#             Calculate flux later?
# ## ------------------------------------------------------- ##
# N_P_flux_raw <- read.csv("Median_NP_Raw_Conc_2.csv")
# N_P_flux_raw_cast <- dcast(N_P_flux_raw, Stream_Name~solute_simplified,
#                            value.var = "median_val", fun.aggregate = mean)
# 
# tot <- merge(tot, N_P_flux_raw_cast, by="Stream_Name")

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

# write.csv(tot, "AllDrivers_Harmonized_20241108_WRTDS_MD_KG_rawNP_GenFlux.csv")
write.csv(tot, "AllDrivers_Harmonized_20241108_WRTDS_MD_KG_NP_GenFlux_Average.csv")
