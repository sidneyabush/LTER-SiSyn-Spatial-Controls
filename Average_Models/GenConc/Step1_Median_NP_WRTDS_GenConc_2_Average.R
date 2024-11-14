## ------------------------------------------------------- ##
            # Silica WG - Median N and Median P
## ------------------------------------------------------- ##
# Written by:
## Sidney A Bush, Keira Johnson

# Purpose:
## Find N and P for the ~400 sites in harmonized DB

## ------------------------------------------------------- ##
                # Housekeeping ----
## ------------------------------------------------------- ##
# Load needed libraries
librarian::shelf(rpymat, readxl, dplyr, googledrive, ggplot2, data.table, chillR)

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

chem <-read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv")
## Get all Nitrogen and P variables, filter out non-values
chem_NP <- chem[chem$chemical %in% c("P", "NO3", "NOx"),]
chem_NP <- subset(chem_NP, chem_NP$GenConc > 0)

## ------------------------------------------------------- ##
# Calculate median N and P values ----
## ------------------------------------------------------- ##
## this is for (GenConc) 
chem_NP_avg <- chem_NP %>%
  dplyr::group_by(Stream_Name, chemical) %>%
  dplyr::summarise(median_Conc = median(GenConc, na.rm = T))

# Convert only NOx and NO3 to NOx, leave other solutes unchanged
chem_NP_avg$solute_simplified <- ifelse(chem_NP_avg$chemical %in% c("NOx", "NO3"),
                                            "NOx", 
                                            chem_NP_avg$chemical)

# Export .csv:
write.csv(chem_NP_avg, "Median_NP_WRTDS_GenConc_2.csv")