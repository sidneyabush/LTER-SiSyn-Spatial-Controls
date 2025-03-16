# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr)

# Clear environment
rm(list = ls())


# ## ------------------------------------------------------- ##
#              # Read in and Tidy Data ----
# ## ------------------------------------------------------- ##

## Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# These data are in mmol/L but need to be converted to mg/L
raw_NP <- read.csv("20241003_masterdata_chem.csv") %>%
  filter(variable %in% c("NOx", "NO3", "SRP", "PO4"))

# This key has units: 
# These data are in mmol/L but need to be converted to mg/L
unit_key <- read.csv("20241003_masterdata_chem.csv") %>%
  filter(variable %in% c("NOx", "NO3", "SRP", "PO4"))