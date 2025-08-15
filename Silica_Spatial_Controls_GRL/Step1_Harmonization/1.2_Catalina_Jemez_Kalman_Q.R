# #############################################################################
# Catalina–Jemez WRTDS-Kalman assembly + daily Q subset
# #############################################################################
# Required inputs:
#   1) <drv_dir>/Catalina Jemez*.csv
#   2) <drv_dir>/WRTDS-input_discharge.csv
#
# Outputs created:
#   A) <drv_dir>/wrtds_kalman_annual_CatalinaJemez.csv
#   B) <drv_dir>/daily_Q_CatalinaJemez.csv
# #############################################################################


library(data.table)
library(stringr)

# 1. set your wd
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")

# 2. list only the Catalina Jemez CSVs
files <- list.files(
  path       = ".",
  pattern    = "^Catalina Jemez.*\\.csv$",
  full.names = TRUE
)

# 3. read, tag, and bind
dt_all <- rbindlist(
  lapply(files, function(f) {
    dt <- fread(f)
    
    # add analyte variable
    dt[, chemical := str_extract(basename(f), "NO3|DSi|P")]
    
    # add fixed LTER column
    dt[, LTER := "Catalina Jemez"]
    
    # extract stream name from filename
    dt[, Stream_Name := str_extract(basename(f), "OR_low|MG_WEIR")]
    
    return(dt)
  }),
  use.names = TRUE,
  fill      = TRUE
)

# Inspect
head(dt_all)

wrtds_CJ <- dt_all

wrtds_CJ <- wrtds_CJ %>%
  mutate(
    Stream_ID  = paste0(LTER, "__", Stream_Name),
    Year       = floor(as.numeric(DecYear)),
    drainSqKm  = case_when(
      Stream_Name == "MG_WEIR" ~ 1.54, # values pulled from the Site Reference Table
      Stream_Name == "OR_low"  ~  1.09,
      TRUE                     ~ NA_real_
    )
  ) %>%
  select(LTER, Stream_Name, chemical, DecYear, Q, FNConc, FNFlux, GenConc, GenFlux, drainSqKm, Stream_ID, Year)

write_csv(wrtds_CJ, "wrtds_kalman_annual_CatalinaJemez.csv")

# Now we want to import daily Q values so that we can calculate RBI and recession curve slope as with the other sites:
daily_Q <- read.csv("WRTDS-input_discharge.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  dplyr::filter(Stream_ID %in% c("Catalina Jemez__OR_low", "Catalina Jemez__OR_WEIR")) %>%
  dplyr::select(-indicate)

write_csv(daily_Q, "daily_Q_CatalinaJemez.csv")
