# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr, corrplot)

# Clear environment
rm(list = ls())

# Define the record length in years (change this to 1, 5, 10, 20... as needed)
record_length <- 5  

# ## ------------------------------------------------------- ##
#              # Read in and Tidy Data ----
# ## ------------------------------------------------------- ##

## Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")

# Read and clean WRTDS data
wrtds_df <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  dplyr::rename(LTER = LTER.x) %>%
  dplyr::filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  dplyr::select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y, -contains("date"),
                -contains("month"), -min_year, -max_year, -duration) %>%
  dplyr::mutate(
    Stream_Name = dplyr::case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    Year = floor(as.numeric(DecYear))
  ) %>%
  dplyr::filter(chemical == "DSi")

wrtds_CJ <- read.csv("wrtds_kalman_annual_CatalinaJemez.csv") %>%
  dplyr::filter(chemical == "DSi")

# Combine main + CJ
wrtds_df <- dplyr::bind_rows(wrtds_df, wrtds_CJ)

# Standardize Stream_ID formatting
standardize_stream_id <- function(df) {
  df %>%
    dplyr::mutate(
      Stream_ID = stringr::str_trim(Stream_ID),
      Stream_ID = stringr::str_replace_all(Stream_ID, "\\s+", " ")
    )
}

# Filter sites by record_length
site_year_counts <- wrtds_df %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::summarise(year_count = n_distinct(Year), .groups="drop") %>%
  dplyr::filter(year_count >= record_length)

wrtds_df <- wrtds_df %>%
  dplyr::filter(Stream_ID %in% site_year_counts$Stream_ID)

# Tidy Finnish site names
finn <- read.csv("FinnishSites.csv") %>%
  dplyr::mutate(
    Stream_ID = paste0("Finnish Environmental Institute__", Site.ID),
    Stream_ID2 = paste0("Finnish Environmental Institute__", Site)
  )

wrtds_df <- wrtds_df %>%
  dplyr::left_join(finn %>% dplyr::select(Stream_ID, Stream_ID2), by="Stream_ID") %>%
  dplyr::mutate(Stream_ID = coalesce(Stream_ID2, Stream_ID)) %>%
  dplyr::select(-Stream_ID2)

# ## ------------------------------------------------------- ##
#           # Calculate Yields ----
# ## ------------------------------------------------------- ##
yields <- wrtds_df %>%
  dplyr::mutate(
    FNYield  = (FNFlux  * 365) / drainSqKm,
    GenYield = (GenFlux * 365) / drainSqKm
  ) %>%
  dplyr::select(-FNFlux, -GenFlux)

tot <- wrtds_df %>%
  dplyr::left_join(yields, by=c("Stream_ID","Year")) %>%
  dplyr::distinct(Stream_ID, Year, .keep_all=TRUE) %>%
  dplyr::select(-contains(".y")) %>%
  dplyr::rename_with(~ stringr::str_remove(., "\\.x$"))

# ## ------------------------------------------------------- ##
# Import Flashiness & Recession Slope
# ## ------------------------------------------------------- ##
flashiness <- readr::read_csv("flashiness_by_stream_id.csv")
tot <- tot %>%
  dplyr::left_join(flashiness, by="Stream_ID")

recession_slope <- readr::read_csv("Recession_Slopes_by_StreamID_Aggregate.csv") %>%
  dplyr::rename(recession_slope = slope) %>%
  dplyr::select(-'...1', -n_days)
tot <- tot %>%
  dplyr::left_join(recession_slope, by="Stream_ID")

# ## ------------------------------------------------------- ##
# Add KG Classifications
# ## ------------------------------------------------------- ##
KG <- read.csv("Koeppen_Geiger_2.csv") %>%
  dplyr::mutate(
    Stream_Name = dplyr::case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)
  )

tot <- tot %>%
  dplyr::left_join(KG, by="Stream_ID") %>%
  dplyr::distinct(Stream_ID, Year, .keep_all=TRUE) %>%
  dplyr::select(-contains(".x")) %>%
  dplyr::rename_with(~ stringr::str_remove(., "\\.y$"))

# ## ------------------------------------------------------- ##
# Import Daylength
# ## ------------------------------------------------------- ##
daylen <- read.csv("Monthly_Daylength_2.csv") %>%
  dplyr::select(-1)

daylen_range <- daylen %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(
    Max_Daylength = max(mean_daylength), .groups="drop"
  ) %>%
  dplyr::mutate(
    Stream_Name = dplyr::case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    )
  )

tot <- tot %>%
  dplyr::left_join(daylen_range, by="Stream_Name", relationship="many-to-many") %>%
  dplyr::distinct(Stream_ID, Year, .keep_all=TRUE) %>%
  dplyr::select(-contains(".x")) %>%
  dplyr::rename_with(~ stringr::str_remove(., "\\.y$"))

# ## ------------------------------------------------------- ##
# Spatial Drivers (collapse to one row per Stream_Name+Year)
# ## ------------------------------------------------------- ##
si_drivers <- read.csv("all-data_si-extract_2_20250325.csv", stringsAsFactors=FALSE) %>%
  dplyr::select(-contains("soil"), -contains("cycle1")) %>%
  dplyr::mutate(
    Stream_Name = dplyr::case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)
  ) %>%
  dplyr::select(-contains(".y"), -contains(".x")) %>%
  standardize_stream_id()

# Convert greenup cols to DOY
greenup_cols <- grep("greenup_", names(si_drivers), value=TRUE)
si_drivers[greenup_cols] <- lapply(si_drivers[greenup_cols], function(x) {
  x <- as.Date(x, "%Y-%m-%d"); as.numeric(format(x,"%j"))
})

# zero-fill permafrost/prop_area
pcols <- grep("permafrost|prop_area", names(si_drivers), value=TRUE)
si_drivers[pcols] <- lapply(si_drivers[pcols], function(x) { x <- as.numeric(x); x[is.na(x)] <- 0; x })

# split annual vs character
months_regex <- "_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_"
month_cols <- si_drivers[, grepl(months_regex, names(si_drivers))]
year_cols  <- si_drivers[, !grepl(months_regex, names(si_drivers))]
year_cols$Stream_Name <- si_drivers$Stream_Name

char_vars <- "elevation|rock|land|soil|slope|permafrost"
character_cols <- year_cols[, grepl(char_vars, names(year_cols)), drop=FALSE]
character_cols$Stream_Name <- year_cols$Stream_Name
year_cols <- year_cols[, !grepl(char_vars, names(year_cols))] %>%
  dplyr::select(-LTER, -Stream_ID, -Shapefile_Name, -Discharge_File_Name)

# melt & tag
year_cols_melt <- reshape2::melt(year_cols, id.vars="Stream_Name")
year_cols_melt$variable <- as.character(year_cols_melt$variable)
year_cols_melt$year <- ifelse(
  grepl("MMDD$", year_cols_melt$variable),
  as.integer(stringr::str_extract(year_cols_melt$variable, "(?<=_)[0-9]{4}(?=MMDD$)")),
  as.integer(sapply(strsplit(year_cols_melt$variable, "_"), `[`, 2))
)

vars_annual    <- c("num_days","prop_area","evapotrans","precip","temp","cycle0","cycle1","npp")
units_annual   <- c("days","prop_watershed","kg_m2","mm_day","deg_C","MMDD","MMDD","kgC_m2_year")
units_df_annual <- data.frame(driver=vars_annual, units=units_annual, stringsAsFactors=FALSE)

year_cols_melt$driver <- NA
for(i in seq_along(vars_annual)) {
  year_cols_melt$driver[grepl(vars_annual[i], year_cols_melt$variable)] <- vars_annual[i]
}
year_cols_melt <- merge(year_cols_melt, units_df_annual, by="driver")

# filter and pivot
drivers_cropped <- subset(year_cols_melt, year>2000 & year<2024)
# … after you’ve built `drivers_cropped` …

drivers_cast <- drivers_cropped %>%
  dplyr::distinct(Stream_Name, year, driver, value) %>%
  dplyr::rename(Year = year) %>%
  dplyr::mutate(Year = as.numeric(Year)) %>%
  tidyr::pivot_wider(
    names_from  = driver,
    values_from = value
  )

# make sure each Stream_Name appears only once in your character data
character_unique <- character_cols %>%
  dplyr::distinct(Stream_Name, .keep_all = TRUE)

# now attach the character columns and de‐duplicate
all_spatial <- drivers_cast %>%
  dplyr::left_join(character_unique, by = "Stream_Name", relationship = "many-to-many") %>%
  dplyr::distinct(Stream_Name, Year, .keep_all = TRUE)

# merge into tot
tot <- tot %>%
  dplyr::left_join(all_spatial, by=c("Stream_Name","Year")) %>%
  dplyr::filter(Year>2000, Year<=2024) %>%
  dplyr::distinct(Stream_ID, Year, .keep_all=TRUE) %>%
  dplyr::mutate(
    permafrost_mean_m = as.numeric(permafrost_mean_m),
    cycle0            = as.numeric(cycle0),
    evapotrans        = as.numeric(evapotrans),
    npp               = as.numeric(npp),
    precip            = as.numeric(precip),
    prop_area         = as.numeric(prop_area),
    temp              = as.numeric(temp)
  ) %>%
  dplyr::mutate(
    permafrost_mean_m = tidyr::replace_na(permafrost_mean_m, 0),
    prop_area         = tidyr::replace_na(prop_area, 0)
  ) %>%
  dplyr::select(-contains(".y")) %>%
  dplyr::rename_with(~ stringr::str_remove(., "\\.x$"))

# -----------------------------------------------------------------------------
# 7) Silicate Weathering
# -----------------------------------------------------------------------------
mapped_lith <- data.table::fread("mapped_lithologies.csv")
setDT(tot); setDT(mapped_lith)
tot[, major_rock := as.character(major_rock)]
mapped_lith[, major_rock := as.character(major_rock)]

tot <- tot[!is.na(major_rock)&major_rock!=""&major_rock!="0"]
weathering <- merge(
  tot[, .(Stream_ID,Year,major_rock,Q,temp,drainSqKm)],
  mapped_lith[, .(major_rock,mapped_lithology)],
  by="major_rock", all.x=TRUE
)[!is.na(mapped_lithology)]

seconds_per_year <- 31536000; kg_m3<-1000; km2_m2<-1e6; R<-8.314
lith_params <- data.table(
  mapped_lithology=c("su","vb","pb","py","va","vi","ss","pi","sm","mt","pa"),
  b=c(0.003364,0.007015,0.007015,0.0061,0.002455,0.007015,0.005341,0.007015,0.012481,0.007626,0.005095),
  sp=c(1,1,1,1,1,1,0.64,0.58,0.24,0.25,0.58),
  sa=c(60,50,50,46,60,50,60,60,60,60,60)
)

weathering[, temp_K := as.numeric(temp) + 273.15]
weathering[, runoff := (Q * seconds_per_year * kg_m3) / (drainSqKm * km2_m2)]

calc_w <- function(lits, runoff, tempK){
  sapply(seq_along(lits), function(i){
    parts <- strsplit(lits[i],",\\s*")[[1]]
    vals <- sapply(parts, function(l){
      p <- lith_params[mapped_lithology==l]
      p$b*(p$sp*exp(((1000*p$sa)/R)*((1/284.2)-(1/tempK[i]))))*runoff[i]
    })
    mean(vals, na.rm=TRUE)
  })
}

weathering[, silicate_weathering := calc_w(mapped_lithology, runoff, temp_K)]
weathering <- unique(weathering, by=c("Stream_ID","Year"))

tot <- merge(tot, weathering[, .(Stream_ID,Year,silicate_weathering)], by=c("Stream_ID","Year"), all.x=TRUE)

# -----------------------------------------------------------------------------
# 8) Land Cover
# -----------------------------------------------------------------------------
lulc <- read.csv("DSi_LULC_filled_interpolated_Simple.csv", stringsAsFactors=FALSE) %>%
  dplyr::select(Stream_Name, Year, Simple_Class, LandClass_sum) %>%
  dplyr::mutate(
    Stream_Name = dplyr::case_when(
      Stream_Name=="East Fork"~"east fork",
      Stream_Name=="West Fork"~"west fork",
      Stream_Name=="Amazon River at Obidos"~"Obidos",
      TRUE~Stream_Name
    ),
    LandClass_sum = if_else(is.na(LandClass_sum)|LandClass_sum==0, LandClass_sum, LandClass_sum*100)
  )

lulc_wide <- lulc %>%
  tidyr::pivot_wider(names_from=Simple_Class, values_from=LandClass_sum, names_prefix="land_")

land_cols <- grep("^land_", names(lulc_wide), value=TRUE)
lulc_wide <- lulc_wide %>%
  dplyr::mutate(major_land = apply(dplyr::select(., all_of(land_cols)), 1, function(x){
    if(all(is.na(x))) NA_character_ else sub("^land_","", names(x)[which.max(x)])
  }))

tot <- tot %>%
  dplyr::left_join(lulc_wide, by=c("Stream_Name","Year")) %>%
  dplyr::mutate(across(where(is.list), ~ sapply(., paste, collapse=",")))

# -----------------------------------------------------------------------------
# 9) Import & Combine N / P Medians
# -----------------------------------------------------------------------------
wrtds_NP <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  dplyr::rename(LTER=LTER.x) %>%
  dplyr::filter(chemical %in% c("P","NO3","NOx"), GenConc>0) %>%
  dplyr::mutate(
    chemical = ifelse(chemical %in% c("NOx","NO3"), "NOx", chemical),
    Stream_ID = paste0(LTER,"__",Stream_Name),
    Year      = floor(as.numeric(DecYear))
  ) %>%
  dplyr::group_by(Stream_ID, Year, chemical) %>%
  dplyr::summarise(GenConc=median(GenConc, na.rm=TRUE), .groups="drop")

wrtds_NP_wide <- wrtds_NP %>%
  tidyr::pivot_wider(id_cols=c(Stream_ID,Year), names_from=chemical, values_from=GenConc, values_fill=list(GenConc=NA))

raw_NP_median <- read.csv("converted_raw_NP.csv") %>%
  dplyr::mutate(
    Year       = as.integer(lubridate::year(as.Date(date))),
    Stream_ID  = paste0(LTER,"__",Stream_Name),
    solute     = dplyr::case_when(variable %in% c("NOx","NO3")~"NOx", variable %in% c("SRP","PO4")~"P", TRUE~NA_character_)
  ) %>%
  dplyr::filter(!is.na(solute)) %>%
  dplyr::group_by(Stream_ID, Year, solute) %>%
  dplyr::summarise(med=median(value,na.rm=TRUE), .groups="drop")

raw_NP_wide <- raw_NP_median %>%
  tidyr::pivot_wider(id_cols=c(Stream_ID,Year), names_from=solute, values_from=med, values_fill=list(med=NA))

combined_NP <- dplyr::full_join(wrtds_NP_wide, raw_NP_wide, by=c("Stream_ID","Year")) %>%
  dplyr::rename(P_wrtds=P.x, NOx_wrtds=NOx.x, P_raw=P.y, NOx_raw=NOx.y) %>%
  dplyr::mutate(
    P_source   = if_else(is.na(P_wrtds), "raw",  "WRTDS"),
    NOx_source = if_else(is.na(NOx_wrtds),"raw",  "WRTDS"),
    P   = coalesce(P_wrtds,  P_raw),
    NOx = coalesce(NOx_wrtds,NOx_raw)
  ) %>%
  dplyr::select(-P_wrtds, -NOx_wrtds, -P_raw, -NOx_raw)

tot <- tot %>%
  dplyr::left_join(combined_NP, by=c("Stream_ID","Year")) %>%
  dplyr::mutate(
    permafrost_mean_m = replace_na(as.numeric(permafrost_mean_m),0),
    prop_area         = replace_na(as.numeric(prop_area),0)
  ) %>%
  dplyr::select(-dplyr::ends_with(".y")) %>%
  dplyr::rename_with(~ str_remove(., "\\.x$"))

# -----------------------------------------------------------------------------
# 10) Final Harmonized Export
# -----------------------------------------------------------------------------
tot_si <- tot %>%
  dplyr::select(Stream_ID,Year,drainSqKm,NOx,P,precip,Q,temp,Max_Daylength,
                prop_area,npp,evapotrans,cycle0,permafrost_mean_m,
                elevation_mean_m,RBI,recession_slope,basin_slope_mean_degree,
                FNConc,FNYield,GenConc,GenYield,major_rock,major_land,
                dplyr::contains("rocks"), starts_with("land_")) %>%
  dplyr::rename(
    drainage_area = drainSqKm,
    snow_cover     = prop_area,
    greenup_day    = cycle0,
    elevation      = elevation_mean_m,
    permafrost     = permafrost_mean_m,
    basin_slope    = basin_slope_mean_degree
  ) %>%
  dplyr::mutate(
    permafrost = replace_na(permafrost,0),
    snow_cover = replace_na(snow_cover,0)
  ) %>%
  dplyr::distinct(Stream_ID,Year,.keep_all=TRUE) %>%
  dplyr::mutate(across(c(drainage_area,NOx,P,precip,Q,temp,Max_Daylength,
                         snow_cover,npp,evapotrans,greenup_day,permafrost,
                         elevation,basin_slope,FNConc,FNYield,GenConc,GenYield),
                       as.numeric)) %>%
  dplyr::filter(FNConc>=0.5*GenConc & FNConc<=1.5*GenConc) %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::filter(n_distinct(Year)>=record_length) %>%
  dplyr::ungroup()

write.csv(tot_si,
          sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv",record_length),
          row.names=FALSE)

# -----------------------------------------------------------------------------
# 11) Build drivers_df, remove outliers
# -----------------------------------------------------------------------------
drivers_df <- tot_si %>%
  dplyr::filter(!is.na(FNConc), !is.na(FNYield)) %>%
  { m1 <- mean(.$FNConc); sd1 <- sd(.$FNConc);
  dplyr::filter(., FNConc >= m1 - 5*sd1, FNConc <= m1 + 5*sd1) } %>%
  { m2 <- mean(.$FNYield); sd2 <- sd(.$FNYield);
  dplyr::filter(., FNYield >= m2 - 5*sd2, FNYield <= m2 + 5*sd2) }

# -----------------------------------------------------------------------------
# 12) Correlation plot & full histograms
# -----------------------------------------------------------------------------
var_order <- c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
               "snow_cover","permafrost","elevation","basin_slope","RBI",
               "recession_slope", grep("^land_|^rocks_", names(drivers_df), value=TRUE))

driver_mat <- cor(drivers_df[var_order], use="pairwise.complete.obs")
CairoPNG("Final_Figures/FigSX_corr_plot.png", width=12, height=12, units="in", res=300)
par(mar=c(6,5,1,1))
corrplot(driver_mat, type="lower", order="original", tl.col="black", diag=FALSE)
dev.off()

long_full <- drivers_df %>%
  tidyr::pivot_longer(all_of(var_order), names_to="driver", values_to="value") %>%
  dplyr::mutate(
    value  = if_else(driver %in% c("NOx","P"), log10(value), value),
    driver = factor(driver, levels=var_order)
  )

means_full <- long_full %>%
  dplyr::group_by(driver) %>%
  dplyr::summarise(mean_val=mean(value, na.rm=TRUE), .groups="drop")

hist_full <- ggplot2::ggplot(long_full, ggplot2::aes(value)) +
  ggplot2::geom_histogram(bins=30, fill="grey85", color="black") +
  ggplot2::geom_vline(data=means_full, ggplot2::aes(xintercept=mean_val), linetype="dashed") +
  ggplot2::facet_wrap(~driver, scales="free", ncol=6) +
  ggplot2::theme_classic()

ggplot2::ggsave("Final_Figures/FigSX_Hist_All.png", hist_full, width=24, height=16, dpi=300)

# -----------------------------------------------------------------------------
# 13) Split into unseen10 / older70 / recent30 and recalc NP-medians
# -----------------------------------------------------------------------------
set.seed(42)
all_sites <- unique(drivers_df$Stream_ID)
n_unseen  <- ceiling(0.10 * length(all_sites))
unseen    <- sample(all_sites, n_unseen)

unseen10  <- drivers_df %>% dplyr::filter(Stream_ID %in% unseen)

trainval <- drivers_df %>%
  dplyr::filter(!Stream_ID %in% unseen) %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::arrange(Year) %>%
  dplyr::mutate(
    tot_count = dplyr::n(),
    n_recent  = ceiling(0.30 * tot_count),
    idx       = dplyr::row_number(),
    split     = if_else(idx > tot_count - n_recent, "recent", "older")
  ) %>%
  dplyr::ungroup()

older70  <- trainval %>% dplyr::filter(split=="older")  %>% dplyr::select(-tot_count, -n_recent, -idx, -split)
recent30 <- trainval %>% dplyr::filter(split=="recent") %>% dplyr::select(-tot_count, -n_recent, -idx, -split)

calc_med <- function(df){
  df %>%
    dplyr::group_by(Stream_ID) %>%
    dplyr::summarise(
      NOx_med = median(NOx, na.rm=TRUE),
      P_med   = median(P,   na.rm=TRUE),
      .groups="drop"
    )
}

med_full   <- calc_med(drivers_df)
med_unseen <- calc_med(unseen10)
med_older  <- calc_med(older70)
med_recent <- calc_med(recent30)

write.csv(unseen10, "AllDrivers_Harmonized_unseen10.csv", row.names=FALSE)
write.csv(older70,  "AllDrivers_Harmonized_older70.csv", row.names=FALSE)
write.csv(recent30, "AllDrivers_Harmonized_recent30.csv", row.names=FALSE)
