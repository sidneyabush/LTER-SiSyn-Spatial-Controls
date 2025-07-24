# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr, corrplot)

# Clear environment
rm(list = ls())

# Define the record length in years
record_length <- 5  

# -------------------------------------------------------
# 1) Read in and tidy WRTDS DSi results
# -------------------------------------------------------
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")

wrtds_df <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  dplyr::rename(LTER = LTER.x) %>%
  dplyr::filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  dplyr::select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y,
                -dplyr::contains("date"), -dplyr::contains("month"),
                -min_year, -max_year, -duration) %>%
  dplyr::mutate(
    Stream_Name = dplyr::case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    Year      = floor(as.numeric(DecYear))
  ) %>%
  dplyr::filter(chemical == "DSi")

wrtds_CJ <- read.csv("wrtds_kalman_annual_CatalinaJemez.csv") %>%
  dplyr::filter(chemical == "DSi")

wrtds_df <- dplyr::bind_rows(wrtds_df, wrtds_CJ)

standardize_stream_id <- function(df) {
  df %>%
    dplyr::mutate(
      Stream_ID = stringr::str_trim(Stream_ID),
      Stream_ID = stringr::str_replace_all(Stream_ID, "\\s+", " ")
    )
}

site_year_counts <- wrtds_df %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::summarise(year_count = dplyr::n_distinct(Year), .groups="drop") %>%
  dplyr::filter(year_count >= record_length)

wrtds_df <- wrtds_df %>%
  dplyr::filter(Stream_ID %in% site_year_counts$Stream_ID)

finn <- read.csv("FinnishSites.csv") %>%
  dplyr::mutate(
    Stream_ID  = paste0("Finnish Environmental Institute__", Site.ID),
    Stream_ID2 = paste0("Finnish Environmental Institute__", Site)
  )

wrtds_df <- wrtds_df %>%
  dplyr::left_join(finn %>% dplyr::select(Stream_ID, Stream_ID2), by = "Stream_ID") %>%
  dplyr::mutate(Stream_ID = dplyr::coalesce(Stream_ID2, Stream_ID)) %>%
  dplyr::select(-Stream_ID2)

# -------------------------------------------------------
# 2) Calculate yields
# -------------------------------------------------------
yields <- wrtds_df %>%
  dplyr::mutate(
    FNYield  = (FNFlux  * 365) / drainSqKm,
    GenYield = (GenFlux * 365) / drainSqKm
  ) %>%
  dplyr::select(-FNFlux, -GenFlux)

tot <- wrtds_df %>%
  dplyr::left_join(yields, by = c("Stream_ID", "Year")) %>%
  dplyr::distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  dplyr::select(-dplyr::contains(".y")) %>%
  dplyr::rename_with(~ stringr::str_remove(., "\\.x$"))

# -------------------------------------------------------
# 3) Flashiness & recession slope
# -------------------------------------------------------
flashiness <- readr::read_csv("flashiness_by_stream_id.csv")
tot <- tot %>% dplyr::left_join(flashiness, by = "Stream_ID")

recession_slope <- readr::read_csv("Recession_Slopes_by_StreamID_Aggregate.csv") %>%
  dplyr::rename(recession_slope = slope) %>%
  dplyr::select(-'...1', -n_days)
tot <- tot %>% dplyr::left_join(recession_slope, by = "Stream_ID")

# -------------------------------------------------------
# 4) Köppen–Geiger classification
# -------------------------------------------------------
KG <- read.csv("Koeppen_Geiger_2.csv") %>%
  dplyr::mutate(
    Stream_Name = dplyr::case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)
  )

tot <- tot %>%
  dplyr::left_join(KG, by = "Stream_ID") %>%
  dplyr::distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  dplyr::select(-dplyr::contains(".x")) %>%
  dplyr::rename_with(~ stringr::str_remove(., "\\.y$"))

# -------------------------------------------------------
# 5) Daylength
# -------------------------------------------------------
daylen <- read.csv("Monthly_Daylength_2.csv") %>%
  dplyr::select(-1)

daylen_range <- daylen %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(Max_Daylength = max(mean_daylength), .groups = "drop") %>%
  dplyr::mutate(
    Stream_Name = dplyr::case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name
    )
  )

tot <- tot %>%
  dplyr::left_join(daylen_range, by = "Stream_Name", relationship = "many-to-many") %>%
  dplyr::distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  dplyr::select(-dplyr::contains(".x")) %>%
  dplyr::rename_with(~ stringr::str_remove(., "\\.y$"))

# -------------------------------------------------------
# 6) Spatial drivers (annual + character)
# -------------------------------------------------------
si_drivers <- read.csv("all-data_si-extract_2_20250325.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-dplyr::contains("soil"), -dplyr::contains("cycle1")) %>%
  dplyr::mutate(
    Stream_Name = dplyr::case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)
  ) %>%
  dplyr::select(-dplyr::contains(".y"), -dplyr::contains(".x")) %>%
  standardize_stream_id()

# convert greenup dates to DOY
greenup_cols <- grep("greenup_", names(si_drivers), value = TRUE)
si_drivers[greenup_cols] <- lapply(si_drivers[greenup_cols], function(x) {
  as.numeric(format(as.Date(x, "%Y-%m-%d"), "%j"))
})

# zero-fill permafrost & prop_area
pcols <- grep("permafrost|prop_area", names(si_drivers), value = TRUE)
si_drivers[pcols] <- lapply(si_drivers[pcols], function(x) { x <- as.numeric(x); x[is.na(x)] <- 0; x })

# split into annual vs character (exclude slope)
months_regex <- "_jan_|_feb_|_mar_|_apr_|_may_|_jun_|_jul_|_aug_|_sep_|_oct_|_nov_|_dec_"
year_cols      <- si_drivers[, !grepl(months_regex, names(si_drivers))]
year_cols$Stream_Name <- si_drivers$Stream_Name

char_vars      <- "elevation|rock|land|soil|permafrost"
character_cols <- year_cols[, grepl(char_vars, names(year_cols)), drop = FALSE]
character_cols$Stream_Name <- year_cols$Stream_Name

year_cols <- year_cols[, !grepl(char_vars, names(year_cols))] %>%
  dplyr::select(-LTER, -Stream_ID, -Shapefile_Name, -Discharge_File_Name)

# melt & tag
year_cols_melt <- reshape2::melt(year_cols, id.vars = "Stream_Name")
year_cols_melt$variable <- as.character(year_cols_melt$variable)
year_cols_melt$year <- ifelse(
  grepl("MMDD$", year_cols_melt$variable),
  as.integer(stringr::str_extract(year_cols_melt$variable, "(?<=_)[0-9]{4}(?=MMDD$)")),
  as.integer(sapply(strsplit(year_cols_melt$variable, "_"), `[`, 2))
)

# annual drivers including slope
vars_annual  <- c("num_days","prop_area","evapotrans","precip","temp",
                  "cycle0","cycle1","npp","basin_slope_mean_degree")
units_annual <- c("days","prop_watershed","kg_m2","mm_day","deg_C",
                  "MMDD","MMDD","kgC_m2_year","deg")
units_df_annual <- data.frame(driver = vars_annual, units = units_annual, stringsAsFactors = FALSE)

year_cols_melt$driver <- NA_character_
for (i in seq_along(vars_annual)) {
  year_cols_melt$driver[grepl(vars_annual[i], year_cols_melt$variable)] <- vars_annual[i]
}
year_cols_melt <- merge(year_cols_melt, units_df_annual, by = "driver")

# filter years & pivot
drivers_cropped <- subset(year_cols_melt, year > 2000 & year < 2024)
drivers_cast   <- drivers_cropped %>%
  dplyr::distinct(Stream_Name, year, driver, value) %>%
  dplyr::rename(Year = year) %>%
  dplyr::mutate(Year = as.numeric(Year)) %>%
  tidyr::pivot_wider(names_from = driver, values_from = value)

character_unique <- character_cols %>%
  dplyr::distinct(Stream_Name, .keep_all = TRUE)

all_spatial <- drivers_cast %>%
  dplyr::left_join(character_unique, by = "Stream_Name", relationship = "many-to-many") %>%
  dplyr::distinct(Stream_Name, Year, .keep_all = TRUE)

tot <- tot %>%
  dplyr::left_join(all_spatial, by = c("Stream_Name","Year")) %>%
  dplyr::filter(Year > 2000, Year <= 2024) %>%
  dplyr::distinct(Stream_ID, Year, .keep_all = TRUE)

# bring basin slope in & coerce to numeric
slope_df <- si_drivers %>%
  dplyr::select(Stream_Name, basin_slope_mean_degree) %>%
  dplyr::rename(basin_slope = basin_slope_mean_degree)

tot <- tot %>%
  dplyr::left_join(slope_df, by = "Stream_Name") %>%
  dplyr::mutate(basin_slope = as.numeric(basin_slope))

# coerce other spatial numerics
tot <- tot %>%
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
    prop_area         = tidyr::replace_na(prop_area,         0)
  )

# -------------------------------------------------------
# 7) Silicate weathering
# -------------------------------------------------------
mapped_lith <- data.table::fread("mapped_lithologies.csv")
setDT(tot); setDT(mapped_lith)
tot[, major_rock := as.character(major_rock)]
mapped_lith[, major_rock := as.character(major_rock)]

tot <- tot[!is.na(major_rock) & major_rock != "" & major_rock != "0"]
weathering <- merge(
  tot[, .(Stream_ID, Year, major_rock, Q, temp, drainSqKm)],
  mapped_lith[, .(major_rock, mapped_lithology)],
  by = "major_rock", all.x = TRUE
)[!is.na(mapped_lithology)]

seconds_per_year <- 31536000; kg_m3 <- 1000; km2_m2 <- 1e6; R <- 8.314
lith_params <- data.table(
  mapped_lithology = c("su","vb","pb","py","va","vi","ss","pi","sm","mt","pa"),
  b  = c(0.003364,0.007015,0.007015,0.0061,0.002455,0.007015,0.005341,0.007015,0.012481,0.007626,0.005095),
  sp = c(1,1,1,1,1,1,0.64,0.58,0.24,0.25,0.58),
  sa = c(60,50,50,46,60,50,60,60,60,60,60)
)

weathering[, temp_K := as.numeric(temp) + 273.15]
weathering[, runoff := (Q * seconds_per_year * kg_m3) / (drainSqKm * km2_m2)]

calc_w <- function(lits, runoff, tempK) {
  sapply(seq_along(lits), function(i) {
    parts <- strsplit(lits[i], ",\\s*")[[1]]
    vals  <- sapply(parts, function(l) {
      p <- lith_params[mapped_lithology == l]
      p$b * (p$sp * exp(((1000 * p$sa) / R) *
                          ((1/284.2) - (1/tempK[i])))) * runoff[i]
    })
    mean(vals, na.rm = TRUE)
  })
}

weathering[, silicate_weathering := calc_w(mapped_lithology, runoff, temp_K)]
weathering <- unique(weathering, by = c("Stream_ID","Year"))

tot <- merge(
  tot,
  weathering[, .(Stream_ID, Year, silicate_weathering)],
  by = c("Stream_ID","Year"),
  all.x = TRUE
)

# -------------------------------------------------------
# 8) Land cover
# -------------------------------------------------------
lulc <- read.csv("DSi_LULC_filled_interpolated_Simple.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(Stream_Name, Year, Simple_Class, LandClass_sum) %>%
  dplyr::mutate(
    Stream_Name = dplyr::case_when(
      Stream_Name == "East Fork"              ~ "east fork",
      Stream_Name == "West Fork"              ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE                                    ~ Stream_Name
    ),
    LandClass_sum = if_else(is.na(LandClass_sum) | LandClass_sum == 0,
                            LandClass_sum, LandClass_sum * 100)
  )

lulc_wide <- lulc %>%
  tidyr::pivot_wider(names_from = Simple_Class,
                     values_from = LandClass_sum,
                     names_prefix = "land_")

land_cols <- grep("^land_", names(lulc_wide), value = TRUE)
lulc_wide <- lulc_wide %>%
  dplyr::mutate(
    major_land = apply(dplyr::select(., dplyr::all_of(land_cols)), 1, function(x) {
      if(all(is.na(x))) NA_character_ else sub("^land_","", names(x)[which.max(x)])
    })
  )

tot <- tot %>%
  dplyr::left_join(lulc_wide, by = c("Stream_Name","Year")) %>%
  dplyr::mutate(across(where(is.list), ~ sapply(., paste, collapse = ",")))

# -------------------------------------------------------
# 9) Import & combine N/P medians
# -------------------------------------------------------
wrtds_NP <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  dplyr::rename(LTER = LTER.x) %>%
  dplyr::filter(chemical %in% c("P","NO3","NOx"), GenConc > 0) %>%
  dplyr::mutate(
    chemical = ifelse(chemical %in% c("NOx","NO3"), "NOx", chemical),
    Stream_ID = paste0(LTER,"__",Stream_Name),
    Year      = floor(as.numeric(DecYear))
  ) %>%
  dplyr::group_by(Stream_ID, Year, chemical) %>%
  dplyr::summarise(GenConc = median(GenConc, na.rm = TRUE), .groups = "drop")

wrtds_NP_wide <- wrtds_NP %>%
  tidyr::pivot_wider(id_cols = c(Stream_ID,Year),
                     names_from = chemical,
                     values_from = GenConc,
                     values_fill = list(GenConc = NA))

raw_NP_median <- read.csv("converted_raw_NP.csv") %>%
  dplyr::mutate(
    Year      = as.integer(lubridate::year(as.Date(date))),
    Stream_ID = paste0(LTER,"__",Stream_Name),
    solute    = dplyr::case_when(
      variable %in% c("NOx","NO3") ~ "NOx",
      variable %in% c("SRP","PO4")  ~ "P",
      TRUE                          ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(solute)) %>%
  dplyr::group_by(Stream_ID, Year, solute) %>%
  dplyr::summarise(med = median(value, na.rm = TRUE), .groups = "drop")

raw_NP_wide <- raw_NP_median %>%
  tidyr::pivot_wider(id_cols = c(Stream_ID,Year),
                     names_from = solute,
                     values_from = med,
                     values_fill = list(med = NA))

combined_NP <- dplyr::full_join(wrtds_NP_wide, raw_NP_wide, by = c("Stream_ID","Year")) %>%
  dplyr::rename(P_wrtds   = P.x,
                NOx_wrtds = NOx.x,
                P_raw     = P.y,
                NOx_raw   = NOx.y) %>%
  dplyr::mutate(
    P_source   = if_else(is.na(P_wrtds),  "raw",  "WRTDS"),
    NOx_source = if_else(is.na(NOx_wrtds),"raw",  "WRTDS"),
    P    = coalesce(P_wrtds,  P_raw),
    NOx  = coalesce(NOx_wrtds, NOx_raw)
  ) %>%
  dplyr::select(-P_wrtds, -NOx_wrtds, -P_raw, -NOx_raw)

tot <- tot %>%
  dplyr::left_join(combined_NP, by = c("Stream_ID","Year")) %>%
  dplyr::mutate(
    permafrost_mean_m = replace_na(as.numeric(permafrost_mean_m), 0),
    prop_area         = replace_na(as.numeric(prop_area),        0)
  ) %>%
  dplyr::select(-dplyr::ends_with(".y")) %>%
  dplyr::rename_with(~ stringr::str_remove(., "\\.x$"))

# -------------------------------------------------------
# 10) Final harmonized export
# -------------------------------------------------------
tot_si <- tot %>%
  dplyr::select(
    Stream_ID, Year, drainSqKm, NOx, P, precip, Q, temp,
    Max_Daylength, prop_area, npp, evapotrans, cycle0,
    permafrost_mean_m, elevation_mean_m, RBI, recession_slope,
    basin_slope, FNConc, FNYield, GenConc, GenYield,
    major_rock, major_land, dplyr::contains("rocks"), dplyr::starts_with("land_")
  ) %>%
  dplyr::rename(
    drainage_area = drainSqKm,
    snow_cover     = prop_area,
    greenup_day    = cycle0,
    elevation      = elevation_mean_m,
    permafrost     = permafrost_mean_m
  ) %>%
  dplyr::mutate(
    permafrost = replace_na(permafrost, 0),
    snow_cover = replace_na(snow_cover,  0)
  ) %>%
  dplyr::distinct(Stream_ID, Year, .keep_all = TRUE) %>%
  dplyr::mutate(across(
    c(drainage_area, NOx, P, precip, Q, temp, Max_Daylength,
      snow_cover, npp, evapotrans, greenup_day, permafrost,
      elevation, basin_slope, FNConc, FNYield, GenConc, GenYield),
    as.numeric
  )) %>%
  dplyr::filter(FNConc >= 0.5 * GenConc, FNConc <= 1.5 * GenConc) %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::filter(dplyr::n_distinct(Year) >= record_length) %>%
  dplyr::ungroup()

write.csv(
  tot_si,
  sprintf("AllDrivers_Harmonized_Yearly_filtered_%d_years_uncleaned.csv", record_length),
  row.names = FALSE
)

# -------------------------------------------------------
# Zero-fill all rock_ and land_ columns before complete-case filter
# -------------------------------------------------------
rock_land_cols <- grep("^(rocks_|land_)", names(tot_si), value = TRUE)

tot_si <- tot_si %>%
  dplyr::mutate(
    across(
      dplyr::all_of(rock_land_cols),
      ~ tidyr::replace_na(., 0)
    )
  )

# -------------------------------------------------------
# 11) Build drivers_df and remove outliers
# -------------------------------------------------------
drivers_df <- tot_si %>%
  dplyr::filter(!is.na(FNConc), !is.na(FNYield)) %>%
  { m1 <- mean(.$FNConc); sd1 <- sd(.$FNConc);
  dplyr::filter(., FNConc >= m1 - 5 * sd1, FNConc <= m1 + 5 * sd1) } %>%
  { m2 <- mean(.$FNYield); sd2 <- sd(.$FNYield);
  dplyr::filter(., FNYield >= m2 - 5 * sd2, FNYield <= m2 + 5 * sd2) }

# -------------------------------------------------------
# 12) Correlation & full histogram
# -------------------------------------------------------
var_order <- c(
  "NOx","P","npp","evapotrans","greenup_day","precip","temp",
  "snow_cover","permafrost","elevation","basin_slope","RBI",
  "recession_slope",
  grep("^land_|^rocks_", names(drivers_df), value = TRUE)
)

driver_mat <- cor(drivers_df[var_order], use = "pairwise.complete.obs")
CairoPNG("Final_Figures/FigSX_corr_plot.png", width = 12, height = 12, units = "in", res = 300)
par(mar = c(6,5,1,1))
corrplot(driver_mat, type = "lower", order = "original", tl.col = "black", diag = FALSE)
dev.off()

long_full <- drivers_df %>%
  tidyr::pivot_longer(dplyr::all_of(var_order),
                      names_to = "driver", values_to = "value") %>%
  dplyr::mutate(
    value  = dplyr::if_else(driver %in% c("NOx","P"), log10(value), value),
    driver = factor(driver, levels = var_order)
  )

means_full <- long_full %>%
  dplyr::group_by(driver) %>%
  dplyr::summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")

hist_full <- ggplot2::ggplot(long_full, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(bins = 30, fill = "grey85", color = "black") +
  ggplot2::geom_vline(data = means_full, ggplot2::aes(xintercept = mean_val),
                      linetype = "dashed") +
  ggplot2::facet_wrap(~driver, scales = "free", ncol = 6) +
  ggplot2::theme_classic()

ggplot2::ggsave("Final_Figures/FigSX_Hist_All.png", hist_full,
                width = 24, height = 16, dpi = 300)

# -------------------------------------------------------
# 13) Stratified split by final_cluster: take 10% of each cluster as unseen10
# -------------------------------------------------------
drivers_required <- var_order

# pull out only those rows with complete drivers
complete_df <- tot_si %>%
  tidyr::drop_na(dplyr::all_of(drivers_required))

# 13a) build a one‑row/site lookup with major_rock + rock‑fractions 
site_clusters <- complete_df %>%
  dplyr::distinct(
    Stream_ID, major_rock,
    rocks_volcanic, rocks_sedimentary,
    rocks_carbonate_evaporite, rocks_metamorphic,
    rocks_plutonic
  ) %>%
  
  # 13b) same consolidation logic as in your plotting script
  dplyr::mutate(
    consolidated_rock = dplyr::case_when(
      major_rock %in% c("volcanic", "volcanic; plutonic")                                   ~ "Volcanic",
      major_rock %in% c("sedimentary",                                                       
                        "volcanic; sedimentary; carbonate_evaporite",
                        "sedimentary; carbonate_evaporite",
                        "sedimentary; plutonic; carbonate_evaporite; metamorphic",
                        "sedimentary; metamorphic")                                         ~ "Sedimentary",
      major_rock %in% c("plutonic", "plutonic; metamorphic", "volcanic; plutonic; metamorphic") ~ "Plutonic",
      major_rock %in% c("metamorphic", "carbonate_evaporite; metamorphic")                   ~ "Metamorphic",
      major_rock %in% c("carbonate_evaporite", "volcanic; carbonate_evaporite")               ~ "Carbonate Evaporite",
      TRUE                                                                                   ~ NA_character_
    ),
    final_cluster = dplyr::case_when(
      consolidated_rock == "Sedimentary" & rocks_sedimentary >= 70 ~ "Sedimentary",
      consolidated_rock == "Sedimentary" & rocks_sedimentary <  70 ~ "Mixed Sedimentary",
      TRUE                                                         ~ consolidated_rock
    )
  ) %>%
  dplyr::select(Stream_ID, final_cluster)

# 13c) do the 10% “unseen” split stratified by final_cluster
set.seed(42)
unseen_sites <- site_clusters %>%
  dplyr::group_by(final_cluster) %>%
  dplyr::slice_sample(prop = 0.10) %>%
  dplyr::ungroup() %>%
  dplyr::pull(Stream_ID)

# the rest are train+val
trainval_sites <- setdiff(site_clusters$Stream_ID, unseen_sites)

unseen10_cc <- complete_df %>%
  dplyr::filter(Stream_ID %in% unseen_sites)

# 13d) within train+val, carve off the most‑recent 30% of *each* site
trainval_split <- complete_df %>%
  dplyr::filter(Stream_ID %in% trainval_sites) %>%
  dplyr::group_by(Stream_ID) %>%
  dplyr::arrange(Year) %>%
  dplyr::mutate(
    tot_count = dplyr::n(),
    n_recent  = ceiling(0.30 * tot_count),
    idx       = dplyr::row_number(),
    split     = dplyr::if_else(idx > tot_count - n_recent, "recent", "older")
  ) %>%
  dplyr::ungroup()

older70_cc <- trainval_split %>%
  dplyr::filter(split == "older") %>%
  dplyr::select(-tot_count, -n_recent, -idx, -split)

recent30_cc <- trainval_split %>%
  dplyr::filter(split == "recent") %>%
  dplyr::select(-tot_count, -n_recent, -idx, -split)

# (Optional) write out
write.csv(unseen10_cc, "AllDrivers_cc_unseen10.csv", row.names = FALSE)
write.csv(older70_cc,  "AllDrivers_cc_older70.csv",  row.names = FALSE)
write.csv(recent30_cc, "AllDrivers_cc_recent30.csv", row.names = FALSE)

# ──────────────────────────────────────────────────────────────────────────────
# 14) Histograms with per‐driver fixed axes (via invisible “extremes”)
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(tidyr)
library(ggplot2)

# 14a) Compute full‐data long form & driver‐by‐driver min/max/counts
full_long <- drivers_df %>%
  pivot_longer(
    cols      = all_of(var_order),
    names_to  = "driver",
    values_to = "value"
  ) %>%
  mutate(
    # only NP get log10-transformed
    value  = if_else(driver %in% c("NOx","P"), log10(value), value),
    driver = factor(driver, levels = var_order)
  )

driver_limits <- full_long %>%
  group_by(driver) %>%
  summarise(
    xmin = min(value, na.rm = TRUE),
    xmax = max(value, na.rm = TRUE),
    # find the maximum bin‐count over a 30‐bin histogram
    ymax = {
      h <- hist(value, breaks = 30, plot = FALSE)
      max(h$counts, na.rm = TRUE)
    },
    .groups = "drop"
  )

# 14b) Build the “dummy” extremes DF
driver_extremes <- driver_limits %>%
  rowwise() %>%
  do(data.frame(
    driver = .$driver,
    value  = c(.$xmin, .$xmax),
    y      = c(0,       .$ymax)
  )) %>% ungroup()

# 14c) Revised helper that injects those extremes invisibly
make_hist <- function(df, name) {
  long <- df %>%
    pivot_longer(
      cols      = all_of(var_order),
      names_to  = "driver",
      values_to = "value"
    ) %>%
    mutate(
      value  = if_else(driver %in% c("NOx","P"), log10(value), value),
      driver = factor(driver, levels = var_order)
    )
  means <- long %>%
    group_by(driver) %>%
    summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot() +
    # real data
    geom_histogram(
      data   = long,
      aes(x = value),
      bins   = 30,
      fill   = "grey85",
      color  = "black"
    ) +
    # per‐driver mean
    geom_vline(
      data = means,
      aes(xintercept = mean_val),
      linetype = "dashed"
    ) +
    # invisible “extremes” to fix scales
    geom_point(
      data  = driver_extremes,
      aes(x = value, y = y),
      shape = NA
    ) +
    facet_wrap(~ driver, scales = "free", ncol = 6) +
    theme_classic() +
    labs(
      title = paste0("Histogram: ", name),
      x     = NULL,
      y     = "Count"
    )
  
  ggsave(
    filename = file.path("Final_Figures", paste0("FigSX_Hist_", name, ".png")),
    plot     = p,
    width    = 24,
    height   = 16,
    dpi      = 300
  )
}

# 14d) Save for full data and each subset
make_hist(drivers_df,  "full")
make_hist(unseen10_cc, "unseen10")
make_hist(older70_cc,  "older70")
make_hist(recent30_cc, "recent30")
