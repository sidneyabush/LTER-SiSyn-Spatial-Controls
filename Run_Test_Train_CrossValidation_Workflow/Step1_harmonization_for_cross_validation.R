# Load needed libraries
librarian::shelf(dplyr, googledrive, ggplot2, data.table, lubridate, tidyr, stringr, readr, corrplot, Cairo)

# Clear environment
rm(list = ls())

# Define the record length in years (change this to 1, 5, 10, 20... as needed)
record_length <- 5  

# -------------------------------------------------------
# Read in and Tidy Data
# -------------------------------------------------------

## Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/harmonization_files")

# Read and clean WRTDS DSi data
wrtds_df <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  rename(LTER = LTER.x) %>%
  filter(chemical == "DSi",
         !if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
  select(-Conc, -Flux, -PeriodLong, -PeriodStart, -LTER.y, -contains("date"),
         -contains("month"), -min_year, -max_year, -duration) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name),
    Year = floor(as.numeric(DecYear))
  )
wrtds_CJ <- read.csv("wrtds_kalman_annual_CatalinaJemez.csv") %>%
  filter(chemical == "DSi")
wrtds_df <- bind_rows(wrtds_df, wrtds_CJ)

standardize_stream_id <- function(df) {
  df %>%
    mutate(
      Stream_ID = str_trim(Stream_ID),
      Stream_ID = str_replace_all(Stream_ID, "\\s+", " ")
    )
}

# Filter by record length
site_year_counts <- wrtds_df %>%
  group_by(Stream_ID) %>%
  summarise(year_count = n_distinct(Year), .groups="drop") %>%
  filter(year_count >= record_length)
wrtds_df <- wrtds_df %>% filter(Stream_ID %in% site_year_counts$Stream_ID)

# Tidy Finnish site names
finn <- read.csv("FinnishSites.csv") %>%
  mutate(
    Stream_ID = paste0("Finnish Environmental Institute__", Site.ID),
    Stream_ID2 = paste0("Finnish Environmental Institute__", Site)
  )
wrtds_df <- wrtds_df %>%
  left_join(finn %>% select(Stream_ID, Stream_ID2), by="Stream_ID") %>%
  mutate(Stream_ID = coalesce(Stream_ID2, Stream_ID)) %>%
  select(-Stream_ID2)

# -------------------------------------------------------
# Calculate Yields
# -------------------------------------------------------
yields <- wrtds_df %>%
  mutate(
    FNYield = (FNFlux * 365) / drainSqKm,
    GenYield = (GenFlux * 365) / drainSqKm
  ) %>%
  select(-FNFlux, -GenFlux)
tot <- wrtds_df %>%
  left_join(yields, by=c("Stream_ID","Year")) %>%
  distinct(Stream_ID, Year, .keep_all=TRUE) %>%
  select(-contains(".y")) %>%
  rename_with(~ str_remove(., "\\.x$"))

# -------------------------------------------------------
# Merge Flashiness (RBI)
# -------------------------------------------------------
flashiness <- read_csv("flashiness_by_stream_id.csv")
tot <- tot %>% left_join(flashiness, by="Stream_ID")

# -------------------------------------------------------
# Merge Recession Curve Slope
# -------------------------------------------------------
recession_slope <- read_csv("Recession_Slopes_by_StreamID_Aggregate.csv") %>%
  rename(recession_slope = slope) %>%
  select(-'...1', -n_days)
tot <- tot %>% left_join(recession_slope, by="Stream_ID")

# -------------------------------------------------------
# Merge Köppen–Geiger Classifications
# -------------------------------------------------------
KG <- read.csv("Koeppen_Geiger_2.csv") %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)
  )
tot <- tot %>%
  left_join(KG, by="Stream_ID") %>%
  distinct(Stream_ID, Year, .keep_all=TRUE) %>%
  select(-contains(".x")) %>%
  rename_with(~ str_remove(., "\\.y$"))

# -------------------------------------------------------
# Merge Daylength Range
# -------------------------------------------------------
daylen <- read.csv("Monthly_Daylength_2.csv") %>% select(-1)
daylen_range <- daylen %>%
  group_by(Stream_Name) %>%
  summarise(
    Min_Daylength = min(mean_daylength),
    Max_Daylength = max(mean_daylength),
    .groups="drop"
  ) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    )
  ) %>%
  select(-Min_Daylength)
tot <- tot %>%
  left_join(daylen_range, by="Stream_Name") %>%
  distinct(Stream_ID, Year, .keep_all=TRUE) %>%
  select(-contains(".x")) %>%
  rename_with(~ str_remove(., "\\.y$"))

# -------------------------------------------------------
# Merge Spatial Drivers
# -------------------------------------------------------
si_drivers <- read.csv("all-data_si-extract_2_20250325.csv", stringsAsFactors=FALSE) %>%
  select(-contains("soil"), -contains("cycle1")) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name == "East Fork" ~ "east fork",
      Stream_Name == "West Fork" ~ "west fork",
      Stream_Name == "Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    Stream_ID = paste0(LTER, "__", Stream_Name)
  ) %>%
  filter(LTER != "MCM") %>%
  select(-contains(".y"), -contains(".x"))
si_drivers <- standardize_stream_id(si_drivers)
greenup_cols <- grep("greenup_", colnames(si_drivers), value=TRUE)
if(length(greenup_cols)>0) {
  si_drivers[greenup_cols] <- lapply(si_drivers[greenup_cols], function(x) {
    as.numeric(format(as.Date(x, "%Y-%m-%d"), "%j"))
  })
}
si_drivers <- si_drivers %>%
  left_join(finn %>% select(Stream_ID, Stream_ID2), by="Stream_ID") %>%
  mutate(Stream_ID = coalesce(Stream_ID2, Stream_ID)) %>%
  select(-Stream_ID2) %>%
  select(-contains(".y")) %>%
  rename_with(~ str_remove(., "\\.y$"))
cols_zero <- grep("permafrost|prop_area", colnames(si_drivers), value=TRUE)
si_drivers[cols_zero] <- lapply(si_drivers[cols_zero], function(x) { x <- as.numeric(x); x[is.na(x)] <- 0; x })

# Melt annual vs monthly
months_pattern <- paste0("_(", paste(tolower(month.abb), collapse="|"), ")_")
months_cols <- si_drivers[, grepl(months_pattern, colnames(si_drivers))]
year_only   <- si_drivers[, !colnames(si_drivers) %in% colnames(months_cols)]
year_only$Stream_Name <- si_drivers$Stream_Name
char_vars <- "(elevation|rock|land|soil|slope|permafrost)"
year_only   <- year_only[, !grepl(char_vars, colnames(year_only))]
library(reshape2)
melted <- melt(year_only, id.vars="Stream_Name")
melted$year <- ifelse(grepl("MMDD", melted$variable),
                      str_extract(melted$variable,"(?<=_)[^_]+(?=MMDD)"),
                      sapply(strsplit(melted$variable,"_"), `[`,2))
annual_vars <- c("num_days","prop_area","evapotrans","precip","temp","cycle0","cycle1","npp")
units <- c("days","prop_watershed","kg_m2","mm_day","deg_C","MMDD","MMDD","kgC_m2_year")
units_df <- data.frame(driver=annual_vars,unit=units,stringsAsFactors=FALSE)
melted$driver <- NA_character_
for(i in seq_along(annual_vars)) {
  melted$driver[grepl(annual_vars[i],melted$variable)] <- annual_vars[i]
}
melted <- subset(melted, !is.na(driver))
melted <- merge(melted, units_df, by="driver")
drivers_cast <- melted %>%
  distinct(Stream_Name,year,driver,value) %>%
  rename(Year=year) %>%
  pivot_wider(names_from=driver, values_from=value)
char_cols <- si_drivers[, grepl(char_vars, colnames(si_drivers))]
char_cols$Stream_Name <- si_drivers$Stream_Name
all_spatial <- drivers_cast %>%
  left_join(char_cols, by="Stream_Name") %>%
  distinct(Stream_Name,Year,.keep_all=TRUE)
tot <- tot %>%
  left_join(all_spatial, by=c("Stream_Name","Year")) %>%
  filter(Year>2000,Year<=2024) %>%
  distinct(Stream_ID,Year,.keep_all=TRUE) %>%
  mutate_at(vars(permafrost_mean_m,cycle0,evapotrans,npp,precip,prop_area,temp),as.numeric) %>%
  mutate(permafrost_mean_m=replace_na(permafrost_mean_m,0),
         prop_area=replace_na(prop_area,0)) %>%
  select(-contains(".y")) %>%
  rename_with(~ str_remove(., "\\.x$"))

# -------------------------------------------------------
# Gap‐fill Missing Slopes & Elevations
# -------------------------------------------------------
# [Insert your existing Krycklan/US_slopes/US_elev gap‐fill code here]

# -------------------------------------------------------
# Import & Combine N/P Data
# -------------------------------------------------------
wrtds_NP <- read.csv("Full_Results_WRTDS_kalman_annual_filtered.csv") %>%
  rename(LTER=LTER.x) %>%
  select(-Conc,-Flux,-PeriodLong,-PeriodStart,-LTER.y,-contains("date"),
         -contains("month"),-min_year,-max_year,-duration) %>%
  mutate(
    Stream_Name = case_when(
      Stream_Name=="East Fork" ~ "east fork",
      Stream_Name=="West Fork" ~ "west fork",
      Stream_Name=="Amazon River at Obidos" ~ "Obidos",
      TRUE ~ Stream_Name
    ),
    Stream_ID=paste0(LTER,"__",Stream_Name),
    Year=floor(as.numeric(DecYear))
  ) %>%
  filter(!if_any(where(is.numeric), ~ .==Inf|.==-Inf)) %>%
  select(-DecYear,-LTER,-contains("FN"),-GenFlux)
wrtds_NP_CJ <- read.csv("wrtds_kalman_annual_CatalinaJemez.csv") %>%
  select(-DecYear,-LTER,-contains("FN"),-GenFlux)
wrtds_NP <- bind_rows(wrtds_NP,wrtds_NP_CJ) %>%
  filter(chemical %in% c("P","NO3","NOx") & GenConc>0) %>%
  mutate(chemical=ifelse(chemical %in% c("NOx","NO3"),"NOx",chemical)) %>%
  group_by(Stream_ID,Year,chemical) %>%
  summarise(GenConc=median(GenConc,na.rm=TRUE),.groups="drop") %>%
  pivot_wider(names_from=chemical,values_from=GenConc)
raw_NP <- read.csv("converted_raw_NP.csv") %>%
  mutate(
    Year=year(as.Date(date,"%Y-%m-%d")),
    Stream_ID=paste(LTER,Stream_Name,sep="__"),
    solute=case_when(variable %in% c("NOx","NO3")~"NOx",
                     variable %in% c("SRP","PO4")~"P",
                     TRUE~NA_character_)
  ) %>%
  filter(!is.na(solute)) %>%
  group_by(Stream_ID,Year,solute) %>%
  summarise(med=median(value,na.rm=TRUE),.groups="drop") %>%
  pivot_wider(names_from=solute,values_from=med)
combined_NP <- full_join(wrtds_NP,raw_NP,by=c("Stream_ID","Year")) %>%
  rename(P_wrtds=P.x,NOx_wrtds=NOx.x,P_raw=P.y,NOx_raw=NOx.y) %>%
  mutate(
    P_source=if_else(is.na(P_wrtds),"raw","WRTDS"),
    NOx_source=if_else(is.na(NOx_wrtds),"raw","WRTDS"),
    P=coalesce(P_wrtds,P_raw),
    NOx=coalesce(NOx_wrtds,NOx_raw)
  ) %>%
  select(Stream_ID,Year,P,NOx,P_source,NOx_source)
tot <- tot %>% left_join(combined_NP, by=c("Stream_ID","Year"))

# -------------------------------------------------------
# Incorporate Land Cover & zero‐fill
# -------------------------------------------------------
lulc <- read.csv("DSi_LULC_filled_interpolated_Simple.csv",stringsAsFactors=FALSE) %>%
  select(Stream_Name,Year,Simple_Class,LandClass_sum) %>%
  mutate(
    Stream_Name=case_when(
      Stream_Name=="East Fork"~"east fork",
      Stream_Name=="West Fork"~"west fork",
      Stream_Name=="Amazon River at Obidos"~"Obidos",
      TRUE~Stream_Name),
    LandClass_sum=if_else(is.na(LandClass_sum)|LandClass_sum==0,
                          LandClass_sum,LandClass_sum*100)
  )
lulc_wide <- lulc %>%
  pivot_wider(names_from=Simple_Class,values_from=LandClass_sum,names_prefix="land_")
land_cols <- grep("^land_",names(lulc_wide),value=TRUE)
lulc_wide <- lulc_wide %>%
  mutate(major_land=apply(select(.,all_of(land_cols)),1,function(x){
    if(all(is.na(x))) NA_character_ else sub("^land_","",names(x)[which.max(x)])
  }))
tot <- tot %>% left_join(lulc_wide,by=c("Stream_Name","Year")) %>%
  mutate(across(where(is.list),~sapply(.,toString))) %>%
  mutate(
    across(starts_with("land_"), ~ replace_na(as.numeric(.),0)),
    across(starts_with("rocks_"), ~ replace_na(as.numeric(.),0))
  )

# -------------------------------------------------------
# Prepare tot_si & tot_annual
# -------------------------------------------------------
tot_si <- tot %>%
  select(Stream_ID,Year,drainSqKm,NOx,P,precip,Q,temp,Max_Daylength,
         prop_area,npp,evapotrans,cycle0,permafrost_mean_m,
         elevation_mean_m,RBI,recession_slope,basin_slope_mean_degree,
         FNConc,FNYield,GenConc,GenYield,major_rock,major_land,
         starts_with("rocks_"),starts_with("land_")) %>%
  rename(
    drainage_area=drainSqKm,
    snow_cover=prop_area,
    greenup_day=cycle0,
    elevation=elevation_mean_m,
    permafrost=permafrost_mean_m,
    basin_slope=basin_slope_mean_degree
  ) %>%
  mutate(
    permafrost=replace_na(permafrost,0),
    snow_cover=replace_na(snow_cover,0)
  )
tot_annual <- tot_si %>%
  distinct(Stream_ID,Year,.keep_all=TRUE) %>%
  mutate(across(c(drainage_area,NOx,P,precip,Q,temp,Max_Daylength,
                  snow_cover,npp,evapotrans,greenup_day,permafrost,
                  elevation,basin_slope,FNConc,FNYield,GenConc,GenYield),
                as.numeric))

# -------------------------------------------------------
# Build drivers_df & filter outliers
# -------------------------------------------------------
drivers_df <- tot_annual %>%
  mutate(across(where(is.character),~na_if(., ""))) %>%
  select(FNConc,everything()) %>%
  replace_na(list(across(21:36,~0))) %>%
  filter(FNConc>=0.5*GenConc, FNConc<=1.5*GenConc) %>%
  filter(complete.cases(.))
μ_FN <- mean(drivers_df$FNConc,na.rm=TRUE); σ_FN <- sd(drivers_df$FNConc,na.rm=TRUE)
drivers_df <- drivers_df %>%
  filter(between(FNConc,μ_FN-5*σ_FN,μ_FN+5*σ_FN))
μ_FY <- mean(drivers_df$FNYield,na.rm=TRUE); σ_FY <- sd(drivers_df$FNYield,na.rm=TRUE)
drivers_df <- drivers_df %>%
  filter(between(FNYield,μ_FY-5*σ_FY,μ_FY+5*σ_FY)) %>%
  group_by(Stream_ID) %>%
  filter(n_distinct(Year)>=5) %>%
  ungroup()

# -------------------------------------------------------
# Create splits & recalc medians
# -------------------------------------------------------
set.seed(123)
unseen10 <- sample(unique(drivers_df$Stream_ID),10)
model_df <- drivers_df %>% filter(!Stream_ID %in% unseen10)
older70 <- model_df %>%
  group_by(Stream_ID) %>%
  filter(Year <= quantile(Year,0.7)) %>%
  ungroup()
recent30 <- model_df %>%
  group_by(Stream_ID) %>%
  filter(Year > quantile(Year,0.7)) %>%
  ungroup()
medians_older70 <- older70 %>%
  summarise(median_NOx=median(NOx,na.rm=TRUE),
            median_P  =median(P,  na.rm=TRUE))
medians_recent30 <- recent30 %>%
  summarise(median_NOx=median(NOx,na.rm=TRUE),
            median_P  =median(P,  na.rm=TRUE))

# -------------------------------------------------------
# Correlation plots & histograms for full, older70, recent30
# -------------------------------------------------------
var_order <- c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
               "snow_cover","permafrost","elevation","basin_slope","RBI",
               "recession_slope",paste0("land_",c("Bare","Cropland","Forest",
                                                  "Grassland_Shrubland","Ice_Snow","Impervious","Salt_Water",
                                                  "Tidal_Wetland","Water","Wetland_Marsh")),
               paste0("rocks_",c("volcanic","sedimentary","carbonate_evaporite",
                                 "metamorphic","plutonic")))
pretty_labels <- c("N","P","NPP","ET","Greenup Day","Precip","Temp",
                   "Snow Cover","Permafrost","Elevation","Basin Slope","Flashiness (RBI)",
                   "Recession Curve Slope", "Land: Bare","Land: Cropland","Land: Forest",
                   "Land: Grass & Shrub","Land: Ice & Snow","Land: Impervious","Land: Salt Water",
                   "Land: Tidal Wetland","Land: Water Body","Land: Wetland Marsh",
                   "Rock: Volcanic","Rock: Sedimentary","Rock: Carbonate Evaporite",
                   "Rock: Metamorphic","Rock: Plutonic")
names(pretty_labels) <- var_order

plot_correlation <- function(df, label){
  df_num <- df %>% select(all_of(var_order)) %>%
    mutate(across(everything(),as.numeric))
  cor_mat <- cor(df_num, use="pairwise.complete.obs")[var_order,var_order]
  dimnames(cor_mat) <- list(pretty_labels,pretty_labels)
  fn <- file.path("Final_Figures", sprintf("Fig_corr_%s_%dyrs.png",label,record_length))
  CairoPNG(fn, width=12, height=12, units="in", res=300)
  par(mar=c(6,5,1,1))
  corrplot(cor_mat, type="lower", order="original", tl.col="black",
           tl.cex=1.2, cl.cex=1.2, tl.pos="ld", tl.srt=90, diag=FALSE)
  dev.off()
}

plot_histograms <- function(df, label){
  long <- df %>%
    select(all_of(var_order)) %>%
    pivot_longer(cols=everything(),names_to="driver",values_to="value") %>%
    mutate(
      value = case_when(
        driver=="NOx" ~ log10(value),
        driver=="P"   ~ log10(value),
        TRUE          ~ value
      ),
      driver = factor(driver, levels=var_order, labels=pretty_labels)
    )
  means_df <- long %>% group_by(driver) %>%
    summarise(mean_val=mean(value,na.rm=TRUE),.groups="drop")
  p <- ggplot(long, aes(x=value)) +
    geom_histogram(bins=30, fill="grey85", color="black") +
    geom_vline(data=means_df, aes(xintercept=mean_val),
               linetype="dashed", size=0.9) +
    facet_wrap(~driver, scales="free", ncol=6) +
    labs(x="Value",y="Count") +
    theme_classic(base_size=20) +
    theme(strip.text=element_text(size=18),
          axis.text=element_text(size=16),
          axis.title=element_text(size=18),
          panel.spacing=unit(0.5,"lines"))
  ggsave(file.path("Final_Figures", sprintf("Fig_hist_%s_%dyrs.png",label,record_length)),
         p, width=24, height=16, dpi=300, bg="white")
}

dir.create("Final_Figures", showWarnings=FALSE)
plot_correlation(drivers_df, "full")
plot_correlation(older70,    "older70")
plot_correlation(recent30,   "recent30")

plot_histograms(drivers_df, "full")
plot_histograms(older70,    "older70")
plot_histograms(recent30,   "recent30")

# -------------------------------------------------------
# Save final outputs
# -------------------------------------------------------
write.csv(drivers_df,
          sprintf("All_Drivers_Harmonized_Yearly_FNConc_FNYield_%d_years.csv",record_length),
          row.names=FALSE)


# -------------------------------------------------------
# Save unseen10, older70, recent30 as CSVs
# -------------------------------------------------------
unseen10_df <- drivers_df %>% filter(Stream_ID %in% unseen10)
write.csv(unseen10_df, sprintf("unseen10_%dyrs.csv", record_length), row.names = FALSE)
write.csv(older70,    sprintf("older70_%dyrs.csv",   record_length), row.names = FALSE)
write.csv(recent30,   sprintf("recent30_%dyrs.csv",  record_length), row.names = FALSE)
