# =============================================================================
# Exploratory LOESS PDPs for every SHAP feature (FNConc & FNYield)
# =============================================================================

# 0) Clear & set WD
rm(list = ls())
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# 1) Libraries
librarian::shelf(
  ggplot2, dplyr, tibble, readr, scales, tools, fastshap
)

# 2) Read recent30 split
recent30_df <- read_csv(
  "harmonization_files/AllDrivers_cc_recent30.csv",
  show_col_types = FALSE
)

# 3) Load SHAP values
load("Final_Models/FNConc_Yearly_shap_values_recent30.RData")    # shap_values_FNConc
shap_FNConc  <- shap_values_FNConc
load("Final_Models/FNYield_Yearly_shap_values_recent30.RData")  # shap_values_FNYield
shap_FNYield <- shap_values_FNYield

# 4) Responses & predictors
response_FNConc  <- recent30_df$FNConc
response_FNYield <- recent30_df$FNYield
X_FNConc  <- recent30_df[, colnames(shap_FNConc)]
X_FNYield <- recent30_df[, colnames(shap_FNYield)]

# 5) Create output dirs
base_out <- "Final_Figures"
pdp_base <- file.path(base_out, "Exploratory_LOESS_PDP")
conc_out <- file.path(pdp_base, "Concentration")
yield_out<- file.path(pdp_base, "Yield")
dir.create(conc_out, recursive = TRUE, showWarnings = FALSE)
dir.create(yield_out,recursive = TRUE, showWarnings = FALSE)

# 6) Recode map
recode_map <- setNames(
  c("N","P","NPP","ET","Greenup Day","Precip","Temp","Snow Cover","Permafrost",
    "Elevation","Basin Slope","Flashiness (RBI)","Recession Curve Slope",
    "Land: Bare","Land: Cropland","Land: Forest","Land: Grass & Shrub",
    "Land: Ice & Snow","Land: Impervious","Land: Salt Water","Land: Tidal Wetland",
    "Land: Water Body","Land: Wetland Marsh","Rock: Volcanic","Rock: Sedimentary",
    "Rock: Carbonate Evaporite","Rock: Metamorphic","Rock: Plutonic"
  ),
  c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
    "snow_cover","permafrost","elevation","basin_slope","RBI",
    "recession_slope","land_Bare","land_Cropland","land_Forest",
    "land_Grassland_Shrubland","land_Ice_Snow","land_Impervious",
    "land_Salt_Water","land_Tidal_Wetland","land_Water","land_Wetland_Marsh",
    "rocks_volcanic","rocks_sedimentary","rocks_carbonate_evaporite",
    "rocks_metamorphic","rocks_plutonic"
  )
)

# 7) LOESS‑PDP function
make_loess_pdp <- function(driver, shap_val, response, feat_key, model_name, recode_map) {
  df <- tibble(driver, shap_val, response) %>%
    filter(is.finite(driver), is.finite(shap_val))
  pretty <- if (grepl("^rocks_", feat_key)) {
    sub("^rocks_", "Rock: ", tools::toTitleCase(feat_key))
  } else recode_map[[feat_key]] %||% feat_key
  legend_title <- switch(
    model_name,
    "Concentration" = expression("Concentration (mg " * L^-1 * ")"),
    "Yield"         = expression("Yield (kg " * km^-2 * " yr"^-1 * ")")
  )
  p <- ggplot(df, aes(driver, shap_val, fill = response)) +
    geom_hline(yintercept=0, linetype="dashed", color="grey40") +
    geom_point(shape=21, color="darkgray", size=2.7, alpha=0.9) +
    geom_smooth(method="loess", se=FALSE, size=1, color="#6699CC") +
    scale_fill_gradient(
      low="white", high="black",
      name=legend_title,
      guide=guide_colourbar(
        title.position="top", title.hjust=0.5,
        barwidth=unit(15,"lines"), barheight=unit(0.6,"cm")
      )
    ) +
    labs(x=pretty, y="SHAP value", title=paste(model_name, "–", pretty)) +
    theme_classic(base_size=18) +
    theme(
      legend.position="bottom",
      axis.title=element_text(size=16),
      axis.text=element_text(size=14),
      plot.title=element_text(size=18, face="bold", hjust=0.5)
    )
  if (feat_key %in% c("P", "NOx")) {
    p <- p + scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x)10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    )
  }
  if (feat_key == "land_Wetland_Marsh") {
    p <- p + coord_cartesian(xlim = c(0,35), expand = FALSE)
  }
  p
}

# 8a) FNConc
for (feat in colnames(shap_FNConc)) {
  p <- make_loess_pdp(
    driver    = X_FNConc[[feat]],
    shap_val  = shap_FNConc[, feat],
    response  = response_FNConc,
    feat_key  = feat,
    model_name= "Concentration",
    recode_map= recode_map
  )
  ggsave(
    filename = file.path(conc_out, sprintf("PDP_Concentration_%s_LOESS.png", feat)),
    plot     = p,
    width    = 6, height = 6, dpi = 300, bg="white"
  )
}

# 8b) FNYield
for (feat in colnames(shap_FNYield)) {
  p <- make_loess_pdp(
    driver    = X_FNYield[[feat]],
    shap_val  = shap_FNYield[, feat],
    response  = response_FNYield,
    feat_key  = feat,
    model_name= "Yield",
    recode_map= recode_map
  )
  ggsave(
    filename = file.path(yield_out, sprintf("PDP_Yield_%s_LOESS.png", feat)),
    plot     = p,
    width    = 6, height = 6, dpi = 300, bg="white"
  )
}
