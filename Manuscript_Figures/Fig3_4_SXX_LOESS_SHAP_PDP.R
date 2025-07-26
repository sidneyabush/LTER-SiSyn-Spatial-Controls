# =============================================================================
# SHAP × LOESS Grids – recent30 Only – Figures 3, 4, and S‑“Other”
# =============================================================================

# 0) Clear & set WD
rm(list = ls())
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# 1) Libraries
librarian::shelf(
  cowplot, ggplot2, dplyr, tibble, purrr, scales, readr, patchwork, tools
)

# 2) Read recent30 split
recent30_df <- read_csv(
  "harmonization_files/AllDrivers_cc_recent30.csv",
  show_col_types = FALSE
)

# 3) Load recent30 SHAP values
load("Final_Models/FNConc_Yearly_shap_values_recent30.RData")    # shap_values_FNConc
shap_FNConc  <- shap_values_FNConc
load("Final_Models/FNYield_Yearly_shap_values_recent30.RData")  # shap_values_FNYield
shap_FNYield <- shap_values_FNYield

# 4) Extract responses & matching predictors
response_FNConc  <- recent30_df$FNConc
response_FNYield <- recent30_df$FNYield
X_FNConc  <- recent30_df[, colnames(shap_FNConc)]
X_FNYield <- recent30_df[, colnames(shap_FNYield)]

# 5) Recode map
recode_map <- setNames(
  c("N","P","NPP","ET","Greenup Day","Precip","Temp","Snow Cover","Permafrost",
    "Elevation","Basin Slope","Flashiness (RBI)","Recession Curve Slope",
    "Land: Bare","Land: Cropland","Land: Forest","Land: Grass & Shrub",
    "Land: Ice & Snow","Land: Impervious","Land: Salt Water","Land: Tidal Wetland",
    "Land: Water Body","Land: Wetland Marsh","Rock: Volcanic","Rock: Sedimentary",
    "Rock: Carbonate Evaporite","Rock: Metamorphic","Rock: Plutonic"),
  c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
    "snow_cover","permafrost","elevation","basin_slope","RBI",
    "recession_slope","land_Bare","land_Cropland","land_Forest",
    "land_Grassland_Shrubland","land_Ice_Snow","land_Impervious",
    "land_Salt_Water","land_Tidal_Wetland","land_Water","land_Wetland_Marsh",
    "rocks_volcanic","rocks_sedimentary","rocks_carbonate_evaporite",
    "rocks_metamorphic","rocks_plutonic")
)

# 6) “Other”‐features grid function
make_shap_loess_grid <- function(shap_matrix, drivers_data, response,
                                 units_expr, recode_map){
  lims <- range(response, na.rm = TRUE)
  label_for <- function(feat){
    if (grepl("^rocks_", feat)) {
      sub("^rocks_", "Rock: ", tools::toTitleCase(feat))
    } else recode_map[[feat]] %||% feat
  }
  panels <- map(colnames(shap_matrix), function(feat){
    df <- tibble(
      driver_value = drivers_data[[feat]],
      shap_value   = shap_matrix[, feat],
      response     = response
    ) %>% filter(is.finite(driver_value), is.finite(shap_value))
    p <- ggplot(df, aes(driver_value, shap_value, fill = response)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point(shape = 21, color = "darkgray", size = 2.7, alpha = 0.9) +
      geom_smooth(method = "loess", se = FALSE, size = 1, color = "#6699CC") +
      scale_fill_gradient(
        low = "white", high = "black", limits = lims, guide = "none"
      ) +
      labs(x = label_for(feat), y = "SHAP value") +
      theme_classic(base_size = 18) +
      theme(
        axis.title = element_text(size = 16),
        axis.text  = element_text(size = 14)
      )
    if (feat %in% c("P", "NOx")) {
      p <- p + scale_x_log10(
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
      )
    }
    if (feat == "land_Wetland_Marsh") {
      p <- p + coord_cartesian(xlim = c(0, 35), expand = FALSE)
    }
    p
  })
  legend_plot <- ggplot(
    tibble(x = 1, y = 1, response = response),
    aes(x, y, fill = response)
  ) +
    geom_tile() +
    scale_fill_gradient(
      low = "white", high = "black", limits = lims,
      name  = units_expr,
      guide = guide_colourbar(
        title.position = "top", title.hjust = 0.5,
        barwidth = unit(15, "lines"), barheight = unit(0.6, "cm")
      )
    ) +
    theme_void() +
    theme(
      legend.position   = "right",
      legend.direction  = "horizontal",
      legend.title      = element_text(size = 14),
      legend.text       = element_text(size = 12)
    )
  shared_leg <- get_legend(legend_plot)
  grid <- plot_grid(
    plotlist       = panels, ncol = 2,
    labels         = LETTERS[1:length(panels)],
    label_size     = 16, label_fontface = "plain", align = "hv"
  )
  plot_grid(grid, shared_leg, ncol = 1, rel_heights = c(1, 0.1))
}

# ──────────────────────────────────────────────────────────────────────────────
# 11) Fig 3: Concentration SHAP–LOESS grid (6 panels)
# ──────────────────────────────────────────────────────────────────────────────
conc_feats3 <- c(
  "basin_slope", "recession_slope", "land_Water",
  "land_Grassland_Shrubland", "precip", "P"
)
present3   <- intersect(conc_feats3, colnames(shap_FNConc))
global_fn_min <- min(response_FNConc, na.rm = TRUE)
global_fn_max <- max(response_FNConc, na.rm = TRUE)

build_panel3 <- function(feat, idx) {
  df <- tibble(
    driver_value = X_FNConc[[feat]],
    shap_value   = shap_FNConc[, feat],
    response     = response_FNConc
  ) %>% filter(is.finite(driver_value), is.finite(shap_value))
  p <- ggplot(df, aes(driver_value, shap_value, fill = response)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(shape = 21, color = "darkgray", size = 2.7, alpha = 0.9) +
    geom_smooth(method = "loess", se = FALSE, size = 1, color = "#6699CC") +
    scale_fill_gradient(
      low = "white", high = "black",
      limits = c(global_fn_min, global_fn_max),
      name   = expression("Concentration (mg " * L^-1 * ")"),
      guide  = guide_colourbar(
        title.position = "top", title.hjust = 0.5,
        barwidth = unit(20, "lines"), barheight = unit(0.6, "cm")
      )
    ) +
    labs(
      x = recode_map[[feat]],
      y = if (idx %% 2 == 1) "SHAP value" else NULL
    ) +
    theme_classic(base_size = 18) +
    theme(
      legend.position = "none",
      axis.title.y    = element_text(size = 16),
      axis.text       = element_text(size = 14),
      plot.margin     = ggplot2::margin(t = 20, r = 5, b = 5, l = 30, unit = "pt")
    )
  if (feat %in% c("P", "NOx")) {
    p <- p + scale_x_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x))
    )
  }
  if (feat == "land_Wetland_Marsh") {
    p <- p + coord_cartesian(xlim = c(0, 35), expand = FALSE)
  }
  p
}

leg3 <- build_panel3(present3[1], 1) +
  theme(
    legend.position  = "right",
    legend.direction = "horizontal"
  )
shared_leg3 <- get_legend(leg3)

panels3 <- map2(present3, seq_along(present3), build_panel3)
grid3   <- plot_grid(
  plotlist = panels3, ncol = 2,
  labels   = LETTERS[1:length(panels3)],
  label_size = 16, label_fontface = "plain", align = "hv"
)
fig3_recent30 <- plot_grid(grid3, shared_leg3, ncol = 1, rel_heights = c(1, 0.1))

ggsave(
  "Final_Figures/Fig3_recent30_Concentration_SHAP_grid.png",
  fig3_recent30, width = 12, height = 14, dpi = 300, bg = "white"
)

# ──────────────────────────────────────────────────────────────────────────────
# 12) Fig 4: Yield SHAP–LOESS grid (4 panels, log scale)
# ──────────────────────────────────────────────────────────────────────────────
yield_feats4 <- c("recession_slope","land_Wetland_Marsh","npp","NOx")
present4   <- intersect(yield_feats4, colnames(shap_FNYield))
global_y_min <- min(response_FNYield, na.rm = TRUE)
global_y_max <- max(response_FNYield, na.rm = TRUE)

build_panel4 <- function(feat, idx) {
  df <- tibble(
    driver_value = X_FNYield[[feat]],
    shap_value   = shap_FNYield[, feat],
    response     = response_FNYield
  ) %>% filter(is.finite(driver_value), is.finite(shap_value))
  p <- ggplot(df, aes(driver_value, shap_value, fill = response)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(shape = 21, color = "darkgray", size = 2.7, alpha = 0.9) +
    geom_smooth(method = "loess", se = FALSE, size = 1, color = "#6699CC") +
    scale_fill_gradient(
      low   = "white", high = "black",
      trans = "log10",
      name  = expression("log(Yield) (kg " * km^{-2} * " yr"^{-1} * ")"),
      guide = guide_colourbar(
        title.position = "top", title.hjust = 0.5,
        barwidth = unit(20, "lines"), barheight = unit(0.6, "cm")
      )
    ) +
    labs(
      x = recode_map[[feat]],
      y = if (idx %% 2 == 1) "SHAP value" else NULL
    ) +
    theme_classic(base_size = 18) +
    theme(
      legend.position = "none",
      axis.title.y    = element_text(size = 16),
      axis.text       = element_text(size = 14),
      plot.margin     = ggplot2::margin(t = 20, r = 5, b = 5, l = 30, unit = "pt")
    )
  if (feat %in% c("P", "NOx")) {
    p <- p + scale_x_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x))
    )
  }
  if (feat == "land_Wetland_Marsh") {
    p <- p + coord_cartesian(xlim = c(0, 35), ylim = c(NA, 3200), expand = FALSE)
  }
  p
}

leg4 <- build_panel4(present4[1], 1) +
  theme(
    legend.position  = "right",
    legend.direction = "horizontal"
  )
shared_leg4 <- get_legend(leg4)

panels4 <- map2(present4, seq_along(present4), build_panel4)
grid4   <- plot_grid(
  plotlist   = panels4, ncol = 2,
  labels     = LETTERS[1:length(panels4)],
  label_size = 16, label_fontface = "plain", align = "hv"
)
fig4_recent30 <- plot_grid(grid4, shared_leg4, ncol = 1, rel_heights = c(1, 0.1))

ggsave(
  "Final_Figures/Fig4_recent30_Yield_SHAP_grid_log.png",
  fig4_recent30, width = 12, height = 11.2, dpi = 300, bg = "white"
)

# ──────────────────────────────────────────────────────────────────────────────
# 13) Figure S‑“Other”: remaining features
# ──────────────────────────────────────────────────────────────────────────────
other_conc <- setdiff(colnames(shap_FNConc), conc_feats3)
figS_conc  <- make_shap_loess_grid(
  shap_FNConc[, other_conc],
  X_FNConc[, other_conc],
  response_FNConc,
  expression("Concentration (mg " * L^-1 * ")"),
  recode_map
)
ggsave(
  "Final_Figures/FigSX_recent30_Conc_SHAP_Grid.png",
  figS_conc, width = 12, height = 15, dpi = 300, bg = "white"
)

other_yield <- setdiff(colnames(shap_FNYield), yield_feats4)
figS_yield  <- make_shap_loess_grid(
  shap_FNYield[, other_yield],
  X_FNYield[, other_yield],
  response_FNYield,
  expression("Yield (kg " * km^-2 * " yr"^-1 * ")"),
  recode_map
)
ggsave(
  "Final_Figures/FigSX_recent30_Yield_SHAP_Grid.png",
  figS_yield, width = 12, height = 15, dpi = 300, bg = "white"
)
