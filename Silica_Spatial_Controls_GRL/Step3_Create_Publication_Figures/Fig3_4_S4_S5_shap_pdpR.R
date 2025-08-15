# #############################################################################
# Figures 3, 4, S4 and S5: SHAP and LOESS for Testing data (recent30) Only
# #############################################################################
# Required inputs:
#   1) <drv_dir>/AllDrivers_recent30_split.csv
#   2) <fm>/FNConc_Yearly_shap_values_recent30_split.RData
#   3) <fm>/FNYield_Yearly_shap_values_recent30_split.RData
#
# Outputs created:
#   A) <od>/Fig3_recent30_Concentration_SHAP_grid_split.png
#   B) <od>/Fig4_recent30_Yield_SHAP_grid_linear_split.png
#   C) <od>/FigS4_recent30_Conc_SHAP_Grid_split.png
#   D) <od>/FigS5_recent30_Yield_SHAP_Grid_split.png

rm(list = ls())
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

librarian::shelf(
  cowplot, ggplot2, dplyr, tibble, purrr, scales, readr, patchwork, tools)


# #############################################################################
# 1. Read recent30 split
# #############################################################################
recent30_df <- read_csv(
  "harmonization_files/AllDrivers_recent30_split.csv",
  show_col_types = FALSE
)

# #############################################################################
# 2. Load recent30 SHAP values
# #############################################################################
load("Final_Models/FNConc_Yearly_shap_values_recent30_split.RData")    
shap_FNConc  <- shap_values_FNConc
load("Final_Models/FNYield_Yearly_shap_values_recent30_split.RData")  
shap_FNYield <- shap_values_FNYield

# #############################################################################
# 3. Extract responses & matching predictors
# #############################################################################
response_FNConc  <- recent30_df$FNConc
response_FNYield <- recent30_df$FNYield
X_FNConc  <- recent30_df[, colnames(shap_FNConc)]
X_FNYield <- recent30_df[, colnames(shap_FNYield)]

# #############################################################################
# 4. Recode map
# #############################################################################
recode_map <- setNames(
  c("Log(N)","Log(P)","NPP","ET","Green-up day","Precip","Temp","Snow cover","Permafrost probability",
    "Elevation","Basin slope","RBI","RCS",
    "Bare land cover","Cropland cover","Forest cover","Grass & shrub cover",
    "Ice & snow cover","Impervious cover","Saltwater cover","Tidal wetland cover",
    "Open-water cover","Wetland cover","Volcanic rock","Sedimentary rock",
    "Carbonate-evaporite rock","Metamorphic rock","Plutonic rock"),
  
  c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
    "snow_cover","permafrost","elevation","basin_slope","RBI",
    "recession_slope","land_Bare","land_Cropland","land_Forest",
    "land_Grassland_Shrubland","land_Ice_Snow","land_Impervious",
    "land_Salt_Water","land_Tidal_Wetland","land_Water","land_Wetland_Marsh",
    "rocks_volcanic","rocks_sedimentary","rocks_carbonate_evaporite",
    "rocks_metamorphic","rocks_plutonic")
)

# helper function to build one panel 
build_one_panel <- function(feat, shap_matrix, drivers_data, response, lims, recode_map) {
  df <- tibble(
    driver_value = drivers_data[[feat]],
    shap_value   = shap_matrix[, feat],
    response     = response
  ) %>% filter(is.finite(driver_value), is.finite(shap_value))
  
  if (feat %in% c("P", "NOx")) {
    df <- df %>% filter(driver_value > 0)
  }
  
  if (nrow(df) > 0) {
    xlims_feat <- quantile(df$driver_value, probs = c(0.05, 0.95), na.rm = TRUE)
    df <- df %>% filter(driver_value >= xlims_feat[1], driver_value <= xlims_feat[2])
  } else {
    warning("No data left after filtering for feature: ", feat)
  }
  
  label_for <- function(f) if (!is.null(recode_map[[f]])) recode_map[[f]] else f
  
  p <- ggplot(df, aes(driver_value, shap_value, fill = response)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(shape = 21, color = "darkgray", size = 2.7, alpha = 0.9) +
    geom_smooth(method = "loess", se = FALSE, size = 1, color = "#6699CC") +
    scale_fill_gradient(low = "white", high = "black", limits = lims, guide = "none") +
    labs(x = label_for(feat), y = "SHAP value") +
    theme_classic(base_size = 18) +
    theme(
      axis.title = element_text(size = 16),
      axis.text  = element_text(size = 14)
    )
  # appropriate x-axis limits from trimmed df
  if (feat %in% c("P", "NOx")) {
    xlims_feat <- quantile(df$driver_value, probs = c(0.05, 0.95), na.rm = TRUE)
    p <- p + scale_x_log10(
      limits = c(xlims_feat[1], xlims_feat[2]),
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x))
    )
  } else {
    p <- p + scale_x_continuous(
      limits = range(df$driver_value, na.rm = TRUE),
      expand = expansion(mult = c(0.03, 0.03))
    )
  }
  
  
  if (feat %in% c("P", "NOx")) {
    p <- p + scale_x_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x))
    )
  }
  p
}

# #############################################################################
# 5. SI features grid function 
# #############################################################################
make_shap_loess_grid <- function(shap_matrix, drivers_data, response,
                                 units_expr, recode_map,
                                 verbose = FALSE) {
  # 1. Sanity checks
  if (ncol(shap_matrix) == 0) stop("shap_matrix has no columns")
  if (!all(colnames(shap_matrix) %in% colnames(drivers_data))) {
    stop("drivers_data missing columns: ", paste(setdiff(colnames(shap_matrix), colnames(drivers_data)), collapse = ", "))
  }
  if (length(response) != nrow(shap_matrix)) {
    stop("Length of response does not match rows of shap_matrix")
  }
  
  # 2. Build trimmed data for each feature first
  trimmed_list <- purrr::map(colnames(shap_matrix), function(feat) {
    df <- tibble(
      driver_value = drivers_data[[feat]],
      shap_value   = shap_matrix[, feat],
      response     = response
    ) %>% filter(is.finite(driver_value), is.finite(shap_value))
    
    if (feat %in% c("P", "NOx")) {
      df <- df %>% filter(driver_value > 0)
    }
    
    # trim driver_value to 5th–95th percentile
    if (nrow(df) > 0) {
      xlims_feat <- quantile(df$driver_value, probs = c(0.05, 0.95), na.rm = TRUE)
      df <- df %>% filter(driver_value >= xlims_feat[1], driver_value <= xlims_feat[2])
    } else {
      warning("No data left after filtering for feature: ", feat)
    }
    
    df$feature <- feat  # keep track
    df
  })
  
  names(trimmed_list) <- colnames(shap_matrix)
  
  # 3. Compute shared response limits from the trimmed data
  combined_trimmed <- bind_rows(trimmed_list)
  lims_trimmed <- range(combined_trimmed$response, na.rm = TRUE)
  
  # 4. Panel builder using pre-trimmed dfs
  label_for <- function(feat) if (!is.null(recode_map[[feat]])) recode_map[[feat]] else feat
  
  panels <- purrr::map(colnames(shap_matrix), function(feat) {
    if (verbose) message("Building panel for: ", feat)
    df <- trimmed_list[[feat]]
    
    # determine trimmed x-limits; for ET drop exact zeros so axis doesn't anchor at 0
    if (feat == "ET") {
      df_for_xlim <- df %>% filter(driver_value > 0)
      if (nrow(df_for_xlim) > 0) {
        xlims_feat <- quantile(df_for_xlim$driver_value, probs = c(0.05, 0.95), na.rm = TRUE)
      } else {
        xlims_feat <- quantile(df$driver_value, probs = c(0.05, 0.95), na.rm = TRUE)
      }
    } else {
      xlims_feat <- quantile(df$driver_value, probs = c(0.05, 0.95), na.rm = TRUE)
    }
    
    p <- ggplot(df, aes(driver_value, shap_value, fill = response)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point(shape = 21, color = "darkgray", size = 2.7, alpha = 0.9) +
      geom_smooth(method = "loess", se = FALSE, size = 1, color = "#6699CC") +
      scale_fill_gradient(
        low = "white", high = "black", limits = lims_trimmed, guide = "none"
      ) +
      labs(x = label_for(feat), y = "SHAP value") +
      theme_classic(base_size = 18) +
      theme(
        axis.title = element_text(size = 16),
        axis.text  = element_text(size = 14),
        plot.margin = ggplot2::margin(t = 10, r = 5, b = 25, l = 10, unit = "pt")  # extra bottom space
      )
    
    # x-axis scaling with trimmed limits and small buffer
    if (feat %in% c("P", "NOx")) {
      p <- p + scale_x_log10(
        limits = c(xlims_feat[1], xlims_feat[2]),
        expand = expansion(mult = c(0.02, 0.02)),
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
      )
    } else {
      p <- p + scale_x_continuous(
        limits = c(xlims_feat[1], xlims_feat[2]),
        expand = expansion(mult = c(0.02, 0.02))
      )
    }
    
    # prevent clipping of labels
    p <- p + coord_cartesian(clip = "off")
    
    p
  })
  
  # 5. Shared legend based on trimmed response range
  legend_plot <- ggplot(
    tibble(x = 1, y = 1, response = combined_trimmed$response),
    aes(x, y, fill = response)
  ) +
    geom_tile() +
    scale_fill_gradient(
      low = "white", high = "black", limits = lims_trimmed,
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
  
  grid <- cowplot::plot_grid(
    plotlist = panels, ncol = 2,
    labels = paste0(letters[seq_along(panels)], ")"),
    label_size = 20, label_fontface = "plain", align = "hv"
  )
  plot_grid(grid, shared_leg, ncol = 1, rel_heights = c(1, 0.1))
}

# #############################################################################
# 6. Create Fig3: Concentration SHAP–LOESS grid (6 panels)
# #############################################################################
conc_feats3 <- c(
  "basin_slope", "recession_slope", "land_Water", "NOx", "P"
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
  
  if (feat %in% c("P", "NOx")) {
    df <- df %>% filter(driver_value > 0)
  }
  
  # trim driver_value to 5th–95th percentile
  if (nrow(df) > 0) {
    xlims_feat <- quantile(df$driver_value, probs = c(0.05, 0.95), na.rm = TRUE)
    df <- df %>% filter(driver_value >= xlims_feat[1], driver_value <= xlims_feat[2])
  } else {
    xlims_feat <- c(NA, NA)
  }
  
  # compute fill limits from this trimmed df
  fill_lims <- range(df$response, na.rm = TRUE)
  
  p <- ggplot(df, aes(driver_value, shap_value, fill = response)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(shape = 21, color = "darkgray", size = 2.7, alpha = 0.9) +
    geom_smooth(method = "loess", se = FALSE, size = 1, color = "#6699CC") +
    scale_fill_gradient(
      low = "white", high = "black",
      limits = fill_lims,
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
      plot.margin     = ggplot2::margin(t = 10, r = 5, b = 25, l = 30, unit = "pt")
    )
  
  # x-axis scaling with trimmed limits and buffer
  if (feat %in% c("P", "NOx")) {
    p <- p + scale_x_log10(
      limits = c(xlims_feat[1], xlims_feat[2]),
      expand = expansion(mult = c(0.02, 0.02)),
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x))
    )
  } else {
    p <- p + scale_x_continuous(
      limits = c(xlims_feat[1], xlims_feat[2]),
      expand = expansion(mult = c(0.02, 0.02))
    )
  }
  
  p <- p + coord_cartesian(clip = "off")
  
  p
}

# build shared legend for Fig 3 from the union of all trimmed panels
all_trimmed3 <- purrr::map_dfr(present3, function(feat) {
  df <- tibble(
    driver_value = X_FNConc[[feat]],
    shap_value   = shap_FNConc[, feat],
    response     = response_FNConc
  ) %>% filter(is.finite(driver_value), is.finite(shap_value))
  
  if (feat %in% c("P", "NOx")) {
    df <- df %>% filter(driver_value > 0)
  }
  
  if (nrow(df) > 0) {
    xlims_feat <- quantile(df$driver_value, probs = c(0.05, 0.95), na.rm = TRUE)
    df <- df %>% filter(driver_value >= xlims_feat[1], driver_value <= xlims_feat[2])
  }
  df
})

fill_lims3 <- range(all_trimmed3$response, na.rm = TRUE)

legend_plot3 <- ggplot(all_trimmed3, aes(x = 1, y = 1, fill = response)) +
  geom_tile() +
  scale_fill_gradient(
    low = "white", high = "black",
    limits = fill_lims3,
    name = expression("Concentration (mg " * L^-1 * ")"),
    guide = guide_colourbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth  = unit(25, "lines"),
      barheight = unit(1, "cm"),
      label.theme = element_text(size = 14),
      title.theme = element_text(size = 16)
    )
  ) +
  theme_void() +
  theme(
    legend.position  = "right",
    legend.direction = "horizontal",
    legend.title     = element_text(size = 16),
    legend.text      = element_text(size = 14),
    legend.key.width = unit(3, "lines"),
    legend.key.height= unit(1.2, "lines")
  )

shared_leg3 <- get_legend(legend_plot3)


# build the 4 real panels
# build the 4 real panels
panels3 <- map2(present3, seq_along(present3), build_panel3)

# assemble grid 
# Fig 3
grid3 <- cowplot::plot_grid(
  plotlist = panels3, ncol = 2,
  labels = paste0(letters[seq_along(panels3)], ")"),
  label_size = 20, label_fontface = "plain", align = "hv"
)


# add legend
fig3_recent30 <- plot_grid(grid3, shared_leg3, ncol = 1, rel_heights = c(1, 0.1))

# #############################################################################
# 7. Create Fig4: Yield SHAP–LOESS grid 
# #############################################################################
yield_feats4 <- c("evapotrans","temp","recession_slope","land_Wetland_Marsh","npp","NOx")
present4   <- intersect(yield_feats4, colnames(shap_FNYield))
global_y_min <- min(response_FNYield, na.rm = TRUE)
global_y_max <- max(response_FNYield, na.rm = TRUE)

build_panel4 <- function(feat, idx) {
  df <- tibble(
    driver_value = X_FNYield[[feat]],
    shap_value   = shap_FNYield[, feat],
    response     = response_FNYield
  ) %>% filter(is.finite(driver_value), is.finite(shap_value))
  
  if (feat %in% c("P", "NOx")) {
    df <- df %>% filter(driver_value > 0)
  }
  
  if (nrow(df) > 0) {
    xlims_feat <- quantile(df$driver_value, probs = c(0.05, 0.95), na.rm = TRUE)
    df <- df %>% filter(driver_value >= xlims_feat[1], driver_value <= xlims_feat[2])
  } else {
    xlims_feat <- c(NA, NA)
  }
  
  fill_lims <- range(df$response, na.rm = TRUE)
  
  p <- ggplot(df, aes(driver_value, shap_value, fill = response)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(shape = 21, color = "darkgray", size = 2.7, alpha = 0.9) +
    geom_smooth(method = "loess", se = FALSE, size = 1, color = "#6699CC") +
    scale_fill_gradient(
      low = "white", high = "black",
      limits = fill_lims,
      name   = expression("Yield (kg " * km^-2 * " yr"^-1 * ")"),
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
      plot.margin     = ggplot2::margin(t = 10, r = 5, b = 25, l = 30, unit = "pt")
    )
  
  if (feat %in% c("P", "NOx")) {
    p <- p + scale_x_log10(
      limits = c(xlims_feat[1], xlims_feat[2]),
      expand = expansion(mult = c(0.02, 0.02)),
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x))
    )
  } else {
    p <- p + scale_x_continuous(
      limits = c(xlims_feat[1], xlims_feat[2]),
      expand = expansion(mult = c(0.02, 0.02))
    )
  }
  
  p <- p + coord_cartesian(clip = "off")
  
  p
}

all_trimmed4 <- purrr::map_dfr(present4, function(feat) {
  df <- tibble(
    driver_value = X_FNYield[[feat]],
    shap_value   = shap_FNYield[, feat],
    response     = response_FNYield
  ) %>% filter(is.finite(driver_value), is.finite(shap_value))
  
  if (feat %in% c("P", "NOx")) {
    df <- df %>% filter(driver_value > 0)
  }
  
  if (nrow(df) > 0) {
    xlims_feat <- quantile(df$driver_value, probs = c(0.05, 0.95), na.rm = TRUE)
    df <- df %>% filter(driver_value >= xlims_feat[1], driver_value <= xlims_feat[2])
  }
  df
})

fill_lims4 <- range(all_trimmed4$response, na.rm = TRUE)

legend_plot4 <- ggplot(all_trimmed4, aes(x = 1, y = 1, fill = response)) +
  geom_tile() +
  scale_fill_gradient(
    low = "white", high = "black",
    limits = fill_lims4,
    name = expression("Yield (kg " * km^-2 * " yr"^-1 * ")"),
    guide = guide_colourbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth  = unit(25, "lines"),
      barheight = unit(1, "cm"),
      label.theme = element_text(size = 14),
      title.theme = element_text(size = 16)
    )
  ) +
  theme_void() +
  theme(
    legend.position   = "right",
    legend.direction  = "horizontal",
    legend.title      = element_text(size = 16),
    legend.text       = element_text(size = 14),
    legend.key.width  = unit(3, "lines"),
    legend.key.height = unit(1.2, "lines")
  )

shared_leg4 <- get_legend(legend_plot4)

panels4 <- map2(present4, seq_along(present4), build_panel4)
# Fig 4
grid4 <- cowplot::plot_grid(
  plotlist = panels4, ncol = 2,
  labels = paste0(letters[seq_along(panels4)], ")"),
  label_size = 20, label_fontface = "plain", align = "hv"
)


fig4_recent30 <- plot_grid(grid4, shared_leg4, ncol = 1, rel_heights = c(1, 0.1))

# #############################################################################
# 8. Create SI Figs: remaining predictors not manually selected in Figs 3 & 4
# #############################################################################
other_conc <- setdiff(colnames(shap_FNConc), conc_feats3)
figS_conc  <- make_shap_loess_grid(
  shap_FNConc[, other_conc],
  X_FNConc[, other_conc],
  response_FNConc,
  expression("Concentration (mg " * L^-1 * ")"),
  recode_map
)
n <- length(other_conc)        
rows <- 5/2

ggsave(
  "Final_Figures/FigS4_recent30_Conc_SHAP_Grid_split.png",
  figS_conc, width = 12, height = 6*rows, dpi = 300, bg = "white"
)

other_yield <- setdiff(colnames(shap_FNYield), yield_feats4)
figS_yield  <- make_shap_loess_grid(
  shap_FNYield[, other_yield],
  X_FNYield[, other_yield],
  response_FNYield,
  expression("Yield (kg " * km^-2 * " yr"^-1 * ")"),
  recode_map
)

# Fig 3:
ggsave("Final_Figures/Fig3_recent30_Concentration_SHAP_grid_split.png",
       fig3_recent30, width = 12, height = 15, dpi = 300, bg = "white")

# Fig 4: 
ggsave("Final_Figures/Fig4_recent30_Yield_SHAP_grid_linear_split.png",
       fig4_recent30, width = 12, height = 15, dpi = 300, bg = "white")

# Fig S4: 
ggsave("Final_Figures/FigS4_recent30_Conc_SHAP_Grid_split.png",
       figS_conc, width = 12, height = 12, dpi = 300, bg = "white")

# Fig S5: 
ggsave("Final_Figures/FigS5_recent30_Yield_SHAP_Grid_split.png",
       figS_yield, width = 12, height = 15, dpi = 300, bg = "white")
