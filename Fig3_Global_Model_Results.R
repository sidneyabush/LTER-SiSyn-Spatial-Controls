###############################################################################
# COMPLETE WORKFLOW: 2×2 Grid of LM (A, B) and SHAP Dot Plots (C, D),
# with a Single Shared Legend (moved under panel C), and No Bold Text
###############################################################################

# 1. Load needed packages
librarian::shelf(
  iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach,
  randomForest, tibble, viridis, RColorBrewer, patchwork, scales
)

#––– set a base theme for all plots –––#
theme_set(
  theme_classic(base_size = 14) +
    theme(
      panel.background  = element_rect(fill   = "white", colour = NA),
      plot.background   = element_rect(fill   = "white", colour = NA),
      legend.background = element_rect(fill   = "white", colour = NA),
      legend.key        = element_rect(fill   = "white", colour = NA)
    )
)

# Clear environment
rm(list = ls())

# 2. Set Working & Output Directories (change paths if needed)
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir       <- "Final_Figures"
final_models_dir <- "Final_Models"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(final_models_dir, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# 3. Load FNConc (Concentration) data & models
# =============================================================================
load(file.path(final_models_dir, "FNConc_Yearly_rf_model2.RData"))       # loads rf_model2
rf_model2_FNConc    <- rf_model2

load(file.path(final_models_dir, "FNConc_Yearly_kept_drivers.RData"))    # loads kept_drivers
kept_drivers_FNConc <- kept_drivers

load(file.path(final_models_dir, "FNConc_Yearly_stream_ids.RData"))      # loads drivers_df
drivers_df_FNConc   <- drivers_df

load(file.path(final_models_dir, "FNConc_Yearly_shap_values_new.RData")) # loads shap_values_FNConc
shap_values_FNConc  <- shap_values_FNConc

load(file.path(final_models_dir, "FNConc_Yearly_numeric.RData"))         # loads drivers_numeric
drivers_numeric_FNConc <- drivers_numeric


# =============================================================================
# 4. Load FNYield (Yield) data & models
# =============================================================================
load(file.path(final_models_dir, "FNYield_Yearly_rf_model2.RData"))      # loads rf_model2
rf_model2_FNYield   <- rf_model2

load(file.path(final_models_dir, "FNYield_Yearly_kept_drivers.RData"))   # loads kept_drivers
kept_drivers_FNYield <- kept_drivers

load(file.path(final_models_dir, "FNYield_Yearly_stream_ids.RData"))     # loads drivers_df
drivers_df_FNYield  <- drivers_df

load(file.path(final_models_dir, "FNYield_Yearly_shap_values_new.RData"))# loads shap_values_FNYield
shap_values_FNYield <- shap_values_FNYield

load(file.path(final_models_dir, "FNYield_Yearly_numeric.RData"))        # loads drivers_numeric
drivers_numeric_FNYield <- drivers_numeric


###############################################################################
# 5. Build two LM plots: Predicted vs Observed (A = Concentration, B = Yield)
###############################################################################

# 5a. For FNConc (Concentration)
df_FNConc <- data.frame(
  predicted = rf_model2_FNConc$predicted,
  observed  = drivers_numeric_FNConc$FNConc
)
oob_r2_FNConc   <- mean(rf_model2_FNConc$rsq)
r2_val_FNConc   <- format(oob_r2_FNConc, digits = 3)
rmse_val_FNConc <- format(sqrt(mean(rf_model2_FNConc$mse)), digits = 3)

lm_plot_FNConc <- ggplot(df_FNConc, aes(x = predicted, y = observed)) +
  geom_point(shape = 16, size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "#6699CC",
              linetype = "dashed", size = 1.2) +
  labs(
    x     = "Predicted",
    y     = "Observed",
    title = "Concentration"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
    axis.title = element_text(size = 14, face = "plain"),
    axis.text  = element_text(size = 14, face = "plain")
  ) +
  annotate("text",
           x     = max(df_FNConc$predicted) * 0.2,
           y     = max(df_FNConc$observed) * 0.95,
           label = paste("R² =", r2_val_FNConc),
           hjust = 0.5, size = 5
  ) +
  annotate("text",
           x     = max(df_FNConc$predicted) * 0.95,
           y     = min(df_FNConc$observed) * 1.05,
           label = paste("RMSE =", rmse_val_FNConc),
           hjust = 1, size = 5
  )

# 5b. For FNYield (Yield)
df_FNYield <- data.frame(
  predicted = rf_model2_FNYield$predicted,
  observed  = drivers_numeric_FNYield$FNYield
)
oob_r2_FNYield   <- mean(rf_model2_FNYield$rsq)
r2_val_FNYield   <- format(oob_r2_FNYield, digits = 3)
rmse_val_FNYield <- format(sqrt(mean(rf_model2_FNYield$mse)), digits = 3)

lm_plot_FNYield <- ggplot(df_FNYield, aes(x = predicted, y = observed)) +
  geom_point(shape = 16, size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "#6699CC",
              linetype = "dashed", size = 1.2) +
  labs(
    x     = "Predicted",
    y     = NULL,
    title = "Yield"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
    axis.title = element_text(size = 14, face = "plain"),
    axis.text  = element_text(size = 14, face = "plain")
  ) +
  annotate("text",
           x     = max(df_FNYield$predicted) * 0.2,
           y     = max(df_FNYield$observed) * 0.95,
           label = paste("R² =", r2_val_FNYield),
           hjust = 0.5, size = 5
  ) +
  annotate("text",
           x     = max(df_FNYield$predicted) * 0.95,
           y     = min(df_FNYield$observed) * 1.05,
           label = paste("RMSE =", rmse_val_FNYield),
           hjust = 1, size = 5
  )


###############################################################################
# 6. SHAP‐dot Plot Function (for both Concentration & Yield)
###############################################################################
create_shap_plots <- function(shap_values, kept_drivers) {
  # 6a. Compute “overall feature importance”
  shap_values_abs <- as.data.frame(shap_values) %>% mutate(across(everything(), abs))
  overall_feature_importance <- shap_values_abs %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    pivot_longer(cols = everything(),
                 names_to  = "feature",
                 values_to = "importance") %>%
    arrange(desc(importance)) %>%
    mutate(
      feature = recode(
        feature,
        "NOx"                      = "Nitrate",
        "P"                        = "P",
        "precip"                   = "Precip",
        "temp"                     = "Temperature",
        "snow_cover"               = "Snow Cover",
        "npp"                      = "NPP",
        "evapotrans"               = "ET",
        "greenup_day"              = "Greenup Day",
        "permafrost"               = "Permafrost",
        "elevation"                = "Elevation",
        "RBI"                      = "Flashiness (RBI)",
        "recession_slope"          = "Recession Slope",
        "basin_slope"              = "Basin Slope",
        "rocks_volcanic"           = "Rock: Volcanic",
        "rocks_sedimentary"        = "Rock: Sedimentary",
        "rocks_carbonate_evaporite"= "Rock: Carbonate Evaporite",
        "rocks_metamorphic"        = "Rock: Metamorphic",
        "rocks_plutonic"           = "Rock: Plutonic",
        "land_Bare"                = "Land: Bare",
        "land_Cropland"            = "Land: Cropland",
        "land_Forest"              = "Land: Forest",
        "land_Grassland_Shrubland" = "Land: Grassland & Shrubland",
        "land_Ice_Snow"            = "Land: Ice & Snow",
        "land_Impervious"          = "Land: Impervious",
        "land_Salt_Water"          = "Land: Salt Water",
        "land_Tidal_Wetland"       = "Land: Tidal Wetland",
        "land_Water"               = "Land: Water Body",
        "land_Wetland_Marsh"       = "Land: Weteland Marsh"
      )
    )
  
  # 6b. Put SHAP + scaled feature values into “long” form
  kept_drivers_scaled <- kept_drivers %>%
    mutate(across(everything(), ~ scales::rescale(., to = c(0, 1))))
  
  kept_drivers_with_id <- kept_drivers_scaled %>%
    mutate(id = seq_len(nrow(.))) %>%
    pivot_longer(-id,
                 names_to  = "feature",
                 values_to = "feature_value")
  
  shap_values_df <- as.data.frame(shap_values) %>%
    mutate(id = seq_len(nrow(shap_values)))
  
  shap_long <- shap_values_df %>%
    pivot_longer(-id,
                 names_to  = "feature",
                 values_to = "shap_value") %>%
    left_join(kept_drivers_with_id, by = c("id", "feature")) %>%
    mutate(
      feature = recode(
        feature,
        "NOx"                      = "Nitrate",
        "P"                        = "P",
        "precip"                   = "Precip",
        "temp"                     = "Temperature",
        "snow_cover"               = "Snow Cover",
        "npp"                      = "NPP",
        "evapotrans"               = "ET",
        "greenup_day"              = "Greenup Day",
        "permafrost"               = "Permafrost",
        "elevation"                = "Elevation",
        "RBI"                      = "Flashiness (RBI)",
        "recession_slope"          = "Recession Slope",
        "basin_slope"              = "Basin Slope",
        "rocks_volcanic"           = "Rock: Volcanic",
        "rocks_sedimentary"        = "Rock: Sedimentary",
        "rocks_carbonate_evaporite"= "Rock: Carbonate Evaporite",
        "rocks_metamorphic"        = "Rock: Metamorphic",
        "rocks_plutonic"           = "Rock: Plutonic",
        "land_Bare"                = "Land: Bare",
        "land_Cropland"            = "Land: Cropland",
        "land_Forest"              = "Land: Forest",
        "land_Grassland_Shrubland" = "Land: Grassland & Shrubland",
        "land_Ice_Snow"            = "Land: Ice & Snow",
        "land_Impervious"          = "Land: Impervious",
        "land_Salt_Water"          = "Land: Salt Water",
        "land_Tidal_Wetland"       = "Land: Tidal Wetland",
        "land_Water"               = "Land: Water Body",
        "land_Wetland_Marsh"       = "Land: Weteland Marsh"
      )
    )
  
  # 6c. Order “feature” so that most‐important appear at the top
  shap_long$feature <- factor(
    shap_long$feature,
    levels = rev(overall_feature_importance$feature)
  )
  
  # 6d. Build the SHAP‐dot plot (with its own legend under panel C)
  dot_plot <- ggplot(
    shap_long,
    aes(x = shap_value, y = feature, fill = feature_value)
  ) +
    geom_point(alpha = 0.6, size = 3, shape = 21, stroke = 0.1, color = "black") +
    scale_fill_gradientn(
      colors = c("white", "gray", "black"),
      name   = "Scaled Value",
      guide  = guide_colorbar(
        title.position = "top",
        title.hjust    = 0.5,
        barwidth       = unit(6, "cm"),
        barheight      = unit(0.4, "cm"),
        label.theme    = element_text(size = 12, face = "plain"),
        title.theme    = element_text(size = 14, face = "plain")
      )
    ) +
    labs(x = "SHAP Value", y = NULL) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    theme_classic(base_size = 14) +
    theme(
      axis.title       = element_text(size = 14, face = "plain"),
      axis.text        = element_text(size = 12, face = "plain"),
      legend.text      = element_text(size = 14, face = "plain"),
      legend.title     = element_text(size = 14, face = "plain"),
      legend.key.size  = unit(1, "lines"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(list(
    overall = overall_feature_importance,  # not used in this grid
    dot     = dot_plot
  ))
}

# 6a. Create SHAP‐dot for FNConc (Concentration)
fnconc_shap     <- create_shap_plots(shap_values_FNConc, kept_drivers_FNConc)
dot_plot_FNConc <- fnconc_shap$dot    # this one has its legend underneath

# 6b. Create SHAP‐dot for FNYield (Yield)
fnyield_shap     <- create_shap_plots(shap_values_FNYield, kept_drivers_FNYield)
dot_plot_FNYield <- fnyield_shap$dot   # also has legend by default, but we’ll remove it below


###############################################################################
# 7. Combine LM and Dot Plots into a 2×2 Grid 
#    - Top row: Panels A & B (LM plots)
#    - Bottom row: Panel C (SHAP‐dot with legend), Panel D (SHAP‐dot without legend)
#    - All tags set manually, no bold text
###############################################################################

# 7.1 Build the TOP ROW (two LM plots), tagged “A” and “B”
top_row <- 
  ( lm_plot_FNConc +
      labs(title = "Concentration") +
      labs(tag = "A") +
      theme(
        plot.tag          = element_text(size = 14, face = "plain"),
        plot.tag.position = c(0, 1)
      )
  ) |
  ( lm_plot_FNYield +
      labs(title = "Yield") +
      labs(tag = "B") +
      theme(
        plot.tag          = element_text(size = 14, face = "plain"),
        plot.tag.position = c(0, 1)
      )
  )

# 7.2 Build the BOTTOM ROW (two SHAP‐dot plots):
#      - Panel C: keep its legend, tag “C”
#      - Panel D: remove its legend, tag “D”
C_plot_tagged <- dot_plot_FNConc +
  theme(legend.position = "bottom") +
  labs(tag = "C") +
  theme(
    plot.tag          = element_text(size = 14, face = "plain"),
    plot.tag.position = c(0, 1)
  )

D_plot_tagged <- dot_plot_FNYield +
  theme(legend.position = "none") +
  labs(tag = "D") +
  theme(
    plot.tag          = element_text(size = 14, face = "plain"),
    plot.tag.position = c(0, 1)
  )

bottom_row <- C_plot_tagged | D_plot_tagged

# 7.3 Stack top_row above bottom_row with unequal heights (top_row taller)
final_grid <- (top_row / bottom_row) +
  plot_layout(
    heights = c(1.3, 1)  # top row = 1.3× height of bottom row
  ) &
  theme(
    plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
  )

# 7.4 Print and Save
print(final_grid)
ggsave(
  filename = file.path(output_dir, "Fig3_Global_Grid_FNConc_FNYield.png"),
  plot     = final_grid,
  width    = 12,
  height   = 10,
  dpi      = 300
)


###############################################################################
# 8. Combine Overall (Mean Absolute SHAP) Bar Plots into a 1×2 Grid
###############################################################################
# 8a. Re‐build the “overall feature importance” bar charts from step 6
overall_bar_FNConc  <- ggplot(fnconc_shap$overall, 
                              aes(x = reorder(feature, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "#5F7F94") +
  coord_flip() +
  labs(x = NULL, y = "Mean Absolute SHAP Value", title = "Concentration") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(size = 14, face = "plain"),
    axis.title.x     = element_text(size = 14, face = "plain"),
    plot.title       = element_text(size = 14, hjust = 0.5, face = "plain")
  )

overall_bar_FNYield <- ggplot(fnyield_shap$overall, 
                              aes(x = reorder(feature, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "#5F7F94") +
  coord_flip() +
  labs(x = NULL, y = "Mean Absolute SHAP Value", title = "Yield") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(size = 14, face = "plain"),
    axis.title.x     = element_text(size = 14, face = "plain"),
    plot.title       = element_text(size = 14, hjust = 0.5, face = "plain")
  )

# 8b. Put them side by side, tag “A” & “B” on those two
final_overall_grid <- ( overall_bar_FNConc + labs(tag = "A") +
                          theme(
                            plot.tag          = element_text(size = 14, face = "plain"),
                            plot.tag.position = c(0, 1)
                          )
) |
  ( overall_bar_FNYield + labs(tag = "B") +
      theme(
        plot.tag          = element_text(size = 14, face = "plain"),
        plot.tag.position = c(0, 1)
      )
  ) +
  plot_layout(widths = c(1, 1))

# 8c. Print and Save
print(final_overall_grid)
ggsave(
  filename = file.path(output_dir, "FigSX_Final_Overall_FNConc_FNYield.png"),
  plot     = final_overall_grid,
  width    = 15,
  height   = 7.5,
  dpi      = 300
)
