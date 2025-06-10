###############################################################################
# UPDATED Fig 2 SCRIPT: 
###############################################################################

# 1. Load needed packages
librarian::shelf(
  iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach,
  randomForest, tibble, viridis, RColorBrewer, patchwork, scales, cowplot
)

#––– set a base theme for all plots –––#
theme_set(
  theme_classic(base_size = 22) +
    theme(
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA)
    )
)

# 2. Clear environment
rm(list = ls())

# 3. Set Working & Output Directories
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir       <- "Final_Figures"
final_models_dir <- "Final_Models"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(final_models_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 4. Load FNConc & FNYield RF‐model objects + SHAP + driver data
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

# =============================================================================
# 5. Build two LM plots: Predicted vs Observed (A = Concentration, B = Yield)
# =============================================================================
# 5a. FNConc (Concentration)
df_FNConc <- data.frame(
  predicted = rf_model2_FNConc$predicted,
  observed  = drivers_numeric_FNConc$FNConc
)
oob_r2_FNConc   <- mean(rf_model2_FNConc$rsq)
r2_val_FNConc   <- format(oob_r2_FNConc, digits = 3)
rmse_val_FNConc <- format(
  sqrt(mean((df_FNConc$predicted - df_FNConc$observed)^2)),
  digits = 3
)

lm_plot_FNConc <- ggplot(df_FNConc, aes(x = predicted, y = observed)) +
  geom_point(shape = 16, size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "#6699CC",
              linetype = "dashed", size = 1.2) +
  labs(
    x     = "Predicted",
    y     = "Observed",
    title = "Concentration"
  ) +
  theme_classic(base_size = 22) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22, face = "plain"),
    axis.title = element_text(size = 22, face = "plain"),
    axis.text  = element_text(size = 22, face = "plain")
  ) +
  annotate("text",
           x     = max(df_FNConc$predicted) * 0.2,
           y     = max(df_FNConc$observed) * 0.95,
           label = paste("R² =", r2_val_FNConc),
           hjust = 0.5, size = 8
  ) +
  annotate("text",
           x     = max(df_FNConc$predicted) * 0.95,
           y     = min(df_FNConc$observed) * 1.05,
           label = paste("RMSE =", rmse_val_FNConc),
           hjust = 1, size = 8
  )

# 5b. FNYield (Yield)
df_FNYield <- data.frame(
  predicted = rf_model2_FNYield$predicted,
  observed  = drivers_numeric_FNYield$FNYield
)
oob_r2_FNYield   <- mean(rf_model2_FNYield$rsq)
r2_val_FNYield   <- format(oob_r2_FNYield, digits = 3)
rmse_val_FNYield <- format(
  sqrt(mean((df_FNYield$predicted - df_FNYield$observed)^2)),
  digits = 3
)

lm_plot_FNYield <- ggplot(df_FNYield, aes(x = predicted, y = observed)) +
  geom_point(shape = 16, size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "#6699CC",
              linetype = "dashed", size = 1.2) +
  labs(
    x     = "Predicted",
    y     = NULL,
    title = "Yield"
  ) +
  theme_classic(base_size = 22) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22, face = "plain"),
    axis.title = element_text(size = 22, face = "plain"),
    axis.text  = element_text(size = 22, face = "plain")
  ) +
  annotate("text",
           x     = max(df_FNYield$predicted) * 0.2,
           y     = max(df_FNYield$observed) * 0.95,
           label = paste("R² =", r2_val_FNYield),
           hjust = 0.5, size = 8
  ) +
  annotate("text",
           x     = max(df_FNYield$predicted) * 0.95,
           y     = min(df_FNYield$observed) * 1.05,
           label = paste("RMSE =", rmse_val_FNYield),
           hjust = 1, size = 8
  )

###############################################################################
# 6. Replace “create_shap_plots()” with SX‐style jittered dot‐plot function
###############################################################################

# 6.1 Define the feature order + recode map (exactly as in Figure SX)
var_order <- c(
  "NOx", "P", "npp", "evapotrans", "greenup_day", "precip", "temp",
  "snow_cover", "permafrost", "elevation", "basin_slope", "RBI",
  "recession_slope", "land_Bare", "land_Cropland", "land_Forest",
  "land_Grassland_Shrubland", "land_Ice_Snow", "land_Impervious",
  "land_Salt_Water", "land_Tidal_Wetland", "land_Water",
  "land_Wetland_Marsh"
)
var_labels <- c(
  "NOx", "P", "NPP", "ET", "Greenup Day", "Precip", "Temp",
  "Snow Cover", "Permafrost", "Elevation", "Basin Slope",
  "Flashiness (RBI)", "Recession Slope", "Land: Bare", "Land: Cropland",
  "Land: Forest", "Land: Grass & Shrub", "Land: Ice & Snow",
  "Land: Impervious", "Land: Salt Water", "Land: Tidal Wetland",
  "Land: Water Body", "Land: Wetland Marsh"
)
recode_map <- setNames(var_labels, var_order)

# 6.2 Log‐transform NOx & P, then rescale *all* features to [0,1]
kept_FNConc_scaled  <- kept_drivers_FNConc %>%
  mutate(
    # NOx = log10(NOx),
    P   = log10(P)
  ) %>%
  mutate(across(everything(), ~ scales::rescale(., to = c(0, 1))))

kept_FNYield_scaled <- kept_drivers_FNYield %>%
  mutate(
    NOx = log10(NOx),
    P   = log10(P)
  ) %>%
  mutate(across(everything(), ~ scales::rescale(., to = c(0, 1))))

# for use in Fig3 plots
save(
  kept_FNConc_scaled,
  kept_FNYield_scaled,
  file = file.path(final_models_dir, "HierClust_scaled_drivers.RData")
)


# 6.3 Compute global color‐scale limits from both FNConc & FNYield scaled drivers
existing_feats_FNConc <- intersect(var_order, colnames(kept_FNConc_scaled))
all_vals_FNConc  <- unlist(kept_FNConc_scaled[, existing_feats_FNConc], use.names = FALSE)

existing_feats_FNYield <- intersect(var_order, colnames(kept_FNYield_scaled))
all_vals_FNYield <- unlist(kept_FNYield_scaled[, existing_feats_FNYield], use.names = FALSE)

global_scaled_min <- min(c(all_vals_FNConc, all_vals_FNYield), na.rm = TRUE)
global_scaled_max <- max(c(all_vals_FNConc, all_vals_FNYield), na.rm = TRUE)


# 6.4 Compute global SHAP limits separately (to fix x‐axis extent)
global_shap_min_conc  <- min(shap_values_FNConc,  na.rm = TRUE)
global_shap_max_conc  <- max(shap_values_FNConc,  na.rm = TRUE)
global_shap_min_yield <- min(shap_values_FNYield, na.rm = TRUE)
global_shap_max_yield <- max(shap_values_FNYield, na.rm = TRUE)


# ——— 6.5 SX‐style “create_shap_dot_global” function (with rocks_ recoding) ———
create_shap_dot_global <- function(shap_matrix, kept_scaled, 
                                   global_shap_min, global_shap_max) {
  # shap_matrix: raw SHAP values (rows = observations, cols = features)
  # kept_scaled: data.frame of same shape, but with features scaled [0,1]
  
  # 6.5a. Pivot SHAP into long form
  shap_df <- as.data.frame(shap_matrix) %>%
    mutate(id = seq_len(nrow(.))) %>%
    pivot_longer(-id, names_to = "feature", values_to = "shap_value")
  
  # 6.5b. Pivot scaled feature values into long form
  feat_df <- kept_scaled %>%
    mutate(id = seq_len(nrow(.))) %>%
    pivot_longer(-id, names_to = "feature", values_to = "feature_value")
  
  # 6.5c. Join and recode feature names, including any that start with “rocks_”
  df_long <- left_join(shap_df, feat_df, by = c("id", "feature")) %>%
    mutate(
      feature = case_when(
        grepl("^rocks_volcanic$", feature)            ~ "Rock: Volcanic",
        grepl("^rocks_sedimentary$", feature)         ~ "Rock: Sedimentary",
        grepl("^rocks_carbonate_evaporite$", feature)  ~ "Rock: Carbonate Evaporite",
        grepl("^rocks_metamorphic$", feature)         ~ "Rock: Metamorphic",
        grepl("^rocks_plutonic$", feature)            ~ "Rock: Plutonic",
        TRUE                                          ~ recode(feature, !!!recode_map, .default = NA_character_)
      )
    ) %>%
    filter(!is.na(feature))  # drop any columns that neither start with 'rocks_' nor appear in recode_map
  
  # 6.5d. Determine feature order by descending mean(|shap|)
  feature_order <- df_long %>%
    group_by(feature) %>%
    summarize(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap)) %>%
    pull(feature)
  
  df_long <- df_long %>%
    mutate(feature = factor(feature, levels = rev(feature_order)))
  
  # 6.5e. Build SX‐style jittered dot plot
  ggplot(df_long, aes(x = shap_value, y = feature)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_jitter(
      aes(fill = feature_value),
      shape = 21,
      color = "darkgray",
      height = 0.2,
      size   = 2.7,
      alpha  = 0.9
    ) +
    scale_fill_gradient(
      low    = "white",
      high   = "black",
      limits = c(global_scaled_min, global_scaled_max),
      name   = "Scaled Value",
      guide = guide_colourbar(
        barheight      = unit(1.3, "cm"),
        barwidth       = unit(20, "lines"),
        title.position = "top",
        title.theme    = element_text(size = 22, face = "plain", hjust = 0.5),
        label.theme    = element_text(size = 20)
      )
    ) +
    scale_x_continuous(limits = c(global_shap_min, global_shap_max)) +
    labs(x = NULL, y = NULL) +
    theme_classic(base_size = 22) +
    theme(
      axis.text.y       = element_text(size = 20),
      axis.text.x       = element_text(size = 20),
      axis.title.x      = element_text(size = 22),
      axis.title.y      = element_text(size = 22),
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      legend.title      = element_text(size = 22, face = "plain"),
      legend.text       = element_text(size = 20),
      legend.key.height = unit(1.3, "cm"),
      legend.key.width  = unit(6, "cm"),
      legend.justification = "center"
    )
}

# 6.6 Create global dot‐plots for FNConc & FNYield
dot_plot_FNConc <- create_shap_dot_global(
  shap_matrix      = shap_values_FNConc,
  kept_scaled      = kept_FNConc_scaled,
  global_shap_min  = global_shap_min_conc,
  global_shap_max  = global_shap_max_conc
)

dot_plot_FNYield <- create_shap_dot_global(
  shap_matrix      = shap_values_FNYield,
  kept_scaled      = kept_FNYield_scaled,
  global_shap_min  = global_shap_min_yield,
  global_shap_max  = global_shap_max_yield
)

###############################################################################
# 7. Create Fig 2
###############################################################################

# 7.0 Define & generate mean-|SHAP| bar-plots with pretty labels & correct order
create_shap_bar_global <- function(shap_matrix, model_name) {
  shap_df <- as.data.frame(shap_matrix) %>%
    pivot_longer(
      cols      = everything(),
      names_to  = "feature",
      values_to = "shap_value"
    )
  
  bar_stats <- shap_df %>%
    group_by(feature) %>%
    summarize(mean_abs = mean(abs(shap_value), na.rm = TRUE), .groups="drop") %>%
    # recode both rocks_ and the var_order features
    mutate(pretty = case_when(
      grepl("^rocks_volcanic$",           feature) ~ "Rock: Volcanic",
      grepl("^rocks_sedimentary$",        feature) ~ "Rock: Sedimentary",
      grepl("^rocks_carbonate_evaporite$",feature) ~ "Rock: Carbonate Evaporite",
      grepl("^rocks_metamorphic$",        feature) ~ "Rock: Metamorphic",
      grepl("^rocks_plutonic$",           feature) ~ "Rock: Plutonic",
      TRUE                                        ~ recode(feature, !!!recode_map)
    )) %>%
    # sort descending by mean_abs, then reverse for factor levels:
    arrange(desc(mean_abs)) %>%
    mutate(pretty = factor(pretty, levels = rev(unique(pretty))))
  
  ggplot(bar_stats, aes(x = pretty, y = mean_abs)) +
    geom_col() +
    coord_flip() +
    labs(
      x     = NULL,
      y     = "Mean Absolute SHAP",
      title = NULL
    ) +
    theme_classic(base_size = 22) +                # match the others
    theme(
      plot.title   = element_text(hjust = 0.5, size = 22, face = "bold"),
      axis.text.y  = element_text(size = 20, face = "plain"),
      axis.text.x  = element_text(size = 20, face = "plain"),
      axis.title.x = element_text(size = 22, face = "plain")
    )
}

# regenerate your bars:
bar_plot_FNConc  <- create_shap_bar_global(shap_values_FNConc,  "Concentration")
bar_plot_FNYield <- create_shap_bar_global(shap_values_FNYield, "Yield")

# 7.1 (unchanged) A & B:
A_tagged <- lm_plot_FNConc + labs(tag="A") +
  theme(plot.tag=element_text(size=24), plot.tag.position=c(0.02,0.98))

B_tagged <- lm_plot_FNYield + labs(tag="B") +
  theme(plot.tag=element_text(size=24), plot.tag.position=c(0.02,0.98),
        axis.title.y=element_blank())

# 7.2 (unchanged) C & D:
C_tagged <- bar_plot_FNConc + labs(tag="C") +
  theme(plot.tag=element_text(size=24), plot.tag.position=c(0.02,0.98))

D_tagged <- bar_plot_FNYield + labs(tag="D") +
  theme(plot.tag=element_text(size=24), plot.tag.position=c(0.02,0.98))

# 7.3 Extract shared legend from the FNConc dot plot
temp_leg <- dot_plot_FNConc + theme(legend.position="right")
shared_legend <- cowplot::get_legend(temp_leg)

# 7.4 Create no-legend dot plots and tag E & F
dot_nolegend_FNConc   <- dot_plot_FNConc   + theme(legend.position="none")
dot_nolegend_FNYield  <- dot_plot_FNYield  + theme(legend.position="none")

E_tagged <- dot_nolegend_FNConc + labs(tag="E") +
  theme(plot.tag=element_text(size=24), plot.tag.position=c(0.02,0.98))

F_tagged <- dot_nolegend_FNYield + labs(tag="F") +
  theme(plot.tag=element_text(size=24), plot.tag.position=c(0.02,0.98))

# 7.5 Assemble rows: 
row1 <- cowplot::plot_grid(A_tagged, B_tagged, ncol=2)
row2 <- cowplot::plot_grid(C_tagged, D_tagged, ncol=2)
row3 <- cowplot::plot_grid(E_tagged, F_tagged, ncol=2)

final_3x2 <- cowplot::plot_grid(
  row1, row2, row3, shared_legend,
  ncol       = 1,
  rel_heights= c(1, 1.1, 1.2, 0.2),
  align      = "v"
)

# 7.6 Save FIG 2
ggsave(
  file.path(output_dir, "Fig2_Global_3row2col_FNConc_FNYield.png"),
  final_3x2,
  width  = 20,
  height = 25,
  dpi    = 300,
  bg     = "white"
)

###############################################################################
# 8. Create Partial Dependence Plots for each driver (using *all* kept_drivers)
###############################################################################

# 8.2 Generate PDP plots for both models, using kept_drivers as training data
cat("Creating PDPs for FN Concentration model (all kept drivers)...\n")
pdp_plots_conc <- create_pdp_plots(
  rf_model                  = rf_model2_FNConc,
  drivers_data              = kept_drivers_FNConc,
  drivers_consolidated_lith = drivers_numeric_consolidated_lith_FNConc,
  model_name                = "Concentration"
)

cat("Creating PDPs for FN Yield model (all kept drivers)...\n")
pdp_plots_yield <- create_pdp_plots(
  rf_model                  = rf_model2_FNYield,
  drivers_data              = kept_drivers_FNYield,
  drivers_consolidated_lith = drivers_numeric_consolidated_lith_FNYield,
  model_name                = "Yield"
)

# 8.3 Define output directory under Final_Figures and create it
global_pdp_dir <- file.path(output_dir, "global_pdp")
dir.create(global_pdp_dir, showWarnings = FALSE, recursive = TRUE)

# 8.4 Save individual PDP plots to Final_Figures/global_pdp
all_pdp_plots <- c(pdp_plots_conc, pdp_plots_yield)
for (plot_name in names(all_pdp_plots)) {
  ggsave(
    filename = file.path(global_pdp_dir, paste0("PDP_", plot_name, ".png")),
    plot     = all_pdp_plots[[plot_name]],
    width    = 10,
    height   = 8,
    dpi      = 300,
    bg       = "white"
  )
}

cat("Partial dependence plots completed and saved in:", global_pdp_dir, "\n")

###############################################################################
# 9. Create SHAP vs Driver Scatterplots by Lithology Group (with LOESS fits)
###############################################################################

# 9.1 Define custom cluster color palette
theme_cluster_colors <- c(
  "Volcanic"            = "#AC7B32",
  "Sedimentary"         = "#579C8E",
  "Mixed Sedimentary"   = "#89C8A0",
  "Plutonic"            = "#8D9A40",
  "Metamorphic"         = "#C26F86",
  "Carbonate Evaporite" = "#5E88B0"
)

# 9.2 Function factory: returns a SHAP scatter plotting function for given method
make_shap_scatter_fn <- function(fit_method, output_subdir) {
  function(shap_matrix, drivers_data, drivers_consolidated_lith, model_name, base_output) {
    feats <- colnames(shap_matrix)
    litho <- factor(drivers_consolidated_lith$final_cluster)
    colors <- theme_cluster_colors[levels(litho)]
    
    out_dir <- file.path(base_output, output_subdir)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    for (feat in feats) {
      if (!feat %in% colnames(drivers_data)) next
      df <- data.frame(
        driver_value = drivers_data[[feat]],
        shap_value   = shap_matrix[, feat],
        lithology    = litho
      )
      df <- df[is.finite(df$driver_value) & is.finite(df$shap_value), ]
      if (nrow(df) < 10) next
      
      feat_label <- if (feat %in% names(recode_map)) recode_map[feat] else feat
      
      p <- ggplot(df, aes(x = driver_value, y = shap_value, color = lithology)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
        geom_point(alpha = 0.6, size = 2) +
        geom_smooth(method = fit_method, se = FALSE, size = 1) +
        scale_color_manual(values = colors, name = "Lithology") +
        labs(
          x     = feat_label,
          y     = paste("SHAP value\n(", model_name, ")", sep = ""),
          title = paste("SHAP vs", feat_label)
        ) +
        theme_classic(base_size = 18) +
        theme(
          plot.title      = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.title      = element_text(size = 16),
          axis.text       = element_text(size = 14),
          legend.position = "bottom",
          legend.title    = element_text(size = 14),
          legend.text     = element_text(size = 12)
        )
      
      fname <- file.path(out_dir, paste0("SHAP_", fit_method, "_", model_name, "_", feat, ".png"))
      ggsave(fname, p, width = 8, height = 6, dpi = 300, bg = "white")
    }
    message("Plots saved to ", out_dir)
  }
}

# 9.3 Instantiate two plotting functions
gscatter_lm   <- make_shap_scatter_fn("lm",   "shap_scatter_lm")
gscatter_loess <- make_shap_scatter_fn("loess", "shap_scatter_loess")

# 9.4 Generate for FNConc and FNYield
for (fn in list(
  list(fn = gscatter_lm,   suffix = "LM"),
  list(fn = gscatter_loess, suffix = "LOESS")
)) {
  fn$fn(
    shap_matrix               = shap_values_FNConc,
    drivers_data              = kept_drivers_FNConc,
    drivers_consolidated_lith = drivers_numeric_consolidated_lith_FNConc,
    model_name                = "Concentration",
    base_output               = output_dir
  )
  fn$fn(
    shap_matrix               = shap_values_FNYield,
    drivers_data              = kept_drivers_FNYield,
    drivers_consolidated_lith = drivers_numeric_consolidated_lith_FNYield,
    model_name                = "Yield",
    base_output               = output_dir
  )
}

message("All SHAP scatterplots generated under '", output_dir, "'.")

###############################################################################
# 10. Create SHAP vs Driver Scatterplots (full‐data LOESS fit, no lithology)
###############################################################################

make_shap_loess_full <- function(shap_matrix, drivers_data, model_name, base_output) {
  feats   <- colnames(shap_matrix)
  out_dir <- file.path(base_output, "shap_scatter_loess_full")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (feat in feats) {
    if (!feat %in% colnames(drivers_data)) next
    
    df <- data.frame(
      driver_value = drivers_data[[feat]],
      shap_value   = shap_matrix[, feat]
    ) %>%
      filter(is.finite(driver_value), is.finite(shap_value))
    if (nrow(df) < 10) next
    
    feat_label <- if (feat %in% names(recode_map)) recode_map[feat] else feat
    
    p <- ggplot(df, aes(x = driver_value, y = shap_value)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
      geom_point(alpha = 0.6, size = 2) +
      geom_smooth(method = "loess", se = FALSE, size = 1) +
      labs(
        x     = feat_label,
        y     = paste0("SHAP value\n(", model_name, ")"),
        title = paste("SHAP vs", feat_label, "(LOESS – full dataset)")
      ) +
      theme_classic(base_size = 18) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text  = element_text(size = 14)
      )
    
    # – log-scale P, NOx, ET –
    if (feat %in% c("P", "NOx", "evapotrans")) {
      p <- p +
        scale_x_log10(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::trans_format("log10", scales::math_format(10^.x))
        ) +
        labs(x = paste0("log10(", feat_label, ")"))
    }
    
    # – cap x at 0.35 for Wetland Marsh –
    if (feat == "land_Wetland_Marsh") {
      p <- p +
        scale_x_continuous(limits = c(0, 35))
    }
    
    fname <- file.path(out_dir, paste0("SHAP_loess_full_", model_name, "_", feat, ".png"))
    ggsave(fname, p, width = 8, height = 6, dpi = 300, bg = "white")
  }
  
  message("Full-data LOESS shap scatterplots saved to ", out_dir)
}

# 10.2 Generate full‐data LOESS plots for both models
make_shap_loess_full(
  shap_matrix  = shap_values_FNConc,
  drivers_data = kept_drivers_FNConc,
  model_name   = "Concentration",
  base_output  = output_dir
)

make_shap_loess_full(
  shap_matrix  = shap_values_FNYield,
  drivers_data = kept_drivers_FNYield,
  model_name   = "Yield",
  base_output  = output_dir
)
