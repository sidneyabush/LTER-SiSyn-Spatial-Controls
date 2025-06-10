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
# 7. Combine LM (A,B) + Dot‐plots (C,D) into a 2×2 grid with shared legend
###############################################################################

# 7.1 Tag LM panels
A_tagged <- lm_plot_FNConc +
  labs(tag = "A") +
  theme(
    plot.tag         = element_text(size = 24, face = "plain"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin      = ggplot2::margin(10, 10, 10, 10, "pt")
  )

B_tagged <- lm_plot_FNYield +
  labs(tag = "B") +
  theme(
    plot.tag         = element_text(size = 24, face = "plain"),
    plot.tag.position = c(0.02, 0.98),
    axis.title.y     = element_blank(),
    plot.margin      = ggplot2::margin(10, 10, 10, 10, "pt")
  )

# 7.2 Prepare FNConc dot‐plot with legend on right (temporarily) to extract it
C_with_legend <- dot_plot_FNConc +
  labs(tag = "C") +
  theme(
    plot.tag         = element_text(size = 24, face = "plain"),
    plot.tag.position = c(0.02, 0.98),
    legend.position  = "right"
  )

# 7.3 Extract shared legend
shared_legend <- cowplot::get_legend(C_with_legend)

# 7.4 Strip legends from C & D
C_nolegend <- C_with_legend + theme(legend.position = "none")

D_tagged <- dot_plot_FNYield +
  labs(tag = "D") +
  theme(
    plot.tag         = element_text(size = 24, face = "plain"),
    plot.tag.position = c(0.02, 0.98),
    legend.position  = "none"
  )

# 7.5 Assemble the 2×2 grid + shared legend
final_2x2 <- cowplot::plot_grid(
  # Top row: A | B
  cowplot::plot_grid(A_tagged, B_tagged, ncol = 2),
  # Bottom row: C_nolegend | D_tagged
  cowplot::plot_grid(C_nolegend, D_tagged, ncol = 2),
  # Legend at bottom
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 1.2, 0.2),
  align = "v"
)

# 7.6 Save FIG 2
ggsave(
  file.path(output_dir, "Fig2_Global_Grid_FNConc_FNYield.png"),
  final_2x2,
  width = 16,
  height = 18,
  dpi = 300,
  bg = "white"
)

###############################################################################
# 8. Create Partial Dependence Plots for each driver (colored by lithologic type)
###############################################################################

# 8.1 Load required additional packages for partial dependence
if (!require(pdp)) install.packages("pdp")
library(pdp)

# 8.2 Create function to generate partial dependence plots
create_pdp_plots <- function(rf_model, drivers_data, drivers_consolidated_lith, model_name, selected_features = NULL) {
  
  # Get feature names from the model
  feature_names <- names(rf_model$forest$xlevels)
  if (is.null(feature_names)) {
    feature_names <- colnames(drivers_data)
  }
  
  # Filter to only selected features if provided
  if (!is.null(selected_features)) {
    feature_names <- intersect(feature_names, selected_features)
    cat("Using only selected features:", paste(feature_names, collapse = ", "), "\n")
  }
  
  # Use the final_cluster column from drivers_consolidated_lith for rock group coloring
  if ("final_cluster" %in% colnames(drivers_consolidated_lith)) {
    litho_type <- drivers_consolidated_lith$final_cluster
    cat("Using 'final_cluster' column for rock group coloring\n")
  } else {
    stop("Error: 'final_cluster' column not found in drivers_consolidated_lith. Please ensure this column exists.")
  }
  
  # Ensure lithologic type is a factor
  litho_type <- as.factor(litho_type)
  
  # Create color palette for lithologic types
  n_types <- length(levels(litho_type))
  if (n_types <= 8) {
    colors <- RColorBrewer::brewer.pal(max(3, n_types), "Set2")[1:n_types]
  } else {
    colors <- rainbow(n_types)
  }
  names(colors) <- levels(litho_type)
  
  # Create partial dependence plots for each feature
  pdp_plots <- list()
  
  for (i in seq_along(feature_names)) {
    feature <- feature_names[i]
    
    # Skip if feature not in drivers_data
    if (!feature %in% colnames(drivers_data)) next
    
    cat("Creating PDP for", feature, "...\n")
    
    # Calculate partial dependence
    pd_data <- partial(rf_model, pred.var = feature, train = drivers_data)
    
    # Add lithologic information for coloring individual points
    # Create expanded data for plotting individual predictions
    feature_range <- seq(min(drivers_data[[feature]], na.rm = TRUE),
                         max(drivers_data[[feature]], na.rm = TRUE),
                         length.out = 50)
    
    # Create data frame for plotting
    plot_data <- data.frame(
      feature_value = drivers_data[[feature]],
      litho_type = litho_type
    )
    
    # Get feature label for plotting
    feature_label <- ifelse(feature %in% names(recode_map), 
                            recode_map[feature], 
                            feature)
    
    # Create the plot
    p <- ggplot() +
      # Add partial dependence line
      geom_line(data = pd_data, 
                aes(x = get(feature), y = yhat), 
                color = "black", size = 1.5, alpha = 0.8) +
      # Add individual points colored by lithology
      geom_point(data = plot_data, 
                 aes(x = feature_value, y = 0, color = litho_type),
                 alpha = 0.6, size = 2, 
                 position = position_jitter(height = 0.02, width = 0)) +
      scale_color_manual(values = colors, name = "Rock Group") +
      labs(
        x = feature_label,
        y = paste("Partial Dependence\n(", model_name, ")", sep = ""),
        title = paste("PDP:", feature_label)
      ) +
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    
    pdp_plots[[paste0(feature, "_", model_name)]] <- p
  }
  
  return(pdp_plots)
}

# 8.3 Load the consolidated lithology dataframes
load(file.path(final_models_dir, "FNConc_HierClust_Workflow_Objects.RData"))
load(file.path(final_models_dir, "FNYield_HierClust_Workflow_Objects.RData"))

# 8.4 Define specific features for each model
# Map display names back to variable names in the data
reverse_recode_map <- setNames(names(recode_map), recode_map)

# Features for Concentration model (using display names, will be mapped back)
conc_features_display <- c(
  "Basin Slope", "Elevation", "Rock: Volcanic", "P", "Land: Water Body", 
  "Recession Slope", "Land: Forest", "Flashiness (RBI)", "Precip", 
  "Land: Cropland", "Rock: Sedimentary", "Land: Grass & Shrub", "Land: Impervious"
)

# Features for Yield model (using display names, will be mapped back)
yield_features_display <- c(
  "Rock: Volcanic", "ET", "NPP", "Flashiness (RBI)", "Temp", "Recession Slope", 
  "Rock: Plutonic", "Elevation", "NOx", "Land: Wetland Marsh", "P"
)

# Map display names back to variable names
conc_features_vars <- character()
for (display_name in conc_features_display) {
  if (display_name %in% reverse_recode_map) {
    conc_features_vars <- c(conc_features_vars, reverse_recode_map[display_name])
  } else if (display_name == "Rock: Volcanic") {
    conc_features_vars <- c(conc_features_vars, "rocks_volcanic")
  } else if (display_name == "Rock: Sedimentary") {
    conc_features_vars <- c(conc_features_vars, "rocks_sedimentary")
  }
}

yield_features_vars <- character()
for (display_name in yield_features_display) {
  if (display_name %in% reverse_recode_map) {
    yield_features_vars <- c(yield_features_vars, reverse_recode_map[display_name])
  } else if (display_name == "Rock: Volcanic") {
    yield_features_vars <- c(yield_features_vars, "rocks_volcanic")
  } else if (display_name == "Rock: Plutonic") {
    yield_features_vars <- c(yield_features_vars, "rocks_plutonic")
  }
}

# 8.5 Generate PDP plots for both models with selected features only
cat("Creating PDPs for FN Concentration model (selected features only)...\n")
pdp_plots_conc <- create_pdp_plots(
  rf_model = rf_model2_FNConc,
  drivers_data = kept_drivers_FNConc,
  drivers_consolidated_lith = drivers_numeric_consolidated_lith_FNConc,
  model_name = "Concentration",
  selected_features = conc_features_vars
)

cat("Creating PDPs for FN Yield model (selected features only)...\n")
pdp_plots_yield <- create_pdp_plots(
  rf_model = rf_model2_FNYield,
  drivers_data = kept_drivers_FNYield,
  drivers_consolidated_lith = drivers_numeric_consolidated_lith_FNYield,
  model_name = "Yield",
  selected_features = yield_features_vars
)

# 8.6 Create global_pdp output directory
global_pdp_dir <- "global_pdp"
dir.create(global_pdp_dir, showWarnings = FALSE, recursive = TRUE)

# 8.7 Combine all PDP plots
all_pdp_plots <- c(pdp_plots_conc, pdp_plots_yield)

# 8.8 Save individual PDP plots to global_pdp folder
for (plot_name in names(all_pdp_plots)) {
  ggsave(
    file.path(global_pdp_dir, paste0("PDP_", plot_name, ".png")),
    all_pdp_plots[[plot_name]],
    width = 10,
    height = 8,
    dpi = 300,
    bg = "white"
  )
}

cat("Partial dependence plots completed and saved!\n")
cat("Files created in 'global_pdp' folder:\n")
for (plot_name in names(all_pdp_plots)) {
  cat(paste0("- PDP_", plot_name, ".png\n"))
}