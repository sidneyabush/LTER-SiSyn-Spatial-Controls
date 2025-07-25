###############################################################################
# 02_make_Fig2_global_multi.R
# Re‑generates Fig 2 with multi‐subset Pred vs Obs panels (fixed dot‐plot)
###############################################################################

# 1. Load needed packages and set theme
librarian::shelf(
  iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, pdp,
  randomForest, tibble, viridis, RColorBrewer, patchwork, scales,
  cowplot
)

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

# 3. Set working & output directories
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
output_dir       <- "Final_Figures"
final_models_dir <- "Final_Models"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 4. Load FNConc & FNYield RF2 models, drivers, SHAP & numeric data

# --- FNConc RF2 model (saved as `rf2`) ---
load(file.path(final_models_dir, "FNConc_Yearly_rf_model2.RData"))  # loads rf2
rf_model2_FNConc <- rf2; rm(rf2)

load(file.path(final_models_dir, "FNConc_Yearly_kept_drivers.RData"))  # kept_drivers
kept_drivers_FNConc <- kept_drivers; rm(kept_drivers)

load(file.path(final_models_dir, "FNConc_Yearly_shap_values_new.RData")) # shap_values_FNConc
shap_values_FNConc <- shap_values_FNConc

load(file.path(final_models_dir, "FNConc_Yearly_numeric.RData"))    # drivers_numeric
drivers_numeric_FNConc <- drivers_numeric

# --- FNYield RF2 model (saved as `rf2`) ---
load(file.path(final_models_dir, "FNYield_Yearly_rf_model2.RData")) # loads rf2
rf_model2_FNYield <- rf2; rm(rf2)

load(file.path(final_models_dir, "FNYield_Yearly_kept_drivers.RData")) # kept_drivers
kept_drivers_FNYield <- kept_drivers; rm(kept_drivers)

load(file.path(final_models_dir, "FNYield_Yearly_shap_values_new.RData"))# shap_values_FNYield
shap_values_FNYield <- shap_values_FNYield

load(file.path(final_models_dir, "FNYield_Yearly_numeric.RData"))      # drivers_numeric
drivers_numeric_FNYield <- drivers_numeric

# 5a. Read back all‐subset predictions and build multi‐subset Pred vs Obs panels
pred_FNConc  <- read.csv(file.path(final_models_dir, "Predictions_FNConc.csv"))
pred_FNYield <- read.csv(file.path(final_models_dir, "Predictions_FNYield.csv"))

multi_lm_FNConc <- ggplot(pred_FNConc, aes(predicted, observed, color = subset)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Predicted", y = "Observed", title = "Concentration") +
  scale_color_manual(
    values = c("older70" = "#1b9e77",
               "recent30" = "#d95f02",
               "unseen10" = "#7570b3"),
    name = "Subset"
  ) +
  theme_classic(base_size = 22)

multi_lm_FNYield <- ggplot(pred_FNYield, aes(predicted, observed, color = subset)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Predicted", y = NULL, title = "Yield") +
  scale_color_manual(
    values = c("older70" = "#1b9e77",
               "recent30" = "#d95f02",
               "unseen10" = "#7570b3"),
    name = "Subset"
  ) +
  theme_classic(base_size = 22)

# Tag and strip legends for A & B
A_tagged <- multi_lm_FNConc + labs(tag = "A") +
  theme(plot.tag = element_text(size = 24),
        plot.tag.position = c(0.02, 0.98),
        legend.position = "none")
B_tagged <- multi_lm_FNYield + labs(tag = "B") +
  theme(plot.tag = element_text(size = 24),
        plot.tag.position = c(0.02, 0.98),
        legend.position = "none")

shared_pred_legend <- cowplot::get_legend(
  multi_lm_FNConc + theme(legend.position = "right")
)

# 5b. Prepare scaled driver data and global SHAP limits for dot plots
var_order <- c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
               "snow_cover","permafrost","elevation","basin_slope","RBI",
               "recession_slope",grep("^land_|^rocks_", names(kept_drivers_FNConc), value=TRUE))
var_labels <- c("N","P","NPP","ET","Greenup Day","Precip","Temp",
                "Snow Cover","Permafrost","Elevation","Basin Slope",
                "Flashiness (RBI)","Recession Curve Slope",
                "Land: Bare","Land: Cropland","Land: Forest",
                "Land: Grass & Shrub","Land: Ice & Snow",
                "Land: Impervious","Land: Salt Water",
                "Land: Tidal Wetland","Land: Water Body",
                "Land: Wetland Marsh")
recode_map <- setNames(as.list(var_labels), var_order)

kept_FNConc_scaled  <- kept_drivers_FNConc %>%
  mutate(P = log10(P)) %>%
  mutate(across(everything(), ~ scales::rescale(., to = c(0,1))))
kept_FNYield_scaled <- kept_drivers_FNYield %>%
  mutate(NOx = log10(NOx), P = log10(P)) %>%
  mutate(across(everything(), ~ scales::rescale(., to = c(0,1))))

global_scaled_min     <- min(c(unlist(kept_FNConc_scaled),
                               unlist(kept_FNYield_scaled)), na.rm=TRUE)
global_scaled_max     <- max(c(unlist(kept_FNConc_scaled),
                               unlist(kept_FNYield_scaled)), na.rm=TRUE)
global_shap_min_conc  <- min(shap_values_FNConc,  na.rm=TRUE)
global_shap_max_conc  <- max(shap_values_FNConc,  na.rm=TRUE)
global_shap_min_yield <- min(shap_values_FNYield, na.rm=TRUE)
global_shap_max_yield <- max(shap_values_FNYield, na.rm=TRUE)

# 6. SX‐style dot‐plot function (uses base gsub instead of str_replace_all)
create_shap_dot_global <- function(shap_matrix, kept_scaled,
                                   global_shap_min, global_shap_max) {
  shap_df <- as.data.frame(shap_matrix) %>%
    mutate(id = seq_len(nrow(.))) %>%
    pivot_longer(-id, names_to = "feature", values_to = "shap_value")
  feat_df <- kept_scaled %>%
    mutate(id = seq_len(nrow(.))) %>%
    pivot_longer(-id, names_to = "feature", values_to = "feature_value")
  
  df_long <- left_join(shap_df, feat_df, by = c("id","feature")) %>%
    mutate(
      feature = case_when(
        grepl("^rocks_", feature) ~ gsub("_", " ", feature),
        TRUE                      ~ recode(feature, !!!recode_map, .default = NA_character_)
      )
    ) %>%
    filter(!is.na(feature))
  
  feature_order <- df_long %>%
    group_by(feature) %>%
    summarize(m = mean(abs(shap_value)), .groups = "drop") %>%
    arrange(desc(m)) %>%
    pull(feature)
  df_long$feature <- factor(df_long$feature, levels = rev(feature_order))
  
  ggplot(df_long, aes(x = shap_value, y = feature)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_jitter(aes(fill = feature_value),
                shape = 21, color = "darkgray",
                height = 0.2, size = 2.7, alpha = 0.9) +
    scale_fill_gradient(low = "white", high = "black",
                        limits = c(global_scaled_min, global_scaled_max),
                        name = "Scaled Value") +
    scale_x_continuous(limits = c(global_shap_min, global_shap_max)) +
    labs(x = NULL, y = NULL) +
    theme_classic(base_size = 22) +
    theme(axis.text.y      = element_text(size = 20),
          axis.text.x      = element_text(size = 20),
          legend.position  = "right",
          legend.direction = "horizontal")
}

dot_plot_FNConc  <- create_shap_dot_global(shap_values_FNConc,
                                           kept_FNConc_scaled,
                                           global_shap_min_conc,
                                           global_shap_max_conc)
dot_plot_FNYield <- create_shap_dot_global(shap_values_FNYield,
                                           kept_FNYield_scaled,
                                           global_shap_min_yield,
                                           global_shap_max_yield)

# 7. Mean‐abs‐SHAP bar‐plot function
create_shap_bar_global <- function(shap_matrix) {
  shap_df <- as.data.frame(shap_matrix) %>%
    pivot_longer(cols = everything(),
                 names_to   = "feature",
                 values_to  = "shap_value")
  bar_stats <- shap_df %>%
    group_by(feature) %>%
    summarize(mean_abs = mean(abs(shap_value)), .groups = "drop") %>%
    mutate(pretty = case_when(
      grepl("^rocks_", feature) ~ gsub("_", " ", feature),
      TRUE                      ~ recode(feature, !!!recode_map)
    )) %>%
    arrange(desc(mean_abs)) %>%
    mutate(pretty = factor(pretty, levels = rev(unique(pretty))))
  
  ggplot(bar_stats, aes(x = pretty, y = mean_abs)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = "Mean Absolute SHAP Value") +
    theme_classic(base_size = 22) +
    theme(axis.text.y = element_text(size = 20),
          axis.text.x = element_text(size = 20))
}

bar_plot_FNConc  <- create_shap_bar_global(shap_values_FNConc)
bar_plot_FNYield <- create_shap_bar_global(shap_values_FNYield)

C_tagged <- bar_plot_FNConc  + labs(tag = "C") + theme(plot.tag = element_text(size = 24))
D_tagged <- bar_plot_FNYield + labs(tag = "D") + theme(plot.tag = element_text(size = 24))
E_tagged <- dot_plot_FNConc   + labs(tag = "E") + theme(plot.tag = element_text(size = 24))
F_tagged <- dot_plot_FNYield  + labs(tag = "F") + theme(plot.tag = element_text(size = 24))

# 8. Assemble the 3×2 grid
row1 <- plot_grid(A_tagged, B_tagged, ncol = 2, align = "hv")
row2 <- plot_grid(C_tagged, D_tagged, ncol = 2, align = "hv")
row3 <- plot_grid(E_tagged, F_tagged, ncol = 2, align = "hv")

final_fig2 <- plot_grid(
  row1, shared_pred_legend,
  row2, row3,
  ncol        = 1,
  rel_heights = c(1, 0.2, 1, 1)
)

# 9. Save the final figure
ggsave(
  file.path(output_dir, "Fig2_Global_FNConc_FNYield_multi.png"),
  final_fig2,
  width  = 20,
  height = 25,
  dpi    = 300,
  bg     = "white"
)
