###############################################################################
# 02_make_Fig2_global_multi.R
# Only loads outputs from STEP 2 (models, kept_drivers, predictions)
# and STEP 3 (SHAP on recent30) — nothing else.
###############################################################################

# 1. Packages & theme
library(iml); library(ggplot2); library(dplyr); library(tidyr); library(randomForest)
library(tibble); library(scales); library(cowplot); library(viridis); library(RColorBrewer); library(patchwork)
theme_set(
  theme_classic(base_size = 22) +
    theme(
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA)
    )
)

# 2. Clean
rm(list = ls())

# 3. Paths
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
final_models_dir <- "Final_Models"; output_dir <- "Final_Figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 4. Load predictions
pred_FNConc  <- read.csv(file.path(final_models_dir, "Predictions_FNConc.csv"))
pred_FNYield <- read.csv(file.path(final_models_dir, "Predictions_FNYield.csv"))

# 5. Load SHAP values
load(file.path(final_models_dir, "FNConc_Yearly_shap_values_recent30.RData"))
shap_values_FNConc  <- shap_values_FNConc
load(file.path(final_models_dir, "FNYield_Yearly_shap_values_recent30.RData"))
shap_values_FNYield <- shap_values_FNYield

# 6. Load kept_drivers
load(file.path(final_models_dir, "FNConc_Yearly_kept_drivers.RData"))
kept_FNConc <- kept_drivers; rm(kept_drivers)
load(file.path(final_models_dir, "FNYield_Yearly_kept_drivers.RData"))
kept_FNYield <- kept_drivers; rm(kept_drivers)

# 7. Load numeric responses
load(file.path(final_models_dir, "FNConc_Yearly_numeric.RData"))
drivers_numeric_FNConc <- drivers_numeric
load(file.path(final_models_dir, "FNYield_Yearly_numeric.RData"))
drivers_numeric_FNYield <- drivers_numeric

# 8. Panel A & B: Pred vs Obs
lm_theme    <- theme_classic(base_size = 22)
subset_cols <- c("older70" = "#1b9e77", "recent30" = "#d95f02", "unseen10" = "#7570b3")

A <- ggplot(pred_FNConc, aes(predicted, observed, color = subset)) +
  geom_point(size = 3, alpha = 0.8) + geom_abline(linetype = "dashed") +
  scale_color_manual(values = subset_cols, name = "Subset") +
  labs(x = "Predicted", y = "Observed", title = "Concentration", tag = "A") +
  lm_theme + theme(legend.position = "none")

B <- ggplot(pred_FNYield, aes(predicted, observed, color = subset)) +
  geom_point(size = 3, alpha = 0.8) + geom_abline(linetype = "dashed") +
  scale_color_manual(values = subset_cols, name = "Subset") +
  labs(x = "Predicted", y = NULL, title = "Yield", tag = "B") +
  lm_theme + theme(legend.position = "none")

# 9. Extract a bottom‑centered, horizontal legend JUST for A & B
legend_pred <- get_legend(
  ggplot(pred_FNConc, aes(predicted, observed, color = subset)) +
    geom_point(size = 3, alpha = 0.8) +
    scale_color_manual(values = subset_cols, name = "Subset") +
    theme_classic(base_size = 22) +
    theme(
      legend.position      = "bottom",
      legend.direction     = "horizontal",
      legend.justification = "center",
      legend.title.align   = 0.5,
      legend.key.width     = unit(1.5, "lines"),
      legend.key.height    = unit(1,   "lines"),
      legend.text          = element_text(size = 20),
      legend.title         = element_text(size = 22)
    )
)

# 10. Prepare recode_map & per‑feature scaling
## 10.1 full spelled‑out labels
var_order  <- c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
                "snow_cover","permafrost","elevation","basin_slope","RBI",
                "recession_slope","land_Bare","land_Cropland","land_Forest",
                "land_Grassland_Shrubland","land_Ice_Snow","land_Impervious",
                "land_Salt_Water","land_Tidal_Wetland","land_Water",
                "land_Wetland_Marsh")
var_labels <- c("N","P","NPP","ET","Greenup Day","Precip","Temp",
                "Snow Cover","Permafrost","Elevation","Basin Slope","Flashiness (RBI)",
                "Recession Curve Slope","Land: Bare","Land: Cropland","Land: Forest",
                "Land: Grass & Shrub","Land: Ice & Snow","Land: Impervious",
                "Land: Salt Water","Land: Tidal Wetland","Land: Water Body",
                "Land: Wetland Marsh")
recode_map <- setNames(var_labels, var_order)

## 10.2 log + rescale each feature 0–1
vars          <- intersect(colnames(kept_FNConc), colnames(kept_FNYield))
scale_and_log <- function(df, log_vars) {
  df2 <- df
  for(v in log_vars) df2[[v]] <- log10(df2[[v]])
  df2 %>% mutate(across(all_of(vars), ~ scales::rescale(., to = c(0,1))))
}
kept_FNConc_s  <- scale_and_log(kept_FNConc,   "P")
kept_FNYield_s <- scale_and_log(kept_FNYield, c("NOx","P"))

gmin <- min(c(unlist(kept_FNConc_s), unlist(kept_FNYield_s)), na.rm = TRUE)
gmax <- max(c(unlist(kept_FNConc_s), unlist(kept_FNYield_s)), na.rm = TRUE)

# 11. SX‑style jittered dot‑plot (with reversed feature order)
dot_plot <- function(shap, kept_s, gmin, gmax) {
  df <- as.data.frame(shap) %>%
    mutate(id = row_number()) %>%
    pivot_longer(-id, names_to = "feature", values_to = "shap") %>%
    left_join(
      kept_s %>% mutate(id = row_number()) %>%
        pivot_longer(-id, names_to = "feature", values_to = "val"),
      by = c("id","feature")
    ) %>%
    mutate(
      pretty = case_when(
        grepl("^rocks_", feature) ~ gsub("_"," ", feature),
        TRUE                     ~ recode(feature, !!!recode_map, .default = NA_character_)
      )
    ) %>%
    filter(!is.na(pretty))
  
  feature_order <- df %>%
    group_by(pretty) %>%
    summarize(mean_abs = mean(abs(shap)), .groups = "drop") %>%
    arrange(desc(mean_abs)) %>%
    pull(pretty)
  
  df$pretty <- factor(df$pretty, levels = rev(feature_order))
  
  ggplot(df, aes(x = shap, y = pretty)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_jitter(aes(fill = val),
                shape = 21, color = "darkgray",
                height = 0.2, size = 2.7, alpha = 0.9) +
    scale_fill_gradient(
      low = "white", high = "black",
      limits = c(gmin, gmax),
      name = "Scaled Value",
      guide = guide_colourbar(
        barheight     = unit(1.3, "cm"),
        barwidth      = unit(20,  "lines"),
        title.position = "top",
        title.theme   = element_text(size = 22, face = "plain", hjust = 0.5),
        label.theme   = element_text(size = 20)
      )
    ) +
    scale_x_continuous(
      limits = c(min(df$shap, na.rm = TRUE), max(df$shap, na.rm = TRUE))
    ) +
    labs(x = NULL, y = NULL) +
    theme_classic(base_size = 22) +
    theme(
      axis.text.y      = element_text(size = 20),
      axis.text.x      = element_text(size = 20),
      legend.position  = "right",
      legend.direction = "horizontal"
    )
}

E <- dot_plot(shap_values_FNConc,  kept_FNConc_s,  gmin, gmax) + labs(tag = "E")
F <- dot_plot(shap_values_FNYield, kept_FNYield_s, gmin, gmax) + labs(tag = "F")
legend_dot <- get_legend(E + theme(legend.position = "right"))

# 12. Mean‑|SHAP| bar‑plots (with reversed feature order)
create_shap_bar_global <- function(shap_matrix) {
  bar_stats <- as.data.frame(shap_matrix) %>%
    pivot_longer(everything(), names_to = "feature", values_to = "shap") %>%
    group_by(feature) %>%
    summarize(mean_abs = mean(abs(shap), na.rm = TRUE), .groups = "drop") %>%
    mutate(
      pretty = case_when(
        grepl("^rocks_", feature) ~ gsub("_"," ", feature),
        TRUE                     ~ recode(feature, !!!recode_map)
      )
    ) %>%
    filter(!is.na(pretty)) %>%
    arrange(desc(mean_abs))
  
  bar_stats$pretty <- factor(bar_stats$pretty, levels = rev(bar_stats$pretty))
  
  ggplot(bar_stats, aes(x = pretty, y = mean_abs)) +
    geom_col() + coord_flip() +
    labs(x = NULL, y = "Mean Absolute SHAP Value") +
    theme_classic(base_size = 22)
}

bar_plot_FNConc  <- create_shap_bar_global(shap_values_FNConc)
bar_plot_FNYield <- create_shap_bar_global(shap_values_FNYield)

C <- bar_plot_FNConc  + labs(tag = "C") + theme(plot.tag = element_text(size = 24), plot.tag.position = c(0.02, 0.98))
D <- bar_plot_FNYield + labs(tag = "D") + theme(plot.tag = element_text(size = 24), plot.tag.position = c(0.02, 0.98))

# 13. Legends & strip: assemble with the pred‑legend UNDER A & B
A_t <- A + theme(legend.position = "none")
B_t <- B + theme(legend.position = "none")
C_t <- C
D_t <- D
E_t <- E + theme(legend.position = "none")
F_t <- F + theme(legend.position = "none")

# A & B side‑by‑side, then their centered legend immediately below
row1             <- plot_grid(A_t, B_t,           ncol = 2, align = "hv")
row1_with_legend <- plot_grid(row1, legend_pred,  ncol = 1, rel_heights = c(1, 0.15))

# Then the two barplots and two dotplots
row2 <- plot_grid(C_t, D_t, ncol = 2, align = "hv")
row3 <- plot_grid(E_t, F_t, ncol = 2, align = "hv")

# Finally stack everything
final_fig2 <- plot_grid(
  row1_with_legend,
  row2,
  row3,
  ncol        = 1,
  rel_heights = c(1, 1, 1),
  align       = "v"
)


# 14. Save in inches
ggsave(
  file.path(output_dir, "Fig2_Global_FNConc_FNYield_multi.png"),
  plot   = final_fig2,
  width  = 15,   # inches
  height = 20,   # inches
  units  = "in",
  dpi    = 300,
  bg     = "white"
)
