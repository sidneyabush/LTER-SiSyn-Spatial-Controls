###############################################################################
# 02_make_Fig2_global_multi.R
# Only loads outputs from STEP 2 (models, kept_drivers, predictions)
# and STEP 3 (SHAP on recent30) — nothing else.
###############################################################################

# 1. Packages & theme
librarian::shelf(
  iml, ggplot2, dplyr, tidyr, randomForest, tibble,
  scales, cowplot, viridis, RColorBrewer, patchwork
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

# 2. Clean
rm(list = ls())

# 3. Paths
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
final_models_dir <- "Final_Models"
output_dir       <- "Final_Figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 4. Load predictions (STEP 2 outputs)
# =============================================================================
pred_FNConc  <- read.csv(file.path(final_models_dir, "Predictions_FNConc.csv"))
pred_FNYield <- read.csv(file.path(final_models_dir, "Predictions_FNYield.csv"))

# =============================================================================
# 5. Load SHAP values on recent30 (STEP 3 outputs)
# =============================================================================
load(file.path(final_models_dir, "FNConc_Yearly_shap_values_recent30.RData"))
shap_values_FNConc <- shap_values_FNConc
load(file.path(final_models_dir, "FNYield_Yearly_shap_values_recent30.RData"))
shap_values_FNYield <- shap_values_FNYield

# =============================================================================
# 6. Load kept_drivers (STEP 2 outputs) for scaling
# =============================================================================
load(file.path(final_models_dir, "FNConc_Yearly_kept_drivers.RData"))
kept_FNConc <- kept_drivers;   rm(kept_drivers)
load(file.path(final_models_dir, "FNYield_Yearly_kept_drivers.RData"))
kept_FNYield <- kept_drivers;   rm(kept_drivers)

# =============================================================================
# 7. Load numeric responses (STEP 1 outputs)
# =============================================================================
load(file.path(final_models_dir, "FNConc_Yearly_numeric.RData"))
drivers_numeric_FNConc <- drivers_numeric
load(file.path(final_models_dir, "FNYield_Yearly_numeric.RData"))
drivers_numeric_FNYield <- drivers_numeric

# =============================================================================
# 8. Panel A & B: multi‐subset Pred vs Obs
# =============================================================================
lm_theme    <- theme_classic(base_size = 22)
subset_cols <- c("older70" = "#1b9e77", "recent30" = "#d95f02", "unseen10" = "#7570b3")

A <- ggplot(pred_FNConc, aes(predicted, observed, color = subset)) +
  geom_point(size=3, alpha=0.8) +
  geom_abline(linetype="dashed") +
  scale_color_manual(values = subset_cols, name="Subset") +
  labs(x="Predicted", y="Observed", title="Concentration") +
  lm_theme + theme(legend.position="none") + labs(tag="A")

B <- ggplot(pred_FNYield, aes(predicted, observed, color = subset)) +
  geom_point(size=3, alpha=0.8) +
  geom_abline(linetype="dashed") +
  scale_color_manual(values = subset_cols, name="Subset") +
  labs(x="Predicted", y=NULL, title="Yield") +
  lm_theme + theme(legend.position="none") + labs(tag="B")

legend_pred <- get_legend(
  ggplot(pred_FNConc, aes(predicted, observed, color = subset)) +
    geom_point(size=3) +
    scale_color_manual(values = subset_cols, name="Subset") +
    theme_classic() + theme(legend.position="right")
)

# =============================================================================
# 9. Prepare scale & recode map for dot plots
# =============================================================================
vars <- intersect(colnames(kept_FNConc), colnames(kept_FNYield))
labels <- c(
  NOx="N", P="P", npp="NPP", evapotrans="ET", greenup_day="Greenup Day",
  precip="Precip", temp="Temp", snow_cover="Snow Cover", permafrost="Permafrost",
  elevation="Elevation", basin_slope="Basin Slope", RBI="Flashiness (RBI)",
  recession_slope="Recession Curve Slope",
  land_Bare="Land: Bare", land_Cropland="Land: Cropland", land_Forest="Land: Forest",
  land_Grassland_Shrubland="Land: Grass & Shrub", land_Ice_Snow="Land: Ice & Snow",
  land_Impervious="Land: Impervious", land_Salt_Water="Land: Salt Water",
  land_Tidal_Wetland="Land: Tidal Wetland", land_Water="Land: Water Body",
  land_Wetland_Marsh="Land: Wetland Marsh"
)

scale_and_log <- function(df, log_vars) {
  df2 <- df
  for (v in log_vars) df2[[v]] <- log10(df2[[v]])
  df2 %>% mutate(across(all_of(vars), ~ scales::rescale(., to=c(0,1))))
}

kept_FNConc_s   <- scale_and_log(kept_FNConc,   log_vars="P")
kept_FNYield_s  <- scale_and_log(kept_FNYield,  log_vars=c("NOx","P"))
global_val      <- c(unlist(kept_FNConc_s), unlist(kept_FNYield_s))
gmin            <- min(global_val, na.rm=TRUE)
gmax            <- max(global_val, na.rm=TRUE)

# =============================================================================
# 10. Dot‐plot function (full recode + correct scaling)
# =============================================================================
dot_plot <- function(shap, kept_s, gmin, gmax) {
  df <- as.data.frame(shap) %>%
    mutate(id = row_number()) %>%
    pivot_longer(-id, names_to="feature", values_to="shap") %>%
    left_join(
      kept_s %>% mutate(id = row_number()) %>%
        pivot_longer(-id, names_to="feature", values_to="val"),
      by = c("id","feature")
    ) %>%
    mutate(
      pretty = ifelse(grepl("^rocks_",feature),
                      gsub("_"," ",feature),
                      labels[feature])
    ) %>%
    filter(!is.na(pretty)) %>%
    group_by(pretty) %>%
    mutate(pretty = factor(pretty, levels=rev(unique(pretty))))
  
  ggplot(df, aes(x=shap, y=pretty)) +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_jitter(aes(fill=val),
                shape=21, color="darkgray",
                height=0.2, size=2.7, alpha=0.9) +
    scale_fill_gradient(low="white", high="black",
                        limits=c(gmin,gmax), name="Scaled Value") +
    theme_classic(base_size=22) +
    theme(axis.text.y=element_text(size=20),
          axis.text.x=element_text(size=20),
          legend.position="right",
          legend.direction="horizontal")
}

E <- dot_plot(shap_values_FNConc,  kept_FNConc_s,  gmin, gmax) + labs(tag="E")
F <- dot_plot(shap_values_FNYield, kept_FNYield_s, gmin, gmax) + labs(tag="F")
legend_dot <- get_legend(E + theme(legend.position="right"))

# =============================================================================
# 11. Define bar‐plot function & panels C & D
# =============================================================================
create_shap_bar_global <- function(shap_matrix, model_name) {
  shap_df  <- as.data.frame(shap_matrix) %>%
    tidyr::pivot_longer(cols=everything(),
                        names_to="feature", values_to="shap_value")
  bar_stats <- shap_df %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(mean_abs=mean(abs(shap_value), na.rm=TRUE), .groups="drop") %>%
    dplyr::mutate(
      pretty = ifelse(grepl("^rocks_",feature),
                      gsub("_"," ",feature),
                      labels[feature])
    ) %>%
    filter(!is.na(pretty)) %>%
    arrange(desc(mean_abs)) %>%
    mutate(pretty=factor(pretty, levels=rev(unique(pretty))))
  
  ggplot(bar_stats, aes(x=pretty, y=mean_abs)) +
    geom_col() +
    coord_flip() +
    labs(x=NULL, y="Mean Absolute SHAP Value") +
    theme_classic(base_size=22)
}

bar_plot_FNConc  <- create_shap_bar_global(shap_values_FNConc,  "Concentration")
bar_plot_FNYield <- create_shap_bar_global(shap_values_FNYield, "Yield")

C <- bar_plot_FNConc  +
  labs(tag="C") +
  theme(plot.tag=element_text(size=24), plot.tag.position=c(0.02,0.98))

D <- bar_plot_FNYield +
  labs(tag="D") +
  theme(plot.tag=element_text(size=24), plot.tag.position=c(0.02,0.98))

# =============================================================================
# 12. Prepare legends & strip them from panels
# =============================================================================
shared_pred_legend <- legend_pred
shared_dot_legend  <- legend_dot

A_tagged <- A + theme(legend.position="none")
B_tagged <- B + theme(legend.position="none")
C_tagged <- C  # bar plots have no internal legend
D_tagged <- D
E_tagged <- E + theme(legend.position="none")
F_tagged <- F + theme(legend.position="none")

# =============================================================================
# 13. Assemble the 3×2 grid + legend row
# =============================================================================
row1 <- cowplot::plot_grid(A_tagged, B_tagged, ncol=2, align="hv")
row2 <- cowplot::plot_grid(C_tagged, D_tagged, ncol=2, align="hv")
row3 <- cowplot::plot_grid(E_tagged, F_tagged, ncol=2, align="hv")
leg  <- cowplot::plot_grid(
  shared_pred_legend, shared_dot_legend,
  ncol       = 2,
  rel_widths = c(0.5, 0.5)
)

final_fig2 <- cowplot::plot_grid(
  row1, leg, row2, row3,
  ncol        = 1,
  rel_heights = c(1, 0.2, 1, 1),
  align       = "v"
)

# 14. Save to disk with a PNG device
png(
  filename = file.path(output_dir, "Fig2_Global_FNConc_FNYield_multi.png"),
  width    = 20,    # in cm
  height   = 25,    # in cm
  units    = "cm",
  res      = 300
)
print(final_fig2)
dev.off()
