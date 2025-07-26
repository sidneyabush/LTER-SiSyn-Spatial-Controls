###############################################################################
# 02_make_Fig2_global_multi.R
###############################################################################

# 1. Packages & theme
library(iml); library(ggplot2); library(dplyr); library(tidyr)
library(randomForest); library(tibble); library(scales); library(cowplot)
theme_set(
  theme_classic(base_size = 22) +
    theme(
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA)
    )
)

# 2. Clear & paths
rm(list = ls())
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
fm <- "Final_Models"; od <- "Final_Figures"
dir.create(od, recursive = TRUE, showWarnings = FALSE)

# 3. Load data
pred_FNConc  <- read.csv(file.path(fm, "Predictions_FNConc.csv"))
pred_FNYield <- read.csv(file.path(fm, "Predictions_FNYield.csv"))
load(file.path(fm, "FNConc_Yearly_shap_values_recent30.RData"));  SV_FN  <- shap_values_FNConc
load(file.path(fm, "FNYield_Yearly_shap_values_recent30.RData")); SV_FY  <- shap_values_FNYield
load(file.path(fm, "FNConc_Yearly_kept_drivers.RData"));  KD_FN  <- kept_drivers; rm(kept_drivers)
load(file.path(fm, "FNYield_Yearly_kept_drivers.RData")); KD_FY  <- kept_drivers; rm(kept_drivers)
load(file.path(fm, "FNConc_Yearly_numeric.RData"));  DN_FN  <- drivers_numeric
load(file.path(fm, "FNYield_Yearly_numeric.RData")); DN_FY  <- drivers_numeric

# 4. Recode & **per‐dataset** scale setup
recode_map <- setNames(
  c("N","P","NPP","ET","Greenup Day","Precip","Temp","Snow Cover","Permafrost",
    "Elevation","Basin Slope","Flashiness (RBI)","Recession Curve Slope",
    "Land: Bare","Land: Cropland","Land: Forest","Land: Grass & Shrub",
    "Land: Ice & Snow","Land: Impervious","Land: Salt Water","Land: Tidal Wetland",
    "Land: Water Body","Land: Wetland Marsh", "Rock: Volcanic", "Rock: Sedimentary", "Rock: Carbonate Evaporite",
    "Rock: Metamorphic", "Rock: Plutonic"),
  
  c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
    "snow_cover","permafrost","elevation","basin_slope","RBI",
    "recession_slope","land_Bare","land_Cropland","land_Forest",
    "land_Grassland_Shrubland","land_Ice_Snow","land_Impervious",
    "land_Salt_Water","land_Tidal_Wetland","land_Water","land_Wetland_Marsh", "rocks_volcanic",
    "rocks_sedimentary", "rocks_carbonate_evaporite", "rocks_metamorphic", "rocks_plutonic")
)

# 4.1 Log‐transform & rescale **FNConc** drivers 0–1
kept_FNConc_scaled <- KD_FN %>%
  mutate(P = log10(P)) %>%
  mutate(across(everything(), ~ scales::rescale(., to = c(0,1))))

# 4.2 Log‐transform & rescale **FNYield** drivers 0–1
kept_FNYield_scaled <- KD_FY %>%
  mutate(NOx = log10(NOx), P = log10(P)) %>%
  mutate(across(everything(), ~ scales::rescale(., to = c(0,1))))

# 4.3 For color‐fill, use the same 0–1 limits within each plot
gmin <- 0
gmax <- 1

# 5. Dot‐plot function (unchanged)
dot_plot <- function(SV, KD_s) {
  shap_df <- as.data.frame(SV) %>% mutate(id = row_number()) %>%
    pivot_longer(-id, names_to="feature", values_to="shap")
  val_df  <- KD_s %>% mutate(id = row_number()) %>%
    pivot_longer(-id, names_to="feature", values_to="val")
  df <- left_join(shap_df, val_df, by = c("id","feature")) %>%
    mutate(
      pretty = recode(feature, !!!recode_map, .default = NA_character_)
    ) %>%
    filter(!is.na(pretty))
  ord <- df %>% group_by(pretty) %>% summarize(m = mean(abs(shap))) %>%
    arrange(desc(m)) %>% pull(pretty)
  df$pretty <- factor(df$pretty, levels = rev(ord))
  ggplot(df, aes(x = shap, y = pretty)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_jitter(aes(fill = val), shape = 21, color = "darkgray",
                height = 0.2, size = 2.7, alpha = 0.9) +
    scale_fill_gradient(
      low    = "white", high = "black",
      limits = c(gmin, gmax),
      name   = "Scaled Value",
      guide  = guide_colourbar(
        barheight      = unit(1.3, "cm"),
        barwidth       = unit(20,  "lines"),
        title.position = "top",
        title.theme    = element_text(size = 22, hjust = 0.5),
        label.theme    = element_text(size = 20)
      )
    ) +
    labs(x = NULL, y = NULL) +
    theme_classic(base_size = 22) +
    theme(axis.text = element_text(size = 20),
          legend.position = "right",
          legend.direction = "horizontal")
}

# 6. Bar‐plot function (unchanged)
bar_plot <- function(SV) {
  bs <- as.data.frame(SV) %>% pivot_longer(
    everything(), names_to="feature", values_to="shap"
  ) %>% group_by(feature) %>% summarize(m = mean(abs(shap), na.rm = TRUE)) %>%
    mutate(
      pretty = recode(feature, !!!recode_map, .default = NA_character_)
    ) %>%
    filter(!is.na(pretty)) %>%
    arrange(desc(m))
  bs$pretty <- factor(bs$pretty, levels = rev(bs$pretty))
  ggplot(bs, aes(x = pretty, y = m)) +
    geom_col() + coord_flip() +
    labs(x = NULL, y = "Mean Absolute SHAP Value") +
    theme_classic(base_size = 22)
}

# 7. Build panels A & B (unchanged)
# 7.1 New colors & labels
subset_cols_new <- c(
  older70  = "#E41A1C",  # Test
  recent30 = "#377EB8",  # Train
  unseen10 = "#4DAF4A"   # Cross‑Validation
)
subset_labels_new <- c(
  older70  = "Test",
  recent30 = "Train",
  unseen10 = "Cross‑Validation"
)

# 7.2 Compute performance metrics for FNConc
metrics_FNConc <- pred_FNConc %>%
  group_by(subset) %>%
  summarize(
    R2    = cor(predicted, observed)^2,
    pRMSE = sqrt(mean((predicted - observed)^2)) / mean(observed) * 100,
    .groups = "drop"
  )

fn_xmin <- min(pred_FNConc$predicted)
fn_xmax <- max(pred_FNConc$predicted)
fn_ymin <- min(pred_FNConc$observed)
fn_ymax <- max(pred_FNConc$observed)
fn_yrng <- fn_ymax - fn_ymin

# 7.3 Panel A: Concentration
A_full <- ggplot(pred_FNConc, aes(predicted, observed, color = subset)) +
  geom_point(size = 4, alpha = 0.4) +
  geom_abline(linetype = "dashed") +
  scale_color_manual(values = subset_cols_new, name = NULL, labels = subset_labels_new) +
  annotate(
    "text",
    x     = fn_xmin + 0.05 * (fn_xmax - fn_xmin),
    y     = fn_ymax - 0 * 0.12 * fn_yrng,
    label = with(filter(metrics_FNConc, subset=="older70"),
                 sprintf(" R² = %.3f, pRMSE = %.1f%%", R2, pRMSE)),
    hjust = 0, size = 6, color = subset_cols_new["older70"]
  ) +
  annotate(
    "text",
    x     = fn_xmin + 0.05 * (fn_xmax - fn_xmin),
    y     = fn_ymax - 1 * 0.12 * fn_yrng,
    label = with(filter(metrics_FNConc, subset=="recent30"),
                 sprintf(" R² = %.3f, pRMSE = %.1f%%", R2, pRMSE)),
    hjust = 0, size = 6, color = subset_cols_new["recent30"]
  ) +
  annotate(
    "text",
    x     = fn_xmin + 0.05 * (fn_xmax - fn_xmin),
    y     = fn_ymax - 2 * 0.12 * fn_yrng,
    label = with(filter(metrics_FNConc, subset=="unseen10"),
                 sprintf(" R² = %.3f, pRMSE = %.1f%%", R2, pRMSE)),
    hjust = 0, size = 6, color = subset_cols_new["unseen10"]
  ) +
  labs(x = "Predicted", y = "Observed", title = "Concentration", tag = "A") +
  theme_classic(base_size = 22)

# 7.4 Compute performance metrics for FNYield
metrics_FNYield <- pred_FNYield %>%
  group_by(subset) %>%
  summarize(
    R2    = cor(predicted, observed)^2,
    pRMSE = sqrt(mean((predicted - observed)^2)) / mean(observed) * 100,
    .groups = "drop"
  )

fy_xmin <- min(pred_FNYield$predicted)
fy_xmax <- max(pred_FNYield$predicted)
fy_ymin <- min(pred_FNYield$observed)
fy_ymax <- max(pred_FNYield$observed)
fy_yrng <- fy_ymax - fy_ymin

# 7.5 Panel B: Yield
B_full <- ggplot(pred_FNYield, aes(predicted, observed, color = subset)) +
  geom_point(size = 4, alpha = 0.4) +
  geom_abline(linetype = "dashed") +
  scale_color_manual(values = subset_cols_new, name = NULL, labels = subset_labels_new) +
  annotate(
    "text",
    x     = fy_xmin + 0.05 * (fy_xmax - fy_xmin),
    y     = fy_ymax - 0 * 0.12 * fy_yrng,
    label = with(filter(metrics_FNYield, subset=="older70"),
                 sprintf(" R² = %.3f, pRMSE = %.1f%%", R2, pRMSE)),
    hjust = 0, size = 6, color = subset_cols_new["older70"]
  ) +
  annotate(
    "text",
    x     = fy_xmin + 0.05 * (fy_xmax - fy_xmin),
    y     = fy_ymax - 1 * 0.12 * fy_yrng,
    label = with(filter(metrics_FNYield, subset=="recent30"),
                 sprintf(" R² = %.3f, pRMSE = %.1f%%", R2, pRMSE)),
    hjust = 0, size = 6, color = subset_cols_new["recent30"]
  ) +
  annotate(
    "text",
    x     = fy_xmin + 0.05 * (fy_xmax - fy_xmin),
    y     = fy_ymax - 2 * 0.12 * fy_yrng,
    label = with(filter(metrics_FNYield, subset=="unseen10"),
                 sprintf(" R² = %.3f, pRMSE = %.1f%%", R2, pRMSE)),
    hjust = 0, size = 6, color = subset_cols_new["unseen10"]
  ) +
  labs(x = "Predicted", y = NULL, title = "Yield", tag = "B") +
  theme_classic(base_size = 22)

# 7a. Combine A & B with shared legend
AB_panels <- plot_grid(
  A_full + theme(legend.position = "none"),
  B_full + theme(legend.position = "none"),
  ncol       = 2,
  align      = "h",
  axis       = "tblr",
  rel_widths = c(0.9,0.9)
)
AB_leg <- get_legend(
  A_full +
    theme(
      legend.position      = "right",
      legend.direction     = "horizontal",
      legend.justification = "center",
      legend.text          = element_text(size = 20),
      legend.key.width     = unit(1.5, "lines"),
      legend.key.height    = unit(1,   "lines")
    )
)
AB <- plot_grid(
  AB_panels,
  AB_leg,
  ncol        = 1,
  rel_heights = c(1, 0.15)
)


# 8. Panels C & D (unchanged)
CD <- plot_grid(
  bar_plot(SV_FN) + labs(tag="C") + theme(plot.tag=element_text(size=24), plot.tag.position=c(0.02,0.98)),
  bar_plot(SV_FY) + labs(tag="D") + theme(plot.tag=element_text(size=24), plot.tag.position=c(0.02,0.98)),
  ncol=2, align="h", axis="tblr", rel_widths=c(1,1)
)

# 9. Panels E & F using per‐dataset scaled values
E_full <- dot_plot(SV_FN,  kept_FNConc_scaled) + labs(tag="E")
F_full <- dot_plot(SV_FY, kept_FNYield_scaled) + labs(tag="F")

EF_panels <- plot_grid(
  E_full + theme(legend.position="none"),
  F_full + theme(legend.position="none"),
  ncol = 2, align = "h", axis = "tblr", rel_widths = c(0.8,0.8)
)
EF_leg <- get_legend(
  E_full +
    theme(legend.position="right",
          legend.direction="horizontal",
          legend.justification="center",
          legend.title=element_text(size=22),
          legend.text=element_text(size=20),
          legend.key.width=unit(1.5,"lines"),
          legend.key.height=unit(1,"lines"))
)
EF <- plot_grid(EF_panels, EF_leg, ncol = 1, rel_heights = c(1, 0.5))

# 10. Final assemble
final_fig2 <- plot_grid(
  AB,
  CD,
  EF,
  ncol = 1,
  rel_heights = c(1.3, 1.3, 1.3)
)

# 11. Save
ggsave(
  file.path(od, "Fig2_Global_FNConc_FNYield_multi.png"),
  final_fig2,
  width  = 20, height = 20, dpi = 300, bg = "white"
)
