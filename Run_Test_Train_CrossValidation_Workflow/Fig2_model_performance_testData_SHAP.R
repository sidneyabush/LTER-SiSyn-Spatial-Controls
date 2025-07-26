###############################################################################
# 02_make_Fig2_global_multi.R
###############################################################################

# 1. Packages & theme
library(iml); library(ggplot2); library(dplyr); library(tidyr); library(randomForest)
library(tibble); library(scales); library(cowplot)
theme_set(
  theme_classic(base_size = 22) +
    theme(
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA)
    )
)

# 2. Clear
rm(list = ls())

# 3. Paths
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")
final_models_dir <- "Final_Models"
output_dir       <- "Final_Figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 4. Load data
pred_FNConc  <- read.csv(file.path(final_models_dir, "Predictions_FNConc.csv"))
pred_FNYield <- read.csv(file.path(final_models_dir, "Predictions_FNYield.csv"))
load(file.path(final_models_dir, "FNConc_Yearly_shap_values_recent30.RData"));  shap_values_FNConc  <- shap_values_FNConc
load(file.path(final_models_dir, "FNYield_Yearly_shap_values_recent30.RData")); shap_values_FNYield <- shap_values_FNYield
load(file.path(final_models_dir, "FNConc_Yearly_kept_drivers.RData"));  kept_FNConc  <- kept_drivers; rm(kept_drivers)
load(file.path(final_models_dir, "FNYield_Yearly_kept_drivers.RData")); kept_FNYield <- kept_drivers; rm(kept_drivers)
load(file.path(final_models_dir, "FNConc_Yearly_numeric.RData"));  drivers_numeric_FNConc  <- drivers_numeric
load(file.path(final_models_dir, "FNYield_Yearly_numeric.RData")); drivers_numeric_FNYield <- drivers_numeric

# 5. Labels & scaling prep
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

vars <- intersect(colnames(kept_FNConc), colnames(kept_FNYield))
scale_and_log <- function(df, log_vars) {
  df2 <- df
  for(v in log_vars) df2[[v]] <- log10(df2[[v]])
  df2 %>% mutate(across(all_of(vars), ~ scales::rescale(., to = c(0,1))))
}
kept_FNConc_s  <- scale_and_log(kept_FNConc,   "P")
kept_FNYield_s <- scale_and_log(kept_FNYield, c("NOx","P"))
gmin <- min(c(unlist(kept_FNConc_s), unlist(kept_FNYield_s)), na.rm = TRUE)
gmax <- max(c(unlist(kept_FNConc_s), unlist(kept_FNYield_s)), na.rm = TRUE)

# 6. Dot‐plot & bar‐plot functions
dot_plot <- function(shap, kept_s, gmin, gmax) {
  df <- as.data.frame(shap) %>%
    mutate(id = row_number()) %>%
    pivot_longer(-id, names_to="feature", values_to="shap") %>%
    left_join(
      kept_s %>% mutate(id = row_number()) %>%
        pivot_longer(-id, names_to="feature", values_to="val"),
      by=c("id","feature")
    ) %>%
    mutate(
      pretty = case_when(
        grepl("^rocks_", feature) ~ gsub("_"," ", feature),
        TRUE                     ~ recode(feature, !!!recode_map, .default=NA_character_)
      )
    ) %>%
    filter(!is.na(pretty))
  fo <- df %>% group_by(pretty) %>% summarize(m = mean(abs(shap))) %>%
    arrange(desc(m)) %>% pull(pretty)
  df$pretty <- factor(df$pretty, levels = rev(fo))
  ggplot(df, aes(shap, pretty)) +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_jitter(aes(fill=val), shape=21, color="darkgray",
                height=0.2, size=2.7, alpha=0.9) +
    scale_fill_gradient(low="white", high="black",
                        limits=c(gmin,gmax), name="Scaled Value",
                        guide=guide_colourbar(barheight=unit(1.3,"cm"),
                                              barwidth=unit(20,"lines"),
                                              title.position="top",
                                              title.theme=element_text(size=22,hjust=0.5),
                                              label.theme=element_text(size=20))) +
    labs(x=NULL,y=NULL) +
    theme_classic(base_size=22) +
    theme(axis.text=element_text(size=20),
          legend.position="right", legend.direction="horizontal")
}

create_shap_bar_global <- function(shap_matrix) {
  bs <- as.data.frame(shap_matrix) %>%
    pivot_longer(everything(), names_to="feature", values_to="shap") %>%
    group_by(feature) %>% summarize(mean_abs = mean(abs(shap),na.rm=TRUE)) %>%
    mutate(pretty = case_when(
      grepl("^rocks_",feature) ~ gsub("_"," ", feature),
      TRUE ~ recode(feature, !!!recode_map)
    )) %>% filter(!is.na(pretty)) %>%
    arrange(desc(mean_abs))
  bs$pretty <- factor(bs$pretty, levels = rev(bs$pretty))
  ggplot(bs, aes(pretty, mean_abs)) +
    geom_col() + coord_flip() +
    labs(x=NULL, y="Mean Absolute SHAP Value") +
    theme_classic(base_size=22)
}

# 7. Build panels A & B and extract legend
lm_theme    <- theme_classic(base_size = 22)
subset_cols <- c("older70"="#1b9e77","recent30"="#d95f02","unseen10"="#7570b3")

A_full <- ggplot(pred_FNConc, aes(predicted, observed, color=subset)) +
  geom_point(size=3,alpha=0.8)+geom_abline(linetype="dashed")+
  scale_color_manual(values=subset_cols,name="Subset")+
  labs(x="Predicted",y="Observed",title="Concentration",tag="A")+
  lm_theme

B_full <- ggplot(pred_FNYield, aes(predicted, observed, color=subset)) +
  geom_point(size=3,alpha=0.8)+geom_abline(linetype="dashed")+
  scale_color_manual(values=subset_cols,name="Subset")+
  labs(x="Predicted",y=NULL,title="Yield",tag="B")+
  lm_theme

shared_legend <- get_legend(
  A_full +
    theme(legend.position="bottom",
          legend.direction="horizontal",
          legend.justification="center",
          legend.title.align=0.5,
          legend.key.width=unit(1.5,"lines"),
          legend.key.height=unit(1,"lines"),
          legend.text=element_text(size=20),
          legend.title=element_text(size=22))
)

A_ <- A_full + theme(legend.position="none")
B_ <- B_full + theme(legend.position="none")

# 8. Build C & D
C_ <- create_shap_bar_global(shap_values_FNConc)  + labs(tag="C") + theme(plot.tag=element_text(size=24),plot.tag.position=c(0.02,0.98))
D_ <- create_shap_bar_global(shap_values_FNYield)+ labs(tag="D") + theme(plot.tag=element_text(size=24),plot.tag.position=c(0.02,0.98))

# 9. Build E & F
E_ <- dot_plot(shap_values_FNConc,kept_FNConc_s,gmin,gmax)+ labs(tag="E")+ theme(plot.tag=element_text(size=24),plot.tag.position=c(0.02,0.98),legend.position="none")
F_ <- dot_plot(shap_values_FNYield,kept_FNYield_s,gmin,gmax)+ labs(tag="F")+ theme(plot.tag=element_text(size=24),plot.tag.position=c(0.02,0.98),legend.position="none")

# 10. Assemble
row1     <- plot_grid(A_, B_, ncol = 2)
row1_leg <- plot_grid(row1, shared_legend, ncol = 1, rel_heights = c(1, 0.2))

row2 <- plot_grid(C_, D_, ncol = 2)
row3 <- plot_grid(E_, F_, ncol = 2)

final_fig2 <- plot_grid(
  row1_leg,
  row2,
  row3,
  ncol        = 1,
  rel_heights = c(1, 1, 1),
  align       = "v"
)

# 11. Save
ggsave(
  file.path(output_dir, "Fig2_Global_FNConc_FNYield_multi.png"),
  final_fig2,
  width  = 12, height = 15, dpi = 300, bg = "white"
)
