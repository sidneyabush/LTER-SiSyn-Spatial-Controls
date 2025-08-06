# ----- Fig S6: Faceted 3x2 Boxplots with shared Y-axis -----

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(colorspace)

# 1. Load & prep data
recent30 <- read.csv("harmonization_files/AllDrivers_cc_recent30.csv", stringsAsFactors=FALSE)
# reconstruct final_cluster exactly as before...
site_clusters <- recent30 %>%
  distinct(Stream_ID, major_rock, rocks_volcanic, rocks_sedimentary,
           rocks_carbonate_evaporite, rocks_metamorphic, rocks_plutonic) %>%
  mutate(
    consolidated_rock = case_when(
      major_rock %in% c("volcanic","volcanic; plutonic") ~ "Volcanic",
      major_rock %in% c("sedimentary","sedimentary; metamorphic",
                        "sedimentary; carbonate_evaporite",
                        "volcanic; sedimentary; carbonate_evaporite",
                        "sedimentary; plutonic; carbonate_evaporite; metamorphic") ~ "Sedimentary",
      major_rock %in% c("plutonic","plutonic; metamorphic",
                        "volcanic; plutonic; metamorphic") ~ "Plutonic",
      major_rock %in% c("metamorphic","carbonate_evaporite; metamorphic") ~ "Metamorphic",
      major_rock %in% c("carbonate_evaporite","volcanic; carbonate_evaporite") ~ "Carbonate Evaporite",
      TRUE ~ NA_character_
    ),
    final_cluster = case_when(
      consolidated_rock=="Sedimentary" & rocks_sedimentary>=70 ~ "Sedimentary",
      consolidated_rock=="Sedimentary" & rocks_sedimentary<70  ~ "Mixed Sedimentary",
      TRUE                                                      ~ consolidated_rock
    )
  ) %>%
  select(Stream_ID, final_cluster)

df <- recent30 %>%
  left_join(site_clusters, by="Stream_ID") %>%
  mutate(NOx = log10(NOx), P = log10(P)) %>%
  mutate(across(where(is.numeric), ~ rescale(.x, to=c(0,1))))

# 2. Manual ordering & labels
var_order  <- c("NOx","P","npp","evapotrans","greenup_day","precip","temp",
                "snow_cover","permafrost","elevation","basin_slope","RBI",
                "recession_slope","land_Bare","land_Cropland","land_Forest",
                "land_Grassland_Shrubland","land_Ice_Snow","land_Impervious",
                "land_Salt_Water","land_Tidal_Wetland","land_Water","land_Wetland_Marsh")
var_labels <- c("log(N)","log(P)","NPP","ET","Greenup Day","Precip","Temp",
                "Snow Cover","Permafrost","Elevation","Basin Slope",
                "Flashiness (RBI)","Recession Curve Slope","Bare Land Cover","Cropland","Forest","Grass & Shrubland",
                "Ice & Snow Cover","Impervious Land","Salt Water Cover","Tidal Wetland",
                "Open Water Cover","Wetland")
recode_map <- setNames(var_labels, var_order)

# 3. Precompute shading spans
span <- function(lab) { which(var_labels==lab) }
prod_range <- span("log(N)") - .5
prod_range[2] <- span("Greenup Day") + .5
clim_range <- c(span("Precip")-.5, span("Permafrost")+.5)
topo_range <- c(span("Elevation")-.5, span("Basin Slope")+.5)
disc_range <- c(span("Flashiness (RBI)")-.5, span("Recession Curve Slope")+.5)
lulc_range <- c(
  span("Bare Land Cover") - 0.5,
  span("Wetland")        + 0.5
)

# 4. Melt to long
df_long <- df %>%
  select(final_cluster, all_of(var_order)) %>%
  pivot_longer(-final_cluster, names_to="feature", values_to="scaled_value") %>%
  mutate(
    feature = recode(feature, !!!recode_map),
    feature = factor(feature, levels=var_labels),
    final_cluster = factor(final_cluster,
                           levels=c("Volcanic","Sedimentary","Mixed Sedimentary",
                                    "Plutonic","Metamorphic","Carbonate Evaporite"))
  )

# 5. Base ggplot
p <- ggplot(df_long, aes(x = feature, y = scaled_value)) +
  # shading with selective alpha and no inherited aesthetics
  annotate(
    "rect",
    xmin        = prod_range[1], xmax = prod_range[2],
    ymin        = -Inf,            ymax = Inf,
    fill        = "#F0F0F0",       color = NA,
    inherit.aes = FALSE
  ) +
  annotate(
    "rect",
    xmin        = clim_range[1],   xmax = clim_range[2],
    ymin        = -Inf,            ymax = Inf,
    fill        = "#FFFFFF",       color = NA,
    inherit.aes = FALSE
  ) +
  annotate(
    "rect",
    xmin        = topo_range[1],   xmax = topo_range[2],
    ymin        = -Inf,            ymax = Inf,
    fill        = "#E5E5E5",       alpha = 0.9,
    color       = NA,
    inherit.aes = FALSE
  ) +
  annotate(
    "rect",
    xmin        = disc_range[1],   xmax = disc_range[2],
    ymin        = -Inf,            ymax = Inf,
    fill        = "#E5E5E5",       alpha = 0.5,
    color       = NA,
    inherit.aes = FALSE
  ) +
  annotate(
    "rect",
    xmin        = lulc_range[1],   xmax = lulc_range[2],
    ymin        = -Inf,            ymax = Inf,
    fill        = "#FFFFFF",       color = NA,
    inherit.aes = FALSE
  ) +
 
  # labels
  annotate("text", x=mean(prod_range), y=1.05, label="Productivity", size=4, vjust=0) +
  annotate("text", x=mean(clim_range), y=1.05, label="Climate",      size=4, vjust=0) +
  annotate("text", x=mean(topo_range), y=1.05, label="Topo",         size=4, vjust=0) +
  annotate("text", x=mean(disc_range), y=1.05, label="Q",            size=4, vjust=0) +
  annotate("text", x=mean(lulc_range), y=1.05, label="LULC",         size=4, vjust=0) +
  
  # actual boxplots

  geom_boxplot(
    outlier.shape = NA,
    width         = 0.7,
    aes(fill       = final_cluster),
    color         = "black",
    alpha         = 0.6       # ← set transparency here
  ) +

  
  
  scale_fill_manual(values=setNames(lighten(c("#AC7B32","#579C8E","#89C8A0",
                                              "#8D9A40","#C26F86","#5E88B0"),0.05),
                                    levels(df_long$final_cluster))) +
  scale_x_discrete(expand=expansion(add=c(1,0))) +
  labs(x=NULL, y="Scaled Value") +
  theme_classic(base_size=14) +
  theme(
    axis.text.x = element_text(angle=90, vjust=.5, hjust=1, size=12),
    axis.title.y = element_text(size=14),
    legend.position="none"
  ) +
  facet_wrap(~ final_cluster, ncol=3)

p2 <- p +
  facet_wrap(~ final_cluster, ncol = 3) +
  theme_classic(base_size = 16) +          
  theme(
    # PANEL
    panel.background = element_rect(fill = "white", color = NA),
    panel.border     = element_rect(color = "gray80", fill = NA, size = 0.5),
    panel.spacing    = unit(0.5, "lines"),
    axis.line        = element_blank(),
    
    # STRIP (facet title)
    strip.background = element_rect(fill = "white", color = NA),
    strip.text.x = element_text(face="bold", size=14, margin=margin(t=4, b=4)),
    
    # GRID: if you want some grid lines, re-add them; otherwise skip
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # AXES
    axis.text.x      = element_text(angle = 90, vjust = .5, hjust = 1, size = 10),
    axis.title.y     = element_text(size = 14),
    
    legend.position  = "none"
  )

# Save it
ggsave("Final_Figures/FigS6_Boxplots_lithology_testData.png",
       plot = p2, width = 15, height = 10, dpi = 300)