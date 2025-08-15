# LTER‐SiSyn Spatial Controls

Code for the “spatial controls” analysis behind our global riverine dissolved silicon (DSi) manuscript.  
This repository contains an end-to-end R workflow to:

1) harmonize watershed-scale drivers,  
2) create train/test/withheld (cross-site) splits,  
3) fit Random Forest models for DSi concentrations and yields,  
4) compute SHAP values for interpretation, and  
5) generate manuscript figures.

---

## Project status

Active (manuscript in submission prep). Breaking changes are possible as we finalize figures and text.  
Code will be archived and versioned upon submission through the creation of a new repo.

---

## Repository structure

- `Step1_Harmonization/` — scripts to convert units, assemble WRTDS–Kalman + discharge tables, and build the harmonized drivers + data partitions.
- `Step2_RF_Model_SHAP/` — scripts to train RF models (FNConc, FNYield), generate predictions/diagnostics, and compute SHAP values.
- `Step3_Create_Publication_Figures/` — scripts to build all manuscript and SI figures.
- `Exploratory_Analyses/` — deprecated code from the beginning of the project retained for reference.
- `Archived/` — deprecated helpers retained for reference.
  
---

## Overview of scripts

### Step1_Harmonization/
- `1.1_Unit_conversions_raw_N_P.R` — Convert raw NOx/NO3 and SRP/PO4 to mg/L using LTER-specific factors; filter to 2001–2023 for listed sites; export `converted_raw_NP.csv`.
- `1.2_Catalina_Jemez_Kalman_Q.R` — Combine Catalina–Jemez annual WRTDS–Kalman CSVs, add IDs/metadata; export the annual table plus a daily-discharge subset for `Catalina Jemez__OR_low` and `Catalina Jemez__OR_WEIR`.
- `1.3_Driver_Harmonization_Data_Partitioning.R` — Build harmonized per-year drivers (yields, RBI, recession slope, land cover, N/P gap-fills), apply QC/outlier rules, and write the full dataset plus 70/30 temporal splits and a 10% spatial holdout.

### Step2_RF_Model_SHAP/
- `Step2.1_RF_Model_FNConc.R` — Train FNConc RF on `older70` (RF1 + bootstrap stability selection → RF2); predict on `recent30` and `unseen10`; save diagnostics, predictions, model artifacts, and kept drivers for SHAP.
- `Step2.2_RF_Model_FNYield.R` — Train FNYield RF on `older70` (RF1 + bootstrap stability selection → RF2); predict on `recent30` and `unseen10`; save diagnostics, predictions, model artifacts, and kept drivers for SHAP.
- `Step2.3_GenerateSHAP_trainingData.R` — Compute SHAP values for FNConc and FNYield using trained RF2 models on the `recent30` kept-drivers subset; save results for re-loading and plotting.

### Step3_Create_Publication_Figures/
- `Fig1_lithology_FNConc_FNYield.R` — **Fig1.** Creates site map and saves `Fig1_map_and_boxplots.png`.
- `Fig2_model_performance_shap_testData.R` — **Fig2.** Creates Figure 2 (train/test/cross-val performance + SHAP bars/dot plots for FNConc & FNYield) and saves `Fig2_Global_FNConc_FNYield_multi_split.png`.
- `Fig3_4_S4_S5_shap_pdpR` — **Figs3,4,S4,S5.** Creates SHAP–LOESS panels for recent30 and assembles Figures 3, 4, S4, and S5.
- `Fig5_weighted_bar_plots_testData.R` — **Fig5.** Builds lithology-weighted stacked SHAP bar charts (concentration & yield) and saves `Fig5_Lithology_Stacked_SHAP_WeightedValues_split.png`.
- `FigS1_GenVsFN.R` — **FigS1.** Generates Gen vs FN scatterplots and OLS predicted-vs-observed panels A–D and saves `FigS1_GenFN_and_OLS.png`.
- `FigS2_FigS3_histograms_corrplots.R` — **FigsS2,S3.** Creates partitioned driver histograms and a Testing-only correlation plot, saving `FigS2_histograms_split.png` and `FigS3_corrplot_testing_split.png`.
- `FigS6_boxplots_lithology_testData.R` — **FigS6.** Generates faceted 3×2 boxplots of scaled driver distributions by lithology for the Testing (recent30) split and saves `FigS6_Boxplots_lithology_split.png`.

---

## Script headers (inputs/outputs)

Each script begins with a brief header that lists:
- **Required inputs** (files/tables the script expects), and  
- **Outputs created** (artifacts the script writes, e.g., CSVs, RDS models, figures).

Update the paths in those headers to match your local setup.

---

## Software & R packages

- **R ≥ 4.3.x** on macOS
- Core packages used across scripts include:  
  `data.table`, `dplyr`, `tidyr`, `stringr`, `lubridate`,  
  `ggplot2`, `cowplot`, `patchwork`, `sf`, `ggspatial`, `ggrepel`, `maps`,  
  `randomForest`, `fastshap`, `iml`, `pdp`,  
  `foreach`, `doParallel`, and `librarian`.

