# ──────────────────────────────────────────────────────────────────────────────
# Compute SHAP values for recent30 data using RF2 models for FNConc and FNYield
# Saves SHAP values as .RData for plotting later (on the recent30 subset)
# ──────────────────────────────────────────────────────────────────────────────

## 1. Load needed packages
librarian::shelf(
  iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach,
  randomForest, tibble, viridis, RColorBrewer, patchwork, fastshap
)

## 2. Clear environment & set seed
rm(list = ls())
set.seed(123)

## 3. Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

## 4. Path to your Final_Models folder
final_models_dir <- "Final_Models"

## 5. Load FNConc model & recent30 drivers
load(file.path(final_models_dir, "FNConc_Yearly_rf_model2.RData"))  
rf_model2_FNConc    <- rf2; rm(rf2)

load(file.path(final_models_dir, "FNConc_Yearly_kept_drivers.RData"))
kept_drivers_FNConc <- kept_drivers; rm(kept_drivers)

## 6. Load FNYield model & recent30 drivers
load(file.path(final_models_dir, "FNYield_Yearly_rf_model2.RData"))
rf_model2_FNYield   <- rf2; rm(rf2)

load(file.path(final_models_dir, "FNYield_Yearly_kept_drivers.RData"))
kept_drivers_FNYield <- kept_drivers; rm(kept_drivers)


###############################################################################
# 7. Define Function to Create SHAP Values (on recent30 data)
###############################################################################
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  # ensure we pass exactly the predictors the RF saw, in the correct order
  model_vars <- names(model$forest$xlevels)
  X_ordered  <- kept_drivers[, model_vars, drop = FALSE]
  
  custom_predict <- function(object, newdata) {
    predict(object, newdata = as.data.frame(newdata))
  }
  
  fastshap::explain(
    object       = model,
    X            = X_ordered,
    pred_wrapper = custom_predict,
    nsim         = sample_size
  )
}


###############################################################################
# 8. Generate SHAP Values for Both FNConc and FNYield (recent30 subset)
###############################################################################
shap_values_FNConc  <- generate_shap_values(
  model        = rf_model2_FNConc,  
  kept_drivers = kept_drivers_FNConc,  
  sample_size  = 30
)

shap_values_FNYield <- generate_shap_values(
  model        = rf_model2_FNYield, 
  kept_drivers = kept_drivers_FNYield, 
  sample_size  = 30
)


###############################################################################
# 9. Save SHAP Values back to Final_Models
###############################################################################
save(
  shap_values_FNConc,
  file = file.path(final_models_dir, "FNConc_Yearly_shap_values_recent30.RData")
)

save(
  shap_values_FNYield,
  file = file.path(final_models_dir, "FNYield_Yearly_shap_values_recent30.RData")
)
