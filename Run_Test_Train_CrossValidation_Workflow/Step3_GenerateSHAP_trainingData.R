# ──────────────────────────────────────────────────────────────────────────────
# Compute SHAP values for recent30 data using RF2 models for FNConc and FNYield
# Saves SHAP values as .RData for plotting later
# ──────────────────────────────────────────────────────────────────────────────

# 1. Load needed packages
librarian::shelf(
  iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach,
  randomForest, tibble, viridis, RColorBrewer, patchwork, fastshap
)

# 2. Clear environment
rm(list = ls()); set.seed(123)

# 3. Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# Define the path to your Final_Models folder
final_models_dir <- "Final_Models"

# 4. Load FNConc model/data from Final_Models
load(file.path(final_models_dir, "FNConc_Yearly_rf_model2.RData"))  
# this brings in `rf2`
rf_model2_FNConc    <- rf2
rm(rf2)

load(file.path(final_models_dir, "FNConc_Yearly_kept_drivers.RData"))
# this brings in `kept_drivers`
kept_drivers_FNConc <- kept_drivers
rm(kept_drivers)

# 5. Load FNYield RF2 model
loaded_objs <- load(file.path(final_models_dir, "FNYield_Yearly_rf_model2.RData"))
# you should see "rf2"
rf_model2_FNYield <- rf2
rm(rf2)

# 6. Load the kept‐drivers for FNYield
loaded_objs <- load(file.path(final_models_dir, "FNYield_Yearly_kept_drivers.RData"))
# you should see "kept_drivers"
kept_drivers_FNYield <- kept_drivers
rm(kept_drivers)


###############################################################################
# 6. Define Function to Create SHAP Values
###############################################################################
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  custom_predict <- function(object, newdata) {
    newdata <- as.data.frame(newdata)
    predict(object, newdata = newdata)
  }
  
  shap_values <- fastshap::explain(
    object       = model,
    X            = kept_drivers,
    pred_wrapper = custom_predict,
    nsim         = sample_size
  )
  return(shap_values)
}

###############################################################################
# 7. Generate SHAP Values for Both FNConc and FNYield
###############################################################################
shap_values_FNConc  <- generate_shap_values(
  rf_model2_FNConc,  
  kept_drivers_FNConc,  
  sample_size = 30
)
shap_values_FNYield <- generate_shap_values(
  rf_model2_FNYield, 
  kept_drivers_FNYield, 
  sample_size = 30
)

###############################################################################
# 8. Save SHAP Values to Final_Models
###############################################################################
save(
  shap_values_FNConc,
  file = file.path(final_models_dir, "FNConc_Yearly_shap_values_recent30.RData")
)

save(
  shap_values_FNYield,
  file = file.path(final_models_dir, "FNYield_Yearly_shap_values_recent30.RData")
)
