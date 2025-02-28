# 1. Load needed packages
librarian::shelf(iml, ggplot2, dplyr, tidyr, reshape2, parallel, foreach, 
                 randomForest, tibble, viridis, RColorBrewer, patchwork)

# 2. Clear environment
rm(list = ls())

# 3. Set working directory
setwd("/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn")

# ------------------------- FNConc Data (Concentration) -------------------------
# 4. Load FNConc model/data
load("FNConc_Yearly_rf_model2_full_new.RData")   # => rf_model2
rf_model2_FNConc <- rf_model2
load("FNConc_Yearly_kept_drivers__full_new.RData")
kept_drivers_FNConc <- kept_drivers
load("FNConc_Yearly_full_new.RData")
drivers_numeric_FNConc <- drivers_numeric
load("FNConc_Yearly_full_stream_ids_full_new.RData")  # if needed

# ------------------------- FNYield Data (Yield) -------------------------
# 5. Load FNYield model/data
load("FNYield_Yearly_rf_model2_full_new.RData")  # => rf_model2
rf_model2_FNYield <- rf_model2
load("FNYield_Yearly_kept_drivers_full_new.RData")
kept_drivers_FNYield <- kept_drivers
load("FNYield_Yearly_full_new.RData")
drivers_numeric_FNYield <- drivers_numeric
load("FNYield_Yearly_full_stream_ids_new.RData") # if needed

# 6. Set seed and output directory
set.seed(123)
output_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/"

###############################################################################
# 7. Define Function to Create SHAP Values
###############################################################################
generate_shap_values <- function(model, kept_drivers, sample_size = 30) {
  # Custom prediction function
  custom_predict <- function(object, newdata) {
    newdata <- as.data.frame(newdata)
    predict(object, newdata = newdata)
  }
  
  # Compute SHAP values using fastshap
  shap_values <- fastshap::explain(
    object       = model,
    X            = kept_drivers,
    pred_wrapper = custom_predict,
    nsim         = sample_size
  )
  return(shap_values)
}

###############################################################################
# 8. Generate SHAP Values for Both FNConc and FNYield
###############################################################################
shap_values_FNConc <- generate_shap_values(rf_model2_FNConc, kept_drivers_FNConc, sample_size = 30)
shap_values_FNYield <- generate_shap_values(rf_model2_FNYield, kept_drivers_FNYield, sample_size = 30)

# 9. (Optional) Save SHAP Values
save(shap_values_FNConc, file = "FNConc_Yearly_shap_values_new.RData")
save(shap_values_FNYield, file = "FNYield_Yearly_shap_values_new.RData")

###############################################################################
# 10. Next Steps
# - You can now use shap_values_FNConc and shap_values_FNYield for plotting, 
#   analyzing feature importance, etc.
###############################################################################