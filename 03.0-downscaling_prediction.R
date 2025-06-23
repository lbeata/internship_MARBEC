
# -----------------------------
# GLOBAL ATTRIBUTES FUNCTION
# -----------------------------
make_global_attributes = function(model_name, scenario, temporal_range) {
  list(
    title = sprintf("Bias-corrected SST projection for IPSL-CM5A-LR %s period", scenario),
    summary = sprintf("Monthly SST %s bias-corrected using Generalized Additive Models (GAMs).",
                      if (scenario == "historical") "reanalysis" else "projections"),
    source = sprintf("IPSL-CM5A-LR model, %s period", scenario),
    institution = "MARBEC",
    author = "BEATA Lou",
    email = "loubeata@gmail.com",
    date_created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    history = sprintf("Bias corrected using GAMs with monthly predictors on %s", format(Sys.Date())),
    software = "Generated in R using gts and mgcv packages",
    comment = "Predictions are from regridded and bias-corrected simulations, units in Celsius",
    spatial_extent = "lon: 266.875–290.125, lat: -20.125–6.125",
    temporal_range = temporal_range,
    model_version = model_name
  )
}

# generate_predictions <- function(model, newdata_df, sim_template) {
#   model_vars <- all.vars(formula(model))
#   
#   # Match factor levels to ensure consistency with model
#   for (v in names(newdata_df)) {
#     if (is.factor(newdata_df[[v]]) && v %in% names(model$model)) {
#       model_levels <- levels(model$model[[v]])
#       newdata_df[[v]] <- factor(newdata_df[[v]], levels = model_levels)
#     }
#   }
#   
#   # Predict SST
#   newdata_df$sst_sim <- predict(model, newdata = newdata_df)
#   
#   # Inject predictions into NetCDF-like structure
#   sim_template$x[] <- newdata_df$sst_sim
#   return(sim_template)
# }
# 
write_prediction_ncdf <- function(gts_object, filepath, attributes, model_name) {
  attributes$model_version <- model_name
  write_ncdf(
    x = gts_object,
    filename = filepath,
    varid = "sst",
    longname = "Bias-corrected sea surface temperature",
    global = attributes
  )
}

generate_predictions <- function(model, newdata_df, sim_template) {
  # Ensure factor levels match model
  for (v in names(newdata_df)) {
    if (is.factor(newdata_df[[v]]) && v %in% names(model$model)) {
      model_levels <- levels(model$model[[v]])
      newdata_df[[v]] <- factor(newdata_df[[v]], levels = model_levels)
    }
  }
  
  # Predict using correct method
  if (inherits(model, "scam")) {
    pred_vals <- scam::predict.scam(model, newdata = newdata_df, type = "response", block.size = 10000)
  } else if (inherits(model, "gam")) {
    pred_vals <- mgcv::predict.gam(model, newdata = newdata_df, type = "response", block.size = 10000)
  } else {
    stop("Unsupported model class: ", class(model))
  }
  
  newdata_df$sst_sim <- pred_vals
  sim_template$x[] <- newdata_df$sst_sim
  return(sim_template)
}


compute_and_save_quantiles <- function(pred_gts, output_path, probs = seq(0, 1, by = 0.05)) {
  q <- apply(pred_gts$x, c(1, 2), quantile, probs = probs, na.rm = TRUE)
  saveRDS(q, output_path)
}
