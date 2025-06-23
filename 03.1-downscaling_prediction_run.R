

#VERSION 2
# Load prediction function
source("03.0-downscaling_prediction.R")
quantileDir = "quantiles"

# Load future simulation datasets
fsim_env = new.env()
load(file.path(quantileDir, "fsim_all.RData"), envir = fsim_env)

library(mgcv)
library(gts)

# Parameters
output_root = "outputs/downscaling"
model_dir   = "models_month"  # change to models_month_seasonality if needed

use_seasonality = TRUE


# VERSION WITH EARLY SKIP FOR EXISTING FILES (faster)
model_files = list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)
# s
rcp_all = c("rcp26", "rcp85", "historical")

for (model_file in model_files) {
  model_name = sub("\\.rds$", "", basename(model_file))
  model_obj  = readRDS(model_file)  # Still early, can optimize more if needed
  
  for (rcp in rcp_all) {
    suffix       = if (use_seasonality) "_seasonality_m" else "m"
    rcp_id       = if (rcp == "historical") "hist" else substr(rcp, 4, 5)
    sim_data_key = paste0("fsim", rcp_id, suffix)
    template_key = paste0("fsim", rcp_id)
    
    sim_data_df  = fsim_env[[sim_data_key]]
    sim_template = fsim_env[[template_key]]
    
    if (is.null(sim_data_df) || is.null(sim_template)) {
      warning("Missing data for: ", sim_data_key, " or ", template_key)
      next
    }
    
    # Define output
    save_dir = file.path(output_root, model_name)
    if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
    
    time_label = if (rcp == "historical") "1950-2005" else "2050-2099"
    nc_filename = if (rcp == "historical") {
      "ipsl-cm5a-lr_historical_to_zs_monthly_195001_200512.nc4"
    } else {
      sprintf("ipsl-cm5a-lr_%s_to_zs_humboldt-n_monthly_%s.nc4", rcp, time_label)
    }
    
    nc_path = file.path(save_dir, nc_filename)
    
    # 🚀 EARLY SKIP IF FILE EXISTS
    if (file.exists(nc_path)) {
      message(sprintf("✓ Skipping %s for model %s (already computed)", rcp, model_name))
      next
    }
    
    # Predict once (no duplication)
    pred_gts = generate_predictions(model_obj, sim_data_df, sim_template)
    
    # Optional: update time_label from prediction
    if (rcp != "historical") {
      attr_time = attr(pred_gts, "time_label")
      if (!is.null(attr_time)) time_label = attr_time
    }
    
    # Write NetCDF + quantiles
    global_attr = make_global_attributes(model_name, rcp, time_label)
    write_prediction_ncdf(pred_gts, nc_path, global_attr, model_name)
    
    quantile_filename = sprintf("quant_%s.rds", rcp)
    quantile_path = file.path(save_dir, quantile_filename)
    compute_and_save_quantiles(pred_gts, quantile_path)
    
  }
}

