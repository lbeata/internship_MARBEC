# # ─────────────────────────────────────
# # 1. SETUP
# # ─────────────────────────────────────
# 
# #Load required libraries
# library(gts)
# library(nctools)
# 
# 
# # === Load required RCP data ===
# rcp26 = read_gts("input/ipsl-cm5a-lr_rcp26_to_zs_monthly_200601_21001231.nc4")
# longitude(rcp26) = longitude(rcp26, "left")
# sim26 = subset(rcp26, grid = obs, expand = 5)
# sim26 = regrid(sim26, grid = obs, method = "akima", extrap = TRUE)
# sim26 = sim26 - 273.15  # Convert from Kelvin to Celsius
# 
# 
# # Prepare the data for prediction 
# fdat26 = sim26
# fdat26_seasonality = month_season_factor(fdat26)
# 
# fdat26m = melt(fdat26)
# names(fdat26m)[1:2] = c("lon", "lat")
# names(fdat26_seasonality)[1:2] = c("lon", "lat")
# 
# 
# # === Auto-detect all model .rds files ===
# model_dir = "models_month"  
# model_files = list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)
# model_names = tools::file_path_sans_ext(basename(model_files))  # Remove .rds extension
# 
# 
# # Global attributes to add to the NetCDF file
# global_attributes = list(
#   title = "Bias-corrected SST projection for IPSL-CM5A-LR RCP2.6",
#   summary = "Monthly SST projections bias-corrected using Generalized Additive Models (GAMs).",
#   source = "IPSL-CM5A-LR model, RCP2.6 scenario",
#   institution = "MARBEC",
#   author = "BEATA Lou",
#   email = "loubeata@gmail.com",
#   date_created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),  # ISO 8601 format with timestamp
#   history = "Bias corrected using GAMs with monthly predictors on 2025-05-23",
#   # references = "https://doi.org/xxxxx", 
#   software = "Generated in R using gts and mgcv packages",
#   comment = "Predictions are from regridded and bias-corrected simulations, units in Celsius",
#   spatial_extent = "lon: 266.875–290.125, lat: -20.125–6.125",  # Domain 
#   temporal_range = "2006-01 to 2100-12",
#   model_version = NA  # Will be filled dynamically in the loop per model
# )
# 
# 
# # Fixed filename for all outputs
# fixed_filename = "ipsl-cm5a-lr_rcp26_to_zs_humboldt-n_monthly_200601_210012.nc4"
# 
# # Base directory where all model folders go
# base_dir = "downscaling"
# 
# # Pattern for model folder naming — %s will be replaced with model name like "modm0.0"
# model_dir_pattern = file.path(base_dir, "%s")
# 
# # ─────────────────────────────────────
# # 2. Prediction Loop
# # ─────────────────────────────────────
# 
# # Loop over each model
# for (i in seq_along(model_files)) {
#   model_path = model_files[i]
#   modelName  = model_names[i]
#   
#   cat(sprintf("\n--- Starting model %s ---\n", modelName))
#   
#   # Define output path
#   outputDir = sprintf("downscaling/%s", modelName)
#   predFile  = file.path(outputDir, fixed_filename)
#   
#   # Skip if output already exists
#   if (file.exists(predFile)) {
#     cat(sprintf("→ Skipping model %s (prediction already exists)\n", modelName))
#     next
#   }
#   
#   # Ensure directory exists
#   if (!dir.exists(outputDir)) {
#     cat("Creating output directory...\n")
#     dir.create(outputDir, recursive = TRUE)
#   }
#   
#   # Load model
#   cat("Reading model from file...\n")
#   model = readRDS(model_path)
#   model_vars = all.vars(formula(model))
#   cat("Model loaded and formula parsed.\n")
#   
#   # Select correct newdata
#   cat("Choosing input data...\n")
#   if (any(c("month", "month_fact", "season_fact") %in% model_vars)) {
#     newdata = fdat26_seasonality
#     cat("→ Using seasonal input data\n")
#   } else {
#     newdata = fdat26m
#     cat("→ Using non-seasonal input data\n")
#   }
#   
#   # Fix factor levels
#   cat("Adjusting factor levels...\n")
#   for (v in names(newdata)) {
#     if (is.factor(newdata[[v]]) && v %in% names(model$model)) {
#       model_levels = levels(model$model[[v]])
#       newdata[[v]] = factor(newdata[[v]], levels = model_levels)
#     }
#   }
#   cat("Factor levels adjusted.\n")
#   
#   # Predict
#   cat("Running prediction...\n")
#   newdata$to = predict(model, newdata = newdata)
#   cat("Prediction complete.\n")
#   
#   # Replace values in gts object
#   cat("Preparing GTS object...\n")
#   sim26_bc = sim26
#   sim26_bc$x[] = newdata$to
#   cat("GTS object ready.\n")
#   
#   # Metadata
#   global_attributes$model_version <- modelName
#   
#   # Write to NetCDF
#   cat(sprintf("Writing NetCDF file to %s ...\n", predFile))
#   write_ncdf(
#     x        = sim26_bc,
#     filename = predFile,
#     varid    = "sst",
#     longname = "Sea Surface Temperature",
#     global   = global_attributes
#   )
#   cat(sprintf("✓ Prediction written for model %s -> %s\n", modelName, predFile))
# }
# 
# 


# # === Load required RCP8.5 data ===
# rcp85 = read_gts("input/ipsl-cm5a-lr_rcp85_to_zs_monthly_200601_21001231.nc4")
# longitude(rcp85) = longitude(rcp85, "left")
# sim85 = subset(rcp85, grid = obs, expand = 5)
# sim85 = regrid(sim85, grid = obs, method = "akima", extrap = TRUE)
# sim85 = sim85 - 273.15  # Convert from Kelvin to Celsius
# 
# # Prepare the data for prediction 
# fdat85 = sim85
# fdat85_seasonality = month_season_factor(fdat85)
# 
# fdat85m = melt(fdat85)
# names(fdat85m)[1:2] = c("lon", "lat")
# names(fdat85_seasonality)[1:2] = c("lon", "lat")
# 
# # === Auto-detect all model .rds files ===
# model_dir = "models_month"
# model_files = list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)
# model_names = tools::file_path_sans_ext(basename(model_files))
# 
# # Global attributes for NetCDF file
# global_attributes = list(
#   title = "Bias-corrected SST projection for IPSL-CM5A-LR RCP8.5",
#   summary = "Monthly SST projections bias-corrected using Generalized Additive Models (GAMs).",
#   source = "IPSL-CM5A-LR model, RCP8.5 scenario",
#   institution = "MARBEC",
#   author = "BEATA Lou",
#   email = "loubeata@gmail.com",
#   date_created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
#   history = "Bias corrected using GAMs with monthly predictors on 2025-05-23",
#   software = "Generated in R using gts and mgcv packages",
#   comment = "Predictions are from regridded and bias-corrected simulations, units in Celsius",
#   spatial_extent = "lon: 266.875–290.125, lat: -20.125–6.125",
#   temporal_range = "2006-01 to 2100-12",
#   model_version = NA
# )
# 
# # Fixed filename for all outputs
# fixed_filename = "ipsl-cm5a-lr_rcp85_to_zs_humboldt-n_monthly_200601_210012.nc4"
# 
# # Base directory where model output folders go
# base_dir = "downscaling"
# 
# # Pattern for model folder naming
# model_dir_pattern = file.path(base_dir, "%s")
# 
# # ─────────────────────────────────────
# # 2. Prediction Loop for RCP8.5
# # ─────────────────────────────────────
# 
# for (i in seq_along(model_files)) {
#   model_path = model_files[i]
#   modelName  = model_names[i]
#   
#   cat(sprintf("\n--- Starting model %s ---\n", modelName))
#   
#   # Define output path
#   outputDir = sprintf("downscaling/%s", modelName)
#   predFile  = file.path(outputDir, fixed_filename)
#   
#   # Skip if output already exists
#   if (file.exists(predFile)) {
#     cat(sprintf("→ Skipping model %s (prediction already exists)\n", modelName))
#     next
#   }
#   
#   # Ensure directory exists
#   if (!dir.exists(outputDir)) {
#     cat("Creating output directory...\n")
#     dir.create(outputDir, recursive = TRUE)
#   }
#   
#   # Load model
#   cat("Reading model from file...\n")
#   model = readRDS(model_path)
#   model_vars = all.vars(formula(model))
#   cat("Model loaded and formula parsed.\n")
#   
#   # Select correct input data
#   cat("Choosing input data...\n")
#   if (any(c("month", "month_fact", "season_fact") %in% model_vars)) {
#     newdata = fdat85_seasonality
#     cat("→ Using seasonal input data\n")
#   } else {
#     newdata = fdat85m
#     cat("→ Using non-seasonal input data\n")
#   }
#   
#   # Fix factor levels
#   cat("Adjusting factor levels...\n")
#   for (v in names(newdata)) {
#     if (is.factor(newdata[[v]]) && v %in% names(model$model)) {
#       model_levels = levels(model$model[[v]])
#       newdata[[v]] = factor(newdata[[v]], levels = model_levels)
#     }
#   }
#   cat("Factor levels adjusted.\n")
#   
#   # Prediction
#   cat("Running prediction...\n")
#   newdata$to = predict(model, newdata = newdata)
#   cat("Prediction complete.\n")
#   
#   # Replace values in gts object
#   cat("Preparing GTS object...\n")
#   sim85_bc = sim85
#   sim85_bc$x[] = newdata$to
#   cat("GTS object ready.\n")
#   
#   # Update metadata
#   global_attributes$model_version = modelName
#   
#   # Write NetCDF
#   cat(sprintf("Writing NetCDF file to %s ...\n", predFile))
#   write_ncdf(
#     x        = sim85_bc,
#     filename = predFile,
#     varid    = "sst",
#     longname = "Sea Surface Temperature",
#     global   = global_attributes
#   )
#   cat(sprintf("✓ Prediction written for model %s -> %s\n", modelName, predFile))
# }


# ─────────────────────────────────────
# 2. Prediction Loop for present data 
# ─────────────────────────────────────

#Load required libraries
library(gts)
library(nctools)

#Just to run as background job 
# Load packages
library(gts)
library(dplyr)

# Source functions that I wrote 
source("_align.R")
source("functions.R")

#Set directories 
inputDir = "input"
outputDir = "databases"
if (!dir.exists(outputDir)) dir.create(outputDir)


# === Load required data ===
#Load observation data to use as grid for other data
obs = read_gts("input/avhrr-only-v2-humboldt-n.198109-201706.nc4")
longitude(obs) = longitude(obs, "left")
#Apply mask 
mask(obs) = mask(obs$x)


sim = read_gts("input/ipsl-cm5a-lr_historical_to_zs_monthly_195001_200512.nc4")
longitude(sim) = longitude(sim, "left")
simh = subset(sim, grid = obs, expand = 5)
simh = regrid(simh, grid = obs, method = "nearest")
simh = simh - 273.15  # Convert from Kelvin to Celsius


# Prepare the data for prediction 
fsim = simh
fsim_seasonality = month_season_factor(fsim)

fsimm = melt(fsim)
names(fsimm)[1:2] = c("lon", "lat")
names(fsim_seasonality)[1:2] = c("lon", "lat")

# === Auto-detect all model .rds files ===
model_dir = "models_month"  
model_files = list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)
model_names = tools::file_path_sans_ext(basename(model_files))  # Remove .rds extension


# Global attributes to add to the NetCDF file
global_attributes = list(
  title = "Bias-corrected SST projection for IPSL-CM5A-LR historical period",
  summary = "Monthly SST projections bias-corrected using Generalized Additive Models (GAMs).",
  source = "IPSL-CM5A-LR model, historical period",
  institution = "MARBEC",
  author = "BEATA Lou",
  email = "loubeata@gmail.com",
  date_created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),  # ISO 8601 format with timestamp
  history = "Bias corrected using GAMs with monthly predictors on 2025-05-23",
  # references = "https://doi.org/xxxxx", 
  software = "Generated in R using gts and mgcv packages",
  comment = "Predictions are from regridded and bias-corrected simulations, units in Celsius",
  spatial_extent = "lon: 266.875–290.125, lat: -20.125–6.125",  # Domain 
  temporal_range = "1950-01 to 2005-12",
  model_version = NA  # Will be filled dynamically in the loop per model
)


# Fixed filename for all outputs
fixed_filename = "ipsl-cm5a-lr_historical_to_zs_monthly_195001_200512.nc4"

# Base directory where all model folders go
base_dir = "downscaling"

# Pattern for model folder naming — %s will be replaced with model name like "modm0.0"
model_dir_pattern = file.path(base_dir, "%s")

#Create prediction's loop 
# ─────────────────────────────────────
# 2. Prediction Loop for present
# ─────────────────────────────────────

for (i in seq_along(model_files)) {
  model_path = model_files[i]
  modelName  = model_names[i]
  
  cat(sprintf("\n--- Starting model %s ---\n", modelName))
  
  # Define output path
  outputDir = sprintf("downscaling/%s", modelName)
  predFile  = file.path(outputDir, fixed_filename)
  
  # Skip if output already exists
  if (file.exists(predFile)) {
    cat(sprintf("→ Skipping model %s (prediction already exists)\n", modelName))
    next
  }
  
  # Ensure directory exists
  if (!dir.exists(outputDir)) {
    cat("Creating output directory...\n")
    dir.create(outputDir, recursive = TRUE)
  }
  
  # Load model
  cat("Reading model from file...\n")
  model = readRDS(model_path)
  model_vars = all.vars(formula(model))
  cat("Model loaded and formula parsed.\n")
  
  # Select correct input data
  cat("Choosing input data...\n")
  if (any(c("month", "month_fact", "season_fact") %in% model_vars)) {
    newdata = fsim_seasonality
    cat("→ Using seasonal input data\n")
  } else {
    newdata = fsimm
    cat("→ Using non-seasonal input data\n")
  }
  
  # Fix factor levels
  cat("Adjusting factor levels...\n")
  for (v in names(newdata)) {
    if (is.factor(newdata[[v]]) && v %in% names(model$model)) {
      model_levels = levels(model$model[[v]])
      newdata[[v]] = factor(newdata[[v]], levels = model_levels)
    }
  }
  cat("Factor levels adjusted.\n")
  
  # Prediction
  cat("Running prediction...\n")
  newdata$to = predict(model, newdata = newdata)
  cat("Prediction complete.\n")
  
  # Replace values in a historical GTS object
  cat("Preparing GTS object...\n")
  sim_h_bc = simh  # You must have defined this before!
  sim_h_bc$x[] = newdata$to
  cat("GTS object ready.\n")
  
  # Update metadata
  global_attributes$model_version = modelName
  
  # Write NetCDF
  cat(sprintf("Writing NetCDF file to %s ...\n", predFile))
  write_ncdf(
    x        =  sim_h_bc,
    filename = predFile,
    varid    = "sst",
    longname = "Sea Surface Temperature",
    global   = global_attributes
  )
  cat(sprintf("✓ Prediction written for model %s -> %s\n", modelName, predFile))
}



#Loop versions 
# Load models
model_files <- list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)
# scenarios <- c("rcp26", "rcp45", "rcp60", "rcp85")
scenarios <- c("rcp26", "rcp85")

for (model_file in model_files) {
  model_name <- sub("\\.rds$", "", basename(model_file))
  model_obj  <- readRDS(model_file)
  
  for (rcp in scenarios) {
    suffix <- if (use_seasonality) "_seasonality_m" else "m"
    rcp_id <- substr(rcp, 4, 5)
    
    sim_data_name <- paste0("fsim", rcp_id, suffix)
    template_name <- paste0("fsim", rcp_id)
    
    # Prepare output path
    time_label <- "2050-2099"  # fallback if attribute missing
    save_dir <- file.path(output_root, model_name)
    if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
    nc_filename <- sprintf("ipsl-cm5a-lr_%s_to_zs_humboldt-n_monthly_%s.nc4", rcp, time_label)
    nc_path <- file.path(save_dir, nc_filename)
    
    # ✅ QUICK SKIP IF FILE EXISTS
    if (file.exists(nc_path)) {
      message(sprintf("✓ Skipping %s for model %s (already computed)", rcp, model_name))
      next
    }
    
    # Only load data if not already saved
    sim_data_df <- fsim_env[[sim_data_name]]
    sim_template <- fsim_env[[template_name]]
    
    if (is.null(sim_data_df) || is.null(sim_template)) {
      warning("Missing data for: ", sim_data_name, " or ", template_name)
      next
    }
    
    # Generate predictions
    pred_gts <- generate_predictions(model_obj, sim_data_df, sim_template)
    
    # Update time label if available
    time_label <- attr(pred_gts, "time_label") %||% time_label
    nc_filename <- sprintf("ipsl-cm5a-lr_%s_to_zs_humboldt-n_monthly_%s.nc4", rcp, time_label)
    nc_path <- file.path(save_dir, nc_filename)
    
    # Save as NetCDF
    global_attr <- make_global_attributes(model_name, rcp, time_label)
    write_prediction_ncdf(pred_gts, nc_path, global_attr, model_name)
    
    # Optional: Save quantiles
    quantile_path <- gsub(".nc4$", "_quantiles.rds", nc_path)
    compute_and_save_quantiles(pred_gts, quantile_path)
  }
}

