# Functions 

#Select the files for the input easily with their names 
#' @param inputDir Path to the folder to search in.
#' @param pattern Pattern to match (e.g., "historical").
#' @param exclude Optional pattern to exclude (e.g., "rcp").
#' @param index Which match to return (default is 1).
#' @param recursive Whether to search subdirectories (default TRUE).
#' @return Full path to the selected file.
#' @export
#' 
#' 
select_nc_file = function(inputDir, pattern, exclude = NULL, index = 1, recursive = TRUE) {
  files = dir(path = inputDir, patt = "\\.nc4$", recursive = recursive, full.names = TRUE)
  files = grep(pattern, files, value = TRUE)
  
  if (!is.null(exclude)) {
    files = grep(exclude, files, value = TRUE, invert = TRUE)
  }
  
  if (length(files) == 0) {
    stop("No files found matching the pattern: ", pattern)
  }
  
  if (index > length(files)) {
    stop("Index ", index, " exceeds number of matched files: ", length(files))
  }
  
  return(files[index])
}

#mean temperatures
mean_temperatures = function(obs, sim, sim_corr) {
  mean_global= mean(obs, na.rm=TRUE) #mean temperature across all time serie
  
  mean_time_obs = mean(obs, by="time")  #mean temperature of observations for every time step (here every month)
  mean_time_sim = mean(sim, by="time")    #mean temperature of simulations for every time step (here every month)
  mean_time_simcorr = mean(sim_corr, by="time")    #mean temperature of corrected simulations for every time step (here every month)
  
  mean_space_obs = mean(obs, by="space")   #mean of temperature for every grid cell
  mean_space_sim = mean(sim, by = "space")
  mean_space_simcorr = mean(sim_corr, by = "space")
  
  return(list(
    mean_global = mean_global,
    mean_time_obs = mean_time_obs,
    mean_time_sim = mean_time_sim,
    mean_time_simcorr = mean_time_simcorr,
    mean_space_obs = mean_space_obs,
    mean_space_sim = mean_space_sim,
    mean_space_simcorr = mean_space_simcorr
  ))
}

#Add month and seasons to an ncdf file (turned into a data.frame)
#' Add month and season factors to an ncdf object (as data.frame)
#'
#' @param ncdf An object of class `gts` or similar containing a `time` column.
#'
#' @return A data.frame with added month, month_fact and season columns
#' @export

month_season_factor = function(ncdf) {
  # Get the name of the input object to dynamically create a new variable name
  ncdf_name = deparse(substitute(ncdf))  # e.g., "obs"
  new_ncdf_name = paste0(ncdf_name, "_seasonality")     # e.g., "obs_df"
  
  # Melt the NetCDF-like object and add month/season factors
  ncdf_seasonality = gts::melt(ncdf) %>%
    dplyr::mutate(
      month = as.integer(format(time, "%m")),  #numeric
      month_fact = factor(month, levels = 1:12, labels = month.abb),
      
      season = dplyr::case_when(  # numeric
        month %in% c(1, 2, 3) ~ 1,    # Summer
        month %in% c(4, 5, 6) ~ 2,    # Autumn
        month %in% c(7, 8, 9) ~ 3,    # Winter
        month %in% c(10, 11, 12) ~ 4  # Spring
      ),
      #season factor 
      season_fact = factor(season, levels= 1:4, labels = c("summer", "autumn", "winter", "spring")
      )
    )
  
  # Assign the new dataframe to the global environment with the dynamic name
  assign(new_ncdf_name, ncdf_seasonality, envir = .GlobalEnv)
}

compute_quantiles = function(ncdf_seasonality, varname = "sst", from = 0, to = 1, by = 0.05) {
  # Get the original object name
  ncdf_name = sub("_seasonality$", "", deparse(substitute(ncdf_seasonality)))
  
  # Create dynamic output column name
  output_name = paste0(varname)
  
  # Define quantile probabilities and names
  probs = seq(from, to, by)
  quantile_names = formatC(probs, format = "f", digits = 2)
  
  # Dynamically refer to the variable (sst or to)
  value_col = rlang::sym(varname)
  
  # Quantiles computation by month
  qt_month = ncdf_seasonality %>%
    group_by(longitude, latitude, month, month_fact) %>%
    summarise(
      q = list(setNames(quantile(!!value_col, probs = probs, na.rm = TRUE), quantile_names)),
      .groups = "drop"
    ) %>%
    tidyr::unnest_longer(q, values_to = output_name, indices_to = "quantile")
  
  # Quantiles computation by season
  qt_season = ncdf_seasonality %>%
    group_by(longitude, latitude, season, season_fact) %>%
    summarise(
      q = list(setNames(quantile(!!value_col, probs = probs, na.rm = TRUE), quantile_names)),
      .groups = "drop"
    ) %>%
    tidyr::unnest_longer(q, values_to =output_name, indices_to = "quantile")
  
  # Assign both results to the global environment
  assign(paste0("qt_month_", ncdf_name), qt_month, envir = .GlobalEnv)
  assign(paste0("qt_season_", ncdf_name), qt_season, envir = .GlobalEnv)
}


getPredFiles <- function(model_name, path, scenarios = c("rcp26", "rcp85"),
                         model_id = "ipsl-cm5a-lr",
                         hist_period = "195001_200512",
                         future_period = "200601_210012") {
  
  # Historical file path
  hist_file <- file.path(path, model_name, 
                         paste0(model_id, "_historical_to_zs_monthly_", hist_period, ".nc4"))
  
  # Future scenario files paths
  future_files <- sapply(scenarios, function(sc) {
    file.path(path, model_name,
              paste0(model_id, "_", sc, "_to_zs_humboldt-n_monthly_", future_period, ".nc4"))
  }, USE.NAMES = TRUE)
  
  return(list(historical = hist_file, future = future_files))
}

getPredData = function(model_name, path, quantileDir, 
                       scenarios = c("rcp26", "rcp85"),
                       model_id = "ipsl-cm5a-lr",
                       hist_period = "195001_200512",
                       future_period = "200601_210012") {
  
  files = getPredFiles(model_name, path, 
                       scenarios = scenarios, 
                       model_id = model_id, 
                       hist_period = hist_period, 
                       future_period = future_period)
  
  output = list()
  
  # Load historical
  nc = nc_open(files$historical)
  output$lon = ncvar_get(nc, "lon")
  output$lat = ncvar_get(nc, "lat")
  output$historical = ncvar_get(nc)
  nc_close(nc)
  
  # Load future (as list of arrays by scenario)
  output$future = lapply(files$future, function(futfile) {
    nc = nc_open(futfile)
    val = ncvar_get(nc)
    nc_close(nc)
    return(val)
  })
  
  # Load quantile data from .rds file
  qfile = file.path(quantileDir, paste0(model_name, ".rds"))
  if (!file.exists(qfile)) {
    stop("Quantile file not found: ", qfile)
  }
  
  quant_table = readRDS(qfile)
  
  # Try to extract just the quantile matrix/vector
  if ("quantile" %in% names(quant_table)) {
    output$quantile = quant_table$quantile
  } else {
    output$quantile = quant_table  # fallback if it's already just the array
  }
  
  dimnames(output$quantile) = NULL
  
  # Optional: addExtremes if needed
  output = addExtremes(output)
  
  return(output)
}


# Function to compute quantiles from gts object and format
compute_quantiles2 <- function(all_dwsc, probs = seq(0, 1, by=0.05)) {
  quant_list <- list()  # initialize before loop
  
  for (sc_name in names(all_dwsc)) {
    qtab <- melt(quantile(all_dwsc[[sc_name]], probs = probs, na.rm = TRUE))
    names(qtab)[1:4] <- c("lon", "lat", "prob", paste0("sst_", sc_name))
    quant_list[[sc_name]] <- qtab
  }
  
  quant_merged <- Reduce(function(x, y) merge(x, y, by = c("lon", "lat", "prob"), all = TRUE), quant_list)
  return(quant_merged)
}


addExtremes = function(output, probs=c(0.05, 0.95)) {
  
  probs = unique(round(probs, 2))
  # min
  output$min = list()
  output$min$historical = if(!is.null(output$historical)) apply(output$historical, 1:2, min) else NULL
  output$min$future     = if(!is.null(output$future)) apply(output$future,     1:2, min) else NULL
  output$min$range = suppressWarnings(range(pretty(unlist(output$min))))
  # mean
  output$mean = list()
  output$mean$historical = if(!is.null(output$historical)) apply(output$historical, 1:2, mean) else NULL
  output$mean$future     = if(!is.null(output$future)) apply(output$future,     1:2, mean) else NULL
  output$mean$range = suppressWarnings(range(pretty(unlist(output$mean))))
  # max
  output$max = list()
  output$max$historical = if(!is.null(output$historical)) apply(output$historical, 1:2, max) else NULL
  output$max$future     = if(!is.null(output$future)) apply(output$future,     1:2, max) else NULL
  output$max$range = suppressWarnings(range(pretty(unlist(output$max))))
  
  if(!is.null(probs)) {
    
    for(i in probs) {
      iName = sprintf("p%02.0f", 100*i)
      output[[iName]] = list()
      output[[iName]]$historical = if(!is.null(output$historical)) apply(output$historical, 1:2, quantile, probs=i, na.rm=TRUE) else NULL
      output[[iName]]$future     = if(!is.null(output$future)) apply(output$future, 1:2, quantile, probs=i, na.rm=TRUE) else NULL
      output[[iName]]$range = suppressWarnings(range(pretty(unlist(output[[iName]]))))
    }
    
  }
  class(output) = c("downscaling.results", class(output))
  
  return(output)
}



# Function to assign indicator names
get_indicator_names <- function() {
  names <- character(30)
  
  # A block: indicators 6 to 11 → A1 to A6
  names[1:12] <- paste0("A", 1:12)
  
  # B block: indicators 13 to 15 → B1 to B3
  names[13:18] <- paste0("B", 1:6)
  
  # C block: all others (used ones only), get names C1, C2, ...
  names[20:27] <- paste0("C", 1:8)
  
  return(names)
}


add_aux_vars = function(data, shelf_data, bathy_data, 
                        lon_var_data = "lon", lat_var_data = "lat",
                        lon_var_cov = "longitude", lat_var_cov = "latitude",
                        shelf_value = "shelf", bathy_value = "depth") {
  
  data_coords = unique(data[c(lon_var_data, lat_var_data)])
  shelf_data  = merge(data_coords, shelf_data, by.x = c(lon_var_data, lat_var_data), by.y = c(lon_var_cov, lat_var_cov), all.x = TRUE)
  bathy_data  = merge(data_coords, bathy_data, by.x = c(lon_var_data, lat_var_data), by.y = c(lon_var_cov, lat_var_cov), all.x = TRUE)
  
  reps = nrow(data) / nrow(data_coords)
  if (!isTRUE(all.equal(reps, round(reps)))) stop("Mismatch between data and coordinates.")
  
  shelf_repeated = shelf_data[[shelf_value]][rep(1:nrow(shelf_data), each = reps)]
  bathy_repeated = bathy_data[[bathy_value]][rep(1:nrow(bathy_data), each = reps)]
  
  slog = function(x) sign(x) * log(abs(x))
  
  data$shelf  = shelf_repeated
  data$lshelf = slog(shelf_repeated)
  data$bathy  = bathy_repeated
  data$lbathy = slog(bathy_repeated)
  
  return(data)
}



# -----------------------------
# GLOBAL ATTRIBUTES FUNCTION
# -----------------------------
make_global_attributes <- function(model_name, scenario, temporal_range) {
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

fix_melted_names <- function(df, value_name = "value") {
  names(df)[1:4] <- c("lon", "lat", "prob", value_name)
  df
}

