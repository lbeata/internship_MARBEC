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


getPredFiles <- function(model_name, path, 
                         scenarios = c("rcp26", "rcp85"),
                         model_id = "ipsl-cm5a-lr") {
  
  model_dir <- file.path(path, model_name)
  
  # Match historical file (any date range)
  hist_pattern <- paste0("^", model_id, "_historical_to_zs_.*\\.nc4$")
  hist_file <- list.files(model_dir, pattern = hist_pattern, full.names = TRUE)
  hist_file <- if (length(hist_file) > 0) hist_file[1] else NULL
  
  # Match future scenario files (any date range)
  future_files <- sapply(scenarios, function(sc) {
    pattern <- paste0("^", model_id, "_", sc, "_to_zs_.*\\.nc4$")
    file <- list.files(model_dir, pattern = pattern, full.names = TRUE)
    if (length(file) > 0) file[1] else NULL
  }, USE.NAMES = TRUE)
  
  return(list(historical = hist_file, future = future_files))
}


getPredData = function(model_name, path, quantileDir, 
                       scenarios = c("rcp26", "rcp85"),
                       model_id = "ipsl-cm5a-lr") {
  
  # Get paths to prediction files
  files = getPredFiles(model_name, path, 
                       scenarios = scenarios, 
                       model_id = model_id)
  
  # Sanity check
  if (is.null(files$historical) || !file.exists(files$historical)) {
    stop("Missing historical file for model: ", model_name)
  }
  if (any(sapply(files$future, is.null) | !file.exists(unlist(files$future)))) {
    stop("Missing one or more future scenario files for model: ", model_name)
  }
  
  output = list()
  
  # Load historical NetCDF
  nc = nc_open(files$historical)
  output$lon = ncvar_get(nc, "lon")
  output$lat = ncvar_get(nc, "lat")
  output$historical = ncvar_get(nc)
  nc_close(nc)
  
  # Load future NetCDFs (returns list named by scenario)
  output$future = lapply(files$future, function(futfile) {
    nc = nc_open(futfile)
    val = ncvar_get(nc)
    nc_close(nc)
    return(val)
  })
  
  # Load quantile .rds
  qfile = file.path(quantileDir, paste0(model_name, ".rds"))
  if (!file.exists(qfile)) {
    stop("Quantile file not found: ", qfile)
  }
  
  quant_table = readRDS(qfile)
  output$quantile = if ("quantile" %in% names(quant_table)) quant_table$quantile else quant_table
  dimnames(output$quantile) = NULL  # remove dimnames for consistency
  
  # Add extremes (min/max)
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

addExtremes1 = function(output, probs=c(0.05, 0.95)) {
  
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

addExtremes2 = function(output, probs=c(0.05, 0.95)) {
  
  probs = unique(round(probs, 2))
  
  # Helper function to compute min/mean/max/quantile if possible
  compute_stat <- function(x, FUN) {
    if(is.null(x)) return(NULL)
    dims <- length(dim(x))
    if (dims < 3) {
      # If only 2D, just return the matrix (no time dimension to aggregate)
      return(x)
    } else {
      # Aggregate over time or third dimension
      return(apply(x, 1:2, FUN, na.rm=TRUE))
    }
  }
  
  # Min
  output$min = list()
  output$min$historical = compute_stat(output$historical, min)
  output$min$future     = compute_stat(output$future, min)
  output$min$range = suppressWarnings(range(pretty(unlist(output$min))))
  
  # Mean
  output$mean = list()
  output$mean$historical = compute_stat(output$historical, mean)
  output$mean$future     = compute_stat(output$future, mean)
  output$mean$range = suppressWarnings(range(pretty(unlist(output$mean))))
  
  # Max
  output$max = list()
  output$max$historical = compute_stat(output$historical, max)
  output$max$future     = compute_stat(output$future, max)
  output$max$range = suppressWarnings(range(pretty(unlist(output$max))))
  
  if(!is.null(probs)) {
    for(i in probs) {
      iName = sprintf("p%02.0f", 100*i)
      output[[iName]] = list()
      output[[iName]]$historical = if(!is.null(output$historical)) {
        dims <- length(dim(output$historical))
        if (dims < 3) {
          output$historical  # No quantiles if no time dim
        } else {
          apply(output$historical, 1:2, quantile, probs=i, na.rm=TRUE)
        }
      } else NULL
      
      output[[iName]]$future = if(!is.null(output$future)) {
        dims <- length(dim(output$future))
        if (dims < 3) {
          output$future
        } else {
          apply(output$future, 1:2, quantile, probs=i, na.rm=TRUE)
        }
      } else NULL
      
      output[[iName]]$range = suppressWarnings(range(pretty(unlist(output[[iName]]))))
    }
  }
  
  class(output) = c("downscaling.results", class(output))
  return(output)
}


addExtremes <- function(output, probs = c(0.05, 0.95)) {
  
  probs <- unique(round(probs, 2))
  
  # Helper function to compute stat if array is 3D, else return as-is
  compute_stat <- function(x, FUN) {
    if (is.null(x)) return(NULL)
    dims <- length(dim(x))
    if (dims < 3) {
      return(x)  # no aggregation if 2D
    } else {
      return(apply(x, 1:2, FUN, na.rm = TRUE))
    }
  }
  
  # Min
  output$min <- list(
    historical = compute_stat(output$historical, min),
    future = compute_stat(output$future, min)
  )
  output$min$range <- suppressWarnings(range(pretty(unlist(output$min))))
  
  # Mean
  output$mean <- list(
    historical = compute_stat(output$historical, mean),
    future = compute_stat(output$future, mean)
  )
  output$mean$range <- suppressWarnings(range(pretty(unlist(output$mean))))
  
  # Max
  output$max <- list(
    historical = compute_stat(output$historical, max),
    future = compute_stat(output$future, max)
  )
  output$max$range <- suppressWarnings(range(pretty(unlist(output$max))))
  
  # Quantiles
  if (!is.null(probs)) {
    for (i in probs) {
      iName <- sprintf("p%02.0f", 100 * i)
      output[[iName]] <- list()
      
      output[[iName]]$historical <- if (!is.null(output$historical)) {
        dims <- length(dim(output$historical))
        if (dims < 3) {
          output$historical  # keep as-is
        } else {
          apply(output$historical, 1:2, quantile, probs = i, na.rm = TRUE)
        }
      } else NULL
      
      output[[iName]]$future <- if (!is.null(output$future)) {
        dims <- length(dim(output$future))
        if (dims < 3) {
          output$future
        } else {
          apply(output$future, 1:2, quantile, probs = i, na.rm = TRUE)
        }
      } else NULL
      
      output[[iName]]$range <- suppressWarnings(range(pretty(unlist(output[[iName]]))))
    }
  }
  
  class(output) <- c("downscaling.results", class(output))
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
                        shelf_value = "shelf", bathy_value = "depth") {
  
  # --- Detect lon/lat column names in `data`
  if ("lon" %in% names(data) && "lat" %in% names(data)) {
    lon_data = "lon"
    lat_data = "lat"
  } else if ("longitude" %in% names(data) && "latitude" %in% names(data)) {
    lon_data = "longitude"
    lat_data = "latitude"
  } else {
    stop("Longitude/latitude columns not found in data.")
  }
  
  # --- Detect lon/lat column names in shelf/bathy data
  if ("lon" %in% names(shelf_data) && "lat" %in% names(shelf_data)) {
    lon_cov = "lon"
    lat_cov = "lat"
  } else if ("longitude" %in% names(shelf_data) && "latitude" %in% names(shelf_data)) {
    lon_cov = "longitude"
    lat_cov = "latitude"
  } else {
    stop("Longitude/latitude columns not found in covariate data.")
  }
  
  # --- Get unique coordinates
  data_coords = unique(data[c(lon_data, lat_data)])
  
  # --- Merge shelf and bathy
  shelf_data = merge(data_coords, shelf_data, 
                     by.x = c(lon_data, lat_data), 
                     by.y = c(lon_cov, lat_cov), 
                     all.x = TRUE)
  
  bathy_data = merge(data_coords, bathy_data, 
                     by.x = c(lon_data, lat_data), 
                     by.y = c(lon_cov, lat_cov), 
                     all.x = TRUE)
  
  # --- Repeat for matching full data size
  reps = nrow(data) / nrow(data_coords)
  if (!isTRUE(all.equal(reps, round(reps)))) stop("Mismatch between data and coordinates.")
  
  shelf_repeated = shelf_data[[shelf_value]][rep(1:nrow(shelf_data), each = reps)]
  bathy_repeated = bathy_data[[bathy_value]][rep(1:nrow(bathy_data), each = reps)]
  
  # --- Define slog and compute new variables
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


getPredDat <- function(model, rcp, inputDir = "outputs/downscaling", quantile_path = NULL,ext_fun = addextremes1) {
  model_path <- file.path(inputDir, model)
  cat("==> Processing model:", model, "with RCP:", rcp, "\n")
  cat("Looking in directory:", model_path, "\n")
  
  # Get .nc files
  all_files <- list.files(model_path, full.names = TRUE)
  hist_file <- all_files[grepl("historical", all_files) & grepl("\\.nc", all_files)]
  future_file <- all_files[grepl(rcp, all_files) & grepl("\\.nc", all_files)]
  
  if (length(hist_file) != 1) stop(paste("Problem with historical file for", model))
  if (length(future_file) != 1) stop(paste("Problem with future file for", model, rcp))
  
  histo <- read_gts(hist_file)
  future <- read_gts(future_file)
  
  # Load and extract quantiles
  if (is.null(quantile_path)) stop("Must provide quantile_path to load .rds file")
  quant_obj <- readRDS(quantile_path)
  
  if (!"sst_historical" %in% names(quant_obj)) stop("Quantile object lacks 'sst_historical'")
  
  quant_hist <- quant_obj[["sst_historical"]]
  
  # Build final list
  X <- list(
    lon = histo$lon,
    lat = histo$lat,
    historical = histo$x,
    future = future$x,
    quantile = quant_hist
  )
  
  #Verification
  cat("==> Inspecting data for model:", model, "\n")
  cat("Dimensions:\n")
  print("  historical:"); print(dim(histo$x))
  print("  future:"); print(dim(future$x))
  print("  quantile:"); print(dim(quant_hist))
  
  cat("Summary:\n")
  print(summary(histo$x))
  print(summary(future$x))
  print(summary(quant_hist))
  
  # Optional: check for NAs
  cat("NA counts:\n")
  cat("  historical:", sum(is.na(histo$x)), "\n")
  cat("  future:", sum(is.na(future$x)), "\n")
  cat("  quantile:", sum(is.na(quant_hist)), "\n")
  
  X <- ext_fun(X)  # Optional depending on your needs
  return(X)
}
