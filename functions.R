# Functions 

#mean temperatures
mean_temperatures <- function(obs, sim, sim_corr) {
  mean_global= mean(obs, na.rm=TRUE) #mean temperature across all time serie
  
  mean_time_obs = mean(obs, by="time")  #mean temperature of observations for every time step (here every month)
  mean_time_sim = mean(sim, by="time")    #mean temperature of simulations for every time step (here every month)
  mean_time_simcorr = mean(sim_corr, by="time")    #mean temperature of corrected simulations for every time step (here every month)
  
  mean_space_obs = mean(obs, by="space")   #mean of temperature for every grid cell
  mean_space_sim <- mean(sim, by = "space")
  mean_space_simcorr <- mean(sim_corr, by = "space")
  
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

month_season_factor <- function(ncdf) {
  # Get the name of the input object to dynamically create a new variable name
  ncdf_name <- deparse(substitute(ncdf))  # e.g., "obs"
  new_ncdf_name <- paste0(ncdf_name, "_seasonality")     # e.g., "obs_df"
  
  # Melt the NetCDF-like object and add month/season factors
  ncdf_seasonality <- gts::melt(ncdf) %>%
    dplyr::mutate(
      month = as.integer(format(time, "%m")),
      month_fact = factor(month, levels = 1:12, labels = month.abb),
      season = factor(
        dplyr::case_when(
          month %in% c(12, 1, 2) ~ "summer",
          month %in% c(3, 4, 5) ~ "autumn",
          month %in% c(6, 7, 8) ~ "winter",
          month %in% c(9, 10, 11) ~ "spring"
        ),
        levels = c("summer", "autumn", "winter", "spring")
      )
    )
  
  # Assign the new dataframe to the global environment with the dynamic name
  assign(new_ncdf_name, ncdf_seasonality, envir = .GlobalEnv)
}

compute_quantiles <- function (ncdf_seasonality, from=0, to =1, by = 0.05) {
#Get the original object name
  ncdf_name <- sub("_seasonality$", "", deparse(substitute(ncdf_seasonality)))
  
  #Define quantile probabilities and names
  probs <- seq(from, to, by)
  quantile_names <- formatC(probs, format = "f", digits = 2)
  
  # Quantiles computation by month 
  qt_month <- ncdf_seasonality %>%
    group_by(longitude, latitude, month_fact) %>%
    summarise(
      q = list(setNames(quantile(sst, probs = probs, na.rm = TRUE), quantile_names)),
      .groups = "drop"
    ) %>%
    tidyr::unnest_longer(q, values_to = "sst_quantile", indices_to = "quantile")
  
  #Quantiles computation by season 
  qt_season <- ncdf_seasonality %>%
    group_by(longitude, latitude, season) %>%
    summarise(
      q = list(setNames(quantile(sst, probs = probs, na.rm = TRUE), quantile_names)),
      .groups = "drop"
    ) %>%
    tidyr::unnest_longer(q, values_to = "sst_quantile", indices_to = "quantile")
  
  # Assign both results to the global environment
  assign(paste0("qt_month_", ncdf_name), qt_month, envir = .GlobalEnv)
  assign(paste0("qt_season_", ncdf_name), qt_season, envir = .GlobalEnv)
}


