# prepare_env_objects.R
library(gts)
library(dplyr)

source("functions.R")

# Directories
inputDir = "input"
quantileDir = "quantiles"
scenarios = c("rcp26", "rcp45", "rcp60", "rcp85")

# ---- Load OBS and HISTORICAL ESM ----
obs = read_gts(file.path(inputDir, "avhrr-only-v2-humboldt-n.198109-201706.nc4"))
longitude(obs) = longitude(obs, "left")
mask(obs) = mask(obs$x)

esm_hist = read_gts(file.path(inputDir, "ipsl-cm5a-lr_historical_to_zs_monthly_195001_200512.nc4"))
longitude(esm_hist) = longitude(esm_hist, "left")
simh = subset(esm_hist, grid = obs, expand = 5)
simh = regrid(simh, grid = obs, method = "nearest")
simh = simh - 273.15

# ---- Compute Historical Quantiles ----
probs = seq(0, 1, by = 0.05)
qt_hist = melt(quantile(simh, probs = probs, na.rm = TRUE)) %>%
  fix_melted_names("sst_sim") %>%
  arrange(lon, lat, prob)

# ---- Initialize full quantile table ----
nenv = qt_hist  # Contains lon, lat, prob, sst_obs

# ---- Loop over RCP scenarios ----
for (scen in scenarios) {
  scen_file = select_nc_file(inputDir, pattern = scen)
  
  # Read and align
  esm_raw = read_gts(scen_file)
  longitude(esm_raw) = longitude(esm_raw, "left")
  sim = subset(esm_raw, grid = obs, expand = 5)
  sim = regrid(sim, grid = obs, method = "nearest")
  sim = sim - 273.15
  
  fsimhist <- simh
  fsimhistm <- melt(simh)
  names(fsimhistm)[1:4] <- c("lon", "lat","time","sst_sim")
  
  fsimhist_seasonality <- month_season_factor(simh)
  fsimhist_seasonality_m <- fsimhist_seasonality
  names(fsimhist_seasonality_m)[1:4] <- c("lon", "lat","time","sst_sim")
  
  
  # Save processed future GTS object for prediction
  assign(paste0("fsim", substr(scen, 4, 5)), sim)  # e.g. fsim26 = sim
  
  # Also generate version with seasonality
  sim_season = month_season_factor(sim)
  assign(paste0("fsim", substr(scen, 4, 5), "_seasonality"), sim_season)
  
  # Melt both versions for model prediction
  sim_melt = melt(sim)
  names(sim_melt)[1:4] = c("lon", "lat","time","sst_sim")
  assign(paste0("fsim", substr(scen, 4, 5), "m"), sim_melt)
  
  names(sim_season)[1:4] = c("lon", "lat","time","sst_sim")
  assign(paste0("fsim", substr(scen, 4, 5), "_seasonality_m"), sim_season)
  
  # Compute quantiles
  qt_future = melt(quantile(sim, probs = probs, na.rm = TRUE)) %>%
    fix_melted_names() %>%
    arrange(lon, lat, prob)
  
  # Sanity check
  stopifnot(nrow(qt_future) == nrow(nenv))
  
  # Add scenario column safely
  scen_col = paste0("sst_", scen)
  nenv[[scen_col]] = qt_future$value
  
  # Save esm structure for that scenario
  dims_sim = list(lon = unique(sim$longitude), lat = unique(sim$latitude), time = unique(sim$time))
  xsim = sim$x
  dim(xsim) = sapply(dims_sim, length)
  
  dims_hist = list(lon = unique(simh$longitude), lat = unique(simh$latitude), time = unique(simh$time))
  xhist = simh$x
  dim(xhist) = sapply(dims_hist, length)
  
  esm = list(
    lon = dims_hist$lon,
    lat = dims_hist$lat,
    historical = xhist,
    future = xsim,
    quantile = qt_future$value
  )
  
  saveRDS(esm, file = file.path(quantileDir, paste0("esm_", scen, ".rds")))
}

# ---- Save nenv and obs (once at the end) ----
saveRDS(nenv, file = file.path(quantileDir, "nenv.rds"))

dims_obs = list(lon = unique(obs$longitude), lat = unique(obs$latitude), time = unique(obs$time))
xobs = obs$x
dim(xobs) = sapply(dims_obs, length)

obs_ = list(
  lon = dims_obs$lon,
  lat = dims_obs$lat,
  historical = xobs,
  future = NULL,
  quantile = qt_hist$sst_obs
)

saveRDS(obs_, file = file.path(quantileDir, "obs.rds"))


# Vector of all object names to update
fsim_names <- c(
  "fsim26m", "fsim26_seasonality_m",
 "fsim45m", "fsim45_seasonality_m",
 "fsim60m", "fsim60_seasonality_m",
  "fsim85m", "fsim85_seasonality_m",
  "fsimhistm", "fsimhist_seasonality_m"
)

# Apply the aux variables and slog transform
for (name in fsim_names) {
  dat <- get(name)
  dat <- add_aux_vars(dat, dfshelf, dfbath)
  # dat <- dat %>% mutate(lshelf = slog(shelf), lbathy = slog(bathy))
  assign(name, dat)
}


# ---- Optional: Save all fsimXX and melted versions for future loading ----
# Save to separate .RData

save(fsim26, fsim26_seasonality, fsim26m, fsim26_seasonality_m,
     fsim45, fsim45_seasonality, fsim45m, fsim45_seasonality_m,
     fsim60, fsim60_seasonality, fsim60m, fsim60_seasonality_m,
     fsim85, fsim85_seasonality, fsim85m, fsim85_seasonality_m,
     fsimhist, fsimhistm, fsimhist_seasonality, fsimhist_seasonality_m,
     file = file.path(quantileDir, "fsim_all.RData"))
