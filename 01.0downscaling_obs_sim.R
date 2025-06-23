# ─────────────────────────────────────
# 1. SETUP
# ─────────────────────────────────────

library(gts)
library(nctools)
library(dplyr)

# Source custom functions
source("_align.R")
source("functions.R")

# Set directories
inputDir = "input"
outputDir = "databases"
if (!dir.exists(outputDir)) dir.create(outputDir)


# ─────────────────────────────────────
# 2. LOAD AND PROCESS OBS & ESM FILES
# ─────────────────────────────────────

# Load files
esm_hist = select_nc_file(inputDir, pattern = "ipsl", exclude = "rcp")
obs_file = select_nc_file(inputDir, pattern = "humboldt", exclude = "rcp")

hesm = read_gts(esm_hist)
obs  = read_gts(obs_file)

# Harmonize longitude
longitude(obs)  = longitude(obs, "left")
longitude(hesm) = longitude(hesm, "left")

# Apply mask to obs
mask(obs) = mask(obs$x)

# Subset and align
sim     = subset(hesm, grid = obs)
aligned = align_time_period(sim, obs)
hsim    = aligned$sim
obs     = aligned$obs

# Regrid and convert units
hsim2 = regrid(hsim, grid = obs, method = "akima", extrap = TRUE)
hsim2 = hsim2 - 273.15

# Delta bias correction
clim_obs = climatology(obs)
clim_sim = climatology(hsim2)
sim_corr = hsim2 - clim_sim + clim_obs

# Compute non-seasonal quantiles
probs     = seq(0, 1, by = 0.05)
dat_hsim  = melt(quantile(hsim2, probs = probs))
dat_obs   = melt(quantile(obs,    probs = probs))
hist      = merge(dat_obs, dat_hsim, all = TRUE)


# ─────────────────────────────────────
# 3. ADD SEASONALITY AND COMPUTE QUANTILES
# ─────────────────────────────────────

month_season_factor(obs)
month_season_factor(hsim2)

compute_quantiles(obs_seasonality)
compute_quantiles(hsim2_seasonality, varname = "to")

hist_month  = merge(qt_month_obs, qt_month_hsim2, all = TRUE)
hist_season = merge(qt_season_obs, qt_season_hsim2, all = TRUE)

# Add season factor
hist_month = hist_month %>%
  mutate(season_fact = factor(
    case_when(
      month %in% 1:3   ~ "summer",
      month %in% 4:6   ~ "autumn",
      month %in% 7:9   ~ "winter",
      month %in% 10:12 ~ "spring"
    ),
    levels = c("summer", "autumn", "winter", "spring")
  ))


# ─────────────────────────────────────
# 4. PROCESS COVARIATES (BATHYMETRY, SHELF)
# ─────────────────────────────────────

# Bathymetry
bath = read_static("bathymetry/ETOPO1_Bed_g_gdal.nc", varid = "z")
names(bath)[names(bath) == "z"] = "depth"
xbath = bath
longitude(xbath) = longitude(xbath, "left")
xbath = subset(xbath, grid = obs)
xbath = regrid(xbath, grid = obs, method = "nearest")
dfbath = melt(xbath)
# names(dfbath)[1:2] = c("lon", "lat")

# Shelf
shelf = read_static("shelfbreak_200-1.0.0/shelfbreak_200.nc", varid = "shelf")
xshelf = shelf
longitude(xshelf) = longitude(xshelf, "left")
xshelf = regrid(xshelf, grid = obs, method = "nearest")
dfshelf = melt(xshelf)
# names(dfshelf)[1:2] = c("lon", "lat")


# Add covariates to datasets
hist         = add_aux_vars(hist, dfshelf, dfbath)
hist_month   = add_aux_vars(hist_month, dfshelf, dfbath)
hist_season  = add_aux_vars(hist_season, dfshelf, dfbath)


# ─────────────────────────────────────
# 5. FINAL CLEANING AND SAVING
# ─────────────────────────────────────

# Rename columns
names(hist)[1:2]        = c("lon", "lat")
names(hist_month)[1:2]  = c("lon", "lat")
names(hist_season)[1:2] = c("lon", "lat")
names(hist)[6]          = "sst_sim"
names(hist_month)[7]    = "sst_sim"
names(hist_season)[7]   = "sst_sim"

# Subset for test (1/24)
set.seed(123)
hist_small        = sample_frac(hist,        1/24)
hist_month_small  = sample_frac(hist_month,  1/24)
hist_season_small = sample_frac(hist_season, 1/24)

# Save all
saveRDS(hist,              file = file.path(outputDir, "hist.rds"))
saveRDS(hist_month,        file = file.path(outputDir, "hist_month.rds"))
saveRDS(hist_season,       file = file.path(outputDir, "hist_season.rds"))
saveRDS(hist_small,        file = file.path(outputDir, "hist_small.rds"))
saveRDS(hist_month_small,  file = file.path(outputDir, "hist_month_small.rds"))
saveRDS(hist_season_small, file = file.path(outputDir, "hist_season_small.rds"))

