# Load all .databases into a named list
data_dir <- "databases"
data_base <- list.files(data_dir, pattern = "\\.rds$", full.names = TRUE)
data_b <- lapply(data_base, readRDS)
names(data_b) <- tools::file_path_sans_ext(basename(data_base))

# Create separate objects in the environment
list2env(data_b, envir = .GlobalEnv)


# ─────────────────────────────────────
# 1. SETUP
# ─────────────────────────────────────

#Required libraries
library(mgcv)
library(scam)

#Set output directory and domain 
outputDir = "models_season" 
# domain = "test"
if (!dir.exists(outputDir)) dir.create(outputDir)

#Filename formatting function 
nameCode = "%s.rds"  # e.g., m0.0_global.rds


# ─────────────────────────────────────
# 2. Models 
# ─────────────────────────────────────

# ==== Null model ====

modelName = "mods0.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ offset(sst_sim), data = hist_season_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}


# ==== Global linear models ====
# ==== Model m0.1 ====
modelName = "mods0.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ offset(sst_sim) + ti(lon, k=20) + ti(lat, k=30) + ti(lon, lat, k=15), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.2 ====
modelName = "mods0.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lon, k=20) + ti(lat, k=30) + ti(lon, lat, k=15), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.3 ====
modelName = "mods0.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lon, k=20) + ti(lat, k=30) + ti(season, k=4) + ti(lon, lat, season, k=4), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Global non-linear models ====
# ==== Model m1.0 ====
modelName = "mods1.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ s(sst_sim) + season_fact + ti(lon, k=20) + ti(lat, k=30) + ti(season, k=4) + ti(lon, lat, season, k=4), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# # ==== Model m1.1 ====
# modelName = "mods1.1"
# modFile = sprintf(nameCode, modelName)
# if (!file.exists(file.path(outputDir, modFile))) {
#   mod = scam(sst ~ s(sst_sim, bs="mpi", k=10) + s(season, bs="cc", k=4) + ti(lon, k=20) + ti(lat, k=25) + ti(season, k=4) + ti(lon, lat, season, k=4), data = hist_season_small)
#   saveRDS(mod, file = file.path(outputDir, modFile))
# }
# 
# # ==== Model m1.2 ====
# modelName = "mods1.2"
# modFile = sprintf(nameCode, modelName)
# if (!file.exists(file.path(outputDir, modFile))) {
#   mod = scam(sst ~ s(sst_sim, bs="micx", k=10) + s(season, bs="cc", k=4) + ti(season, k=4) + ti(lon, lat, season, k=4), data = hist_season_small)
#   saveRDS(mod, file = file.path(outputDir, modFile))
# }
# 
# # ==== Model m1.2 ====
# modelName = "mods1.3"
# modFile = sprintf(nameCode, modelName)
# if (!file.exists(file.path(outputDir, modFile))) {
#   mod = scam(sst ~ s(sst_sim, bs="micv", k=10) + s(season, bs="cc", k=4) + ti(lon, k=20) + ti(lat, k=25) + ti(season, k=4) + ti(lon, lat, season, k=4), data = hist_season_small)
#   saveRDS(mod, file = file.path(outputDir, modFile))
# }

# ==== Separate smooths for each season ====
# ==== Model m1.4 ====
modelName = "mods1.4"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ s(sst_sim, by=season_fact) + ti(lon, k=20) + ti(lat, k=25) + ti(season, k=4) + ti(lon, lat, season, k=4), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m1.5 ====
modelName = "mods1.5"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ s(sst_sim, by=season_fact) + season_fact + ti(lon, k=20) + ti(lat, k=25) + ti(season, k=4) + ti(lon, lat, season, k=4), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Interaction models ====
# ==== Model m2.0 ====
modelName = "mods2.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ s(season, by=sst_sim, k=4) + ti(lon, k=20) + ti(lat, k=30) + ti(season, k=4) + ti(lon, lat, season, k=4), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m2.1 ====
modelName = "mods2.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + s(season, by=sst_sim, k=4) + ti(lon, k=20) + ti(lat, k=30) + ti(season, k=4) + ti(lon, lat, season, k=4), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# COVARIATES 

# ==== Null model ====

# ==== Global linear models ====
# ==== Model m0.1 ====
modelName = "mods_cov0.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ offset(sst_sim) + ti(lon, k=20) + ti(lat, k=30) + ti(lon, lat, k=15)+ s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.2 ====
modelName = "mods_cov0.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lon, k=20) + ti(lat, k=30) + ti(lon, lat, k=15)+ s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.3 ====
modelName = "mods_cov0.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lon, k=20) + ti(lat, k=30) + ti(season, k=4) + ti(lon, lat, season, k=4)+ s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Global non-linear models ====
# ==== Model m1.0 ====
modelName = "mods_cov1.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ s(sst_sim) + season_fact + ti(lon, k=20) + ti(lat, k=30) + ti(season, k=4) + ti(lon, lat, season, k=4)+ s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# # ==== Model m1.1 ====
# modelName = "mods_cov1.1"
# modFile = sprintf(nameCode, modelName)
# if (!file.exists(file.path(outputDir, modFile))) {
#   mod = scam(sst ~ s(sst_sim, bs="mpi", k=10) + s(season, bs="cc", k=4) + ti(lon, k=20) + ti(lat, k=25) + ti(season, k=4) + ti(lon, lat, season, k=4), data = hist_season_small)
#   saveRDS(mod, file = file.path(outputDir, modFile))
# }
# 
# # ==== Model m1.2 ====
# modelName = "mods_cov1.2"
# modFile = sprintf(nameCode, modelName)
# if (!file.exists(file.path(outputDir, modFile))) {
#   mod = scam(sst ~ s(sst_sim, bs="micx", k=10) + s(season, bs="cc", k=4) + ti(season, k=4) + ti(lon, lat, season, k=4), data = hist_season_small)
#   saveRDS(mod, file = file.path(outputDir, modFile))
# }
# 
# # ==== Model m1.2 ====
# modelName = "mods_cov1.3"
# modFile = sprintf(nameCode, modelName)
# if (!file.exists(file.path(outputDir, modFile))) {
#   mod = scam(sst ~ s(sst_sim, bs="micv", k=10) + s(season, bs="cc", k=4) + ti(lon, k=20) + ti(lat, k=25) + ti(season, k=4) + ti(lon, lat, season, k=4), data = hist_season_small)
#   saveRDS(mod, file = file.path(outputDir, modFile))
# }

# ==== Separate smooths for each season ====
# ==== Model m1.4 ====
modelName = "mods_cov1.4"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ s(sst_sim, by=season_fact) + ti(lon, k=20) + ti(lat, k=25) + ti(season, k=4) + ti(lon, lat, season, k=4)+ s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m1.5 ====
modelName = "mods_cov1.5"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ s(sst_sim, by=season_fact) + season_fact + ti(lon, k=20) + ti(lat, k=25) + ti(season, k=4) + ti(lon, lat, season, k=4)+ s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Interaction models ====
# ==== Model m2.0 ====
modelName = "mods_cov2.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ s(season, by=sst_sim, k=4) + ti(lon, k=20) + ti(lat, k=30) + ti(season, k=4) + ti(lon, lat, season, k=4)+ s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m2.1 ====
modelName = "mods_cov2.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + s(season, by=sst_sim, k=4) + ti(lon, k=20) + ti(lat, k=30) + ti(season, k=4) + ti(lon, lat, season, k=4)+ s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), data = hist_season_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}