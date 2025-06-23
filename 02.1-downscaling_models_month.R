# Load all .databases into a named list
data_dir <- "databases"
data_base <- list.files(data_dir, pattern = "\\.rds$", full.names = TRUE)
data_b <- lapply(data_base, readRDS)
names(data_b) <- tools::file_path_sans_ext(basename(data_base))

# Create separate objects in the environment
list2env(data_b, envir = .GlobalEnv)

hist_month_small$month = as.numeric(hist_month_small$month)
# ─────────────────────────────────────
# 1. SETUP
# ─────────────────────────────────────

#Required libraries
library(mgcv)
library(scam)

#Set output directory and domain 
outputDir = "models_month" 
# domain = "test"
if (!dir.exists(outputDir)) dir.create(outputDir)

#Filename formatting function 
nameCode = "%s.rds"  # e.g., m0.0_global.rds


# ─────────────────────────────────────
# 2. Models 
# ─────────────────────────────────────

# ==== Null model ====
modelName = "modm0.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ offset(sst_sim), data = hist_month_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.1 ====
modelName = "modm0.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ offset(sst_sim) + ti(lat, k = 30) + ti(lon, k = 20) + ti(lon, lat, k = 15), 
            data = hist_month_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.2 ====
modelName = "modm0.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lat, k = 30) + ti(lon, k = 20) + ti(lon, lat, k = 15), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.3 ====
modelName = "modm0.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lat, k = 30) + ti(lon, k = 20) + ti(lon, lat, k = 15) + month_fact, 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.3b ====
modelName = "modm0.3b"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lat, k = 30) + ti(lon, k = 20) + ti(lon, lat, k = 15) + season_fact, 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4 ====
modelName = "modm0.4"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lon, lat, k = 15) + ti(month) + ti(lat, month) + ti(lon, month), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4a ====
modelName = "modm0.4a"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lon, lat, k = 15) + month_fact + s(lat, by = month_fact) + s(lon, by = month_fact), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4b ====
modelName = "modm0.4b"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lon, lat, k = 15) + ti(month) + s(lat, by = season_fact) + ti(lon, month), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4c ====
modelName = "modm0.4c"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lon, lat, k = 15) + month_fact + s(lat, by = month_fact) + s(lon, by = season_fact, k = 4), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4d ====
modelName = "modm0.4d"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lon, lat, k = 15) + month_fact + season_fact + s(lat, by = season_fact) + s(lon, by = month_fact, k = 4), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4f ====
modelName = "modm0.4f"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lon, lat, k = 15) + season_fact + s(lat, by = season_fact) + s(lon, by = season_fact), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4e ====
modelName = "modm0.4e"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lon, lat, k = 15) + month_fact + s(lat, by = month_fact) + 
              s(lon, by = month_fact) + s(lon, lat, by = month_fact), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4g ====
modelName = "modm0.4g"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lon, lat, k = 15) + month_fact + s(lon, lat, by = month_fact), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "modm1.1a"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs="mpi", k=10) + s(month, bs="cc", k=12) + ti(lon, k=20) + ti(lat, k=25) + ti(lon, lat, month,bs = c("ps", "ps", "cc"), k = c(15, 15, 12)), data = hist_month_small)
  # mod = scam(sst ~
  #              s(sst_sim, bs = "mpi", k = 10) +
  #              s(month, bs = "cc", k = 12) +
  #              s(lon, k = 20) +
  #              s(lat, k = 25) +
  #              s(month, bs = "cc", k = 12),
  #            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}



# ==== Model m1.2 ====
modelName = "modm1.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs="micx", k=10) + s(month, bs="cc", k=12) + ti(month,bs = "cc", k=12) + ti(lon, lat, month,bs = c("tp", "tp", "cc"), k = c(10, 10, 12)), data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m1.2 ====
modelName = "modm1.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs="micv", k=10) + s(month, bs="cc", k=12) + ti(lon, k=20) + ti(lat, k=25) + ti(month, k=12) + ti(lon, lat, month, k=12), data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}



# COVARIATES MODELS 

# ==== Model m0.1 ====
modelName = "modm_cov0.1a"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam (sst ~ offset(sst_sim) + ti(lat, k = 30) + ti(lon, k = 20) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15) , 
            data = hist_month_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "modm_cov0.1b"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ offset(sst_sim) + ti(lat, k = 30) + ti(lon, k = 20) + ti(lon, lat, k = 15) + s(lbathy, bs = "cr", k = 15) , 
            data = hist_month_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "modm_cov0.1c"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ offset(sst_sim) + ti(lat, k = 30) + ti(lon, k = 20) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), 
            data = hist_month_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.2 ====
modelName = "modm_cov0.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lat, k = 30) + ti(lon, k = 20) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.3 ====
modelName = "modm_cov0.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lat, k = 30) + ti(lon, k = 20) + ti(lon, lat, k = 15) + month_fact + s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.3b ====
modelName = "modm_cov0.3b"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lat, k = 30) + ti(lon, k = 20) + ti(lon, lat, k = 15) + season_fact + s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4 ====
modelName = "modm_cov0.4"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lon, lat, k = 15) + ti(month) + ti(lat, month) + ti(lon, month) + s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4a ====
modelName = "modm_cov0.4a"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lon, lat, k = 15) + month_fact + s(lat, by = month_fact) + s(lon, by = month_fact) + s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4b ====
modelName = "modm_cov0.4b"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lon, lat, k = 15) + ti(month) + s(lat, by = season_fact) + ti(lon, month) + s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4c ====
modelName = "modm_cov0.4c"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lon, lat, k = 15) + month_fact + s(lat, by = month_fact) + s(lon, by = season_fact, k = 4) + s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4d ====
modelName = "modm_cov0.4d"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lon, lat, k = 15) + month_fact + season_fact + s(lat, by = season_fact) + s(lon, by = month_fact, k = 4) + s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4f ====
modelName = "modm_cov0.4f"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lon, lat, k = 15) + season_fact + s(lat, by = season_fact) + s(lon, by = season_fact) + s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4e ====
modelName = "modm_cov0.4e"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lon, lat, k = 15) + month_fact + s(lat, by = month_fact) + 
              s(lon, by = month_fact) + s(lon, lat, by = month_fact) + s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Model m0.4g ====
modelName = "modm_cov0.4g"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lon, lat, k = 15) + month_fact + s(lon, lat, by = month_fact) + s(lshelf, bs = "cr", k = 15) + s(lbathy, bs = "cr", k = 15), 
            data = hist_month_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}