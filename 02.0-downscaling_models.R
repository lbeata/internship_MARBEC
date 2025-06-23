

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
outputDir = "models" 
# domain = "test"
if (!dir.exists(outputDir)) dir.create(outputDir)

#Filename formatting function 
nameCode = "%s.rds"  # e.g., m0.0_global.rds


# ─────────────────────────────────────
# 2. Models 
# ─────────────────────────────────────

# ==== Null model ====
modelName = "mod0.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ offset(sst_sim), data = hist_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Linear models ====
modelName = "mod0.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ offset(sst_sim) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "mod0.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "mod0.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + te(lon, lat, k = 15), data = hist_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Global nonlinear models ====
modelName = "mod1.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ s(sst_sim, bs = "ad", k = 40) + ti(lon, k = 20) + ti(lat, k = 25) + ti(lon, lat, k = 15), data = hist_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "mod1.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs = "mpi", k = 10) + ti(lon, k = 20) + ti(lat, k = 25) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "mod1.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs = "micx", k = 10) + ti(lon, k = 20) + ti(lat, k = 25) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "mod1.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs = "micv", k = 10) + ti(lon, k = 20) + ti(lat, k = 25) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 2.0 ----
modelName = "mod2.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ te(by = sst_sim, lon, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 2.1 ----
modelName = "mod2.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ te(by = sst_sim, lon, k = 20) + te(by = sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 2.2 ----
modelName = "mod2.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ sst_sim + sst_sim:lon + sst_sim:lat + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.0 ----
modelName = "mod3.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ s(sst_sim, bs = "ad", k = 40) + ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.1 ----
modelName = "mod3.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs = "mpi", k = 20) + ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.2 ----
modelName = "mod3.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs = "micx", k = 20) + ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.3 ----
modelName = "mod3.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs = "micv", k = 20) + ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.4 ----
modelName = "mod3.4"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.5 ----
modelName = "mod3.5"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ te(sst_sim, lon, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.6 ----
modelName = "mod3.6"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ te(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 4.0 ----
modelName = "mod4.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ te(by = sst_sim, lon, lat, k = 20) + s(sst_sim, bs = "ad", k = 40) + ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 4.1 ----
modelName = "mod4.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ te(by = sst_sim, lon, lat, k = 20) + ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 4.2 ----
modelName = "mod4.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ te(by = sst_sim, lon, lat, k = 20) + s(sst_sim, bs = "ad", k = 40) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 4.3 ----
modelName = "mod4.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = gam(sst ~ te(lon, lat, sst_sim, k = 10) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# COVARIATES MODELS 

# ==== Linear models ====
modelName = "modcov0.1a"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ offset(sst_sim) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15) , data = hist_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "modcov0.1b"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ offset(sst_sim) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lbathy, bs = "cr", k = 15) , data = hist_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "modcov0.1c"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ offset(sst_sim) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15) , data = hist_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "modcov0.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "modcov0.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + te(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ==== Global nonlinear models ====
modelName = "modcov1.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ s(sst_sim, bs = "ad", k = 40) + ti(lon, k = 20) + ti(lat, k = 25) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small, method = "REML")
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "modcov1.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs = "mpi", k = 10) + ti(lon, k = 20) + ti(lat, k = 25) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "modcov1.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs = "micx", k = 10) + ti(lon, k = 20) + ti(lat, k = 25) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

modelName = "modcov1.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs = "micv", k = 10) + ti(lon, k = 20) + ti(lat, k = 25) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 2.0 ----
modelName = "modcov2.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ te(by = sst_sim, lon, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 2.1 ----
modelName = "modcov2.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ te(by = sst_sim, lon, k = 20) + te(by = sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 2.2 ----
modelName = "modcov2.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ sst_sim + sst_sim:lon + sst_sim:lat + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.0 ----
modelName = "modcov3.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ s(sst_sim, bs = "ad", k = 40) + ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.1 ----
modelName = "modcov3.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs = "mpi", k = 20) + ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.2 ----
modelName = "modcov3.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs = "micx", k = 20) + ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.3 ----
modelName = "modcov3.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = scam(sst ~ s(sst_sim, bs = "micv", k = 20) + ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.4 ----
modelName = "modcov3.4"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.5 ----
modelName = "modcov3.5"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ te(sst_sim, lon, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 3.6 ----
modelName = "modcov3.6"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ te(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 4.0 ----
modelName = "modcov4.0"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ te(by = sst_sim, lon, lat, k = 20) + s(sst_sim, bs = "ad", k = 40) + ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 4.1 ----
modelName = "modcov4.1"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ te(by = sst_sim, lon, lat, k = 20) + ti(sst_sim, lon, k = 20) + ti(sst_sim, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 4.2 ----
modelName = "modcov4.2"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ te(by = sst_sim, lon, lat, k = 20) + s(sst_sim, bs = "ad", k = 40) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}

# ---- Model 4.3 ----
modelName = "modcov4.3"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ te(lon, lat, sst_sim, k = 10) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15) + s(lshelf, bs = "cr", k = 15)  + s(lbathy, bs = "cr", k = 15), data = hist_small)
  saveRDS(mod, file = file.path(outputDir, modFile))
}


start.time <- Sys.time()
modelName = "mod2.0bam"
modFile = sprintf(nameCode, modelName)
if (!file.exists(file.path(outputDir, modFile))) {
  mod = bam(sst ~ te(by = sst_sim, lon, lat, k = 20) + ti(lon, k = 20) + ti(lat, k = 30) + ti(lon, lat, k = 15), data = hist)
  saveRDS(mod, file = file.path(outputDir, modFile))
}
result <- sum(1:10000)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken