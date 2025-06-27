# ─────────────────────────────────────
# 1. SETUP
# ─────────────────────────────────────
library(gts)
library(dplyr)
library(mgcv)
library(stringr)

# Load custom functions
source("_align.R")
source("functions.R")

# Define directories
modelDir    <- "models"
quantileDir <- "quantiles"
inputDir    <- "outputs/downscaling"
outputDir   <- "outputs/indicators"
figDir      <- "outputs/figures"

# Create output directories if needed
dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
dir.create(figDir, showWarnings = FALSE, recursive = TRUE)

# ─────────────────────────────────────
# 2. LOAD STATIC DATA
# ─────────────────────────────────────

# Load observed SST quantiles and apply extreme quantiles
obs_ <- readRDS(file.path(quantileDir, "obs.rds")) |> addExtremes()

# Load environmental covariates (used for thresholding)
nenv <- readRDS(file.path(quantileDir, "nenv.rds"))

# Load ESM quantiles for each RCP and apply extremes
rcp_scenarios <- c("rcp26", "rcp85")
esm_list <- list()
for (rcp in rcp_scenarios) {
  esm_path <- file.path(quantileDir, paste0("esm_", rcp, ".rds"))
  esm_list[[rcp]] <- readRDS(esm_path) |> addExtremes()
}

# Load trained models
model_files <- list.files(modelDir, pattern = "\\.rds$", full.names = TRUE)
model_names <- tools::file_path_sans_ext(basename(model_files))
models_     <- setNames(lapply(model_files, readRDS), model_names)

# ─────────────────────────────────────
# 3. PREDICTION DATA PER MODEL × RCP
# ─────────────────────────────────────

# pred_data_list <- list()
# 
# for (model_name in model_names) {
#   cat("\n*** Processing model:", model_name, "***\n")
#   
#   quantile_path <- file.path(quantileDir, paste0("quant_", model_name, ".rds"))
#   if (!file.exists(quantile_path)) {
#     cat("Quantile file not found for model:", model_name, "\n")
#     next
#   }
#   
#   pred_data_list[[model_name]] <- list()
#   
#   for (rcp in rcp_scenarios) {
#     cat("  - Scenario:", rcp, "\n")
#     pred_data <- tryCatch(
#       {
#         getPredDat(
#           model = model_name,
#           rcp = rcp,
#           inputDir = inputDir,
#           quantile_path = quantile_path
#         )
#       },
#       error = function(e) {
#         cat("ERROR in getPredDat:", e$message, "\n")
#         NULL
#       }
#     )
#     
#     if (!is.null(pred_data)) {
#       pred_data_list[[model_name]][[rcp]] <- pred_data
#     } else {
#       cat("  Skipping due to error\n")
#     }
#   }
# }

pred_data_list <- list()

for (model_name in model_names) {
  cat("\n*** Processing model:", model_name, "***\n")
  
  quantile_path <- file.path(quantileDir, paste0("quant_", model_name, ".rds"))
  if (!file.exists(quantile_path)) {
    cat("Quantile file not found for model:", model_name, "\n")
    next
  }
  
  pred_data_list[[model_name]] <- list()
  
  # Select the correct version of addextremes
  ext_fun <- if (grepl("cov", model_name)) addExtremes2 else addExtremes1
  cat("  --> Using", if (identical(ext_fun, addExtremes2)) "addExtremes2" else "addExtremes1", "\n")
  
  for (rcp in rcp_scenarios) {
    cat("  - Scenario:", rcp, "\n")
    pred_data <- tryCatch(
      {
        getPredDat(
          model = model_name,
          rcp = rcp,
          inputDir = inputDir,
          quantile_path = quantile_path,
          ext_fun = ext_fun  # 👈 pass selected function
        )
      },
      error = function(e) {
        cat("ERROR in getPredDat:", e$message, "\n")
        NULL
      }
    )
    
    if (!is.null(pred_data)) {
      pred_data_list[[model_name]][[rcp]] <- pred_data
    } else {
      cat("  Skipping due to error\n")
    }
  }
}


# ─────────────────────────────────────
# 4. BASELINE DEVIANCE FROM NULL MODEL
# ─────────────────────────────────────

# Identify null model name (e.g. "mod0.0") using naming convention
null_model_name <- grep("0\\.0$", model_names, value = TRUE)
stopifnot(length(null_model_name) == 1)

# Extract predicted quantiles from pred_data_list
null_quant <- pred_data_list[[null_model_name]][["rcp26"]]$quantile  # Assumes rcp26 present
null_error <- null_quant - obs_$quantile

# Define extreme probability thresholds
ind00  <- which(nenv$prob <= 0)
ind05 = which(nenv$prob <= 0.05)
ind95 = which(nenv$prob >= 0.95)
ind100 <- which(nenv$prob >= 1)

# Compute null deviances (baseline)
nullDev00  <- sum(null_error[ind00]^2, na.rm = TRUE)
nullDev100 <- sum(null_error[ind100]^2, na.rm = TRUE)

# Save for later model performance comparison
null_deviances <- list(dev0 = nullDev00, dev100 = nullDev100)
saveRDS(null_deviances, file = file.path(outputDir, "null_deviance.rds"))

# ─────────────────────────────────────
# 5. PRESENT BIAS INDICATORS (ESM vs OBS)
# ─────────────────────────────────────

esm_rcp26 <- esm_list[["rcp26"]]
esm_bias  <- esm_rcp26$quantile - obs_$quantile
bias_min  <- esm_rcp26$min$historical - obs_$min$historical
bias_max  <- esm_rcp26$max$historical - obs_$max$historical

bias_summary <- list(
  bias_min_mean = mean(bias_min, na.rm = TRUE),
  bias_max_mean = mean(bias_max, na.rm = TRUE),
  bias_min_sd   = sd(bias_min, na.rm = TRUE),
  bias_max_sd   = sd(bias_max, na.rm = TRUE)
)
saveRDS(bias_summary, file = file.path(outputDir, "bias_summary.rds"))

# ─────────────────────────────────────
# 6. COMPUTE INDICATORS FOR ALL MODELS
# ─────────────────────────────────────

all_indicators <- list()

for (model_name in model_names) {
  model_pred <- pred_data_list[[model_name]]
  if (is.null(model_pred)) next
  
  for (rcp in rcp_scenarios) {
    esm_ <- esm_list[[rcp]]
    model_rcp_data <- model_pred[[rcp]]
    if (is.null(model_rcp_data)) next
    
    cat("Model:", model_name, "| RCP:", rcp, "\n")
    cat("  - model_rcp_data:\n"); str(model_rcp_data)
    cat("  - esm_:\n"); str(esm_)
    cat("  - obs_:\n"); str(obs_)
    
    # PRESENT ERRORS
    presentError    <- model_rcp_data$quantile - obs_$quantile
    min_presentBias <- model_rcp_data$min$historical - obs_$min$historical
    max_presentBias <- model_rcp_data$max$historical - obs_$max$historical
    
    # FUTURE ERRORS
    cmin  <- esm_$min$future  - model_rcp_data$min$future
    cmean <- esm_$mean$future - model_rcp_data$mean$future
    cmax  <- esm_$max$future  - model_rcp_data$max$future
    
    ind <- numeric(30)
    
    # FUTURE: Minima bias indicators
    range_min_pb <- diff(range(min_presentBias, na.rm = TRUE))
    ind[6]  <- if (range_min_pb != 0) diff(range(cmin, na.rm = TRUE)) / range_min_pb - 1 else NA
    ind[7]  <- mean(cmin, na.rm = TRUE) - mean(min_presentBias, na.rm = TRUE)
    ind[8]  <- if (range_min_pb != 0) ind[7] / range_min_pb else NA
    
    # FUTURE: Maxima bias indicators
    range_max_pb <- diff(range(max_presentBias, na.rm = TRUE))
    ind[9]  <- if (range_max_pb != 0) diff(range(cmax, na.rm = TRUE)) / range_max_pb - 1 else NA
    ind[10] <- mean(cmax, na.rm = TRUE) - mean(max_presentBias, na.rm = TRUE)
    ind[11] <- if (range_max_pb != 0) ind[10] / range_max_pb else NA
    
    # PRESENT: Overall error indicators
    ind[13] <- mean(abs(presentError), na.rm = TRUE)
    ind[14] <- diff(range(presentError, na.rm = TRUE))
    ind[15] <- mean(sign(presentError), na.rm = TRUE)
    
    # PRESENT: Deviance explained (in extremes)
    ind[22] <- 1 - sum(presentError[ind00]^2, na.rm = TRUE) / nullDev00
    ind[27] <- 1 - sum(presentError[ind100]^2, na.rm = TRUE) / nullDev100
    
    # Store in list with key model_scenario
    all_indicators[[paste(model_name, rcp, sep = "_")]] <- c(
      model = model_name,
      scenario = rcp,
      as.list(ind)
    )
  }
}

# # Convert list of indicators to a data.frame
# indicators_df <- do.call(rbind, lapply(all_indicators, function(x) as.data.frame(t(x))))
# 
# # Make sure columns are named correctly (esp. numeric indicators)
# indicator_names <- c("model", "scenario", paste0("ind", 1:30))
# colnames(indicators_df) <- indicator_names
# 
# # Save as .rds and .csv
# saveRDS(indicators_df, file = file.path(outputDir, "indicators.rds"))
# # write.csv(indicators_df, file = file.path(outputDir, "indicators.csv"), row.names = FALSE)

# Safely flatten and convert all_indicators
indicators_df <- do.call(rbind, lapply(all_indicators, function(x) {
  # Ensure it's unlisted and then transposed
  as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)
}))

indicator_names <- c("model", "scenario", paste0("ind", 1:30))
colnames(indicators_df) <- indicator_names

# Convert any list-columns to character or numeric if needed
ind_df[] <- lapply(ind_df, function(col) {
  if (is.list(col)) unlist(col) else col
})


#PLOT 
library(ComplexHeatmap)
library(circlize)

# Load the saved indicators just in case
ind_df <- readRDS(file.path(outputDir, "indicators.rds"))

# Keep only rows with no missing indicators for plotting
ind_df <- ind_df[complete.cases(ind_df), ]

# Extract the model-scenario name for row labels
rownames(ind_df) <- paste(ind_df$model, ind_df$scenario, sep = "_")

# Select indicator columns (example: 6–11, 13–15, 22, 27)
selected_inds <- c(paste0("ind", c(6:11, 13:15, 22, 27)))
mat <- as.matrix(ind_df[, selected_inds])

# Convert to numeric (in case factors got introduced)
mat <- apply(mat, 2, as.numeric)
rownames(mat) <- paste(ind_df$model, ind_df$scenario, sep = "_")

# Scale the data (optional but useful for heatmap coloring)
mat_scaled <- scale(mat)

# Define color scale
col_fun <- colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))

# Plot with ComplexHeatmap
Heatmap(
  mat_scaled,
  name = "scaled indicator",
  col = col_fun,
  cluster_rows = TRUE,
  cluster_columns = TRUE,
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_gp = gpar(fontsize = 10),
  column_names_gp = gpar(fontsize = 10)
)
