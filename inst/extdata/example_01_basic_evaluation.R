# =============================================================================
# evaluatr Example 1: Basic Binary Outcome Evaluation
# =============================================================================
#
# This script demonstrates a complete evaluation workflow using the MIMIC
# sample dataset and the publicly accessible AKI prediction model hosted
# on the evaluatr test repository.
#
# Model: Logistic regression predicting acute kidney injury (AKI)
# Data:  MIMIC-III sample (n = 3670, included with the evaluatr package)
#
# Prerequisites:
#   install.packages(c("evaluatr", ...))   # or devtools::install_github(...)
#
# The test repository uses a PUBLIC repo with a read-only shared token so
# you can try this without contacting a developer. In real use, the developer
# would provide you with a private-repo token.
# =============================================================================

library(evaluatr)

# ---- Credentials (test repository) -----------------------------------------
GITHUB_USERNAME <- "JoieEnsor"
REPO_NAME       <- "clinical-models-test"
GITHUB_TOKEN    <- ""          # <-- insert the shared read-only PAT here
MODEL_ID        <- "sample_model_002"

# ---- Load the sample dataset ------------------------------------------------
mimic_path   <- system.file("extdata", "mimic_sample.csv", package = "evaluatr")
mimic_sample <- read.csv(mimic_path)

cat("Dataset dimensions:", nrow(mimic_sample), "rows x", ncol(mimic_sample), "columns\n")
cat("Outcome prevalence (AKI):", round(mean(mimic_sample$AKI), 3), "\n")

# ---- Run secure validation --------------------------------------------------
mimic_result <- secure_model_validation(
  repo_owner      = GITHUB_USERNAME,
  repo_name       = REPO_NAME,
  model_id        = MODEL_ID,
  github_token    = GITHUB_TOKEN,
  validation_data = mimic_sample,
  outcome         = "AKI"
)

# ---- Compute performance metrics --------------------------------------------
performance <- calculate_pmextval_metrics(
  validation_result    = mimic_result,
  generate_plots       = TRUE,
  confidence_intervals = TRUE,
  n_boot               = 200,          # increase to 1000 for publication
  decision_threshold   = 0.25
)

# ---- Inspect results --------------------------------------------------------
performance$metrics

# Individual plots are also available:
# performance$plots$calibration
# performance$plots$decision_curve
# performance$plots$distribution
