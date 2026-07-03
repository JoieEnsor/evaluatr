# =============================================================================
# evaluatr Example 1: Logistic regression — DVT diagnosis
# =============================================================================
#
# Demonstrates the complete evaluatr workflow for a binary outcome model.
#
# Model: Logistic regression predicting deep vein thrombosis (DVT) diagnosis
# Data:  dvt_example dataset (n = 9,805; studies 1-7, included in evaluatr)
#
# Development set: studies 1-5 (n = 7,370) — used to fit and register the model
# Validation set:  studies 6-7 (n = 2,435) — used for external validation here
#
# The model is hosted on the public evaluatr demo repository. The token below
# is a read-only shared PAT for demonstration purposes. In real use, the model
# developer provides a private-repo token to each evaluator.
# =============================================================================

library(evaluatr)

# ---- Credentials (demo repository) ------------------------------------------
GITHUB_USERNAME <- "JoieEnsor"
REPO_NAME       <- "evaluatr-demo-models"
GITHUB_TOKEN    <- "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"
MODEL_ID        <- "dvt_logistic_v1"

# ---- Load the DVT dataset ---------------------------------------------------
dvt_path <- system.file("extdata", "dvt_example.csv", package = "evaluatr")
dvt_data <- read.csv(dvt_path)

# Use studies 6-7 as the external validation set
dvt_val <- dvt_data[dvt_data$study %in% 6:7, ]

cat("Validation set:", nrow(dvt_val), "patients\n")
cat("DVT prevalence:", round(mean(dvt_val$dvt), 3), "\n")

# ---- Run secure validation --------------------------------------------------
dvt_result <- secure_model_validation(
  repo_owner      = GITHUB_USERNAME,
  repo_name       = REPO_NAME,
  model_id        = MODEL_ID,
  github_token    = GITHUB_TOKEN,
  validation_data = dvt_val,
  outcome         = "dvt"
)

# ---- Compute performance metrics --------------------------------------------
performance <- eval_performance(
  validation_result    = dvt_result,
  generate_plots       = TRUE,
  confidence_intervals = TRUE,
  n_boot               = 200,
  decision_threshold   = 0.2
)

# ---- Inspect results --------------------------------------------------------
performance$metrics

# Individual plots:
# performance$plots$calibration
# performance$plots$decision_curve
# performance$plots$distribution
