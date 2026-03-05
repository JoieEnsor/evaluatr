# =============================================================================
# evaluatr Example 2: Subgroup Evaluation Using the 'by' Argument
# =============================================================================
#
# This script demonstrates subgroup-specific evaluation using the `by`
# argument in both secure_model_validation() and calculate_pmextval_metrics().
#
# The workflow has two stages:
#   Stage A — Pass `by` to secure_model_validation() so that the shuffled
#             output retains the subgroup label alongside each prediction.
#   Stage B — Pass result$shuffled_by to calculate_pmextval_metrics() to
#             obtain separate performance metrics per subgroup.
#
# Model: AKI logistic regression (sample_model_002)
# Data:  MIMIC-III sample; subgroup variable = gender (0 = female, 1 = male)
# =============================================================================

library(evaluatr)

# ---- Credentials (test repository) -----------------------------------------
GITHUB_USERNAME <- "JoieEnsor"
REPO_NAME       <- "evaluatr_testing_environment"
GITHUB_TOKEN    <- "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"
MODEL_ID        <- "sample_model_002"

# ---- Load sample dataset ----------------------------------------------------
mimic_path   <- system.file("extdata", "mimic_sample.csv", package = "evaluatr")
mimic_sample <- read.csv(mimic_path)

cat("Gender distribution:\n")
print(table(Gender = mimic_sample$gender))
cat("\nAKI by gender:\n")
print(table(Gender = mimic_sample$gender, AKI = mimic_sample$AKI))

# ---- Stage A: secure validation with subgroup label -------------------------
# By passing by = "gender", the shuffled output retains a 'shuffled_by'
# column that preserves the subgroup membership of each prediction.
mimic_by_result <- secure_model_validation(
  repo_owner      = GITHUB_USERNAME,
  repo_name       = REPO_NAME,
  model_id        = MODEL_ID,
  github_token    = GITHUB_TOKEN,
  validation_data = mimic_sample,
  outcome         = "AKI",
  by              = "gender"           # name of subgroup column in your data
)

# The shuffled subgroup labels are in:
cat("\nSubgroup labels retained:", length(mimic_by_result$shuffled_by), "\n")
cat("Unique values:", unique(mimic_by_result$shuffled_by), "\n")

# ---- Stage B: metrics by subgroup -------------------------------------------
performance_by_gender <- calculate_pmextval_metrics(
  validation_result    = mimic_by_result,
  generate_plots       = TRUE,        # generates separate plots per subgroup
  confidence_intervals = TRUE,
  n_boot               = 50,
  decision_threshold   = 0.25,
  by                   = mimic_by_result$shuffled_by   # pass the shuffled labels
)

# ---- Inspect results --------------------------------------------------------
# Combined metrics table with a 'Subgroup' column
performance_by_gender$metrics

# Plots are organised by subgroup:
# performance_by_gender$plots[["0"]]$calibration   # female
# performance_by_gender$plots[["1"]]$calibration   # male
