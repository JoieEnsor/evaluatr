# =============================================================================
# evaluatr Example 3: Evaluation with Pre-processing
# =============================================================================
#
# Some prediction models require variables that are derived from raw data —
# for example, log-transformed values, squared terms, or interaction terms.
# The developer can include pre-processing code in their JSON specification
# (executed server-side in the isolated prediction environment), but
# evaluators may also need to prepare variables before calling
# secure_model_validation().
#
# This script illustrates two pre-processing scenarios:
#
#   Scenario A — Evaluator-side pre-processing:
#     The model requires log(creatinine) and creatinine^2. The evaluator
#     creates these columns before calling secure_model_validation().
#
#   Scenario B — Developer-side pre-processing (automatic):
#     When the developer has embedded transformation code in the JSON
#     prediction_function string, no extra steps are needed by the evaluator.
#     This scenario runs the standard AKI model (sample_model_002) to
#     illustrate that developer-specified transformations just work.
#
# NOTE: Scenario A uses a *hypothetical* model (sample_model_004) that
# expects log_creatinine and creatinine_sq. To test it you would need
# that model uploaded to the test repository. Scenario B runs immediately
# with the existing test model.
# =============================================================================

library(evaluatr)

# ---- Credentials (test repository) -----------------------------------------
GITHUB_USERNAME <- "JoieEnsor"
REPO_NAME       <- "evaluatr_testing_environment"
GITHUB_TOKEN    <- "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"

# ---- Load sample dataset ----------------------------------------------------
mimic_path   <- system.file("extdata", "mimic_sample.csv", package = "evaluatr")
mimic_sample <- read.csv(mimic_path)

# =============================================================================
# SCENARIO A: Evaluator-side pre-processing
# =============================================================================
# Suppose sample_model_004 was developed using log(creatinine) and
# creatinine^2 as predictors instead of raw creatinine.
# We create these columns before passing the data.

mimic_processed <- mimic_sample

# Log transformation (add small constant to handle zeros if present)
mimic_processed$log_creatinine <- log(mimic_processed$creatinine_mean + 0.001)

# Squared term
mimic_processed$creatinine_sq  <- mimic_processed$creatinine_mean^2

cat("New columns added:\n")
cat("  log_creatinine: range [",
    round(range(mimic_processed$log_creatinine), 3), "]\n")
cat("  creatinine_sq:  range [",
    round(range(mimic_processed$creatinine_sq), 3),  "]\n\n")

# Now call secure_model_validation with the processed dataset.
# The JSON for sample_model_003 lists "log_creatinine" and "creatinine_sq"
# in its variables field, so these columns are automatically used.

## Uncomment once sample_model_003 is uploaded to the test repository:
# result_processed <- secure_model_validation(
#   repo_owner      = GITHUB_USERNAME,
#   repo_name       = REPO_NAME,
#   model_id        = "sample_model_003",
#   github_token    = GITHUB_TOKEN,
#   validation_data = mimic_processed,
#   outcome         = "AKI"
# )
# performance_processed <- calculate_pmextval_metrics(
#   result_processed, n_boot = 50, decision_threshold = 0.25
# )
# performance_processed$metrics

# =============================================================================
# SCENARIO B: Developer-side pre-processing (runs immediately)
# =============================================================================
# The standard AKI model (sample_model_004) uses raw variables only, so no
# evaluator-side pre-processing is needed. Developer-embedded transformations
# in the prediction_function string are executed automatically inside the
# isolated prediction environment.

result_standard <- secure_model_validation(
  repo_owner      = GITHUB_USERNAME,
  repo_name       = REPO_NAME,
  model_id        = "sample_model_004",
  github_token    = GITHUB_TOKEN,
  validation_data = mimic_sample,
  outcome         = "AKI"
)

performance_standard <- calculate_pmextval_metrics(
  validation_result    = result_standard,
  generate_plots       = TRUE,
  confidence_intervals = TRUE,
  n_boot               = 50,
  decision_threshold   = 0.25
)

cat("Standard model performance (no evaluator pre-processing required):\n")
print(performance_standard$metrics[, c("Metric", "Value")])

# =============================================================================
# TIP: How to include pre-processing in the developer JSON
# =============================================================================
# Developers can embed transformation code directly in the
# "prediction_function" field of their JSON. For example:
#
# "prediction_function": "
#   # Developer-specified pre-processing
#   validation_data$log_creatinine <- log(validation_data$creatinine_mean + 0.001)
#   validation_data$creatinine_sq  <- validation_data$creatinine_mean^2
#
#   # LP is then computed using the transformed variables
#   LP <- b0 + b_log_creatinine * validation_data$log_creatinine +
#              b_creatinine_sq  * validation_data$creatinine_sq + ...
#
#   probabilities <- 1 / (1 + exp(-LP))
#   matrix(probabilities, ncol = 1, dimnames = list(NULL, 'probability'))
# "
#
# When this approach is used, the evaluator does NOT need to create any
# derived columns — everything happens automatically inside the isolated
# prediction environment on their machine.
