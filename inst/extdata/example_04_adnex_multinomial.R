# =============================================================================
# evaluatr Example 4: Multinomial model — ADNEX ovarian mass classification
# =============================================================================
#
# Demonstrates the evaluatr workflow for a multinomial outcome model.
#
# Model: ADNEX multinomial logistic regression with CA-125
#        (Van Calster et al. BMJ 2014; doi:10.1136/bmj.g5920)
# Data:  adnex_sample dataset (n = 1,500, included in evaluatr)
#
# The ADNEX model classifies ovarian masses into five categories:
#   1. Benign (reference)
#   2. Borderline malignant
#   3. Stage I invasive ovarian cancer
#   4. Stage II-IV invasive ovarian cancer
#   5. Secondary metastatic cancer
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
MODEL_ID        <- "adnex_with_v1"

# ---- Load the ADNEX validation dataset --------------------------------------
adnex_path <- system.file("extdata", "adnex_sample.rds", package = "evaluatr")
adnex_data <- readRDS(adnex_path)

# Outcome variable: outcome5
#   1 = benign, 2 = borderline, 3 = stage I,
#   4 = stage II-IV, 5 = metastatic
cat("Dataset:", nrow(adnex_data), "patients\n")
cat("Outcome distribution:\n")
print(table(adnex_data$outcome5))

# ---- Run secure validation --------------------------------------------------
# The model's preprocessing code (log2 transforms) is applied automatically
# inside secure_model_validation() before prediction.
adnex_result <- secure_model_validation(
  repo_owner      = GITHUB_USERNAME,
  repo_name       = REPO_NAME,
  model_id        = MODEL_ID,
  github_token    = GITHUB_TOKEN,
  validation_data = adnex_data,
  outcome         = "outcome5"
)

# ---- Inspect predicted probabilities ----------------------------------------
# Returns a row-shuffled matrix with one column per outcome category.
# Column order matches the model's category order: benign, borderline,
# stage_I, stage_II_IV, metastatic.
head(adnex_result$full_shuffled_matrix)

# To verify predictions match published values (sorted comparison):
# Reference predictions are in adnex_data columns pbenw, pborw, pst1w,
# pst2_4w, pmetaw (pre-computed by the model developer).
#
max(abs(sort(adnex_data$pbenw) - sort(adnex_result$full_shuffled_matrix[, "reference"])))


max(abs(sort(adnex_data$pborw)   - sort(adnex_result$full_shuffled_matrix[, "borderline"])))
max(abs(sort(adnex_data$pst1w)   - sort(adnex_result$full_shuffled_matrix[, "stage_I"])))
max(abs(sort(adnex_data$pst2_4w) - sort(adnex_result$full_shuffled_matrix[, "stage_II_IV"])))
max(abs(sort(adnex_data$pmetaw)  - sort(adnex_result$full_shuffled_matrix[, "metastatic"])))

