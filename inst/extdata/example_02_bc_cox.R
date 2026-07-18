# =============================================================================
# evaluatr Example 2: Cox proportional hazards — breast cancer relapse
# =============================================================================
#
# Demonstrates the evaluatr workflow for a survival outcome model (Cox PH).
#
# Model: Cox proportional hazards model for relapse-free survival
# Data:  bc_survival_example dataset (n = 2,232, included in evaluatr)
#
# Development set: val == 0 (n = 1,546) — used to fit and register the model
# Validation set:  val == 1 (n = 686)   — used for external validation here
#
# Note: eval_performance() currently supports binary outcomes. For survival
# models, secure_model_validation() returns predicted survival probabilities
# at specified timepoints (here 5 and 10 years) which can be evaluated
# using standard survival calibration and discrimination methods.
#
# The model is hosted on the public evaluatr demo repository. The token below
# is a shared public demo credential (not a real GitHub token) that the key
# service accepts for demo models only.
# =============================================================================

library(evaluatr)

# ---- Credentials (demo repository) ------------------------------------------
GITHUB_USERNAME <- "JoieEnsor"
REPO_NAME       <- "evaluatr-demo-models"
GITHUB_TOKEN    <- "evaluatr-demo"
MODEL_ID        <- "bc_cox_v1"

# ---- Load the breast cancer dataset -----------------------------------------
bc_path <- system.file("extdata", "bc_survival_example.csv",
                       package = "evaluatr")
bc_data <- read.csv(bc_path)

# Use the pre-assigned validation set
bc_val <- bc_data[bc_data$val == 1, ]

cat("Validation set:", nrow(bc_val), "patients\n")
cat("Relapse events:", sum(bc_val$rfi), "\n")

# ---- Run secure validation --------------------------------------------------
# outcome must refer to a Surv() compatible pair: here we pass the event
# indicator; secure_model_validation() uses the model's time variable
# (rf, in months) and event indicator (rfi) as specified in the JSON.
bc_cox_result <- secure_model_validation(
  repo_owner      = GITHUB_USERNAME,
  repo_name       = REPO_NAME,
  model_id        = MODEL_ID,
  github_token    = GITHUB_TOKEN,
  validation_data = bc_val,
  outcome         = "rfi"
)

# ---- Inspect predicted survival probabilities --------------------------------
# Returns a shuffled matrix; columns correspond to the registered timepoints
# (60 months = 5 years, 120 months = 10 years).
# Values are predicted survival probabilities (not event probabilities).
head(bc_cox_result$full_shuffled_matrix)

