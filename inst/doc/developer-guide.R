## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## -----------------------------------------------------------------------------
# # Write your function as normal R code
# my_function <- '
# probabilities <- 1 / (1 + exp(-LP))
# result_matrix <- matrix(probabilities, ncol = 1)
# colnames(result_matrix) <- "probability"
# result_matrix
# '
# 
# # Convert to JSON-safe single string
# json_safe <- gsub("\n", "\\n", trimws(my_function))
# cat(json_safe)
# # Copy the output and paste into your JSON file

## -----------------------------------------------------------------------------
# library(evaluatr)
# 
# # Use your own development dataset for this check
# dev_data <- read.csv("path/to/your/development_data.csv")
# 
# # Compute predictions manually (exactly as you would from the model object)
# manual_lp    <- -1.25 + 0.02 * dev_data$age + 0.80 * dev_data$biomarker_score
# manual_probs <- 1 / (1 + exp(-manual_lp))
# 
# # Compute predictions via evaluatr using your GitHub repo
# result <- secure_model_validation(
#   repo_owner      = "your-github-username",
#   repo_name       = "my-clinical-models",
#   model_id        = "model_v1",
#   github_token    = Sys.getenv("GITHUB_PAT"),   # your own PAT, full access
#   validation_data = dev_data,
#   outcome         = "outcome_column"
# )
# 
# # Sort both sets of predictions and compare
# # (result is shuffled, so sort before comparing)
# max(abs(sort(manual_probs) - sort(result$shuffled_predictions)))
# # Should be < 1e-10 if the JSON is correct

## -----------------------------------------------------------------------------
# secure_model_validation(
#   repo_owner   = "your-github-username",
#   repo_name    = "my-clinical-models",
#   model_id     = "model_v1",
#   github_token = "token_you_issued",
#   ...
# )

