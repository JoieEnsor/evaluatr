## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## -----------------------------------------------------------------------------
# devtools::install_github("JoieEnsor/evaluatr")
# library(evaluatr)

## -----------------------------------------------------------------------------
# # Run once — opens your user-level .Renviron
# usethis::edit_r_environ(scope = "user")

## -----------------------------------------------------------------------------
# library(evaluatr)
# 
# # Load the sample dataset included with the package
# mimic_path   <- system.file("extdata", "mimic_sample.csv", package = "evaluatr")
# mimic_sample <- read.csv(mimic_path)
# 
# # Run secure validation
# # In real use, replace these with the values sent to you by the developer
# result <- secure_model_validation(
#   repo_owner      = "JoieEnsor",
#   repo_name       = "evaluatr_testing_environment",
#   model_id        = "sample_model_001",
#   github_token    = Sys.getenv("EVALUATR_TOKEN"),
#   validation_data = mimic_sample,
#   outcome         = "AKI"
# )

## -----------------------------------------------------------------------------
# # Compute performance metrics
# performance <- eval_performance(
#   validation_result    = result,
#   generate_plots       = TRUE,
#   confidence_intervals = TRUE,
#   n_boot               = 50,        # low here for illustration purposes
#   decision_threshold   = 0.25
# )
# 
# # View the metrics table
# performance$metrics
# 
# # Access individual diagnostic plots
# performance$plots$calibration      # calibration plot
# performance$plots$decision_curve   # decision curve analysis
# performance$plots$distribution     # predicted probability distributions

## -----------------------------------------------------------------------------
# # Stage 1 — run validation with subgroup labels retained
# result_by_gender <- secure_model_validation(
#   repo_owner      = "JoieEnsor",
#   repo_name       = "evaluatr_testing_environment",
#   model_id        = "sample_model_001",
#   github_token    = Sys.getenv("EVALUATR_TOKEN"),
#   validation_data = mimic_sample,
#   outcome         = "AKI",
#   by              = "gender"    # column name in your dataset
# )
# 
# # Stage 2 — compute metrics separately per subgroup
# performance_by_gender <- eval_performance(
#   validation_result    = result_by_gender,
#   generate_plots       = TRUE,
#   confidence_intervals = TRUE,
#   n_boot               = 50,
#   decision_threshold   = 0.25,
#   by                   = result_by_gender$shuffled_by
# )
# 
# # The metrics table now has a Subgroup column
# performance_by_gender$metrics
# 
# # Plots are organised by subgroup level
# performance_by_gender$plots[["0"]]$calibration   # gender = 0
# performance_by_gender$plots[["1"]]$calibration   # gender = 1

## -----------------------------------------------------------------------------
# # Create the required transformed variable before validation
# mimic_sample$log_creatinine_mean <- log(mimic_sample$creatinine_mean + 0.001)
# 
# # Now run validation — evaluatr will find the log_creatinine_mean column
# result_preprocessed <- secure_model_validation(
#   repo_owner      = "JoieEnsor",
#   repo_name       = "evaluatr_testing_environment",
#   model_id        = "sample_model_002",
#   github_token    = Sys.getenv("EVALUATR_TOKEN"),
#   validation_data = mimic_sample,
#   outcome         = "AKI"
# )
# 
# performance_preprocessed <- eval_performance(
#   validation_result    = result_preprocessed,
#   generate_plots       = TRUE,
#   confidence_intervals = TRUE,
#   n_boot               = 50,
#   decision_threshold   = 0.25
# )
# 
# performance_preprocessed$metrics

## -----------------------------------------------------------------------------
# # No pre-processing needed — the developer's specification handles it
# result_autoprepro <- secure_model_validation(
#   repo_owner      = "JoieEnsor",
#   repo_name       = "evaluatr_testing_environment",
#   model_id        = "sample_model_003",
#   github_token    = Sys.getenv("EVALUATR_TOKEN"),
#   validation_data = mimic_sample,   # raw creatinine_mean provided as-is
#   outcome         = "AKI"
# )
# 
# performance_autoprepro <- eval_performance(
#   validation_result    = result_autoprepro,
#   generate_plots       = TRUE,
#   confidence_intervals = TRUE,
#   n_boot               = 50,
#   decision_threshold   = 0.25
# )
# 
# performance_autoprepro$metrics

