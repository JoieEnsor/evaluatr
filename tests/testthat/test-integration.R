# Integration-style tests for eval_performance() using synthetic data
# that simulates realistic output from secure_model_validation()

# Helper to create a mock result with known properties
make_well_calibrated_result <- function(n = 500, seed = 2648) {
  set.seed(seed)
  x <- rnorm(n)
  lp <- -1 + 1.5 * x
  y <- rbinom(n, 1, plogis(lp))
  # True model predictions => well-calibrated

  p <- plogis(lp)

  list(
    shuffled_outcome_predictions = cbind(outcome = y, prediction = p),
    shuffled_outcomes            = y,
    shuffled_predictions         = p,
    prediction_matrix            = matrix(p, ncol = 1),
    model_info = list(
      model_id             = "well_calibrated",
      model_name           = "Well-Calibrated Test",
      model_type           = "binary",
      version              = "1.0",
      required_variables   = "x",
      n_predictions        = n,
      prediction_columns   = NULL,
      validation_timestamp = Sys.time()
    )
  )
}

make_miscalibrated_result <- function(n = 500, seed = 9174) {
  set.seed(seed)
  x <- rnorm(n)
  lp_true <- -1 + 1.5 * x
  y <- rbinom(n, 1, plogis(lp_true))
  # Shift and shrink the LP to create miscalibration
  p <- plogis(0.5 + 0.3 * lp_true)

  list(
    shuffled_outcome_predictions = cbind(outcome = y, prediction = p),
    shuffled_outcomes            = y,
    shuffled_predictions         = p,
    prediction_matrix            = matrix(p, ncol = 1),
    model_info = list(
      model_id             = "miscalibrated",
      model_name           = "Miscalibrated Test",
      model_type           = "binary",
      version              = "1.0",
      required_variables   = "x",
      n_predictions        = n,
      prediction_columns   = NULL,
      validation_timestamp = Sys.time()
    )
  )
}

# ---- Well-calibrated model metrics ------------------------------------------

test_that("well-calibrated model has good calibration metrics", {
  res  <- make_well_calibrated_result()
  perf <- eval_performance(res, generate_plots = FALSE,
                           confidence_intervals = FALSE)

  get_val <- function(m) perf$metrics$Value[perf$metrics$Metric == m]

  # Calibration intercept should be near 0
  expect_equal(get_val("Cal. intercept"), 0, tolerance = 0.2)

  # Calibration slope should be near 1
  expect_equal(get_val("Cal. slope"), 1, tolerance = 0.2)

  # O:E ratio should be near 1
  expect_equal(get_val("O:E ratio"), 1, tolerance = 0.15)

  # ICI should be small
  expect_true(get_val("ICI") < 0.05)
})

# ---- Miscalibrated model shows worse calibration ----------------------------

test_that("miscalibrated model has detectably worse calibration", {
  good <- eval_performance(make_well_calibrated_result(),
                           generate_plots = FALSE,
                           confidence_intervals = FALSE)
  bad  <- eval_performance(make_miscalibrated_result(),
                           generate_plots = FALSE,
                           confidence_intervals = FALSE)

  get_val <- function(perf, m) perf$metrics$Value[perf$metrics$Metric == m]

  # Miscalibrated slope should deviate more from 1
  expect_true(
    abs(get_val(bad, "Cal. slope") - 1) > abs(get_val(good, "Cal. slope") - 1)
  )

  # ICI should be higher for the miscalibrated model
  expect_true(get_val(bad, "ICI") > get_val(good, "ICI"))
})

# ---- Uninformative model gives AUROC near 0.5 --------------------------------

test_that("uninformative model has AUROC near 0.5", {
  set.seed(4093)
  n <- 1000
  y <- rbinom(n, 1, 0.3)
  p <- rep(mean(y), n) + runif(n, -0.01, 0.01)
  p <- pmin(pmax(p, 0.001), 0.999)

  res <- list(
    shuffled_outcome_predictions = cbind(outcome = y, prediction = p),
    shuffled_outcomes            = y,
    shuffled_predictions         = p,
    prediction_matrix            = matrix(p, ncol = 1),
    model_info = list(
      model_id = "null_model", model_name = "Null",
      model_type = "binary", version = "1.0",
      required_variables = character(0), n_predictions = n,
      prediction_columns = NULL, validation_timestamp = Sys.time()
    )
  )

  perf <- eval_performance(res, generate_plots = FALSE,
                           confidence_intervals = FALSE)
  auc <- perf$metrics$Value[perf$metrics$Metric == "AUROC/c statistic"]
  expect_equal(auc, 0.5, tolerance = 0.05)
})

# ---- Subgroup analysis produces separate metrics ----------------------------

test_that("subgroup analysis metrics differ across groups with different data", {
  set.seed(6712)
  n <- 300

  # Group A: well-separated; Group B: poorly separated
  y_a <- rbinom(150, 1, 0.3)
  p_a <- plogis(rnorm(150, ifelse(y_a == 1, 2, -2), 1))

  y_b <- rbinom(150, 1, 0.3)
  p_b <- plogis(rnorm(150, 0, 0.1))
  p_b <- pmin(pmax(p_b, 0.001), 0.999)

  y <- c(y_a, y_b)
  p <- c(p_a, p_b)
  by_vec <- c(rep("good_model", 150), rep("bad_model", 150))

  res <- list(
    shuffled_outcome_predictions = cbind(outcome = y, prediction = p),
    shuffled_outcomes            = y,
    shuffled_predictions         = p,
    prediction_matrix            = matrix(p, ncol = 1),
    model_info = list(
      model_id = "subgroup_test", model_name = "Subgroup Test",
      model_type = "binary", version = "1.0",
      required_variables = "x", n_predictions = n,
      prediction_columns = NULL, validation_timestamp = Sys.time()
    )
  )

  perf <- eval_performance(res, generate_plots = FALSE,
                           confidence_intervals = FALSE, by = by_vec)

  auc_good <- perf$metrics$Value[perf$metrics$Subgroup == "good_model" &
                                   perf$metrics$Metric == "AUROC/c statistic"]
  auc_bad  <- perf$metrics$Value[perf$metrics$Subgroup == "bad_model" &
                                   perf$metrics$Metric == "AUROC/c statistic"]

  expect_true(auc_good > auc_bad)
})

# ---- Reproducibility of bootstrap CIs (fixed seed) --------------------------

test_that("bootstrap CIs are reproducible across calls", {
  res   <- make_well_calibrated_result()
  perf1 <- eval_performance(res, generate_plots = FALSE,
                            confidence_intervals = TRUE, n_boot = 50)
  perf2 <- eval_performance(res, generate_plots = FALSE,
                            confidence_intervals = TRUE, n_boot = 50)

  # The function uses set.seed(123) internally, so CIs should be identical
  expect_equal(perf1$metrics$CI_Lower, perf2$metrics$CI_Lower)
  expect_equal(perf1$metrics$CI_Upper, perf2$metrics$CI_Upper)
})

# ---- Plot generation ---------------------------------------------------------

test_that("eval_performance generates plots when requested", {
  skip_if_not_installed("CalibrationCurves")
  skip_if_not_installed("rmda")

  res  <- make_well_calibrated_result(n = 200)
  perf <- eval_performance(res, generate_plots = TRUE,
                           confidence_intervals = FALSE)

  expect_true("plots" %in% names(perf))
  expect_type(perf$plots, "list")
  # Distribution plot should be a ggplot object
  expect_s3_class(perf$plots$distribution, "ggplot")
})
