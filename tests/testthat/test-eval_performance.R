# Tests for eval_performance() input validation and output structure

# -- Helper: build a mock validation_result that looks like secure_model_validation() output
make_mock_result <- function(n = 100, seed = 7381) {
  set.seed(seed)
  outcomes    <- rbinom(n, 1, 0.3)
  predictions <- plogis(rnorm(n, ifelse(outcomes == 1, 1, -1), 1))

  structure(
    list(
      shuffled_outcome_predictions = cbind(outcome = outcomes, prediction = predictions),
      shuffled_outcomes            = outcomes,
      shuffled_predictions         = predictions,
      prediction_matrix            = matrix(predictions, ncol = 1),
      model_info = list(
        model_id             = "test_model",
        model_name           = "Test Model",
        model_type           = "binary",
        version              = "1.0",
        required_variables   = c("x1", "x2"),
        n_predictions        = n,
        prediction_columns   = NULL,
        validation_timestamp = Sys.time()
      )
    ),
    class = "evaluatr_result"
  )
}

# ---- Input validation --------------------------------------------------------

test_that("eval_performance() rejects invalid validation_result", {
  expect_error(eval_performance(list(a = 1)), "must be the output of")
  expect_error(eval_performance("not a list"), "must be the output of")
})

test_that("eval_performance() requires matching outcome/prediction lengths", {
  res <- make_mock_result()
  res$shuffled_outcomes <- res$shuffled_outcomes[1:50]
  expect_error(eval_performance(res), "same length")
})

test_that("eval_performance() requires binary outcomes", {
  res <- make_mock_result()
  res$shuffled_outcomes[1] <- 2
  expect_error(eval_performance(res), "binary")
})

test_that("eval_performance() requires probabilities in [0, 1]", {
  res <- make_mock_result()
  res$shuffled_predictions[1] <- 1.5
  expect_error(eval_performance(res), "between 0 and 1")

  res2 <- make_mock_result()
  res2$shuffled_predictions[1] <- -0.1
  expect_error(eval_performance(res2), "between 0 and 1")
})

test_that("'by' must match prediction length", {
  res <- make_mock_result()
  expect_error(
    eval_performance(res, by = c("A", "B")),
    "same length"
  )
})

test_that("'by' must have at least 2 categories", {
  res <- make_mock_result()
  by_vec <- rep("A", length(res$shuffled_outcomes))
  expect_error(
    eval_performance(res, by = by_vec),
    "at least 2"
  )
})

test_that("subgroups with fewer than 10 observations cause an error", {
  res <- make_mock_result(n = 100)
  by_vec <- c(rep("A", 95), rep("B", 5))
  expect_error(
    eval_performance(res, by = by_vec),
    "fewer than 10"
  )
})

# ---- Output structure (overall) ----------------------------------------------

test_that("eval_performance() returns expected structure without plots/CIs", {
  res  <- make_mock_result()
  perf <- eval_performance(res, generate_plots = FALSE,
                           confidence_intervals = FALSE)

  expect_type(perf, "list")
  expect_named(perf, c("metrics", "plots", "model_info", "decision_threshold"),
               ignore.order = TRUE)

  expect_s3_class(perf$metrics, "data.frame")
  expect_true("Metric" %in% names(perf$metrics))
  expect_true("Value" %in% names(perf$metrics))

  expected_metrics <- c(
    "AUROC/c statistic", "Cal. intercept", "Cal. slope",
    "ECI", "ICI", "ECE", "O:E ratio", "Brier",
    "Nagelkerke R2", "Cox-Snell R2", "McFadden R2",
    "Net benefit", "Standardized net benefit"
  )
  expect_equal(perf$metrics$Metric, expected_metrics)
  expect_equal(perf$decision_threshold, 0.1)
})

test_that("confidence intervals are included when requested", {
  res  <- make_mock_result()
  perf <- eval_performance(res, generate_plots = FALSE,
                           confidence_intervals = TRUE, n_boot = 20)

  expect_true("CI_Lower" %in% names(perf$metrics))
  expect_true("CI_Upper" %in% names(perf$metrics))
  expect_true("CI_String" %in% names(perf$metrics))
  expect_true(all(perf$metrics$CI_Lower <= perf$metrics$CI_Upper))
})

# ---- Output structure (subgroup) ---------------------------------------------

test_that("subgroup analysis returns correct structure", {
  res <- make_mock_result(n = 200)
  by_vec <- rep(c("GroupA", "GroupB"), each = 100)

  perf <- eval_performance(res, generate_plots = FALSE,
                           confidence_intervals = FALSE, by = by_vec)

  expect_true("Subgroup" %in% names(perf$metrics))
  expect_true("subgroup_variable" %in% names(perf))
  expect_true("subgroup_categories" %in% names(perf))
  expect_setequal(unique(perf$metrics$Subgroup), c("GroupA", "GroupB"))

  # 13 metrics per subgroup
  expect_equal(nrow(perf$metrics), 2 * 13)
})

# ---- Metric value sanity checks ---------------------------------------------

test_that("metric values are within plausible ranges", {
  res  <- make_mock_result(n = 500, seed = 4821)
  perf <- eval_performance(res, generate_plots = FALSE,
                           confidence_intervals = FALSE)

  get_val <- function(metric) perf$metrics$Value[perf$metrics$Metric == metric]

  # AUROC should be between 0 and 1

  expect_true(get_val("AUROC/c statistic") >= 0 && get_val("AUROC/c statistic") <= 1)

  # Brier score should be between 0 and 1
  expect_true(get_val("Brier") >= 0 && get_val("Brier") <= 1)

  # Calibration slope should be positive for a reasonable model
  expect_true(get_val("Cal. slope") > 0)

  # R-squared values bounded
  expect_true(get_val("Nagelkerke R2") >= 0 && get_val("Nagelkerke R2") <= 1)
  expect_true(get_val("Cox-Snell R2") >= 0 && get_val("Cox-Snell R2") <= 1)
  expect_true(get_val("McFadden R2") >= 0 && get_val("McFadden R2") <= 1)

  # Since our mock data has decent separation, AUROC should be well above 0.5
  expect_true(get_val("AUROC/c statistic") > 0.6)
})

# ---- Custom decision threshold -----------------------------------------------

test_that("custom decision_threshold is respected", {
  res <- make_mock_result(n = 200)

  perf_low  <- eval_performance(res, generate_plots = FALSE,
                                confidence_intervals = FALSE,
                                decision_threshold = 0.05)
  perf_high <- eval_performance(res, generate_plots = FALSE,
                                confidence_intervals = FALSE,
                                decision_threshold = 0.5)

  nb_low  <- perf_low$metrics$Value[perf_low$metrics$Metric == "Net benefit"]
  nb_high <- perf_high$metrics$Value[perf_high$metrics$Metric == "Net benefit"]

  expect_equal(perf_low$decision_threshold, 0.05)
  expect_equal(perf_high$decision_threshold, 0.5)
  # Net benefit typically differs across thresholds
  expect_false(nb_low == nb_high)
})
