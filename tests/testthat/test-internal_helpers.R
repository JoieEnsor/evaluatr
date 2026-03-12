# Tests for internal (non-exported) helper functions
# Access via evaluatr:::fn_name

# ---- %||% operator -----------------------------------------------------------

test_that("%||% returns first argument when non-NULL", {
  op <- evaluatr:::`%||%`
  expect_equal(op("a", "b"), "a")
  expect_equal(op(0, 1), 0)
  expect_equal(op(FALSE, TRUE), FALSE)
})

test_that("%||% returns second argument when first is NULL", {
  op <- evaluatr:::`%||%`
  expect_equal(op(NULL, "fallback"), "fallback")
  expect_equal(op(NULL, 42), 42)
  expect_null(op(NULL, NULL))
})

# ---- .fastAUC ----------------------------------------------------------------

test_that(".fastAUC returns 1.0 for perfect separation", {
  # All positives have higher predicted probability than all negatives
  p <- c(0.1, 0.2, 0.3, 0.8, 0.9, 1.0)
  y <- c(0,   0,   0,   1,   1,   1)
  expect_equal(evaluatr:::.fastAUC(p, y), 1.0)
})

test_that(".fastAUC returns ~0.5 for random predictions", {
  set.seed(6294)
  n <- 5000
  y <- rbinom(n, 1, 0.5)
  p <- runif(n)
  auc <- evaluatr:::.fastAUC(p, y)
  expect_true(abs(auc - 0.5) < 0.05)
})

test_that(".fastAUC returns 0.0 for perfectly reversed predictions", {
  p <- c(0.8, 0.9, 1.0, 0.1, 0.2, 0.3)
  y <- c(0,   0,   0,   1,   1,   1)
  expect_equal(evaluatr:::.fastAUC(p, y), 0.0)
})

test_that(".fastAUC agrees with pROC::auc on synthetic data", {
  skip_if_not_installed("pROC")
  set.seed(2917)
  n <- 200
  y <- rbinom(n, 1, 0.4)
  p <- plogis(rnorm(n, ifelse(y == 1, 1, -0.5)))
  fast <- evaluatr:::.fastAUC(p, y)
  proc_auc <- as.numeric(pROC::auc(pROC::roc(y, p, quiet = TRUE)))
  expect_equal(fast, proc_auc, tolerance = 1e-10)
})

# ---- .DiscPerfBin ------------------------------------------------------------

test_that(".DiscPerfBin returns a 1-row data.frame with correct column name", {
  set.seed(5102)
  y <- rbinom(100, 1, 0.3)
  p <- plogis(rnorm(100, ifelse(y == 1, 0.5, -0.5)))
  result <- evaluatr:::.DiscPerfBin(y = y, p = p)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(names(result), "AUROC/c statistic")
})

# ---- .OvPerfBin --------------------------------------------------------------

test_that(".OvPerfBin returns expected columns", {
  set.seed(8413)
  y <- rbinom(100, 1, 0.3)
  p <- plogis(rnorm(100, ifelse(y == 1, 0.5, -0.5)))
  result <- evaluatr:::.OvPerfBin(y = y, p = p)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expected_cols <- c("Loglikelihood", "Logloss", "Brier", "Scaled Brier",
                     "McFadden R2", "Cox-Snell R2", "Nagelkerke R2",
                     "Discrimination slope", "MAPE")
  expect_equal(names(result), expected_cols)
})

test_that(".OvPerfBin Brier score is 0 for perfect predictions", {
  y <- c(0, 0, 0, 1, 1, 1)
  p <- c(0, 0, 0, 1, 1, 1)
  result <- evaluatr:::.OvPerfBin(y = y, p = p)
  expect_equal(result$Brier, 0)
})

test_that(".OvPerfBin Brier score equals 1 for worst-case predictions", {
  y <- c(0, 0, 0, 1, 1, 1)
  p <- c(1, 1, 1, 0, 0, 0)
  result <- evaluatr:::.OvPerfBin(y = y, p = p)
  expect_equal(result$Brier, 1)
})

# ---- .CalPerfBin -------------------------------------------------------------

test_that(".CalPerfBin returns expected columns", {
  set.seed(1647)
  y <- rbinom(200, 1, 0.3)
  p <- plogis(rnorm(200, ifelse(y == 1, 0.5, -0.5)))
  result <- evaluatr:::.CalPerfBin(y = y, p = p)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expected_cols <- c("O:E ratio", "Cal. intercept", "Cal. slope",
                     "ECI", "ICI", "ECE")
  expect_equal(names(result), expected_cols)
})

test_that(".CalPerfBin calibration slope is approximately 1 for well-calibrated model", {
  # Simulate data from a known logistic model, then predict with the true model
  set.seed(3756)
  n <- 1000
  x <- rnorm(n)
  lp <- -1 + 1.5 * x
  y <- rbinom(n, 1, plogis(lp))
  p <- plogis(lp)

  result <- evaluatr:::.CalPerfBin(y = y, p = p)
  expect_equal(result$`Cal. slope`, 1, tolerance = 0.15)
  expect_equal(result$`Cal. intercept`, 0, tolerance = 0.15)
})

# ---- .UtilPerfBin ------------------------------------------------------------

test_that(".UtilPerfBin returns expected columns", {
  set.seed(5839)
  y <- rbinom(100, 1, 0.3)
  p <- plogis(rnorm(100, ifelse(y == 1, 0.5, -0.5)))
  result <- evaluatr:::.UtilPerfBin(y = y, p = p, cut = 0.1)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(names(result), c("Net benefit", "Standardized net benefit"))
})

test_that(".UtilPerfBin net benefit is 0 when threshold exceeds all predictions", {
  y <- c(0, 0, 0, 1, 1, 1)
  p <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  # Cut above all predictions => no one treated => NB = 0

  result <- evaluatr:::.UtilPerfBin(y = y, p = p, cut = 0.99)
  expect_equal(result$`Net benefit`, 0)
})

test_that(".UtilPerfBin net benefit computation is correct for known values", {
  # Manual calculation: cut = 0.5
  # p >= 0.5: indices 4, 5, 6 => y = 1, 1, 1 (3 TP), y = 0 (0 FP)
  y <- c(0, 0, 0, 1, 1, 1)
  p <- c(0.1, 0.2, 0.3, 0.8, 0.9, 1.0)
  cut <- 0.5
  n <- length(y)

  # NB = mean((p >= cut) * (y == 1)) - (cut / (1 - cut)) * mean((p >= cut) * (y == 0))
  expected_nb <- 3 / n - (cut / (1 - cut)) * 0 / n
  result <- evaluatr:::.UtilPerfBin(y = y, p = p, cut = cut)
  expect_equal(result$`Net benefit`, expected_nb)
})
