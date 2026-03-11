# Tests for the full secure_model_validation() pipeline using a mocked
# .fetch_github_model(). This avoids needing a live GitHub token while
# exercising LP computation, prediction function execution, shuffling,
# and output assembly.

# -- Helper: create a mock fetch return for a simple logistic model -----------
# True model: logit(p) = -1 + 0.5 * x1 + 0.8 * x2
mock_fetch_logistic <- function(api_url, token) {
  list(
    coefficients_json = list(
      `(Intercept)` = -1,
      x1 = 0.5,
      x2 = 0.8
    ),
    prediction_function = "1 / (1 + exp(-LP))",
    metadata_json = list(
      model_name   = "Mock Logistic",
      outcome_type = "binary",
      version      = "1.0",
      variables    = c("x1", "x2")
    ),
    http_status = 200,
    success     = TRUE
  )
}

# -- Helper: create a mock fetch return for a model with a custom prediction fn
mock_fetch_custom_pred <- function(api_url, token) {
  list(
    coefficients_json = list(
      `(Intercept)` = 0,
      x1 = 1
    ),
    prediction_function = paste0(
      "lp <- `(Intercept)` + x1 * validation_data$x1\n",
      "1 / (1 + exp(-lp))"
    ),
    metadata_json = list(
      model_name   = "Mock Custom",
      outcome_type = "binary",
      version      = "1.0",
      variables    = c("x1")
    ),
    http_status = 200,
    success     = TRUE
  )
}

# -- Helper: create a mock fetch that returns an error ------------------------
mock_fetch_error <- function(api_url, token) {
  list(error = "Model not found (404). Check repo_owner, repo_name, and model_id.",
       http_status = 404)
}

# -- Helper: create a mock fetch for a model that requires a missing variable -
mock_fetch_missing_var <- function(api_url, token) {
  list(
    coefficients_json = list(
      `(Intercept)` = 0,
      x1 = 1,
      x_missing = 2
    ),
    prediction_function = "1 / (1 + exp(-LP))",
    metadata_json = list(
      model_name   = "Mock Missing Var",
      outcome_type = "binary",
      version      = "1.0",
      variables    = c("x1", "x_missing")
    ),
    http_status = 200,
    success     = TRUE
  )
}

# -- Helper: mock fetch for a simple 3-class multinomial model ----------------
# Prediction function computes its own LPs and returns a softmax matrix.
mock_fetch_multinomial <- function(api_url, token) {
  # 3-class model (reference = class A): 2 logit equations
  # z_B = -0.5 + 0.3 * x1 + 0.2 * x2
  # z_C =  0.5 + 0.1 * x1 - 0.4 * x2
  pred_fn <- paste(
    "z_B <- z_B_intercept + z_B_x1 * validation_data$x1 + z_B_x2 * validation_data$x2",
    "z_C <- z_C_intercept + z_C_x1 * validation_data$x1 + z_C_x2 * validation_data$x2",
    "denom <- 1 + exp(z_B) + exp(z_C)",
    "p_A <- 1 / denom",
    "p_B <- exp(z_B) / denom",
    "p_C <- exp(z_C) / denom",
    "m <- cbind(class_A = p_A, class_B = p_B, class_C = p_C)",
    "m",
    sep = "\n"
  )

  list(
    coefficients_json = list(
      z_B_intercept = -0.5, z_B_x1 = 0.3, z_B_x2 = 0.2,
      z_C_intercept =  0.5, z_C_x1 = 0.1, z_C_x2 = -0.4
    ),
    prediction_function = pred_fn,
    metadata_json = list(
      model_name     = "Mock Multinomial",
      outcome_type   = "multinomial",
      outcome_classes = c("class_A", "class_B", "class_C"),
      reference_class = "class_A",
      version        = "1.0",
      variables      = c("x1", "x2")
    ),
    http_status = 200,
    success     = TRUE
  )
}

# -- Helper: build validation data (binary) -----------------------------------
make_validation_data <- function(n = 100, seed = 5831) {
  set.seed(seed)
  data.frame(
    x1      = rnorm(n),
    x2      = rnorm(n),
    outcome = rbinom(n, 1, 0.4)
  )
}


# ---- Tests -------------------------------------------------------------------

test_that("full pipeline works with mocked logistic model", {
  df <- make_validation_data()

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_logistic,
    .package = "evaluatr"
  )

  expect_type(result, "list")
  expect_true("shuffled_outcomes" %in% names(result))
  expect_true("shuffled_predictions" %in% names(result))
  expect_true("model_info" %in% names(result))

  # Correct number of predictions

  expect_equal(length(result$shuffled_outcomes), nrow(df))
  expect_equal(length(result$shuffled_predictions), nrow(df))

  # Predictions are probabilities
  expect_true(all(result$shuffled_predictions >= 0))
  expect_true(all(result$shuffled_predictions <= 1))
})

test_that("predictions are correct (match manual LP calculation)", {
  df <- make_validation_data()

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_logistic,
    .package = "evaluatr"
  )

  # Compute expected predictions manually
  lp_expected <- -1 + 0.5 * df$x1 + 0.8 * df$x2
  p_expected  <- 1 / (1 + exp(-lp_expected))

  # Predictions are shuffled, so compare sorted values
  expect_equal(
    sort(result$shuffled_predictions),
    sort(p_expected),
    tolerance = 1e-10
  )
})

test_that("output is shuffled (not in original row order)", {
  # Use enough data that identical ordering is astronomically unlikely
  df <- make_validation_data(n = 200, seed = 3344)

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_logistic,
    .package = "evaluatr"
  )

  lp_expected <- -1 + 0.5 * df$x1 + 0.8 * df$x2
  p_expected  <- 1 / (1 + exp(-lp_expected))

  # Shuffled predictions should NOT match original order
  expect_false(all(result$shuffled_predictions == p_expected))
})

test_that("model_info contains expected metadata", {
  df <- make_validation_data()

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_logistic,
    .package = "evaluatr"
  )

  info <- result$model_info
  expect_equal(info$model_id, "mock")
  expect_equal(info$model_name, "Mock Logistic")
  expect_equal(info$model_type, "binary")
  expect_equal(info$version, "1.0")
  expect_equal(info$required_variables, c("x1", "x2"))
  expect_equal(info$n_predictions, nrow(df))
  expect_s3_class(info$validation_timestamp, "POSIXct")
})

test_that("GitHub fetch error propagates as a stop()", {
  df <- make_validation_data()

  expect_error(
    with_mocked_bindings(
      secure_model_validation(
        repo_owner = "fake", repo_name = "repo", model_id = "mock",
        github_token = "fake_token", validation_data = df,
        outcome = "outcome"
      ),
      .fetch_github_model = mock_fetch_error,
      .package = "evaluatr"
    ),
    "Model not found"
  )
})

test_that("missing required variable in data is caught after fetch", {
  df <- make_validation_data()

  expect_error(
    with_mocked_bindings(
      secure_model_validation(
        repo_owner = "fake", repo_name = "repo", model_id = "mock",
        github_token = "fake_token", validation_data = df,
        outcome = "outcome"
      ),
      .fetch_github_model = mock_fetch_missing_var,
      .package = "evaluatr"
    ),
    "Missing required variables.*x_missing"
  )
})

test_that("custom prediction function is executed correctly", {
  set.seed(7621)
  df <- data.frame(
    x1      = rnorm(50),
    outcome = rbinom(50, 1, 0.5)
  )

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_custom_pred,
    .package = "evaluatr"
  )

  # The custom prediction fn computes its own LP, ignoring the default LP path
  p_expected <- 1 / (1 + exp(-(0 + 1 * df$x1)))
  expect_equal(sort(result$shuffled_predictions), sort(p_expected),
               tolerance = 1e-10)
})

test_that("subgroup ('by') column is included in shuffled output", {
  set.seed(4412)
  df <- data.frame(
    x1      = rnorm(100),
    x2      = rnorm(100),
    outcome = rbinom(100, 1, 0.4),
    sex     = rep(c("M", "F"), each = 50)
  )

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome", by = "sex"
    ),
    .fetch_github_model = mock_fetch_logistic,
    .package = "evaluatr"
  )

  expect_true("shuffled_by" %in% names(result))
  expect_equal(length(result$shuffled_by), nrow(df))
  expect_setequal(unique(result$shuffled_by), c("M", "F"))
  expect_equal(info <- result$model_info$by_variable, "sex")
})

test_that("outcome_predictions matrix has correct dimensions", {
  df <- make_validation_data(n = 80)

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_logistic,
    .package = "evaluatr"
  )

  mat <- result$shuffled_outcome_predictions
  expect_equal(nrow(mat), 80)
  expect_equal(ncol(mat), 2)
  expect_equal(colnames(mat), c("outcome", "prediction"))
})

test_that("outcome values are preserved after shuffling", {
  df <- make_validation_data()

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_logistic,
    .package = "evaluatr"
  )

  # Same set of outcomes, just reordered
  expect_equal(sort(result$shuffled_outcomes), sort(df$outcome))
})


# ==== Multinomial path ========================================================

# Helper: build multinomial validation data (outcome is integer 0/1/2)
make_multinomial_data <- function(n = 100, seed = 4283) {
  set.seed(seed)
  data.frame(
    x1      = rnorm(n),
    x2      = rnorm(n),
    outcome = sample(0:2, n, replace = TRUE)
  )
}

# Helper: manually compute expected multinomial probabilities
compute_expected_multinomial <- function(df) {
  z_B <- -0.5 + 0.3 * df$x1 + 0.2 * df$x2
  z_C <-  0.5 + 0.1 * df$x1 - 0.4 * df$x2
  denom <- 1 + exp(z_B) + exp(z_C)
  cbind(class_A = 1 / denom, class_B = exp(z_B) / denom, class_C = exp(z_C) / denom)
}

test_that("multinomial pipeline returns prediction_matrix with multiple columns", {
  df <- make_multinomial_data()

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_multinomial,
    .package = "evaluatr"
  )

  expect_type(result, "list")
  expect_true("prediction_matrix" %in% names(result))
  expect_true("full_shuffled_matrix" %in% names(result))
  expect_equal(nrow(result$prediction_matrix), nrow(df))
  expect_equal(ncol(result$prediction_matrix), 3)
  expect_equal(colnames(result$prediction_matrix), c("class_A", "class_B", "class_C"))
})

test_that("multinomial predictions are valid probabilities summing to 1", {
  df <- make_multinomial_data()

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_multinomial,
    .package = "evaluatr"
  )

  pm <- result$prediction_matrix
  # All probabilities in [0, 1]
  expect_true(all(pm >= 0 & pm <= 1))
  # Each row sums to 1
  row_sums <- rowSums(pm)
  expect_equal(row_sums, rep(1, nrow(pm)), tolerance = 1e-10)
})

test_that("multinomial predictions match manual calculation (after sorting)", {
  df <- make_multinomial_data()

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_multinomial,
    .package = "evaluatr"
  )

  expected <- compute_expected_multinomial(df)

  # Sort both by the first column to compare (output is shuffled)
  result_sorted   <- result$prediction_matrix[order(result$prediction_matrix[, 1]), ]
  expected_sorted <- expected[order(expected[, 1]), ]
  expect_equal(result_sorted, expected_sorted, tolerance = 1e-10)
})

test_that("multinomial output does NOT contain shuffled_predictions (binary field)", {
  df <- make_multinomial_data()

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_multinomial,
    .package = "evaluatr"
  )

  # Binary-specific fields should be absent in multinomial path
  expect_false("shuffled_predictions" %in% names(result))
  expect_false("shuffled_outcome_predictions" %in% names(result))
})

test_that("multinomial model_info has correct metadata", {
  df <- make_multinomial_data()

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_multinomial,
    .package = "evaluatr"
  )

  info <- result$model_info
  expect_equal(info$model_type, "multinomial")
  expect_equal(info$model_name, "Mock Multinomial")
  expect_equal(info$required_variables, c("x1", "x2"))
  expect_equal(info$n_predictions, nrow(df))
  expect_equal(info$prediction_columns, c("class_A", "class_B", "class_C"))
})

test_that("multinomial outcomes are preserved after shuffling", {
  df <- make_multinomial_data()

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_multinomial,
    .package = "evaluatr"
  )

  expect_equal(sort(result$shuffled_outcomes), sort(df$outcome))
})

test_that("multinomial full_shuffled_matrix has correct dimensions", {
  df <- make_multinomial_data(n = 80)

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome"
    ),
    .fetch_github_model = mock_fetch_multinomial,
    .package = "evaluatr"
  )

  # full_shuffled_matrix = outcome column + 3 prediction columns = 4
  fsm <- result$full_shuffled_matrix
  expect_equal(nrow(fsm), 80)
  expect_equal(ncol(fsm), 4)
})

test_that("multinomial with 'by' includes shuffled_by in output", {
  set.seed(6199)
  df <- data.frame(
    x1      = rnorm(100),
    x2      = rnorm(100),
    outcome = sample(0:2, 100, replace = TRUE),
    center  = rep(c("site_1", "site_2"), each = 50)
  )

  result <- with_mocked_bindings(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "fake_token", validation_data = df,
      outcome = "outcome", by = "center"
    ),
    .fetch_github_model = mock_fetch_multinomial,
    .package = "evaluatr"
  )

  expect_true("shuffled_by" %in% names(result))
  expect_equal(length(result$shuffled_by), 100)
  expect_setequal(unique(result$shuffled_by), c("site_1", "site_2"))

  # full_shuffled_matrix = outcome + by + 3 predictions = 5
  expect_equal(ncol(result$full_shuffled_matrix), 5)
})

test_that("multinomial LP loop is skipped (coefficients used only in pred fn)", {

  # If the LP loop ran for multinomial, it would fail because the coefficient

  # names (z_B_intercept, etc.) don't match the variable names (x1, x2).
  # The fact that this works at all confirms the LP loop is correctly skipped.
  df <- make_multinomial_data()

  expect_no_error(
    with_mocked_bindings(
      secure_model_validation(
        repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
        github_token = "fake_token", validation_data = df,
        outcome = "outcome"
      ),
      .fetch_github_model = mock_fetch_multinomial,
      .package = "evaluatr"
    )
  )
})
