# Tests for the full secure_model_validation() pipeline using a mocked
# .fetch_github_model(). This avoids needing a live GitHub token while
# exercising the C++ decode → de-obfuscate → predict → shuffle pipeline
# and the evaluatr_result output assembly.
#
# All mock functions now return encoded_content (base64-encoded v1 JSON),
# matching what the real .fetch_github_model() returns after the Phase 1
# refactor.

library(jsonlite)

# ============================================================
# Helpers: build base64-encoded v1 JSON strings
# ============================================================

# True model: logit(p) = -1 + 0.5*x1 + 0.8*x2
make_mock_logistic_encoded <- function() {
  obj <- list(
    model_type      = "logistic",
    obfuscation_key = NULL,
    coefficients    = list(`(Intercept)` = -1, x1 = 0.5, x2 = 0.8),
    preprocessing   = NULL,
    model_parameters = NULL,
    metadata = list(
      model_name   = "Mock Logistic",
      version      = "1.0",
      outcome_type = "binary",
      variables    = c("x1", "x2"),
      description  = "Mock logistic model"
    )
  )
  base64_enc(toJSON(obj, auto_unbox = TRUE, null = "null"))
}

# Custom prediction: logit(p) = 0 + 1*x1 (no intercept in LP, uses raw coeff)
make_mock_custom_encoded <- function() {
  obj <- list(
    model_type      = "logistic",
    obfuscation_key = NULL,
    coefficients    = list(`(Intercept)` = 0, x1 = 1),
    preprocessing   = NULL,
    model_parameters = NULL,
    metadata = list(
      model_name   = "Mock Custom",
      version      = "1.0",
      outcome_type = "binary",
      variables    = c("x1"),
      description  = "Mock custom model"
    )
  )
  base64_enc(toJSON(obj, auto_unbox = TRUE, null = "null"))
}

# Missing variable mock
make_mock_missing_var_encoded <- function() {
  obj <- list(
    model_type      = "logistic",
    obfuscation_key = NULL,
    coefficients    = list(`(Intercept)` = 0, x1 = 1, x_missing = 2),
    preprocessing   = NULL,
    model_parameters = NULL,
    metadata = list(
      model_name   = "Mock Missing Var",
      version      = "1.0",
      outcome_type = "binary",
      variables    = c("x1", "x_missing"),
      description  = "Mock with missing variable"
    )
  )
  base64_enc(toJSON(obj, auto_unbox = TRUE, null = "null"))
}

# Multinomial mock: reference = cat_A, non-reference: cat_B, cat_C
# cat_B: (Intercept)=-0.5, x1=0.3, x2=0.2
# cat_C: (Intercept)= 0.5, x1=0.1, x2=-0.4
make_mock_multinomial_encoded <- function() {
  obj <- list(
    model_type      = "multinomial",
    obfuscation_key = NULL,
    coefficients    = list(
      cat_B = list(`(Intercept)` = -0.5, x1 = 0.3, x2 = 0.2),
      cat_C = list(`(Intercept)` =  0.5, x1 = 0.1, x2 = -0.4)
    ),
    preprocessing   = NULL,
    model_parameters = NULL,
    metadata = list(
      model_name   = "Mock Multinomial",
      version      = "1.0",
      outcome_type = "multinomial",
      variables    = c("x1", "x2"),
      description  = "Mock multinomial model"
    )
  )
  base64_enc(toJSON(obj, auto_unbox = TRUE, null = "null"))
}

# ============================================================
# Mock fetch functions (return encoded_content, not parsed JSON)
# ============================================================

mock_fetch_logistic <- function(api_url, token) {
  list(encoded_content = make_mock_logistic_encoded(),
       http_status = 200, success = TRUE)
}

mock_fetch_custom_pred <- function(api_url, token) {
  list(encoded_content = make_mock_custom_encoded(),
       http_status = 200, success = TRUE)
}

mock_fetch_error <- function(api_url, token) {
  list(error = "Model not found (404). Check repo_owner, repo_name, and model_id.",
       http_status = 404)
}

mock_fetch_missing_var <- function(api_url, token) {
  list(encoded_content = make_mock_missing_var_encoded(),
       http_status = 200, success = TRUE)
}

mock_fetch_multinomial <- function(api_url, token) {
  list(encoded_content = make_mock_multinomial_encoded(),
       http_status = 200, success = TRUE)
}

# Mock for .fetch_decryption_key — Worker A now returns only the encryption key.
# The obfuscation key and salts are fetched by C++ from Worker B (Phase 2).
# For v0/v1 mock JSONs (no encryption), the encryption_key value is unused.
mock_fetch_key <- function(model_id, n, github_token, repo_owner, repo_name) {
  list(
    encryption_key = paste0(rep("b", 64), collapse = ""),
    validation_id  = "1"
  )
}

# Helper: run secure_model_validation() with both the GitHub fetch and the
# key service call mocked. Used by all tests in this file.
with_mocked_smv <- function(expr, fetch_fn) {
  with_mocked_bindings(
    with_mocked_bindings(
      expr,
      .fetch_github_model   = fetch_fn,
      .package = "evaluatr"
    ),
    .fetch_decryption_key = mock_fetch_key,
    .package = "evaluatr"
  )
}

# ============================================================
# Helpers: validation datasets
# ============================================================

make_validation_data <- function(n = 100, seed = 5831) {
  set.seed(seed)
  data.frame(
    x1      = rnorm(n),
    x2      = rnorm(n),
    outcome = rbinom(n, 1, 0.4)
  )
}

make_multinomial_data <- function(n = 100, seed = 4283) {
  set.seed(seed)
  data.frame(
    x1      = rnorm(n),
    x2      = rnorm(n),
    outcome = sample(0:2, n, replace = TRUE)
  )
}

compute_expected_multinomial <- function(df) {
  z_B   <- -0.5 + 0.3 * df$x1 + 0.2 * df$x2
  z_C   <-  0.5 + 0.1 * df$x1 - 0.4 * df$x2
  denom <- 1 + exp(z_B) + exp(z_C)
  cbind(reference = 1 / denom,
        cat_B     = exp(z_B) / denom,
        cat_C     = exp(z_C) / denom)
}

# ============================================================
# Binary / logistic tests
# ============================================================

test_that("full pipeline works with mocked logistic model", {
  df <- make_validation_data()

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_logistic
  )

  expect_type(result, "list")
  expect_true("shuffled_outcomes"     %in% names(result))
  expect_true("shuffled_predictions"  %in% names(result))
  expect_true("model_info"            %in% names(result))
  expect_equal(length(result$shuffled_outcomes),    nrow(df))
  expect_equal(length(result$shuffled_predictions), nrow(df))
  expect_true(all(result$shuffled_predictions >= 0))
  expect_true(all(result$shuffled_predictions <= 1))
})

test_that("predictions are correct (match manual LP calculation)", {
  df <- make_validation_data()

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_logistic
  )

  lp_expected <- -1 + 0.5 * df$x1 + 0.8 * df$x2
  p_expected  <- 1 / (1 + exp(-lp_expected))

  expect_equal(sort(result$shuffled_predictions), sort(p_expected),
               tolerance = 1e-6)
})

test_that("output is shuffled (not in original row order)", {
  df <- make_validation_data(n = 200, seed = 3344)

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_logistic
  )

  lp_expected <- -1 + 0.5 * df$x1 + 0.8 * df$x2
  p_expected  <- 1 / (1 + exp(-lp_expected))
  expect_false(all(result$shuffled_predictions == p_expected))
})

test_that("model_info contains expected metadata", {
  df <- make_validation_data()

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_logistic
  )

  info <- result$model_info
  expect_equal(info$model_id,    "mock")
  expect_equal(info$model_name,  "Mock Logistic")
  expect_equal(info$version,     "1.0")
  expect_equal(sort(info$required_variables), sort(c("x1", "x2")))
  expect_equal(info$n_predictions, nrow(df))
  expect_s3_class(info$validation_timestamp, "POSIXct")
})

test_that("GitHub fetch error propagates as a stop()", {
  df <- make_validation_data()

  expect_error(
    with_mocked_smv(
      secure_model_validation(
        repo_owner = "fake", repo_name = "repo", model_id = "mock",
        github_token = "", validation_data = df,
        outcome = "outcome"
      ),
      fetch_fn = mock_fetch_error
    ),
    "Model not found"
  )
})

test_that("missing required variable in data is caught after fetch", {
  df <- make_validation_data()

  expect_error(
    with_mocked_smv(
      secure_model_validation(
        repo_owner = "fake", repo_name = "repo", model_id = "mock",
        github_token = "", validation_data = df,
        outcome = "outcome"
      ),
      fetch_fn = mock_fetch_missing_var
    ),
    "Missing required variables.*x_missing"
  )
})

test_that("model with intercept=0 and x1 coeff produces correct predictions", {
  set.seed(7621)
  df <- data.frame(
    x1      = rnorm(50),
    outcome = rbinom(50, 1, 0.5)
  )

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_custom_pred
  )

  p_expected <- 1 / (1 + exp(-(0 + 1 * df$x1)))
  expect_equal(sort(result$shuffled_predictions), sort(p_expected),
               tolerance = 1e-6)
})

test_that("subgroup ('by') column is included in shuffled output", {
  set.seed(4412)
  df <- data.frame(
    x1      = rnorm(100),
    x2      = rnorm(100),
    outcome = rbinom(100, 1, 0.4),
    sex     = rep(c("M", "F"), each = 50)
  )

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "", validation_data = df,
      outcome = "outcome", by = "sex"
    ),
    fetch_fn = mock_fetch_logistic
  )

  expect_true("shuffled_by" %in% names(result))
  expect_equal(length(result$shuffled_by), nrow(df))
  expect_setequal(unique(result$shuffled_by), c("M", "F"))
  expect_equal(result$model_info$by_variable, "sex")
})

test_that("outcome_predictions matrix has correct dimensions", {
  df <- make_validation_data(n = 80)

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_logistic
  )

  mat <- result$shuffled_outcome_predictions
  expect_equal(nrow(mat), 80)
  expect_equal(ncol(mat), 2)
  expect_equal(colnames(mat), c("outcome", "prediction"))
})

test_that("outcome values are preserved after shuffling", {
  df <- make_validation_data()

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_logistic
  )

  expect_equal(sort(result$shuffled_outcomes), sort(df$outcome))
})

# ============================================================
# Multinomial tests
# ============================================================

test_that("multinomial pipeline returns prediction_matrix with multiple columns", {
  df <- make_multinomial_data()

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_multinomial
  )

  expect_type(result, "list")
  expect_true("prediction_matrix"    %in% names(result))
  expect_true("full_shuffled_matrix" %in% names(result))
  expect_equal(nrow(result$prediction_matrix), nrow(df))
  expect_equal(ncol(result$prediction_matrix), 3)
  # C++ multinomial: col 1 = reference, col 2 = cat_B, col 3 = cat_C
  expect_equal(colnames(result$prediction_matrix),
               c("reference", "cat_B", "cat_C"))
})

test_that("multinomial predictions are valid probabilities summing to 1", {
  df <- make_multinomial_data()

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_multinomial
  )

  pm <- result$prediction_matrix
  expect_true(all(pm >= 0 & pm <= 1))
  expect_equal(rowSums(pm), rep(1, nrow(pm)), tolerance = 1e-10)
})

test_that("multinomial predictions match manual softmax (after sorting)", {
  df <- make_multinomial_data()

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_multinomial
  )

  expected <- compute_expected_multinomial(df)

  # Predictions are shuffled — compare sorted reference-category column
  expect_equal(sort(result$prediction_matrix[, "reference"]),
               sort(expected[, "reference"]), tolerance = 1e-6)
  expect_equal(sort(result$prediction_matrix[, "cat_B"]),
               sort(expected[, "cat_B"]), tolerance = 1e-6)
  expect_equal(sort(result$prediction_matrix[, "cat_C"]),
               sort(expected[, "cat_C"]), tolerance = 1e-6)
})

test_that("multinomial output does NOT contain shuffled_predictions (binary field)", {
  df <- make_multinomial_data()

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_multinomial
  )

  expect_false("shuffled_predictions"         %in% names(result))
  expect_false("shuffled_outcome_predictions" %in% names(result))
})

test_that("multinomial model_info has correct metadata", {
  df <- make_multinomial_data()

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_multinomial
  )

  info <- result$model_info
  expect_equal(info$model_type,    "multinomial")
  expect_equal(info$model_name,    "Mock Multinomial")
  expect_equal(sort(info$required_variables), sort(c("x1", "x2")))
  expect_equal(info$n_predictions, nrow(df))
})

test_that("multinomial outcomes are preserved after shuffling", {
  df <- make_multinomial_data()

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_multinomial
  )

  expect_equal(sort(result$shuffled_outcomes), sort(df$outcome))
})

test_that("multinomial full_shuffled_matrix has correct dimensions", {
  df <- make_multinomial_data(n = 80)

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "", validation_data = df,
      outcome = "outcome"
    ),
    fetch_fn = mock_fetch_multinomial
  )

  # full_shuffled_matrix: outcome column + 3 prediction columns = 4
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

  result <- with_mocked_smv(
    secure_model_validation(
      repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
      github_token = "", validation_data = df,
      outcome = "outcome", by = "center"
    ),
    fetch_fn = mock_fetch_multinomial
  )

  expect_true("shuffled_by" %in% names(result))
  expect_equal(length(result$shuffled_by), 100)
  expect_setequal(unique(result$shuffled_by), c("site_1", "site_2"))

  # full_shuffled_matrix: outcome + by + 3 predictions = 5
  expect_equal(ncol(result$full_shuffled_matrix), 5)
})

test_that("multinomial LP loop is skipped (C++ handles prediction internally)", {
  # C++ engine handles the full prediction pipeline; the R code no longer
  # runs an LP loop at all — this test confirms no error occurs for a
  # multinomial model.
  df <- make_multinomial_data()

  expect_no_error(
    with_mocked_smv(
      secure_model_validation(
        repo_owner = "fake", repo_name = "repo", model_id = "mock_multi",
        github_token = "", validation_data = df,
        outcome = "outcome"
      ),
      fetch_fn = mock_fetch_multinomial
    )
  )
})
