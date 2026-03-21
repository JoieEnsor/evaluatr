# Tests for R/key_service.R
#
# .register_model_with_key_service() and .fetch_decryption_key() make HTTP
# calls to the evaluatr key service. All HTTP calls are mocked here using
# with_mocked_bindings() on curl::curl_fetch_memory so that no network
# access is required.
#
# Integration tests for secure_model_validation() and generate_model_json()
# with key service calls mocked are also included here.

library(jsonlite)

# ============================================================
# Helpers: build mock curl responses
# ============================================================

make_curl_response <- function(status_code, body_list) {
  list(
    status_code = status_code,
    content     = charToRaw(jsonlite::toJSON(body_list, auto_unbox = TRUE))
  )
}

dummy_key <- paste0(rep("a", 64), collapse = "")  # 64 'a's

# ============================================================
# .register_model_with_key_service() unit tests
# ============================================================

test_that("successful registration returns 64-char hex encryption_key", {
  mock_response <- make_curl_response(200, list(
    encryption_key = dummy_key,
    registered_at  = "2026-03-19T12:00:00Z"
  ))

  result <- with_mocked_bindings(
    .register_model_with_key_service(
      model_id        = "test_model_001",
      developer_id    = "JoieEnsor",
      model_name      = "Test Model",
      obfuscation_key = paste0(rep("a", 32), collapse = "")
    ),
    curl_fetch_memory = function(url, handle) mock_response,
    .package = "curl"
  )

  expect_type(result, "list")
  expect_true(!is.null(result$encryption_key))
  expect_equal(nchar(result$encryption_key), 64)
  expect_equal(result$encryption_key, dummy_key)
  expect_equal(result$registered_at, "2026-03-19T12:00:00Z")
})

test_that("duplicate model_id (409) stops with informative error", {
  mock_response <- make_curl_response(409, list(
    error = "Model 'test_model_001' is already registered. Use a different model_id or contact the maintainer."
  ))

  expect_error(
    with_mocked_bindings(
      .register_model_with_key_service(
        model_id        = "test_model_001",
        developer_id    = "JoieEnsor",
        model_name      = "Test Model",
        obfuscation_key = paste0(rep("a", 32), collapse = "")
      ),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "already registered"
  )
})

test_that("non-200 non-409 response from /register stops with error", {
  mock_response <- make_curl_response(500, list(error = "Internal server error"))

  expect_error(
    with_mocked_bindings(
      .register_model_with_key_service(
        model_id        = "test_model_001",
        developer_id    = "JoieEnsor",
        model_name      = "Test Model",
        obfuscation_key = paste0(rep("a", 32), collapse = "")
      ),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "registration failed"
  )
})

test_that("unreachable service stops with informative error", {
  expect_error(
    with_mocked_bindings(
      .register_model_with_key_service(
        model_id        = "test_model_001",
        developer_id    = "JoieEnsor",
        model_name      = "Test Model",
        obfuscation_key = paste0(rep("a", 32), collapse = "")
      ),
      curl_fetch_memory = function(url, handle) stop("Could not connect"),
      .package = "curl"
    ),
    "key service unreachable"
  )
})

test_that("registration fails with short key (< 64 chars) from service", {
  mock_response <- make_curl_response(200, list(
    encryption_key = "tooshort",
    registered_at  = "2026-03-19T12:00:00Z"
  ))

  expect_error(
    with_mocked_bindings(
      .register_model_with_key_service(
        model_id        = "test_model_001",
        developer_id    = "JoieEnsor",
        model_name      = "Test Model",
        obfuscation_key = paste0(rep("a", 32), collapse = "")
      ),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "invalid encryption key"
  )
})

# ============================================================
# .fetch_decryption_key() unit tests
# ============================================================

test_that("successful key fetch returns list with encryption_key and obfuscation_key", {
  dummy_obf_key <- paste0(rep("b", 32), collapse = "")
  mock_response <- make_curl_response(200, list(
    encryption_key  = dummy_key,
    obfuscation_key = dummy_obf_key
  ))

  result <- with_mocked_bindings(
    .fetch_decryption_key(model_id = "test_model_001", n = 500L),
    curl_fetch_memory = function(url, handle) mock_response,
    .package = "curl"
  )

  expect_type(result, "list")
  expect_equal(nchar(result$encryption_key), 64)
  expect_equal(result$encryption_key, dummy_key)
  expect_equal(nchar(result$obfuscation_key), 32)
  expect_equal(result$obfuscation_key, dummy_obf_key)
})

test_that("unknown model_id (404) stops with informative error", {
  mock_response <- make_curl_response(404, list(
    error = "Model 'unknown_model' not found."
  ))

  expect_error(
    with_mocked_bindings(
      .fetch_decryption_key(model_id = "unknown_model", n = 100L),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "not registered"
  )
})

test_that("non-200 non-404 response from /key stops with error", {
  mock_response <- make_curl_response(503, list(error = "Service unavailable"))

  expect_error(
    with_mocked_bindings(
      .fetch_decryption_key(model_id = "test_model_001", n = 100L),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "key service error"
  )
})

test_that("unreachable service at /key stops with informative error", {
  expect_error(
    with_mocked_bindings(
      .fetch_decryption_key(model_id = "test_model_001", n = 100L),
      curl_fetch_memory = function(url, handle) stop("Connection refused"),
      .package = "curl"
    ),
    "key service unreachable"
  )
})

test_that("key fetch fails with short encryption_key (< 64 chars) from service", {
  mock_response <- make_curl_response(200, list(
    encryption_key  = "bad",
    obfuscation_key = paste0(rep("b", 32), collapse = "")
  ))

  expect_error(
    with_mocked_bindings(
      .fetch_decryption_key(model_id = "test_model_001", n = 100L),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "invalid encryption key"
  )
})

test_that("key fetch fails with missing or short obfuscation_key from service", {
  mock_response <- make_curl_response(200, list(
    encryption_key  = dummy_key,
    obfuscation_key = "tooshort"
  ))

  expect_error(
    with_mocked_bindings(
      .fetch_decryption_key(model_id = "test_model_001", n = 100L),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "invalid obfuscation key"
  )
})

# ============================================================
# Integration: secure_model_validation() with key service mocked
# ============================================================

# Reuse the mock helpers from test-mock_fetch.R inline
make_mock_logistic_encoded_ks <- function() {
  obj <- list(
    model_type       = "logistic",
    obfuscation_key  = NULL,
    coefficients     = list(`(Intercept)` = -1, x1 = 0.5, x2 = 0.8),
    preprocessing    = NULL,
    model_parameters = NULL,
    metadata = list(
      model_name   = "Mock Logistic KS",
      version      = "1.0",
      outcome_type = "binary",
      variables    = c("x1", "x2"),
      description  = "Mock logistic for key-service tests"
    )
  )
  jsonlite::base64_enc(jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null"))
}

mock_fetch_logistic_ks <- function(api_url, token) {
  list(encoded_content = make_mock_logistic_encoded_ks(),
       http_status = 200, success = TRUE)
}

mock_fetch_decryption_key_ok <- function(model_id, n) {
  list(
    encryption_key  = dummy_key,
    obfuscation_key = paste0(rep("b", 32), collapse = "")
  )
}

test_that("secure_model_validation() succeeds with key service mocked", {
  set.seed(1234)
  df <- data.frame(
    x1      = rnorm(50),
    x2      = rnorm(50),
    outcome = rbinom(50, 1, 0.4)
  )

  result <- with_mocked_bindings(
    with_mocked_bindings(
      secure_model_validation(
        repo_owner      = "fake",
        repo_name       = "repo",
        model_id        = "test_model",
        github_token    = "fake_token",
        validation_data = df,
        outcome         = "outcome"
      ),
      .fetch_github_model    = mock_fetch_logistic_ks,
      .package = "evaluatr"
    ),
    .fetch_decryption_key = mock_fetch_decryption_key_ok,
    .package = "evaluatr"
  )

  expect_type(result, "list")
  expect_true("shuffled_outcomes" %in% names(result))
  expect_true("shuffled_predictions" %in% names(result))
  expect_equal(length(result$shuffled_predictions), nrow(df))
})

test_that("secure_model_validation() propagates key service error", {
  set.seed(1234)
  df <- data.frame(
    x1      = rnorm(50),
    x2      = rnorm(50),
    outcome = rbinom(50, 1, 0.4)
  )

  expect_error(
    with_mocked_bindings(
      with_mocked_bindings(
        secure_model_validation(
          repo_owner      = "fake",
          repo_name       = "repo",
          model_id        = "unregistered_model",
          github_token    = "fake_token",
          validation_data = df,
          outcome         = "outcome"
        ),
        .fetch_github_model = mock_fetch_logistic_ks,
        .package = "evaluatr"
      ),
      .fetch_decryption_key = function(model_id, n) {
        stop("evaluatr key service: model 'unregistered_model' is not registered.")
      },
      .package = "evaluatr"
    ),
    "not registered"
  )
})

# ============================================================
# Integration: generate_model_json() with key service mocked
# ============================================================

mock_register_ok <- function(model_id, developer_id, model_name,
                             obfuscation_key) {
  list(
    encryption_key = dummy_key,
    registered_at  = "2026-03-19T12:00:00Z"
  )
}

test_that("generate_model_json() succeeds with registration mocked", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  result <- with_mocked_bindings(
    generate_model_json(
      coefficients = c("(Intercept)" = -1.25, age = 0.02, score = 0.8),
      model_type   = "logistic",
      model_id     = "test_json_001",
      developer_id = "JoieEnsor",
      model_name   = "Test JSON Model",
      outcome_type = "binary",
      variables    = c("age", "score"),
      output_dir   = tmp_dir
    ),
    .register_model_with_key_service = mock_register_ok,
    .package = "evaluatr"
  )

  expect_type(result, "list")
  expect_equal(result$model_type, "logistic")
  expect_true(file.exists(file.path(tmp_dir, "coefficients.json")))
})

test_that("generate_model_json() propagates registration error", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_error(
    with_mocked_bindings(
      generate_model_json(
        coefficients = c("(Intercept)" = -1.25, age = 0.02),
        model_type   = "logistic",
        model_id     = "duplicate_model",
        developer_id = "JoieEnsor",
        model_name   = "Duplicate Model",
        outcome_type = "binary",
        variables    = c("age"),
        output_dir   = tmp_dir
      ),
      .register_model_with_key_service = function(model_id, developer_id,
                                                     model_name, obfuscation_key) {
        stop("evaluatr key service: model_id 'duplicate_model' is already registered.")
      },
      .package = "evaluatr"
    ),
    "already registered"
  )
})

test_that("generate_model_json() requires developer_id", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_error(
    generate_model_json(
      coefficients = c("(Intercept)" = -1.25, age = 0.02),
      model_type   = "logistic",
      model_id     = "test_001",
      developer_id = NULL,
      model_name   = "Test Model",
      outcome_type = "binary",
      variables    = c("age"),
      output_dir   = tmp_dir
    ),
    "developer_id is required"
  )
})

test_that("generate_model_json() rejects empty developer_id string", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_error(
    generate_model_json(
      coefficients = c("(Intercept)" = -1.25, age = 0.02),
      model_type   = "logistic",
      model_id     = "test_001",
      developer_id = "  ",
      model_name   = "Test Model",
      outcome_type = "binary",
      variables    = c("age"),
      output_dir   = tmp_dir
    ),
    "developer_id is required"
  )
})
