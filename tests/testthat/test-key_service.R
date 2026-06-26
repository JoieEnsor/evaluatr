# Tests for R/key_service.R
#
# .register_model_with_key_service() and .fetch_decryption_key() make HTTP
# calls to the evaluatr key service. All HTTP calls are mocked here using
# with_mocked_bindings() on curl::curl_fetch_memory so that no network
# access is required.
#
# Integration tests for secure_model_validation() and register_model()
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

dummy_key    <- paste0(rep("a", 64), collapse = "")  # 64 'a's
dummy_salt_a <- paste0(rep("c", 16), collapse = "")  # 16 'c's
dummy_salt_b <- paste0(rep("d", 16), collapse = "")  # 16 'd's

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
      obfuscation_key = paste0(rep("a", 32), collapse = ""),
      salt_a          = dummy_salt_a,
      salt_b          = dummy_salt_b
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
        obfuscation_key = paste0(rep("a", 32), collapse = ""),
        salt_a          = dummy_salt_a,
        salt_b          = dummy_salt_b
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
        obfuscation_key = paste0(rep("a", 32), collapse = ""),
        salt_a          = dummy_salt_a,
        salt_b          = dummy_salt_b
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
        obfuscation_key = paste0(rep("a", 32), collapse = ""),
        salt_a          = dummy_salt_a,
        salt_b          = dummy_salt_b
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
        obfuscation_key = paste0(rep("a", 32), collapse = ""),
        salt_a          = dummy_salt_a,
        salt_b          = dummy_salt_b
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

test_that("successful key fetch returns list with encryption_key only", {
  mock_response <- make_curl_response(200, list(
    encryption_key = dummy_key
  ))

  result <- with_mocked_bindings(
    .fetch_decryption_key(
      model_id     = "test_model_001",
      n            = 500L,
      github_token = "fake_token",
      repo_owner   = "JoieEnsor",
      repo_name    = "evaluatr_testing_environment"
    ),
    curl_fetch_memory = function(url, handle) mock_response,
    .package = "curl"
  )

  expect_type(result, "list")
  expect_equal(nchar(result$encryption_key), 64)
  expect_equal(result$encryption_key, dummy_key)
  expect_null(result$obfuscation_key)  # obf key no longer returned by Worker A
})

test_that("unknown model_id (404) stops with informative error", {
  mock_response <- make_curl_response(404, list(
    error = "Model 'unknown_model' not found."
  ))

  expect_error(
    with_mocked_bindings(
      .fetch_decryption_key(
        model_id     = "unknown_model",
        n            = 100L,
        github_token = "fake_token",
        repo_owner   = "JoieEnsor",
        repo_name    = "evaluatr_testing_environment"
      ),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "not registered"
  )
})

test_that("invalid GitHub token (401) stops with informative error", {
  mock_response <- make_curl_response(401, list(
    error = "GitHub token validation failed: invalid or expired token"
  ))

  expect_error(
    with_mocked_bindings(
      .fetch_decryption_key(
        model_id     = "test_model_001",
        n            = 100L,
        github_token = "bad_token",
        repo_owner   = "JoieEnsor",
        repo_name    = "evaluatr_testing_environment"
      ),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "GitHub token validation failed"
  )
})

test_that("rate limit exceeded (429) stops with informative error", {
  mock_response <- make_curl_response(429, list(
    error = "Rate limit exceeded"
  ))

  expect_error(
    with_mocked_bindings(
      .fetch_decryption_key(
        model_id     = "test_model_001",
        n            = 100L,
        github_token = "fake_token",
        repo_owner   = "JoieEnsor",
        repo_name    = "evaluatr_testing_environment"
      ),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "rate limit exceeded"
  )
})

test_that("non-200 non-404 response from /key stops with error", {
  mock_response <- make_curl_response(503, list(error = "Service unavailable"))

  expect_error(
    with_mocked_bindings(
      .fetch_decryption_key(
        model_id     = "test_model_001",
        n            = 100L,
        github_token = "fake_token",
        repo_owner   = "JoieEnsor",
        repo_name    = "evaluatr_testing_environment"
      ),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "key service error"
  )
})

test_that("unreachable service at /key stops with informative error", {
  expect_error(
    with_mocked_bindings(
      .fetch_decryption_key(
        model_id     = "test_model_001",
        n            = 100L,
        github_token = "fake_token",
        repo_owner   = "JoieEnsor",
        repo_name    = "evaluatr_testing_environment"
      ),
      curl_fetch_memory = function(url, handle) stop("Connection refused"),
      .package = "curl"
    ),
    "key service unreachable"
  )
})

test_that("key fetch fails with short encryption_key (< 64 chars) from service", {
  mock_response <- make_curl_response(200, list(
    encryption_key = "bad"
  ))

  expect_error(
    with_mocked_bindings(
      .fetch_decryption_key(
        model_id     = "test_model_001",
        n            = 100L,
        github_token = "fake_token",
        repo_owner   = "JoieEnsor",
        repo_name    = "evaluatr_testing_environment"
      ),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "invalid encryption key"
  )
})

# ============================================================
# Integration: secure_model_validation() with key service mocked
# ============================================================

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

mock_fetch_decryption_key_ok <- function(model_id, n, github_token,
                                         repo_owner, repo_name) {
  list(encryption_key = dummy_key)
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
        github_token    = "",
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
          github_token    = "",
          validation_data = df,
          outcome         = "outcome"
        ),
        .fetch_github_model = mock_fetch_logistic_ks,
        .package = "evaluatr"
      ),
      .fetch_decryption_key = function(model_id, n, github_token,
                                       repo_owner, repo_name) {
        stop("evaluatr key service: model 'unregistered_model' is not registered.")
      },
      .package = "evaluatr"
    ),
    "not registered"
  )
})

# ============================================================
# Integration: register_model() with key service mocked
# ============================================================

mock_register_ok <- function(model_id, developer_id, model_name,
                             obfuscation_key, salt_a, salt_b, ...) {
  list(
    encryption_key = dummy_key,
    registered_at  = "2026-03-19T12:00:00Z"
  )
}

test_that("register_model() succeeds with registration mocked", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  result <- with_mocked_bindings(
    register_model(
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
  expect_true(file.exists(file.path(tmp_dir, "test_json_001_specification.json")))
})

test_that("register_model() propagates registration error", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_error(
    with_mocked_bindings(
      register_model(
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
                                                   model_name, obfuscation_key,
                                                   salt_a, salt_b, ...) {
        stop("evaluatr key service: model_id 'duplicate_model' is already registered.")
      },
      .package = "evaluatr"
    ),
    "already registered"
  )
})

test_that("register_model() requires developer_id", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_error(
    register_model(
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

test_that("register_model() rejects empty developer_id string", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_error(
    register_model(
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

# ============================================================
# .register_model_with_key_service() — registry fields
# ============================================================

test_that("registration forwards optional registry fields", {
  result <- with_mocked_bindings(
    .register_model_with_key_service(
      model_id          = "test_registry_001",
      developer_id      = "JoeBloggs",
      model_name        = "Registry Test Model",
      obfuscation_key   = paste0(rep("b", 32), collapse = ""),
      salt_a            = dummy_salt_a,
      salt_b            = dummy_salt_b,
      developer_name    = "Joe Bloggs",
      developer_email   = "j.bloggs@example.ac.uk",
      model_description = "A model for testing the registry",
      public_listing    = TRUE
    ),
    curl_fetch_memory = function(url, handle) {
      make_curl_response(200, list(
        encryption_key = dummy_key,
        registered_at  = "2026-04-10T09:00:00Z"
      ))
    },
    .package = "curl"
  )

  expect_equal(result$encryption_key, dummy_key)
})

test_that("registration with public_listing = FALSE is forwarded", {
  result <- with_mocked_bindings(
    .register_model_with_key_service(
      model_id        = "private_model_001",
      developer_id    = "JoeBloggs",
      model_name      = "Private Model",
      obfuscation_key = paste0(rep("c", 32), collapse = ""),
      salt_a          = dummy_salt_a,
      salt_b          = dummy_salt_b,
      public_listing  = FALSE
    ),
    curl_fetch_memory = function(url, handle) {
      make_curl_response(200, list(
        encryption_key = dummy_key,
        registered_at  = "2026-04-10T09:00:00Z"
      ))
    },
    .package = "curl"
  )

  expect_equal(result$encryption_key, dummy_key)
})

test_that("register_model() forwards registry fields to key service", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  result <- with_mocked_bindings(
    register_model(
      coefficients      = c("(Intercept)" = -1.25, age = 0.02),
      model_type        = "logistic",
      model_id          = "registry_test_json",
      developer_id      = "JoeBloggs",
      model_name        = "Registry JSON Model",
      outcome_type      = "binary",
      variables         = c("age"),
      developer_name    = "Joe Bloggs",
      developer_email   = "j.bloggs@example.ac.uk",
      public_listing    = TRUE,
      output_dir        = tmp_dir
    ),
    .register_model_with_key_service = function(model_id, developer_id,
                                                model_name, obfuscation_key,
                                                salt_a, salt_b,
                                                developer_name = NULL,
                                                developer_email = NULL,
                                                model_description = NULL,
                                                public_listing = TRUE) {
      expect_equal(developer_name, "Joe Bloggs")
      expect_equal(developer_email, "j.bloggs@example.ac.uk")
      expect_true(isTRUE(public_listing))
      list(encryption_key = dummy_key, registered_at = "2026-04-10T09:00:00Z")
    },
    .package = "evaluatr"
  )

  expect_type(result, "list")
  expect_true(file.exists(file.path(tmp_dir, "registry_test_json_specification.json")))
})

# ============================================================
# list_registered_models()
# ============================================================

make_public_models_response <- function() {
  list(
    n = 2L,
    models = list(
      list(
        model_id          = "aki_v1",
        model_name        = "AKI Prediction Model",
        developer_id      = "JoeBloggs",
        developer_name    = "Joe Bloggs",
        developer_email   = "j.bloggs@example.ac.uk",
        model_description = "Predicts AKI from routine bloods",
        registered_at     = "2026-04-01T10:00:00Z"
      ),
      list(
        model_id          = "adnex_v1",
        model_name        = "ADNEX Model",
        developer_id      = "KULeuven",
        developer_name    = NULL,
        developer_email   = NULL,
        model_description = "Ovarian tumour classification",
        registered_at     = "2026-04-02T11:00:00Z"
      )
    )
  )
}

test_that("list_registered_models() returns a data.frame with correct columns", {
  mock_response <- make_curl_response(200, make_public_models_response())

  result <- with_mocked_bindings(
    list_registered_models(),
    curl_fetch_memory = function(url, handle) mock_response,
    .package = "curl"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expected_cols <- c("model_id", "model_name", "developer_id",
                     "developer_name", "developer_email",
                     "model_description")
  expect_true(all(expected_cols %in% names(result)))
  expect_false("registered_at" %in% names(result))
  expect_equal(result$model_id[1], "aki_v1")
  expect_equal(result$developer_email[1], "j.bloggs@example.ac.uk")
})

test_that("list_registered_models() returns empty data.frame when no models", {
  mock_response <- make_curl_response(200, list(n = 0L, models = list()))

  result <- with_mocked_bindings(
    list_registered_models(),
    curl_fetch_memory = function(url, handle) mock_response,
    .package = "curl"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_true("model_id" %in% names(result))
})

test_that("list_registered_models() stops on HTTP error", {
  mock_response <- make_curl_response(500, list(error = "Internal server error"))

  expect_error(
    with_mocked_bindings(
      list_registered_models(),
      curl_fetch_memory = function(url, handle) mock_response,
      .package = "curl"
    ),
    "key service error"
  )
})

test_that("list_registered_models() stops when service unreachable", {
  expect_error(
    with_mocked_bindings(
      list_registered_models(),
      curl_fetch_memory = function(url, handle) stop("Connection refused"),
      .package = "curl"
    ),
    "key service unreachable"
  )
})

test_that("list_registered_models() with as_data_frame = FALSE returns raw list", {
  mock_response <- make_curl_response(200, make_public_models_response())

  result <- with_mocked_bindings(
    list_registered_models(as_data_frame = FALSE),
    curl_fetch_memory = function(url, handle) mock_response,
    .package = "curl"
  )

  expect_type(result, "list")
  expect_equal(result$n, 2L)
  expect_type(result$models, "list")
})
