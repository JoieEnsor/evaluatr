# Tests for register_model() â€” Phase 2 developer utility
#
# Tests cover:
#  1. Logistic from glm object
#  2. Logistic from manual coefficients
#  3. Multinomial from multinom object
#  4. Cox from coxph object
#  5. Weibull from survreg object
#  6. Round-trip verification (generate â†’ base64 â†’ .predict_secure())
#  7. Invalid inputs
#  8. Output file existence and valid JSON
#  9. Preprocessing preserved

library(jsonlite)

# ============================================================
# Key service mock
# ============================================================

# Mock .register_model_with_key_service() so tests do not need a live key
# service. Signature updated: now includes salt_a and salt_b (Phase 1).
mock_register_ok_gj <- function(model_id, developer_id, model_name,
                                 obfuscation_key, salt_a, salt_b, ...) {
  list(
    encryption_key = paste0(rep("c", 64), collapse = ""),
    registered_at  = "2026-03-19T12:00:00Z"
  )
}

# Helper: run register_model() with registration mocked.
with_mocked_gmj <- function(expr) {
  with_mocked_bindings(
    expr,
    .register_model_with_key_service = mock_register_ok_gj,
    .package = "evaluatr"
  )
}

# Helper: like with_mocked_gmj but captures the obfuscation_key and salts that
# register_model() sends to the key service.
# Returns a list: $result, $obfuscation_key, $salt_a, $salt_b.
with_mocked_gmj_capture <- function(expr) {
  captured_obf_key <- NULL
  captured_salt_a  <- NULL
  captured_salt_b  <- NULL
  mock_register_capture <- function(model_id, developer_id, model_name,
                                    obfuscation_key, salt_a, salt_b, ...) {
    captured_obf_key <<- obfuscation_key
    captured_salt_a  <<- salt_a
    captured_salt_b  <<- salt_b
    list(
      encryption_key = paste0(rep("c", 64), collapse = ""),
      registered_at  = "2026-03-19T12:00:00Z"
    )
  }
  result <- with_mocked_bindings(
    expr,
    .register_model_with_key_service = mock_register_capture,
    .package = "evaluatr"
  )
  list(result = result, obfuscation_key = captured_obf_key,
       salt_a = captured_salt_a, salt_b = captured_salt_b)
}

# ============================================================
# Helpers
# ============================================================

# Read and parse a JSON file produced by register_model()
read_json_file <- function(path) {
  fromJSON(readLines(path, warn = FALSE), simplifyVector = FALSE)
}

# Compute logistic predictions manually from un-obfuscated coefficients
manual_logistic_pred <- function(data, coeffs) {
  # coeffs is a named numeric vector including "(Intercept)"
  lp <- coeffs["(Intercept)"]
  for (v in names(coeffs)[names(coeffs) != "(Intercept)"]) {
    lp <- lp + coeffs[v] * data[[v]]
  }
  1 / (1 + exp(-lp))
}

# ============================================================
# Test 1: Logistic from glm object
# ============================================================

test_that("logistic from glm: JSON structure is valid and coefficients are obfuscated", {
  set.seed(42)
  n     <- 50
  df    <- data.frame(age   = rnorm(n, 60, 10),
                      score = rnorm(n, 0, 1),
                      y     = rbinom(n, 1, 0.3))
  fit   <- glm(y ~ age + score, data = df, family = binomial)
  true_coeffs <- coef(fit)

  tmp <- tempdir()
  result <- with_mocked_gmj(register_model(
    model        = fit,
    model_id     = "test_glm_001",
    developer_id = "test_developer",
    model_name   = "Test GLM Model",
    output_dir   = tmp
  ))

  # Return value is a list
  expect_type(result, "list")
  expect_equal(result$model_type, "logistic")
  expect_equal(result$metadata$model_id, "test_glm_001")
  expect_equal(result$metadata$model_name, "Test GLM Model")
  expect_equal(result$metadata$outcome_type, "binary")

  # Improvement B: obfuscation_key NOT stored in JSON â€” held by key service
  expect_true(is.null(result$obfuscation_key),
              label = "obfuscation_key absent from v1.1 JSON (held by key service)")

  # Phase 3b: encrypted format â€” no plaintext coefficients field
  expect_false("coefficients" %in% names(result),
               label = "plaintext coefficients not present in encrypted JSON")
  expect_true("encrypted_coefficients" %in% names(result))
  expect_true("encryption_iv" %in% names(result))
  expect_equal(result$metadata$encryption, "aes256gcm")

  # Variables in metadata
  expect_true("age"   %in% unlist(result$metadata$variables))
  expect_true("score" %in% unlist(result$metadata$variables))
  expect_false("(Intercept)" %in% unlist(result$metadata$variables))

  # File was written
  out_path <- file.path(tmp, "test_glm_001_specification.json")
  expect_true(file.exists(out_path))
})


# ============================================================
# Test 2: Logistic from manual coefficients
# ============================================================

test_that("logistic from manual coefficients: JSON structure is valid", {
  real_coeffs <- c("(Intercept)" = -1.25, age = 0.02,
                   biomarker_score = 0.8, treatment_group = -0.6)

  tmp <- tempdir()
  result <- with_mocked_gmj(register_model(
    coefficients = real_coeffs,
    model_type   = "logistic",
    model_id     = "test_manual_001",
    developer_id = "test_developer",
    model_name   = "Test Manual Logistic",
    outcome_type = "binary",
    variables    = c("age", "biomarker_score", "treatment_group"),
    output_dir   = tmp
  ))

  expect_equal(result$model_type, "logistic")
  expect_equal(result$metadata$model_id, "test_manual_001")
  # Improvement B: obfuscation_key absent from JSON (held by key service)
  expect_true(is.null(result$obfuscation_key))

  # Phase 3b: encrypted format
  expect_false("coefficients" %in% names(result))
  expect_true("encrypted_coefficients" %in% names(result))
  expect_equal(result$metadata$encryption, "aes256gcm")

  # Variables correct
  expect_equal(sort(unlist(result$metadata$variables)),
               sort(c("age", "biomarker_score", "treatment_group")))
})


# ============================================================
# Test 3: Multinomial from multinom object
# ============================================================

test_that("multinomial from multinom: nested coefficient structure", {
  skip_if_not_installed("nnet")
  set.seed(123)
  n  <- 90
  df <- data.frame(
    x1  = rnorm(n),
    x2  = rnorm(n),
    out = factor(rep(c("cat_A", "cat_B", "cat_C"), n / 3))
  )
  fit <- nnet::multinom(out ~ x1 + x2, data = df, trace = FALSE)

  tmp <- tempdir()
  result <- with_mocked_gmj(register_model(
    model              = fit,
    model_id           = "test_multinom_001",
    developer_id       = "test_developer",
    model_name         = "Test Multinomial",
    reference_category = "cat_A",
    output_dir         = tmp
  ))

  expect_equal(result$model_type, "multinomial")
  expect_equal(result$metadata$outcome_type, "multinomial")

  # Phase 3b: encrypted format â€” plaintext coefficients not present
  expect_false("coefficients" %in% names(result))
  expect_true("encrypted_coefficients" %in% names(result))
  expect_equal(result$metadata$encryption, "aes256gcm")

  # Variables in metadata (no intercept)
  vars <- unlist(result$metadata$variables)
  expect_true("x1" %in% vars)
  expect_true("x2" %in% vars)
  expect_false("(Intercept)" %in% vars)

  # File exists
  expect_true(file.exists(file.path(tmp, "test_multinom_001_specification.json")))
})


# ============================================================
# Test 4: Cox from coxph object
# ============================================================

test_that("cox from coxph: model_parameters preserved", {
  skip_if_not_installed("survival")
  library(survival)

  set.seed(7)
  n  <- 60
  df <- data.frame(
    time   = rexp(n, rate = 0.1),
    status = rbinom(n, 1, 0.7),
    age    = rnorm(n, 55, 10),
    bmi    = rnorm(n, 27, 4)
  )
  fit <- coxph(Surv(time, status) ~ age + bmi, data = df)

  mp  <- list(
    timepoints        = c(365, 730, 1095),
    baseline_survival = c(0.85, 0.72, 0.61)
  )
  tmp <- tempdir()
  result <- with_mocked_gmj(register_model(
    model            = fit,
    model_id         = "test_cox_001",
    developer_id     = "test_developer",
    model_name       = "Test Cox Model",
    model_parameters = mp,
    output_dir       = tmp
  ))

  expect_equal(result$model_type, "cox")
  expect_equal(result$metadata$outcome_type, "survival")

  # model_parameters preserved
  stored_mp <- result$model_parameters
  expect_equal(unlist(stored_mp$timepoints),        mp$timepoints)
  expect_equal(unlist(stored_mp$baseline_survival), mp$baseline_survival)

  # Phase 3b: encrypted format
  expect_false("coefficients" %in% names(result))
  expect_true("encrypted_coefficients" %in% names(result))
  expect_equal(result$metadata$encryption, "aes256gcm")

  # Variables correct (no intercept for Cox)
  vars <- unlist(result$metadata$variables)
  expect_true("age" %in% vars)
  expect_true("bmi" %in% vars)
})


# ============================================================
# Test 5: Weibull from survreg object
# ============================================================

test_that("weibull from survreg: shape parameter extracted and preserved", {
  skip_if_not_installed("survival")
  library(survival)

  set.seed(8)
  n  <- 60
  df <- data.frame(
    time   = rweibull(n, shape = 1.5, scale = 10),
    status = rbinom(n, 1, 0.8),
    age    = rnorm(n, 55, 10)
  )
  fit <- survreg(Surv(time, status) ~ age, data = df, dist = "weibull")

  mp  <- list(timepoints = c(5, 10, 15))
  tmp <- tempdir()
  result <- with_mocked_gmj(register_model(
    model            = fit,
    model_id         = "test_weibull_001",
    developer_id     = "test_developer",
    model_name       = "Test Weibull Model",
    model_parameters = mp,
    output_dir       = tmp
  ))

  expect_equal(result$model_type, "weibull")
  expect_equal(result$metadata$outcome_type, "survival")

  # Shape parameter present
  expect_false(is.null(result$model_parameters$shape))
  expect_true(is.numeric(unlist(result$model_parameters$shape)))
  expect_true(unlist(result$model_parameters$shape) > 0)

  # Parameterisation defaults to "aft"
  expect_equal(result$model_parameters$parameterisation, "aft")

  # Timepoints preserved
  expect_equal(unlist(result$model_parameters$timepoints), mp$timepoints)

  # Variables correct
  vars <- unlist(result$metadata$variables)
  expect_true("age" %in% vars)
})


# ============================================================
# Test 6: Round-trip verification
# ============================================================

test_that("round-trip logistic: JSON decrypts and produces valid predictions", {
  # Verifies the full generate -> encrypt -> decrypt -> predict pipeline runs
  # without error and produces structurally valid output.
  # Numeric accuracy is not checked here: de-obfuscation requires a live
  # Worker B call (github_token not available in unit tests). Numeric
  # round-trip accuracy is covered in test-predict_secure.R via unobfuscated JSON.
  real_coeffs <- c("(Intercept)" = -1.25, age = 0.02,
                   biomarker_score = 0.8, treatment_group = -0.6)

  tmp <- tempdir()
  with_mocked_gmj_capture(register_model(
    coefficients = real_coeffs,
    model_type   = "logistic",
    model_id     = "roundtrip_logistic",
    developer_id = "test_developer",
    model_name   = "Round-trip Test",
    outcome_type = "binary",
    variables    = c("age", "biomarker_score", "treatment_group"),
    output_dir   = tmp
  ))

  set.seed(99)
  n      <- 50
  val_df <- data.frame(
    age             = rnorm(n, 60, 10),
    biomarker_score = rnorm(n, 0, 1),
    treatment_group = rbinom(n, 1, 0.5),
    outcome         = rbinom(n, 1, 0.4)
  )

  json_str     <- readLines(file.path(tmp, "roundtrip_logistic_specification.json"), warn = FALSE)
  encoded      <- base64_enc(paste(json_str, collapse = "\n"))
  mock_enc_key <- paste0(rep("c", 64), collapse = "")

  expect_no_error({
    pred_result <- evaluatr:::.predict_secure(
      encoded_content = encoded,
      validation_data = val_df,
      outcome         = "outcome",
      model_id        = "roundtrip_logistic",
      decryption_key  = mock_enc_key
    )
  })
  cpp_preds <- pred_result$shuffled_predictions
  expect_equal(length(cpp_preds), n)
  expect_true(all(cpp_preds >= 0 & cpp_preds <= 1))
})


test_that("round-trip multinomial: JSON decrypts and produces valid predictions", {
  skip_if_not_installed("nnet")
  real_coeffs <- list(
    cat_B = c("(Intercept)" = -0.5, x1 = 0.3,  x2 =  0.2),
    cat_C = c("(Intercept)" =  0.5, x1 = 0.1,  x2 = -0.4)
  )

  tmp <- tempdir()
  with_mocked_gmj_capture(register_model(
    coefficients       = real_coeffs,
    model_type         = "multinomial",
    model_id           = "roundtrip_multinom",
    developer_id       = "test_developer",
    model_name         = "Round-trip Multinomial",
    outcome_type       = "multinomial",
    variables          = c("x1", "x2"),
    reference_category = "cat_A",
    output_dir         = tmp
  ))

  set.seed(77)
  n      <- 50
  val_df <- data.frame(
    x1      = rnorm(n),
    x2      = rnorm(n),
    outcome = sample(c(0, 1, 2), n, replace = TRUE)
  )

  json_str     <- readLines(file.path(tmp, "roundtrip_multinom_specification.json"), warn = FALSE)
  encoded      <- base64_enc(paste(json_str, collapse = "\n"))
  mock_enc_key <- paste0(rep("c", 64), collapse = "")

  expect_no_error({
    pred_result <- evaluatr:::.predict_secure(
      encoded_content = encoded,
      validation_data = val_df,
      outcome         = "outcome",
      model_id        = "roundtrip_multinom",
      decryption_key  = mock_enc_key
    )
  })
  cpp_preds <- pred_result$prediction_matrix
  expect_equal(nrow(cpp_preds), n)
  expect_equal(ncol(cpp_preds), 3L)  # reference + 2 categories
  # Row probabilities must sum to 1
  row_sums <- rowSums(cpp_preds)
  expect_true(all(abs(row_sums - 1) < 1e-6))
})


test_that("round-trip Cox: JSON decrypts and produces valid predictions", {
  skip_if_not_installed("survival")
  real_coeffs <- c("(Intercept)" = 0.0, age = 0.05, bmi = 0.03)
  mp <- list(
    timepoints        = c(1, 2, 5),
    baseline_survival = c(0.95, 0.90, 0.80)
  )

  tmp <- tempdir()
  with_mocked_gmj_capture(register_model(
    coefficients     = real_coeffs,
    model_type       = "cox",
    model_id         = "roundtrip_cox",
    developer_id     = "test_developer",
    model_name       = "Round-trip Cox",
    outcome_type     = "survival",
    variables        = c("age", "bmi"),
    model_parameters = mp,
    output_dir       = tmp
  ))

  set.seed(55)
  n      <- 40
  val_df <- data.frame(
    age     = rnorm(n, 55, 10),
    bmi     = rnorm(n, 27, 4),
    outcome = rbinom(n, 1, 0.5)
  )

  json_str     <- readLines(file.path(tmp, "roundtrip_cox_specification.json"), warn = FALSE)
  encoded      <- base64_enc(paste(json_str, collapse = "\n"))
  mock_enc_key <- paste0(rep("c", 64), collapse = "")

  expect_no_error({
    pred_result <- evaluatr:::.predict_secure(
      encoded_content = encoded,
      validation_data = val_df,
      outcome         = "outcome",
      model_id        = "roundtrip_cox",
      decryption_key  = mock_enc_key
    )
  })
  cpp_preds <- pred_result$prediction_matrix
  expect_equal(nrow(cpp_preds), n)
  expect_equal(ncol(cpp_preds), 3L)  # three timepoints
  # Survival probabilities must be in [0, 1]
  expect_true(all(cpp_preds >= 0 & cpp_preds <= 1))
})


test_that("round-trip Weibull AFT: JSON decrypts and produces valid predictions", {
  skip_if_not_installed("survival")
  real_coeffs <- c("(Intercept)" = 3.0, age = -0.02)
  mp <- list(
    timepoints       = c(1, 3, 5),
    shape            = 1.5,
    parameterisation = "aft"
  )

  tmp <- tempdir()
  with_mocked_gmj_capture(register_model(
    coefficients     = real_coeffs,
    model_type       = "weibull",
    model_id         = "roundtrip_weibull",
    developer_id     = "test_developer",
    model_name       = "Round-trip Weibull",
    outcome_type     = "survival",
    variables        = c("age"),
    model_parameters = mp,
    output_dir       = tmp
  ))

  set.seed(33)
  n      <- 40
  val_df <- data.frame(
    age     = rnorm(n, 60, 10),
    outcome = rbinom(n, 1, 0.6)
  )

  json_str     <- readLines(file.path(tmp, "roundtrip_weibull_specification.json"), warn = FALSE)
  encoded      <- base64_enc(paste(json_str, collapse = "\n"))
  mock_enc_key <- paste0(rep("c", 64), collapse = "")

  expect_no_error({
    pred_result <- evaluatr:::.predict_secure(
      encoded_content = encoded,
      validation_data = val_df,
      outcome         = "outcome",
      model_id        = "roundtrip_weibull",
      decryption_key  = mock_enc_key
    )
  })
  cpp_preds <- pred_result$prediction_matrix
  expect_equal(nrow(cpp_preds), n)
  expect_equal(ncol(cpp_preds), length(mp$timepoints))
  # Survival probabilities must be in [0, 1]
  expect_true(all(cpp_preds >= 0 & cpp_preds <= 1))
})


# ============================================================
# Test 7: Invalid inputs
# ============================================================

test_that("error when model_id is missing", {
  expect_error(
    register_model(
      coefficients = c("(Intercept)" = -1.25, age = 0.02),
      model_type   = "logistic",
      model_name   = "Test",
      variables    = "age"
    ),
    "model_id"
  )
})

test_that("error when model_name is missing", {
  expect_error(
    register_model(
      coefficients = c("(Intercept)" = -1.25, age = 0.02),
      model_type   = "logistic",
      model_id     = "test",
      developer_id = "test_developer",
      variables    = "age"
    ),
    "model_name"
  )
})

test_that("error when both model and coefficients are NULL", {
  expect_error(
    register_model(
      model_id     = "test",
      developer_id = "test_developer",
      model_name   = "Test"
    ),
    "One of 'model' or 'coefficients'"
  )
})

test_that("error when both model and coefficients are provided", {
  set.seed(1)
  fit <- glm(rbinom(30, 1, 0.5) ~ rnorm(30), family = binomial)
  expect_error(
    register_model(
      model        = fit,
      coefficients = c(x = 1.0),
      model_id     = "test",
      developer_id = "test_developer",
      model_name   = "Test"
    ),
    "not both"
  )
})

test_that("error for unsupported model class", {
  fit <- lm(rnorm(30) ~ rnorm(30))
  expect_error(
    register_model(
      model        = fit,
      model_id     = "test",
      developer_id = "test_developer",
      model_name   = "Test"
    ),
    "Unsupported model class"
  )
})

test_that("error for non-binomial glm", {
  fit <- glm(rpois(30, 2) ~ rnorm(30), family = poisson)
  expect_error(
    register_model(
      model        = fit,
      model_id     = "test",
      developer_id = "test_developer",
      model_name   = "Test"
    ),
    "binomial"
  )
})

test_that("error when model_type missing for manual coefficients", {
  expect_error(
    register_model(
      coefficients = c("(Intercept)" = -1.0, age = 0.02),
      model_id     = "test",
      developer_id = "test_developer",
      model_name   = "Test",
      variables    = "age"
    ),
    "model_type"
  )
})

test_that("error when variables missing for manual coefficients", {
  expect_error(
    register_model(
      coefficients = c("(Intercept)" = -1.0, age = 0.02),
      model_type   = "logistic",
      model_id     = "test",
      developer_id = "test_developer",
      model_name   = "Test"
    ),
    "variables"
  )
})

test_that("error for Cox without model_parameters", {
  expect_error(
    register_model(
      coefficients = c(age = 0.05),
      model_type   = "cox",
      model_id     = "test",
      developer_id = "test_developer",
      model_name   = "Test",
      variables    = "age"
    ),
    "timepoints"
  )
})

test_that("error for Cox without baseline_survival", {
  expect_error(
    register_model(
      coefficients     = c(age = 0.05),
      model_type       = "cox",
      model_id         = "test",
      developer_id     = "test_developer",
      model_name       = "Test",
      variables        = "age",
      model_parameters = list(timepoints = c(1, 2, 5))
    ),
    "baseline_survival"
  )
})

test_that("error for multinomial without reference_category in manual mode", {
  expect_error(
    register_model(
      coefficients = list(
        cat_B = c("(Intercept)" = -0.5, x1 = 0.3)
      ),
      model_type   = "multinomial",
      model_id     = "test",
      developer_id = "test_developer",
      model_name   = "Test",
      variables    = "x1"
    ),
    "reference_category"
  )
})

test_that("error for multinomial with non-list coefficients", {
  expect_error(
    register_model(
      coefficients       = c("(Intercept)" = -0.5, x1 = 0.3),
      model_type         = "multinomial",
      model_id           = "test",
      developer_id       = "test_developer",
      model_name         = "Test",
      variables          = "x1",
      reference_category = "cat_A"
    ),
    "named list"
  )
})


# ============================================================
# Test 8: Output file existence and valid JSON
# ============================================================

test_that("output file exists, is valid JSON, and contains all required fields", {
  real_coeffs <- c("(Intercept)" = -1.25, age = 0.02, score = 0.8)
  tmp <- tempdir()
  with_mocked_gmj(register_model(
    coefficients = real_coeffs,
    model_type   = "logistic",
    model_id     = "file_test_001",
    developer_id = "test_developer",
    model_name   = "File Test Model",
    outcome_type = "binary",
    variables    = c("age", "score"),
    output_dir   = tmp
  ))

  out_path <- file.path(tmp, "file_test_001_specification.json")
  expect_true(file.exists(out_path))

  # Parseable JSON
  parsed <- tryCatch(
    fromJSON(readLines(out_path, warn = FALSE), simplifyVector = FALSE),
    error = function(e) NULL
  )
  expect_false(is.null(parsed), label = "JSON file is valid and parseable")

  # Required top-level fields (Improvement B: obfuscation_key NOT in JSON)
  for (field in c("model_type", "encrypted_coefficients",
                  "encryption_iv", "metadata")) {
    expect_true(field %in% names(parsed),
                label = paste("field", field, "present in JSON"))
  }
  expect_false("obfuscation_key" %in% names(parsed),
               label = "obfuscation_key absent from JSON (held by key service)")
  expect_false("coefficients" %in% names(parsed),
               label = "plaintext coefficients absent from encrypted JSON")

  # Required metadata fields
  for (field in c("model_id", "model_name", "version", "outcome_type", "variables")) {
    expect_true(field %in% names(parsed$metadata),
                label = paste("metadata field", field, "present"))
  }
})

test_that("output filename is always derived from model_id", {
  real_coeffs <- c("(Intercept)" = -1.0, age = 0.02)
  tmp <- tempdir()
  with_mocked_gmj(register_model(
    coefficients = real_coeffs,
    model_type   = "logistic",
    model_id     = "filename_test",
    developer_id = "test_developer",
    model_name   = "Filename Convention Test",
    variables    = "age",
    output_dir   = tmp
  ))
  expect_true(file.exists(file.path(tmp, "filename_test_specification.json")))
})

test_that("output_dir is created if it does not exist", {
  real_coeffs <- c("(Intercept)" = -1.0, age = 0.02)
  new_dir <- file.path(tempdir(), paste0("new_dir_", as.integer(Sys.time())))
  expect_false(dir.exists(new_dir))

  with_mocked_gmj(register_model(
    coefficients = real_coeffs,
    model_type   = "logistic",
    model_id     = "dir_test",
    developer_id = "test_developer",
    model_name   = "Dir Creation Test",
    variables    = "age",
    output_dir   = new_dir
  ))

  expect_true(dir.exists(new_dir))
  expect_true(file.exists(file.path(new_dir, "dir_test_specification.json")))
})


# ============================================================
# Test 9: Preprocessing string preserved
# ============================================================

test_that("preprocessing string is written to JSON and round-trips correctly", {
  real_coeffs  <- c("(Intercept)" = -0.5, age = 0.03, sexfemale = -0.2)
  preprocessing <- "validation_data$sexfemale <- ifelse(validation_data$sex == 'female', 1, 0)"

  tmp <- tempdir()
  result <- with_mocked_gmj(register_model(
    coefficients  = real_coeffs,
    model_type    = "logistic",
    model_id      = "preproc_test",
    developer_id  = "test_developer",
    model_name    = "Preprocessing Test",
    variables     = c("age", "sexfemale"),
    preprocessing = preprocessing,
    output_dir    = tmp
  ))

  # In-memory result
  expect_equal(result$preprocessing, preprocessing)

  # In file
  parsed <- fromJSON(readLines(file.path(tmp, "preproc_test_specification.json"), warn = FALSE),
                     simplifyVector = FALSE)
  expect_equal(parsed$preprocessing, preprocessing)
})

test_that("preprocessing round-trip: .predict_secure() executes preprocessing correctly", {
  real_coeffs   <- c("(Intercept)" = -0.5, age = 0.03, sexfemale = -0.2)
  preprocessing <- "validation_data$sexfemale <- ifelse(validation_data$sex == 'female', 1, 0)"

  tmp <- tempdir()
  with_mocked_gmj_capture(register_model(
    coefficients  = real_coeffs,
    model_type    = "logistic",
    model_id      = "preproc_roundtrip",
    developer_id  = "test_developer",
    model_name    = "Preprocessing Round-trip",
    variables     = c("age", "sexfemale"),
    preprocessing = preprocessing,
    output_dir    = tmp
  ))

  set.seed(44)
  n      <- 40
  val_df <- data.frame(
    age     = rnorm(n, 60, 10),
    sex     = sample(c("male", "female"), n, replace = TRUE),
    outcome = rbinom(n, 1, 0.4)
  )
  # Note: 'sexfemale' is NOT in val_df â€” it must be created by preprocessing

  json_str    <- readLines(file.path(tmp, "preproc_roundtrip_specification.json"), warn = FALSE)
  encoded     <- base64_enc(paste(json_str, collapse = "\n"))

  # Should not error (preprocessing creates 'sexfemale')
  mock_key <- paste0(rep("c", 64), collapse = "")
  expect_no_error({
    pred_result <- evaluatr:::.predict_secure(
      encoded_content = encoded,
      validation_data = val_df,
      outcome         = "outcome",
      model_id        = "preproc_roundtrip",
      decryption_key  = mock_key
    )
  })

  cpp_preds <- pred_result$shuffled_predictions
  expect_equal(length(cpp_preds), n)
  expect_true(all(cpp_preds >= 0 & cpp_preds <= 1))
})


# ============================================================
# Phase 3b: Encryption tests
# ============================================================

test_that("Phase 3b: generated JSON has encrypted_coefficients, not plaintext", {
  real_coeffs <- c("(Intercept)" = -1.25, age = 0.02, score = 0.8)
  tmp <- tempdir()
  result <- with_mocked_gmj(register_model(
    coefficients = real_coeffs,
    model_type   = "logistic",
    model_id     = "enc_test_001",
    developer_id = "test_developer",
    model_name   = "Encryption Test",
    outcome_type = "binary",
    variables    = c("age", "score"),
    output_dir   = tmp
  ))

  expect_true("encrypted_coefficients" %in% names(result))
  expect_true("encryption_iv" %in% names(result))
  expect_false("coefficients" %in% names(result))
  expect_equal(result$metadata$encryption, "aes256gcm")

  # encrypted_coefficients should be a non-empty base64 string
  expect_true(nzchar(result$encrypted_coefficients))
  expect_true(nzchar(result$encryption_iv))
})


test_that("Phase 3b: .validate_json_structure() accepts encrypted format", {
  # Improvement B: obfuscation_key is NOT in the encrypted JSON
  enc_list <- list(
    model_type             = "logistic",
    encrypted_coefficients = "base64ciphertexthere",
    encryption_iv          = "base64ivhere",
    preprocessing          = NULL,
    model_parameters       = NULL,
    metadata               = list(
      model_id     = "enc_valid_test",
      model_name   = "Enc Valid Test",
      version      = "1.0",
      outcome_type = "binary",
      variables    = list("age"),
      description  = "",
      encryption   = "aes256gcm"
    )
  )
  expect_true(evaluatr:::.validate_json_structure(enc_list))
})


test_that("Phase 3b: .decrypt_coefficients_in_json() round-trips correctly", {
  # v1.1 format: obfuscation_key NOT in JSON (held by Worker B, fetched by C++)
  real_coeffs <- c("(Intercept)" = -1.25, "age" = 0.02, "score" = 0.8)
  obf_key     <- evaluatr:::.generate_obfuscation_key()
  salt_a      <- evaluatr:::.generate_salt64()
  salt_b      <- evaluatr:::.generate_salt64()
  obf_coeffs  <- as.list(evaluatr:::.obfuscate_coefficients(
    real_coeffs, obf_key, salt_a, salt_b
  ))
  coeff_json  <- jsonlite::toJSON(obf_coeffs, auto_unbox = TRUE)

  enc_key_hex <- paste0(rep("c", 64), collapse = "")
  key_raw     <- evaluatr:::.hex_to_raw(enc_key_hex)
  iv          <- openssl::rand_bytes(12)
  ciphertext  <- openssl::aes_gcm_encrypt(charToRaw(coeff_json), key_raw, iv)

  json_list <- list(
    model_type             = "logistic",
    encrypted_coefficients = openssl::base64_encode(ciphertext),
    encryption_iv          = openssl::base64_encode(iv),
    preprocessing          = NULL,
    model_parameters       = NULL,
    metadata               = list(
      model_id     = "decrypt_test",
      model_name   = "Decrypt Test",
      version      = "1.0",
      outcome_type = "binary",
      variables    = list("age", "score"),
      description  = "",
      encryption   = "aes256gcm"
    )
  )

  encoded <- openssl::base64_encode(charToRaw(
    jsonlite::toJSON(json_list, auto_unbox = TRUE, null = "null", digits = 10)
  ))

  # decrypt_coefficients_in_json now takes only encoded + decryption_key;
  # obfuscation_key is NOT injected (C++ fetches it from Worker B)
  decrypted_encoded <- evaluatr:::.decrypt_coefficients_in_json(
    encoded, enc_key_hex
  )

  decrypted_json <- rawToChar(openssl::base64_decode(decrypted_encoded))
  parsed         <- jsonlite::fromJSON(decrypted_json, simplifyVector = FALSE)

  expect_true("coefficients" %in% names(parsed))
  expect_false("encrypted_coefficients" %in% names(parsed))
  expect_false("encryption_iv" %in% names(parsed))
  expect_null(parsed$metadata$encryption)
  # obfuscation_key is NOT injected â€” held by Worker B
  expect_null(parsed$obfuscation_key)

  # Coefficient names preserved
  expect_true("(Intercept)" %in% names(parsed$coefficients))
  expect_true("age" %in% names(parsed$coefficients))
  expect_true("score" %in% names(parsed$coefficients))
})


test_that("Phase 3b: .decrypt_coefficients_in_json() passes through unencrypted JSON", {
  # Unencrypted JSON should be returned unchanged
  real_coeffs <- c("(Intercept)" = -1.25, "age" = 0.02)
  obf_key     <- evaluatr:::.generate_obfuscation_key()
  salt_a      <- evaluatr:::.generate_salt64()
  salt_b      <- evaluatr:::.generate_salt64()
  obf_coeffs  <- as.list(evaluatr:::.obfuscate_coefficients(
    real_coeffs, obf_key, salt_a, salt_b
  ))

  json_list <- list(
    model_type      = "logistic",
    obfuscation_key = obf_key,
    coefficients    = obf_coeffs,
    preprocessing   = NULL,
    model_parameters = NULL,
    metadata        = list(
      model_id     = "unenc_pass",
      model_name   = "Unencrypted",
      version      = "1.0",
      outcome_type = "binary",
      variables    = list("age")
    )
  )

  encoded          <- openssl::base64_encode(charToRaw(
    jsonlite::toJSON(json_list, auto_unbox = TRUE, null = "null", digits = 10)
  ))
  enc_key_hex      <- paste0(rep("c", 64), collapse = "")

  result_encoded   <- evaluatr:::.decrypt_coefficients_in_json(encoded, enc_key_hex)

  # Should return unchanged (same base64 content encodes the same JSON)
  parsed <- jsonlite::fromJSON(rawToChar(openssl::base64_decode(result_encoded)),
                               simplifyVector = FALSE)
  expect_true("coefficients" %in% names(parsed))
  expect_false("encrypted_coefficients" %in% names(parsed))
})


test_that("Phase 3b: wrong decryption key produces an error", {
  real_coeffs <- c("(Intercept)" = -1.25, "age" = 0.02)
  obf_key     <- evaluatr:::.generate_obfuscation_key()
  salt_a      <- evaluatr:::.generate_salt64()
  salt_b      <- evaluatr:::.generate_salt64()
  obf_coeffs  <- as.list(evaluatr:::.obfuscate_coefficients(
    real_coeffs, obf_key, salt_a, salt_b
  ))
  coeff_json  <- jsonlite::toJSON(obf_coeffs, auto_unbox = TRUE)

  correct_key <- paste0(rep("c", 64), collapse = "")
  wrong_key   <- paste0(rep("d", 64), collapse = "")
  key_raw     <- evaluatr:::.hex_to_raw(correct_key)
  iv          <- openssl::rand_bytes(12)
  ciphertext  <- openssl::aes_gcm_encrypt(charToRaw(coeff_json), key_raw, iv)

  json_list <- list(
    model_type             = "logistic",
    encrypted_coefficients = openssl::base64_encode(ciphertext),
    encryption_iv          = openssl::base64_encode(iv),
    preprocessing          = NULL,
    model_parameters       = NULL,
    metadata               = list(
      model_id = "wrong_key_test", model_name = "WK", version = "1.0",
      outcome_type = "binary", variables = list("age"),
      encryption = "aes256gcm"
    )
  )

  encoded <- openssl::base64_encode(charToRaw(
    jsonlite::toJSON(json_list, auto_unbox = TRUE, null = "null", digits = 10)
  ))

  expect_error(
    evaluatr:::.decrypt_coefficients_in_json(encoded, wrong_key),
    label = "wrong key should cause decryption error"
  )
})
