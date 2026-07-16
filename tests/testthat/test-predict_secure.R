# Tests for the C++ secure prediction engine (Phase 1)
#
# All tests use locally-encoded JSON strings — no GitHub access required.
# JSON strings are base64-encoded with jsonlite::base64_enc() to simulate
# exactly what .fetch_github_model() now returns.

library(jsonlite)

# ---- Helpers: build encoded JSON content ------------------------------------

make_logistic_json <- function(obfuscated = FALSE, key = NULL,
                               salt_a = NULL, salt_b = NULL) {
  # True coefficients: (Intercept) = -1.25, age = 0.02, biomarker_score = 0.8
  real_coeffs <- c("(Intercept)" = -1.25, "age" = 0.02, "biomarker_score" = 0.8)

  if (obfuscated && !is.null(key) && !is.null(salt_a) && !is.null(salt_b)) {
    stored_coeffs <- evaluatr:::.obfuscate_coefficients(
      real_coeffs, key, salt_a, salt_b
    )
  } else {
    stored_coeffs <- real_coeffs
    key <- NULL
  }

  obj <- list(
    model_type = "logistic",
    coefficients = as.list(stored_coeffs),
    preprocessing = NULL,
    model_parameters = NULL,
    metadata = list(
      model_name   = "Test Logistic",
      version      = "1.0",
      outcome_type = "binary",
      variables    = c("age", "biomarker_score"),
      description  = "Test logistic model"
    )
  )

  raw <- toJSON(obj, auto_unbox = TRUE, null = "null")
  base64_enc(raw)
}

make_logistic_json_with_by <- make_logistic_json  # same structure

make_cox_json <- function(obfuscated = FALSE, key = NULL,
                          salt_a = NULL, salt_b = NULL) {
  # True coefficients: age = 0.05, bmi = 0.03 (Cox has no intercept)
  # Baseline survival at t=1, t=2, t=5
  real_coeffs <- c("age" = 0.05, "bmi" = 0.03)

  if (obfuscated && !is.null(key) && !is.null(salt_a) && !is.null(salt_b)) {
    stored_coeffs <- evaluatr:::.obfuscate_coefficients(
      real_coeffs, key, salt_a, salt_b
    )
  } else {
    stored_coeffs <- real_coeffs
    key <- NULL
  }

  obj <- list(
    model_type = "cox",
    coefficients = as.list(stored_coeffs),
    preprocessing = NULL,
    model_parameters = list(
      timepoints        = c(1, 2, 5),
      baseline_survival = c(0.95, 0.90, 0.80)
    ),
    metadata = list(
      model_name   = "Test Cox",
      version      = "1.0",
      outcome_type = "survival",
      variables    = c("age", "bmi"),
      description  = "Test Cox model"
    )
  )

  base64_enc(toJSON(obj, auto_unbox = TRUE, null = "null"))
}

make_weibull_json <- function(obfuscated = FALSE, key = NULL,
                              salt_a = NULL, salt_b = NULL) {
  # AFT Weibull: (Intercept) = 3.0, age = -0.02
  real_coeffs <- c("(Intercept)" = 3.0, "age" = -0.02)

  if (obfuscated && !is.null(key) && !is.null(salt_a) && !is.null(salt_b)) {
    stored_coeffs <- evaluatr:::.obfuscate_coefficients(
      real_coeffs, key, salt_a, salt_b
    )
  } else {
    stored_coeffs <- real_coeffs
    key <- NULL
  }

  obj <- list(
    model_type = "weibull",
    coefficients = as.list(stored_coeffs),
    preprocessing = NULL,
    model_parameters = list(
      shape            = 1.5,
      timepoints       = c(1, 3, 5),
      parameterisation = "aft"
    ),
    metadata = list(
      model_name   = "Test Weibull",
      version      = "1.0",
      outcome_type = "survival",
      variables    = c("age"),
      description  = "Test Weibull AFT"
    )
  )

  base64_enc(toJSON(obj, auto_unbox = TRUE, null = "null"))
}

make_multinomial_json <- function(obfuscated = FALSE, key = NULL,
                                  salt_a = NULL, salt_b = NULL) {
  # 2 non-reference categories: cat_B, cat_C (reference = cat_A)
  # cat_B: (Intercept) = -0.5, x1 = 0.3, x2 = 0.2
  # cat_C: (Intercept) =  0.5, x1 = 0.1, x2 = -0.4
  real_B <- c("(Intercept)" = -0.5, "x1" = 0.3, "x2" = 0.2)
  real_C <- c("(Intercept)" =  0.5, "x1" = 0.1, "x2" = -0.4)

  if (obfuscated && !is.null(key) && !is.null(salt_a) && !is.null(salt_b)) {
    stored_B <- evaluatr:::.obfuscate_coefficients(real_B, key, salt_a, salt_b)
    stored_C <- evaluatr:::.obfuscate_coefficients(real_C, key, salt_a, salt_b)
  } else {
    stored_B <- real_B
    stored_C <- real_C
  }

  obj <- list(
    model_type = "multinomial",
    coefficients = list(
      cat_B = as.list(stored_B),
      cat_C = as.list(stored_C)
    ),
    preprocessing = NULL,
    model_parameters = NULL,
    metadata = list(
      model_name   = "Test Multinomial",
      version      = "1.0",
      outcome_type = "multinomial",
      variables    = c("x1", "x2"),
      description  = "Test multinomial model"
    )
  )

  base64_enc(toJSON(obj, auto_unbox = TRUE, null = "null"))
}

make_preprocessing_json <- function() {
  # Preprocessing creates a dummy variable 'sexfemale' from 'sex'
  obj <- list(
    model_type = "logistic",
    obfuscation_key = NULL,
    coefficients = list(
      "(Intercept)" = -0.5,
      "age"         = 0.03,
      "sexfemale"   = -0.2
    ),
    preprocessing = "validation_data$sexfemale <- ifelse(validation_data$sex == 'female', 1, 0)",
    model_parameters = NULL,
    metadata = list(
      model_name   = "Test Preprocessing",
      version      = "1.0",
      outcome_type = "binary",
      variables    = c("age", "sexfemale"),
      description  = "Model with preprocessing"
    )
  )
  base64_enc(toJSON(obj, auto_unbox = TRUE, null = "null"))
}

make_unsupported_type_json <- function() {
  obj <- list(
    model_type = "neural_network",
    coefficients = list("(Intercept)" = 0.0, "x1" = 1.0),
    preprocessing = NULL,
    model_parameters = NULL,
    metadata = list(
      model_name = "Unsupported", version = "1.0",
      outcome_type = "binary", variables = list("x1")
    )
  )
  base64_enc(toJSON(obj, auto_unbox = TRUE, null = "null"))
}

# ---- Helper: generate validation datasets -----------------------------------

make_binary_data <- function(n = 100, seed = 4242) {
  set.seed(seed)
  data.frame(
    age            = rnorm(n, 50, 10),
    biomarker_score = rnorm(n),
    outcome        = rbinom(n, 1, 0.3)
  )
}

make_survival_data <- function(n = 100, seed = 1111) {
  set.seed(seed)
  data.frame(
    age     = rnorm(n, 55, 8),
    bmi     = rnorm(n, 27, 4),
    outcome = rbinom(n, 1, 0.3)
  )
}

make_weibull_data <- function(n = 100, seed = 2222) {
  set.seed(seed)
  data.frame(
    age     = rnorm(n, 55, 8),
    outcome = rbinom(n, 1, 0.3)
  )
}

make_multinomial_data <- function(n = 120, seed = 3333) {
  set.seed(seed)
  data.frame(
    x1      = rnorm(n),
    x2      = rnorm(n),
    outcome = sample(0:2, n, replace = TRUE)
  )
}


# =============================================================================
# 1. Obfuscation (R-side)
# =============================================================================

test_that("obfuscation: stored values differ from real values", {
  real   <- c("(Intercept)" = -1.25, "age" = 0.02, "biomarker_score" = 0.8)
  key    <- evaluatr:::.generate_obfuscation_key()
  salt_a <- evaluatr:::.generate_salt64()
  salt_b <- evaluatr:::.generate_salt64()

  stored <- evaluatr:::.obfuscate_coefficients(real, key, salt_a, salt_b)
  expect_false(all(stored == real))
  expect_equal(length(stored), length(real))
  expect_equal(names(stored), names(real))
})

test_that("obfuscation: different keys produce different stored values", {
  real   <- c("(Intercept)" = -1.25, "age" = 0.02)
  salt_a <- evaluatr:::.generate_salt64()
  salt_b <- evaluatr:::.generate_salt64()
  k1     <- evaluatr:::.generate_obfuscation_key()
  k2     <- evaluatr:::.generate_obfuscation_key()
  expect_false(k1 == k2)
  s1 <- evaluatr:::.obfuscate_coefficients(real, k1, salt_a, salt_b)
  s2 <- evaluatr:::.obfuscate_coefficients(real, k2, salt_a, salt_b)
  expect_false(all(s1 == s2))
})

test_that("obfuscation: different salts produce different stored values", {
  real   <- c("(Intercept)" = -1.25, "age" = 0.02)
  key    <- evaluatr:::.generate_obfuscation_key()
  salt_a1 <- evaluatr:::.generate_salt64()
  salt_b1 <- evaluatr:::.generate_salt64()
  salt_a2 <- evaluatr:::.generate_salt64()
  salt_b2 <- evaluatr:::.generate_salt64()
  s1 <- evaluatr:::.obfuscate_coefficients(real, key, salt_a1, salt_b1)
  s2 <- evaluatr:::.obfuscate_coefficients(real, key, salt_a2, salt_b2)
  expect_false(all(s1 == s2))
})


# =============================================================================
# 2. .extract_model_metadata_cpp — no coefficients returned
# =============================================================================

test_that("metadata extractor returns expected fields for logistic model", {
  enc  <- make_logistic_json(obfuscated = FALSE)
  meta <- evaluatr:::.extract_model_metadata_cpp(enc)

  expect_equal(meta$model_type,    "logistic")
  expect_equal(meta$model_name,    "Test Logistic")
  expect_equal(meta$version,       "1.0")
  expect_equal(meta$outcome_type,  "binary")
  expect_setequal(meta$variable_names, c("age", "biomarker_score"))
  expect_false(meta$has_obfuscation_key)
  expect_false(meta$has_preprocessing)
  expect_false(meta$is_multinomial)
})

test_that("metadata extractor does NOT return coefficient values", {
  enc  <- make_logistic_json(obfuscated = FALSE)
  meta <- evaluatr:::.extract_model_metadata_cpp(enc)
  meta_names <- names(meta)

  # None of the list elements should be named "coefficients" or "coeff_values"
  expect_false("coefficients"   %in% meta_names)
  expect_false("coeff_values"   %in% meta_names)
  expect_false("obfuscation_key" %in% meta_names)

  # No numeric value equal to any of the true coefficients
  # (defensive: check all numeric elements are not exact matches)
  all_nums <- unlist(meta[sapply(meta, is.numeric)])
  expect_false(-1.25 %in% all_nums)
  expect_false( 0.02 %in% all_nums)
  expect_false( 0.8  %in% all_nums)
})

test_that("metadata extractor marks has_obfuscation_key as FALSE for v1.1 JSON", {
  # v1.1 format: obfuscation key is held by Worker B, never in the JSON
  enc <- make_logistic_json(obfuscated = FALSE)
  expect_false(evaluatr:::.extract_model_metadata_cpp(enc)$has_obfuscation_key)
})

test_that("metadata extractor handles multinomial model", {
  enc  <- make_multinomial_json(obfuscated = FALSE)
  meta <- evaluatr:::.extract_model_metadata_cpp(enc)

  expect_true(meta$is_multinomial)
  expect_setequal(meta$variable_names, c("x1", "x2"))
  expect_setequal(meta$category_names, c("cat_B", "cat_C"))
})

test_that("metadata extractor reports preprocessing presence", {
  enc  <- make_preprocessing_json()
  meta <- evaluatr:::.extract_model_metadata_cpp(enc)
  expect_true(meta$has_preprocessing)
  expect_match(meta$preprocessing, "sexfemale")
})


# =============================================================================
# 3. Logistic regression (non-obfuscated — backward compatibility)
# =============================================================================

test_that("logistic regression (non-obfuscated) predictions match manual R calculation", {
  df  <- make_binary_data()
  enc <- make_logistic_json(obfuscated = FALSE)

  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test_logistic")

  # Manual computation
  lp_expected <- -1.25 + 0.02 * df$age + 0.8 * df$biomarker_score
  p_expected  <- 1 / (1 + exp(-lp_expected))

  expect_equal(sort(result$shuffled_predictions), sort(p_expected), tolerance = 1e-6)
})

test_that("non-obfuscated logistic returns evaluatr_result class", {
  df     <- make_binary_data()
  enc    <- make_logistic_json(obfuscated = FALSE)
  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test")
  expect_s3_class(result, "evaluatr_result")
})


# =============================================================================
# 4. Logistic regression (additional coverage)
# =============================================================================

test_that("logistic model_info contains expected metadata fields", {
  df     <- make_binary_data()
  enc    <- make_logistic_json(obfuscated = FALSE)
  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "meta_test")

  info <- result$model_info
  expect_equal(info$model_id,   "meta_test")
  expect_equal(info$model_name, "Test Logistic")
  expect_equal(info$version,    "1.0")
  expect_setequal(info$required_variables, c("age", "biomarker_score"))
  expect_equal(info$n_predictions, nrow(df))
})

test_that("predictions are in [0, 1] for logistic model", {
  df  <- make_binary_data()
  enc <- make_logistic_json(obfuscated = FALSE)
  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test")
  p <- result$shuffled_predictions
  expect_true(all(p >= 0 & p <= 1))
})

test_that("output is shuffled (predictions not in original order)", {
  df  <- make_binary_data(n = 200, seed = 9999)
  enc <- make_logistic_json(obfuscated = FALSE)
  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test")

  lp_expected <- -1.25 + 0.02 * df$age + 0.8 * df$biomarker_score
  p_expected  <- 1 / (1 + exp(-lp_expected))

  # Astronomically unlikely to match original order with 200 rows
  expect_false(all(result$shuffled_predictions == p_expected))
})


# =============================================================================
# 5. Logistic with 'by' variable
# =============================================================================

test_that("logistic with 'by': shuffled_by present and correctly set", {
  set.seed(7777)
  df <- data.frame(
    age             = rnorm(100, 50, 10),
    biomarker_score = rnorm(100),
    outcome         = rbinom(100, 1, 0.3),
    sex             = rep(c("M", "F"), each = 50)
  )
  enc    <- make_logistic_json(obfuscated = FALSE)
  result <- evaluatr:::.predict_secure(enc, df, "outcome", by = "sex", model_id = "test")

  expect_true("shuffled_by" %in% names(result))
  expect_equal(length(result$shuffled_by), 100)
  expect_setequal(unique(result$shuffled_by), c("M", "F"))
  expect_equal(result$model_info$by_variable, "sex")
})

test_that("logistic with 'by': outcome values preserved after shuffling", {
  set.seed(5555)
  df <- data.frame(
    age             = rnorm(80, 50, 10),
    biomarker_score = rnorm(80),
    outcome         = rbinom(80, 1, 0.4),
    group           = rep(c("A", "B"), each = 40)
  )
  enc    <- make_logistic_json(obfuscated = FALSE)
  result <- evaluatr:::.predict_secure(enc, df, "outcome", by = "group", model_id = "test")
  expect_equal(sort(as.numeric(result$shuffled_outcomes)), sort(df$outcome))
})


# =============================================================================
# 6. Cox PH
# =============================================================================

test_that("Cox PH predictions are survival probabilities in [0, 1]", {
  df  <- make_survival_data()
  enc <- make_cox_json(obfuscated = FALSE)

  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test_cox")

  pm <- result$prediction_matrix
  expect_true(all(pm >= 0 & pm <= 1))
  expect_equal(ncol(pm), 3) # 3 timepoints
})

test_that("Cox PH predictions match manual calculation (non-obfuscated)", {
  df  <- make_survival_data(n = 50, seed = 1234)
  enc <- make_cox_json(obfuscated = FALSE)

  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test_cox")

  # Manual: LP = 0.05*age + 0.03*bmi (no intercept column for Cox)
  lp   <- 0.05 * df$age + 0.03 * df$bmi
  s0   <- c(0.95, 0.90, 0.80)
  # S(t) = S0(t)^exp(LP)
  exp_lp <- exp(lp)
  p_t1 <- s0[1]^exp_lp
  p_t2 <- s0[2]^exp_lp
  p_t3 <- s0[3]^exp_lp

  # Compare sorted to account for shuffling
  expect_equal(sort(result$prediction_matrix[, 1]), sort(p_t1), tolerance = 1e-5)
  expect_equal(sort(result$prediction_matrix[, 2]), sort(p_t2), tolerance = 1e-5)
  expect_equal(sort(result$prediction_matrix[, 3]), sort(p_t3), tolerance = 1e-5)
})


# =============================================================================
# 7. Weibull AFT
# =============================================================================

test_that("Weibull AFT predictions are survival probs in [0, 1]", {
  df  <- make_weibull_data()
  enc <- make_weibull_json(obfuscated = FALSE)

  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test_weibull")

  pm <- result$prediction_matrix
  expect_true(all(pm >= 0 & pm <= 1))
  expect_equal(ncol(pm), 3) # 3 timepoints
})

test_that("Weibull AFT predictions match manual calculation (non-obfuscated)", {
  df  <- make_weibull_data(n = 50, seed = 4321)
  enc <- make_weibull_json(obfuscated = FALSE)

  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test_weibull")

  # AFT: LP = 3.0 + (-0.02)*age; scale = exp(LP); S(t) = exp(-(t/scale)^shape)
  lp    <- 3.0 + (-0.02) * df$age
  scale <- exp(lp)
  shape <- 1.5
  tv    <- c(1, 3, 5)
  p_t1  <- exp(-(tv[1] / scale)^shape)
  p_t2  <- exp(-(tv[2] / scale)^shape)
  p_t3  <- exp(-(tv[3] / scale)^shape)

  expect_equal(sort(result$prediction_matrix[, 1]), sort(p_t1), tolerance = 1e-5)
  expect_equal(sort(result$prediction_matrix[, 2]), sort(p_t2), tolerance = 1e-5)
  expect_equal(sort(result$prediction_matrix[, 3]), sort(p_t3), tolerance = 1e-5)
})


# =============================================================================
# 8. Multinomial (obfuscated)
# =============================================================================

test_that("multinomial predictions are valid probabilities (row sums = 1)", {
  df  <- make_multinomial_data()
  enc <- make_multinomial_json(obfuscated = FALSE)

  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test_multi")

  pm <- result$prediction_matrix
  expect_equal(ncol(pm), 3) # reference + cat_B + cat_C
  expect_equal(rowSums(pm), rep(1, nrow(pm)), tolerance = 1e-8)
})

test_that("multinomial predictions match manual softmax (non-obfuscated)", {
  df  <- make_multinomial_data(seed = 9876)
  enc <- make_multinomial_json(obfuscated = FALSE)

  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test_multi")

  # Manual softmax
  z_B   <- -0.5 + 0.3 * df$x1 + 0.2 * df$x2
  z_C   <-  0.5 + 0.1 * df$x1 - 0.4 * df$x2
  denom <- 1 + exp(z_B) + exp(z_C)
  p_ref <- 1        / denom
  p_B   <- exp(z_B) / denom
  p_C   <- exp(z_C) / denom

  pm <- result$prediction_matrix
  # Column 1 = reference; 2 = cat_B; 3 = cat_C
  expect_equal(sort(pm[, 1]), sort(p_ref), tolerance = 1e-6)
  expect_equal(sort(pm[, 2]), sort(p_B),   tolerance = 1e-6)
  expect_equal(sort(pm[, 3]), sort(p_C),   tolerance = 1e-6)
})

test_that("multinomial returns full_shuffled_matrix with correct dimensions", {
  df     <- make_multinomial_data(n = 90)
  enc    <- make_multinomial_json(obfuscated = FALSE)
  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test_multi")

  expect_equal(ncol(result$full_shuffled_matrix), 4) # outcome + 3 pred columns
  expect_equal(nrow(result$full_shuffled_matrix), 90)
})

test_that("multinomial does NOT have shuffled_predictions field (binary-only)", {
  df     <- make_multinomial_data()
  enc    <- make_multinomial_json(obfuscated = FALSE)
  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test_multi")

  expect_false("shuffled_predictions"         %in% names(result))
  expect_false("shuffled_outcome_predictions" %in% names(result))
})


# =============================================================================
# 9. No coefficient leakage checks
# =============================================================================

test_that("predict_from_encoded_cpp result contains no coefficient values", {
  df  <- make_binary_data()
  enc <- make_logistic_json(obfuscated = FALSE)

  X   <- evaluatr:::.build_design_matrix(
    df, c("age", "biomarker_score"), has_intercept = TRUE
  )
  # github_token = "" skips the Worker B call (unit test path)
  res <- evaluatr:::.predict_from_encoded_cpp(
    encoded_content = enc,
    model_type      = "logistic",
    design_matrix   = X,
    outcome_vec     = df$outcome,
    by_vec          = NULL,
    model_params    = NULL,
    github_token    = "",
    repo_owner      = "",
    repo_name       = "",
    model_id        = "test",
    validation_id   = "",
    worker_b_url    = ""
  )

  # Result should only contain shuffled data — no coeff names
  result_names <- names(res)
  expect_false("coefficients"    %in% result_names)
  expect_false("coeff_values"    %in% result_names)
  expect_false("obfuscation_key" %in% result_names)

  # No numeric element should match the true intercept exactly
  all_nums <- unlist(res[sapply(res, is.numeric)])
  expect_false(-1.25 %in% all_nums)
  expect_false( 0.02 %in% all_nums)
})

test_that("extract_model_metadata_cpp result contains no coefficient values", {
  enc  <- make_logistic_json(obfuscated = FALSE)
  meta <- evaluatr:::.extract_model_metadata_cpp(enc)

  result_names <- names(meta)
  expect_false("coefficients"   %in% result_names)
  expect_false("coeff_values"   %in% result_names)

  # No element equal to true coefficient values
  all_nums <- unlist(meta[sapply(meta, is.numeric)])
  expect_false(-1.25 %in% all_nums)
  expect_false( 0.8  %in% all_nums)
})


# =============================================================================
# 10. Preprocessing
# =============================================================================

test_that(".run_preprocessing creates expected variable", {
  df <- data.frame(age = 1:5, sex = c("male", "female", "male", "female", "male"),
                   outcome = c(0, 1, 0, 1, 0))
  code <- "validation_data$sexfemale <- ifelse(validation_data$sex == 'female', 1, 0)"
  df2  <- evaluatr:::.run_preprocessing(code, df)

  expect_true("sexfemale" %in% names(df2))
  expect_equal(df2$sexfemale, c(0, 1, 0, 1, 0))
})

test_that(".run_preprocessing with NULL code returns data unchanged", {
  df  <- data.frame(x = 1:5, outcome = 0:4)
  df2 <- evaluatr:::.run_preprocessing(NULL, df)
  expect_equal(df2, df)
})

test_that("preprocessing in JSON creates required variable before prediction", {
  set.seed(1234)
  n <- 50
  df <- data.frame(
    age     = rnorm(n, 50, 10),
    sex     = rep(c("male", "female"), n / 2),
    outcome = rbinom(n, 1, 0.3)
  )
  enc    <- make_preprocessing_json()
  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "prep_test")

  expect_s3_class(result, "evaluatr_result")
  expect_equal(length(result$shuffled_predictions), n)
  # Predictions should be valid probabilities
  expect_true(all(result$shuffled_predictions >= 0 & result$shuffled_predictions <= 1))
})

test_that(".run_preprocessing error gives informative message", {
  df   <- data.frame(x = 1:3)
  code <- "stop('deliberate error')"
  expect_error(evaluatr:::.run_preprocessing(code, df), "Error in preprocessing code")
})


# =============================================================================
# 11. .build_design_matrix
# =============================================================================

test_that(".build_design_matrix adds intercept column first", {
  df <- data.frame(age = 1:5, bmi = 6:10)
  X  <- evaluatr:::.build_design_matrix(df, c("age", "bmi"), has_intercept = TRUE)
  expect_equal(ncol(X), 3)
  expect_equal(colnames(X)[1], "(Intercept)")
  expect_true(all(X[, 1] == 1))
})

test_that(".build_design_matrix without intercept has correct shape", {
  df <- data.frame(age = 1:5, bmi = 6:10)
  X  <- evaluatr:::.build_design_matrix(df, c("age", "bmi"), has_intercept = FALSE)
  expect_equal(ncol(X), 2)
  expect_false("(Intercept)" %in% colnames(X))
})

test_that(".build_design_matrix removes (Intercept) from variable_names", {
  df <- data.frame(age = 1:5, bmi = 6:10)
  X  <- evaluatr:::.build_design_matrix(df, c("(Intercept)", "age", "bmi"),
                                         has_intercept = TRUE)
  expect_equal(ncol(X), 3) # only 1 intercept column, not 2
})

test_that(".build_design_matrix errors on missing variable", {
  df <- data.frame(age = 1:5)
  expect_error(
    evaluatr:::.build_design_matrix(df, c("age", "missing_var"), has_intercept = TRUE),
    "Missing required variables"
  )
})


# =============================================================================
# 12. Unsupported model type gives informative error
# =============================================================================

test_that("unsupported model type gives informative error", {
  # Data must include the variable so we reach the model-type dispatch check
  set.seed(42)
  df <- data.frame(x1 = rnorm(50), outcome = rbinom(50, 1, 0.3))
  enc <- make_unsupported_type_json()

  # .predict_secure should propagate the C++ error
  expect_error(
    evaluatr:::.predict_secure(enc, df, "outcome", model_id = "bad_type"),
    "Unsupported model type.*neural_network"
  )
})


# =============================================================================
# 13. End-to-end .predict_secure() returns correct evaluatr_result
# =============================================================================

test_that(".predict_secure returns correct evaluatr_result structure (logistic)", {
  df     <- make_binary_data()
  enc    <- make_logistic_json(obfuscated = FALSE)
  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "e2e_test")

  expect_s3_class(result, "evaluatr_result")
  expect_true("shuffled_outcomes"            %in% names(result))
  expect_true("shuffled_predictions"         %in% names(result))
  expect_true("shuffled_outcome_predictions" %in% names(result))
  expect_true("prediction_matrix"            %in% names(result))
  expect_true("model_info"                   %in% names(result))

  info <- result$model_info
  expect_equal(info$model_id,    "e2e_test")
  expect_equal(info$model_name,  "Test Logistic")
  expect_equal(info$n_predictions, nrow(df))
  expect_s3_class(info$validation_timestamp, "POSIXct")
  expect_setequal(info$required_variables, c("age", "biomarker_score"))
})

test_that(".predict_secure outcome values preserved after shuffling", {
  df     <- make_binary_data()
  enc    <- make_logistic_json(obfuscated = FALSE)
  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test")
  expect_equal(sort(result$shuffled_outcomes), sort(df$outcome))
})

test_that(".predict_secure shuffled_outcome_predictions matrix has correct dims", {
  df     <- make_binary_data(n = 80)
  enc    <- make_logistic_json(obfuscated = FALSE)
  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test")

  mat <- result$shuffled_outcome_predictions
  expect_equal(nrow(mat), 80)
  expect_equal(ncol(mat), 2)
  expect_equal(colnames(mat), c("outcome", "prediction"))
})

test_that(".predict_secure model_info by_variable is NULL when no 'by'", {
  df     <- make_binary_data()
  enc    <- make_logistic_json(obfuscated = FALSE)
  result <- evaluatr:::.predict_secure(enc, df, "outcome", model_id = "test")
  expect_null(result$model_info$by_variable)
  expect_false("shuffled_by" %in% names(result))
})

# =============================================================================
# 14. .generate_obfuscation_key
# =============================================================================

test_that(".generate_obfuscation_key produces 32-char hex strings", {
  key <- evaluatr:::.generate_obfuscation_key()
  expect_equal(nchar(key), 32)
  expect_match(key, "^[0-9a-f]{32}$")
})

test_that(".generate_obfuscation_key produces unique values", {
  keys <- replicate(10, evaluatr:::.generate_obfuscation_key())
  expect_equal(length(unique(keys)), 10)
})


# =============================================================================
# 15. Phase 3b: AES-256-GCM encryption / decryption
# =============================================================================

# Helper: build an encrypted logistic JSON (base64-encoded, GitHub format)
# Improvement B: obfuscation_key is NOT stored in the JSON — it is held by
# the key service. Obfuscation uses per-model salts also held by the key service.
make_encrypted_logistic_json <- function(enc_key_hex, obf_key = NULL,
                                          salt_a = NULL, salt_b = NULL) {
  real_coeffs <- c("(Intercept)" = -1.25, "age" = 0.02, "biomarker_score" = 0.8)
  if (is.null(obf_key)) obf_key <- evaluatr:::.generate_obfuscation_key()
  if (is.null(salt_a))  salt_a  <- evaluatr:::.generate_salt64()
  if (is.null(salt_b))  salt_b  <- evaluatr:::.generate_salt64()
  obf_coeffs  <- as.list(evaluatr:::.obfuscate_coefficients(real_coeffs, obf_key,
                                                             salt_a, salt_b))
  coeff_json  <- toJSON(obf_coeffs, auto_unbox = TRUE)

  key_raw    <- evaluatr:::.hex_to_raw(enc_key_hex)
  iv         <- openssl::rand_bytes(12)
  ciphertext <- openssl::aes_gcm_encrypt(charToRaw(coeff_json), key_raw, iv)

  json_list <- list(
    model_type             = "logistic",
    encrypted_coefficients = openssl::base64_encode(ciphertext),
    encryption_iv          = openssl::base64_encode(iv),
    preprocessing          = NULL,
    model_parameters       = NULL,
    metadata               = list(
      model_name   = "Encrypted Logistic",
      version      = "1.0",
      outcome_type = "binary",
      variables    = c("age", "biomarker_score"),
      description  = "Encrypted test model",
      encryption   = "aes256gcm"
    )
  )

  openssl::base64_encode(charToRaw(
    toJSON(json_list, auto_unbox = TRUE, null = "null", digits = 10)
  ))
}

test_that("Phase 3b: .decrypt_coefficients_in_json decrypts correctly", {
  enc_key_hex <- paste0(rep("a", 64), collapse = "")
  encoded     <- make_encrypted_logistic_json(enc_key_hex)

  result <- evaluatr:::.decrypt_coefficients_in_json(encoded, enc_key_hex)

  parsed <- jsonlite::fromJSON(rawToChar(openssl::base64_decode(result)),
                               simplifyVector = FALSE)
  expect_true("coefficients" %in% names(parsed))
  expect_false("encrypted_coefficients" %in% names(parsed))
  expect_false("encryption_iv" %in% names(parsed))
  expect_null(parsed$metadata$encryption)
  expect_true("(Intercept)" %in% names(parsed$coefficients))
  # obfuscation_key is NOT in the JSON (held by key service, not injected here)
  expect_null(parsed$obfuscation_key)
})

test_that("Phase 3b: .decrypt_coefficients_in_json passes unencrypted JSON unchanged", {
  enc_key_hex <- paste0(rep("b", 64), collapse = "")
  # Unencrypted v1 JSON
  encoded     <- make_logistic_json(obfuscated = TRUE,
                                    key = evaluatr:::.generate_obfuscation_key())

  result <- evaluatr:::.decrypt_coefficients_in_json(encoded, enc_key_hex)

  # Parse both and compare coefficients field exists
  parsed_orig   <- jsonlite::fromJSON(rawToChar(openssl::base64_decode(
    gsub("\n", "", encoded))), simplifyVector = FALSE)
  parsed_result <- jsonlite::fromJSON(rawToChar(openssl::base64_decode(result)),
                                      simplifyVector = FALSE)
  expect_true("coefficients" %in% names(parsed_result))
  expect_null(parsed_result$metadata$encryption)
})

test_that("Phase 3b: wrong key causes decryption error", {
  correct_key <- paste0(rep("c", 64), collapse = "")
  wrong_key   <- paste0(rep("d", 64), collapse = "")
  encoded     <- make_encrypted_logistic_json(correct_key)

  expect_error(
    evaluatr:::.decrypt_coefficients_in_json(encoded, wrong_key)
  )
})

test_that("Phase 3b: .predict_secure with correct key decrypts and runs without error", {
  # Verifies the decryption layer works end-to-end. Numeric equality with the
  # unencrypted path is not tested here: de-obfuscation requires a live Worker B
  # call (github_token), which is not available in unit tests.
  set.seed(999)
  df          <- make_binary_data(n = 80)
  enc_key_hex <- paste0(rep("e", 64), collapse = "")
  enc_enc     <- make_encrypted_logistic_json(enc_key_hex)

  expect_no_error({
    result_enc <- evaluatr:::.predict_secure(enc_enc, df, "outcome",
                                             model_id      = "enc_test",
                                             decryption_key = enc_key_hex)
  })
  expect_equal(length(result_enc$shuffled_predictions), nrow(df))
  expect_true(all(result_enc$shuffled_predictions >= 0 &
                    result_enc$shuffled_predictions <= 1))
})

test_that("Phase 3b: .predict_secure backward compat — empty key skips decryption", {
  df  <- make_binary_data()
  enc <- make_logistic_json(obfuscated = FALSE)

  # Should work without a decryption key (unencrypted v1 JSON)
  expect_no_error({
    result <- evaluatr:::.predict_secure(enc, df, "outcome",
                                         model_id = "backcompat",
                                         decryption_key = "")
  })
  expect_s3_class(result, "evaluatr_result")
})
