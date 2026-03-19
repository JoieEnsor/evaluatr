# Tests for generate_model_json() — Phase 2 developer utility
#
# Tests cover:
#  1. Logistic from glm object
#  2. Logistic from manual coefficients
#  3. Multinomial from multinom object
#  4. Cox from coxph object
#  5. Weibull from survreg object
#  6. Round-trip verification (generate → base64 → .predict_secure())
#  7. Invalid inputs
#  8. Output file existence and valid JSON
#  9. Preprocessing preserved

library(jsonlite)

# ============================================================
# Helpers
# ============================================================

# Read and parse a JSON file produced by generate_model_json()
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
  result <- generate_model_json(
    model      = fit,
    model_id   = "test_glm_001",
    model_name = "Test GLM Model",
    output_dir = tmp
  )

  # Return value is a list
  expect_type(result, "list")
  expect_equal(result$model_type, "logistic")
  expect_equal(result$metadata$model_id, "test_glm_001")
  expect_equal(result$metadata$model_name, "Test GLM Model")
  expect_equal(result$metadata$outcome_type, "binary")

  # Obfuscation key present and 32 chars
  expect_false(is.null(result$obfuscation_key))
  expect_equal(nchar(result$obfuscation_key), 32L)

  # Coefficient names match
  coeff_names <- names(result$coefficients)
  expect_true("(Intercept)" %in% coeff_names)
  expect_true("age"   %in% coeff_names)
  expect_true("score" %in% coeff_names)

  # Coefficients are OBFUSCATED — values differ from originals
  for (nm in names(true_coeffs)) {
    expect_false(
      isTRUE(all.equal(unlist(result$coefficients[[nm]]), true_coeffs[[nm]], tolerance = 1e-8)),
      label = paste("coefficient", nm, "should be obfuscated, not equal to original")
    )
  }

  # Variables in metadata
  expect_true("age"   %in% unlist(result$metadata$variables))
  expect_true("score" %in% unlist(result$metadata$variables))
  expect_false("(Intercept)" %in% unlist(result$metadata$variables))

  # File was written
  out_path <- file.path(tmp, "coefficients.json")
  expect_true(file.exists(out_path))
})


# ============================================================
# Test 2: Logistic from manual coefficients
# ============================================================

test_that("logistic from manual coefficients: JSON structure is valid", {
  real_coeffs <- c("(Intercept)" = -1.25, age = 0.02,
                   biomarker_score = 0.8, treatment_group = -0.6)

  tmp <- tempdir()
  result <- generate_model_json(
    coefficients = real_coeffs,
    model_type   = "logistic",
    model_id     = "test_manual_001",
    model_name   = "Test Manual Logistic",
    outcome_type = "binary",
    variables    = c("age", "biomarker_score", "treatment_group"),
    output_dir   = tmp
  )

  expect_equal(result$model_type, "logistic")
  expect_equal(result$metadata$model_id, "test_manual_001")
  expect_equal(nchar(result$obfuscation_key), 32L)

  # All coefficient names present
  coeff_names <- names(result$coefficients)
  for (nm in names(real_coeffs)) {
    expect_true(nm %in% coeff_names)
  }

  # Coefficients obfuscated
  for (nm in names(real_coeffs)) {
    stored <- unlist(result$coefficients[[nm]])
    expect_false(isTRUE(all.equal(stored, real_coeffs[[nm]], tolerance = 1e-8)))
  }

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
  result <- generate_model_json(
    model              = fit,
    model_id           = "test_multinom_001",
    model_name         = "Test Multinomial",
    reference_category = "cat_A",
    output_dir         = tmp
  )

  expect_equal(result$model_type, "multinomial")
  expect_equal(result$metadata$outcome_type, "multinomial")

  # coefficients is a nested list (one entry per non-reference category)
  expect_type(result$coefficients, "list")
  cat_names <- names(result$coefficients)
  expect_true(length(cat_names) >= 2)
  # Reference category should NOT be a key in coefficients
  expect_false("cat_A" %in% cat_names)

  # Each category has coefficient sub-list with (Intercept) and predictors
  for (cat in cat_names) {
    cat_coeffs <- result$coefficients[[cat]]
    expect_true("(Intercept)" %in% names(cat_coeffs))
    expect_true("x1" %in% names(cat_coeffs))
    expect_true("x2" %in% names(cat_coeffs))
  }

  # Variables in metadata (no intercept)
  vars <- unlist(result$metadata$variables)
  expect_true("x1" %in% vars)
  expect_true("x2" %in% vars)
  expect_false("(Intercept)" %in% vars)

  # File exists
  expect_true(file.exists(file.path(tmp, "coefficients.json")))
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
  result <- generate_model_json(
    model            = fit,
    model_id         = "test_cox_001",
    model_name       = "Test Cox Model",
    model_parameters = mp,
    output_dir       = tmp
  )

  expect_equal(result$model_type, "cox")
  expect_equal(result$metadata$outcome_type, "survival")

  # model_parameters preserved
  stored_mp <- result$model_parameters
  expect_equal(unlist(stored_mp$timepoints),        mp$timepoints)
  expect_equal(unlist(stored_mp$baseline_survival), mp$baseline_survival)

  # Coefficients obfuscated (Cox has no intercept)
  true_coeffs <- coef(fit)
  for (nm in names(true_coeffs)) {
    stored <- unlist(result$coefficients[[nm]])
    expect_false(isTRUE(all.equal(stored, true_coeffs[[nm]], tolerance = 1e-8)))
  }

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
  result <- generate_model_json(
    model            = fit,
    model_id         = "test_weibull_001",
    model_name       = "Test Weibull Model",
    model_parameters = mp,
    output_dir       = tmp
  )

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

test_that("round-trip logistic: predictions from JSON match manual calculation", {
  # Real coefficients (test model)
  real_coeffs <- c("(Intercept)" = -1.25, age = 0.02,
                   biomarker_score = 0.8, treatment_group = -0.6)

  tmp <- tempdir()
  result <- generate_model_json(
    coefficients = real_coeffs,
    model_type   = "logistic",
    model_id     = "roundtrip_logistic",
    model_name   = "Round-trip Test",
    outcome_type = "binary",
    variables    = c("age", "biomarker_score", "treatment_group"),
    output_dir   = tmp
  )

  # Build validation dataset
  set.seed(99)
  n       <- 50
  val_df  <- data.frame(
    age             = rnorm(n, 60, 10),
    biomarker_score = rnorm(n, 0, 1),
    treatment_group = rbinom(n, 1, 0.5),
    outcome         = rbinom(n, 1, 0.4)
  )

  # Base64-encode the JSON for .predict_secure()
  json_str     <- readLines(file.path(tmp, "coefficients.json"), warn = FALSE)
  encoded      <- base64_enc(paste(json_str, collapse = "\n"))

  # Run secure prediction
  pred_result  <- evaluatr:::.predict_secure(
    encoded_content = encoded,
    validation_data = val_df,
    outcome         = "outcome",
    model_id        = "roundtrip_logistic"
  )

  cpp_preds   <- pred_result$shuffled_predictions

  # Manual predictions (un-shuffled, then sort by magnitude for comparison)
  manual_preds <- manual_logistic_pred(val_df, real_coeffs)

  # The shuffled predictions must be a permutation of the manual predictions
  expect_equal(length(cpp_preds), n)
  expect_equal(sort(round(cpp_preds, 6)), sort(round(manual_preds, 6)),
               tolerance = 1e-4,
               label = "round-trip logistic predictions match manual calculation")

  # Correlation should be very high (>0.9999) when sorted
  expect_gt(cor(sort(cpp_preds), sort(manual_preds)), 0.9999)
})


test_that("round-trip multinomial: predictions from JSON match softmax calculation", {
  skip_if_not_installed("nnet")
  # Real multinomial coefficients (test model)
  # cat_B: (Intercept) = -0.5, x1 = 0.3, x2 = 0.2
  # cat_C: (Intercept) =  0.5, x1 = 0.1, x2 = -0.4
  # reference = cat_A
  real_coeffs <- list(
    cat_B = c("(Intercept)" = -0.5, x1 = 0.3,  x2 =  0.2),
    cat_C = c("(Intercept)" =  0.5, x1 = 0.1,  x2 = -0.4)
  )

  tmp <- tempdir()
  result <- generate_model_json(
    coefficients       = real_coeffs,
    model_type         = "multinomial",
    model_id           = "roundtrip_multinom",
    model_name         = "Round-trip Multinomial",
    outcome_type       = "multinomial",
    variables          = c("x1", "x2"),
    reference_category = "cat_A",
    output_dir         = tmp
  )

  set.seed(77)
  n      <- 50
  val_df <- data.frame(
    x1      = rnorm(n),
    x2      = rnorm(n),
    outcome = sample(c(0, 1, 2), n, replace = TRUE)
  )

  json_str    <- readLines(file.path(tmp, "coefficients.json"), warn = FALSE)
  encoded     <- base64_enc(paste(json_str, collapse = "\n"))

  pred_result <- evaluatr:::.predict_secure(
    encoded_content = encoded,
    validation_data = val_df,
    outcome         = "outcome",
    model_id        = "roundtrip_multinom"
  )

  cpp_preds <- pred_result$prediction_matrix
  expect_equal(nrow(cpp_preds), n)
  expect_equal(ncol(cpp_preds), 3L)  # reference + 2 categories

  # Manual softmax probabilities
  lp_B <- real_coeffs$cat_B["(Intercept)"] +
          real_coeffs$cat_B["x1"] * val_df$x1 +
          real_coeffs$cat_B["x2"] * val_df$x2
  lp_C <- real_coeffs$cat_C["(Intercept)"] +
          real_coeffs$cat_C["x1"] * val_df$x1 +
          real_coeffs$cat_C["x2"] * val_df$x2
  denom       <- 1 + exp(lp_B) + exp(lp_C)
  manual_refA <- 1 / denom
  manual_B    <- exp(lp_B) / denom
  manual_C    <- exp(lp_C) / denom

  # Shuffled: sort and compare
  expect_equal(sort(round(cpp_preds[, "reference"], 6)),
               sort(round(manual_refA, 6)), tolerance = 1e-4)
  expect_equal(sort(round(cpp_preds[, "cat_B"], 6)),
               sort(round(manual_B, 6)), tolerance = 1e-4)
  expect_equal(sort(round(cpp_preds[, "cat_C"], 6)),
               sort(round(manual_C, 6)), tolerance = 1e-4)
})


test_that("round-trip Cox: predictions from JSON match manual calculation", {
  skip_if_not_installed("survival")
  # Cox has no intercept conceptually, but the design matrix always has an
  # intercept column prepended by .build_design_matrix(). The C++ engine
  # aligns by position, so an explicit (Intercept)=0.0 entry must be first
  # in the coefficient list — exactly as the Phase 1 test model uses.
  real_coeffs <- c("(Intercept)" = 0.0, age = 0.05, bmi = 0.03)
  mp <- list(
    timepoints        = c(1, 2, 5),
    baseline_survival = c(0.95, 0.90, 0.80)
  )

  tmp <- tempdir()
  result <- generate_model_json(
    coefficients     = real_coeffs,
    model_type       = "cox",
    model_id         = "roundtrip_cox",
    model_name       = "Round-trip Cox",
    outcome_type     = "survival",
    variables        = c("age", "bmi"),
    model_parameters = mp,
    output_dir       = tmp
  )

  set.seed(55)
  n      <- 40
  val_df <- data.frame(
    age     = rnorm(n, 55, 10),
    bmi     = rnorm(n, 27, 4),
    outcome = rbinom(n, 1, 0.5)
  )

  json_str    <- readLines(file.path(tmp, "coefficients.json"), warn = FALSE)
  encoded     <- base64_enc(paste(json_str, collapse = "\n"))

  pred_result <- evaluatr:::.predict_secure(
    encoded_content = encoded,
    validation_data = val_df,
    outcome         = "outcome",
    model_id        = "roundtrip_cox"
  )

  cpp_preds <- pred_result$prediction_matrix
  expect_equal(nrow(cpp_preds), n)
  expect_equal(ncol(cpp_preds), 3L)  # three timepoints

  # Manual Cox predictions: S(t) = S0(t)^exp(LP)
  # LP = 0*(intercept) + age_coeff*age + bmi_coeff*bmi
  lp          <- real_coeffs["age"] * val_df$age + real_coeffs["bmi"] * val_df$bmi
  manual_t1   <- mp$baseline_survival[1]^exp(lp)
  manual_t2   <- mp$baseline_survival[2]^exp(lp)
  manual_t5   <- mp$baseline_survival[3]^exp(lp)

  expect_equal(sort(round(cpp_preds[, 1], 6)), sort(round(manual_t1, 6)), tolerance = 1e-4)
  expect_equal(sort(round(cpp_preds[, 2], 6)), sort(round(manual_t2, 6)), tolerance = 1e-4)
  expect_equal(sort(round(cpp_preds[, 3], 6)), sort(round(manual_t5, 6)), tolerance = 1e-4)
})


test_that("round-trip Weibull AFT: predictions from JSON match manual calculation", {
  skip_if_not_installed("survival")
  real_coeffs <- c("(Intercept)" = 3.0, age = -0.02)
  shape_val   <- 1.5
  mp <- list(
    timepoints       = c(1, 3, 5),
    shape            = shape_val,
    parameterisation = "aft"
  )

  tmp <- tempdir()
  result <- generate_model_json(
    coefficients     = real_coeffs,
    model_type       = "weibull",
    model_id         = "roundtrip_weibull",
    model_name       = "Round-trip Weibull",
    outcome_type     = "survival",
    variables        = c("age"),
    model_parameters = mp,
    output_dir       = tmp
  )

  set.seed(33)
  n      <- 40
  val_df <- data.frame(
    age     = rnorm(n, 60, 10),
    outcome = rbinom(n, 1, 0.6)
  )

  json_str    <- readLines(file.path(tmp, "coefficients.json"), warn = FALSE)
  encoded     <- base64_enc(paste(json_str, collapse = "\n"))

  pred_result <- evaluatr:::.predict_secure(
    encoded_content = encoded,
    validation_data = val_df,
    outcome         = "outcome",
    model_id        = "roundtrip_weibull"
  )

  cpp_preds <- pred_result$prediction_matrix
  expect_equal(nrow(cpp_preds), n)

  # Manual Weibull AFT: S(t) = exp(-(t/scale)^shape), scale = exp(LP)
  lp    <- real_coeffs["(Intercept)"] + real_coeffs["age"] * val_df$age
  scale <- exp(lp)
  for (ti in seq_along(mp$timepoints)) {
    tv           <- mp$timepoints[ti]
    manual_surv  <- exp(-(tv / scale)^shape_val)
    expect_equal(sort(round(cpp_preds[, ti], 5)),
                 sort(round(manual_surv, 5)),
                 tolerance = 1e-4,
                 label = paste("Weibull AFT t =", tv))
  }
})


# ============================================================
# Test 7: Invalid inputs
# ============================================================

test_that("error when model_id is missing", {
  expect_error(
    generate_model_json(
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
    generate_model_json(
      coefficients = c("(Intercept)" = -1.25, age = 0.02),
      model_type   = "logistic",
      model_id     = "test",
      variables    = "age"
    ),
    "model_name"
  )
})

test_that("error when both model and coefficients are NULL", {
  expect_error(
    generate_model_json(
      model_id   = "test",
      model_name = "Test"
    ),
    "One of 'model' or 'coefficients'"
  )
})

test_that("error when both model and coefficients are provided", {
  set.seed(1)
  fit <- glm(rbinom(30, 1, 0.5) ~ rnorm(30), family = binomial)
  expect_error(
    generate_model_json(
      model        = fit,
      coefficients = c(x = 1.0),
      model_id     = "test",
      model_name   = "Test"
    ),
    "not both"
  )
})

test_that("error for unsupported model class", {
  fit <- lm(rnorm(30) ~ rnorm(30))
  expect_error(
    generate_model_json(
      model      = fit,
      model_id   = "test",
      model_name = "Test"
    ),
    "Unsupported model class"
  )
})

test_that("error for non-binomial glm", {
  fit <- glm(rpois(30, 2) ~ rnorm(30), family = poisson)
  expect_error(
    generate_model_json(
      model      = fit,
      model_id   = "test",
      model_name = "Test"
    ),
    "binomial"
  )
})

test_that("error when model_type missing for manual coefficients", {
  expect_error(
    generate_model_json(
      coefficients = c("(Intercept)" = -1.0, age = 0.02),
      model_id     = "test",
      model_name   = "Test",
      variables    = "age"
    ),
    "model_type"
  )
})

test_that("error when variables missing for manual coefficients", {
  expect_error(
    generate_model_json(
      coefficients = c("(Intercept)" = -1.0, age = 0.02),
      model_type   = "logistic",
      model_id     = "test",
      model_name   = "Test"
    ),
    "variables"
  )
})

test_that("error for Cox without model_parameters", {
  expect_error(
    generate_model_json(
      coefficients = c(age = 0.05),
      model_type   = "cox",
      model_id     = "test",
      model_name   = "Test",
      variables    = "age"
    ),
    "timepoints"
  )
})

test_that("error for Cox without baseline_survival", {
  expect_error(
    generate_model_json(
      coefficients     = c(age = 0.05),
      model_type       = "cox",
      model_id         = "test",
      model_name       = "Test",
      variables        = "age",
      model_parameters = list(timepoints = c(1, 2, 5))
    ),
    "baseline_survival"
  )
})

test_that("error for multinomial without reference_category in manual mode", {
  expect_error(
    generate_model_json(
      coefficients = list(
        cat_B = c("(Intercept)" = -0.5, x1 = 0.3)
      ),
      model_type   = "multinomial",
      model_id     = "test",
      model_name   = "Test",
      variables    = "x1"
    ),
    "reference_category"
  )
})

test_that("error for multinomial with non-list coefficients", {
  expect_error(
    generate_model_json(
      coefficients       = c("(Intercept)" = -0.5, x1 = 0.3),
      model_type         = "multinomial",
      model_id           = "test",
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
  generate_model_json(
    coefficients = real_coeffs,
    model_type   = "logistic",
    model_id     = "file_test_001",
    model_name   = "File Test Model",
    outcome_type = "binary",
    variables    = c("age", "score"),
    output_dir   = tmp
  )

  out_path <- file.path(tmp, "coefficients.json")
  expect_true(file.exists(out_path))

  # Parseable JSON
  parsed <- tryCatch(
    fromJSON(readLines(out_path, warn = FALSE), simplifyVector = FALSE),
    error = function(e) NULL
  )
  expect_false(is.null(parsed), label = "JSON file is valid and parseable")

  # Required top-level fields
  for (field in c("model_type", "obfuscation_key", "coefficients", "metadata")) {
    expect_true(field %in% names(parsed),
                label = paste("field", field, "present in JSON"))
  }

  # Required metadata fields
  for (field in c("model_id", "model_name", "version", "outcome_type", "variables")) {
    expect_true(field %in% names(parsed$metadata),
                label = paste("metadata field", field, "present"))
  }
})

test_that("custom output_filename is used", {
  real_coeffs <- c("(Intercept)" = -1.0, age = 0.02)
  tmp <- tempdir()
  generate_model_json(
    coefficients    = real_coeffs,
    model_type      = "logistic",
    model_id        = "custom_fn",
    model_name      = "Custom Filename Test",
    variables       = "age",
    output_dir      = tmp,
    output_filename = "my_model.json"
  )
  expect_true(file.exists(file.path(tmp, "my_model.json")))
})

test_that("output_dir is created if it does not exist", {
  real_coeffs <- c("(Intercept)" = -1.0, age = 0.02)
  new_dir <- file.path(tempdir(), paste0("new_dir_", as.integer(Sys.time())))
  expect_false(dir.exists(new_dir))

  generate_model_json(
    coefficients = real_coeffs,
    model_type   = "logistic",
    model_id     = "dir_test",
    model_name   = "Dir Creation Test",
    variables    = "age",
    output_dir   = new_dir
  )

  expect_true(dir.exists(new_dir))
  expect_true(file.exists(file.path(new_dir, "coefficients.json")))
})


# ============================================================
# Test 9: Preprocessing string preserved
# ============================================================

test_that("preprocessing string is written to JSON and round-trips correctly", {
  real_coeffs  <- c("(Intercept)" = -0.5, age = 0.03, sexfemale = -0.2)
  preprocessing <- "validation_data$sexfemale <- ifelse(validation_data$sex == 'female', 1, 0)"

  tmp <- tempdir()
  result <- generate_model_json(
    coefficients  = real_coeffs,
    model_type    = "logistic",
    model_id      = "preproc_test",
    model_name    = "Preprocessing Test",
    variables     = c("age", "sexfemale"),
    preprocessing = preprocessing,
    output_dir    = tmp
  )

  # In-memory result
  expect_equal(result$preprocessing, preprocessing)

  # In file
  parsed <- fromJSON(readLines(file.path(tmp, "coefficients.json"), warn = FALSE),
                     simplifyVector = FALSE)
  expect_equal(parsed$preprocessing, preprocessing)
})

test_that("preprocessing round-trip: .predict_secure() executes preprocessing correctly", {
  real_coeffs   <- c("(Intercept)" = -0.5, age = 0.03, sexfemale = -0.2)
  preprocessing <- "validation_data$sexfemale <- ifelse(validation_data$sex == 'female', 1, 0)"

  tmp <- tempdir()
  generate_model_json(
    coefficients  = real_coeffs,
    model_type    = "logistic",
    model_id      = "preproc_roundtrip",
    model_name    = "Preprocessing Round-trip",
    variables     = c("age", "sexfemale"),
    preprocessing = preprocessing,
    output_dir    = tmp
  )

  set.seed(44)
  n      <- 40
  val_df <- data.frame(
    age     = rnorm(n, 60, 10),
    sex     = sample(c("male", "female"), n, replace = TRUE),
    outcome = rbinom(n, 1, 0.4)
  )
  # Note: 'sexfemale' is NOT in val_df — it must be created by preprocessing

  json_str    <- readLines(file.path(tmp, "coefficients.json"), warn = FALSE)
  encoded     <- base64_enc(paste(json_str, collapse = "\n"))

  # Should not error (preprocessing creates 'sexfemale')
  expect_no_error({
    pred_result <- evaluatr:::.predict_secure(
      encoded_content = encoded,
      validation_data = val_df,
      outcome         = "outcome",
      model_id        = "preproc_roundtrip"
    )
  })

  cpp_preds    <- pred_result$shuffled_predictions
  expect_equal(length(cpp_preds), n)
  expect_true(all(cpp_preds >= 0 & cpp_preds <= 1))

  # Manual predictions with preprocessing applied
  val_df$sexfemale <- ifelse(val_df$sex == "female", 1, 0)
  manual_preds     <- manual_logistic_pred(val_df, real_coeffs)

  expect_equal(sort(round(cpp_preds, 5)), sort(round(manual_preds, 5)),
               tolerance = 1e-4)
})
