# predict_secure_helpers.R — R-side orchestration for the C++ secure engine
#
# All functions are internal (. prefix). They orchestrate the flow:
#   encoded_content (base64 string) → metadata → preprocessing → design matrix
#   → C++ prediction + shuffle → evaluatr_result

# ---- .run_preprocessing() ---------------------------------------------------
# Run developer preprocessing code in a restricted environment.
# Only base R functions are available; no package namespaces.
#
# @param preprocessing_code Character string of R code, or NULL.
# @param validation_data Data frame to expose in the eval environment.
# @return Modified validation_data (with any new columns the code created).

.run_preprocessing <- function(preprocessing_code, validation_data) {
  if (is.null(preprocessing_code) || nchar(trimws(preprocessing_code)) == 0) {
    return(validation_data)
  }
  env <- new.env(parent = baseenv())
  env$validation_data <- validation_data
  tryCatch(
    eval(parse(text = preprocessing_code), envir = env),
    error = function(e) stop("Error in preprocessing code: ", e$message)
  )
  env$validation_data
}


# ---- .build_design_matrix() -------------------------------------------------
# Build a numeric design matrix for the C++ prediction engine.
#
# Constructs a matrix with rows = observations and columns = model terms.
# If "(Intercept)" is in variable_names (or always_intercept = TRUE), an
# intercept column of 1s is prepended. The remaining columns correspond to
# the predictor variables in the order supplied.
#
# @param validation_data Data frame containing all required variables.
# @param variable_names Character vector of predictor names (from JSON metadata).
# @param has_intercept Logical. Whether to add an intercept column.
# @return Numeric matrix.

.build_design_matrix <- function(validation_data, variable_names, has_intercept = TRUE) {
  n <- nrow(validation_data)

  # Remove "(Intercept)" from variable list — it is a column we generate
  pred_vars <- variable_names[variable_names != "(Intercept)"]

  missing_vars <- setdiff(pred_vars, names(validation_data))
  if (length(missing_vars) > 0) {
    stop("Missing required variables in validation_data: ",
         paste(missing_vars, collapse = ", "))
  }

  if (has_intercept) {
    X <- matrix(1.0, nrow = n, ncol = 1 + length(pred_vars))
    colnames(X) <- c("(Intercept)", pred_vars)
    for (j in seq_along(pred_vars)) {
      X[, j + 1] <- as.numeric(validation_data[[pred_vars[j]]])
    }
  } else {
    X <- matrix(0.0, nrow = n, ncol = length(pred_vars))
    colnames(X) <- pred_vars
    for (j in seq_along(pred_vars)) {
      X[, j] <- as.numeric(validation_data[[pred_vars[j]]])
    }
  }
  X
}


# ---- .predict_secure() ------------------------------------------------------
# Main orchestrator — calls C++ engine and packages result as evaluatr_result.
#
# @param encoded_content Character. Raw base64-encoded JSON from GitHub API.
# @param validation_data Data frame.
# @param outcome Character. Name of outcome column.
# @param by Character or NULL. Name of subgroup column.
# @param model_id Character. Used to populate model_info.
# @return An object of class "evaluatr_result".

.predict_secure <- function(encoded_content, validation_data, outcome,
                            by = NULL, model_id = "unknown") {

  # Step 1: Extract metadata from C++ (no coefficients returned)
  meta <- .extract_model_metadata_cpp(encoded_content)

  model_type     <- meta$model_type
  variable_names <- meta$variable_names
  model_name     <- meta$model_name
  version        <- meta$version
  is_multinomial <- isTRUE(meta$is_multinomial)

  # Step 2: Run preprocessing if present
  if (isTRUE(meta$has_preprocessing) && nchar(meta$preprocessing) > 0) {
    validation_data <- .run_preprocessing(meta$preprocessing, validation_data)
  }

  # Step 3: Check for missing variables (after preprocessing, which may create them)
  pred_vars <- variable_names[variable_names != "(Intercept)"]
  missing_vars <- setdiff(pred_vars, names(validation_data))
  if (length(missing_vars) > 0) {
    stop("Missing required variables in validation_data: ",
         paste(missing_vars, collapse = ", "))
  }

  # Step 4: Build design matrix
  # Determine whether model has an intercept term.
  # For multinomial, the C++ engine also expects intercept-first layout per category.
  # We always build an intercept column and let C++ handle alignment via coeff_names.
  X <- .build_design_matrix(validation_data, variable_names, has_intercept = TRUE)

  # Step 5: Prepare vectors
  outcome_vec <- as.numeric(validation_data[[outcome]])
  by_vec      <- if (!is.null(by)) validation_data[[by]] else NULL

  # Step 6: Call C++ prediction engine
  cpp_result <- .predict_from_encoded_cpp(
    encoded_content = encoded_content,
    model_type      = model_type,
    design_matrix   = X,
    outcome_vec     = outcome_vec,
    by_vec          = by_vec,
    model_params    = NULL
  )

  # Step 7: Package into evaluatr_result
  shuffled_outcomes  <- cpp_result$shuffled_outcomes
  shuffled_pred_mat  <- cpp_result$shuffled_pred_matrix
  is_single_col      <- isTRUE(cpp_result$is_single_col)
  has_by             <- isTRUE(cpp_result$has_by)
  shuffled_by        <- if (has_by) cpp_result$shuffled_by else NULL

  n_preds <- length(shuffled_outcomes)

  if (is_single_col && !is_multinomial) {
    # --- Binary / continuous path ---
    shuffled_predictions <- shuffled_pred_mat[, 1]

    # Build the 2-or-3-column outcome_predictions matrix
    if (!is.null(by)) {
      shuffled_outcome_predictions <- cbind(
        outcome    = shuffled_outcomes,
        prediction = shuffled_predictions,
        by         = shuffled_by
      )
    } else {
      shuffled_outcome_predictions <- cbind(
        outcome    = shuffled_outcomes,
        prediction = shuffled_predictions
      )
    }

    result <- structure(
      list(
        shuffled_outcome_predictions = shuffled_outcome_predictions,
        shuffled_outcomes            = shuffled_outcomes,
        shuffled_predictions         = shuffled_predictions,
        prediction_matrix            = matrix(shuffled_predictions, ncol = 1,
                                              dimnames = list(NULL, "prediction")),
        model_info = list(
          model_id             = model_id,
          model_name           = model_name,
          model_type           = if (nchar(meta$outcome_type) > 0) meta$outcome_type else model_type,
          version              = version,
          required_variables   = pred_vars,
          by_variable          = by,
          n_predictions        = n_preds,
          prediction_columns   = colnames(shuffled_pred_mat),
          validation_timestamp = Sys.time()
        )
      ),
      class = "evaluatr_result"
    )

    if (!is.null(by)) result$shuffled_by <- shuffled_by

  } else {
    # --- Multinomial path ---
    # shuffled_pred_mat is n x k (includes reference column at position 1)
    pred_col_names <- colnames(shuffled_pred_mat)

    base_matrix <- if (!is.null(by)) {
      cbind(outcome = shuffled_outcomes, by = shuffled_by)
    } else {
      cbind(outcome = shuffled_outcomes)
    }

    full_shuffled_matrix <- cbind(base_matrix, shuffled_pred_mat)

    result <- structure(
      list(
        shuffled_outcomes    = shuffled_outcomes,
        prediction_matrix    = shuffled_pred_mat,
        full_shuffled_matrix = full_shuffled_matrix,
        model_info = list(
          model_id             = model_id,
          model_name           = model_name,
          model_type           = if (nchar(meta$outcome_type) > 0) meta$outcome_type else model_type,
          version              = version,
          required_variables   = pred_vars,
          by_variable          = by,
          n_predictions        = n_preds,
          prediction_columns   = pred_col_names,
          validation_timestamp = Sys.time()
        )
      ),
      class = "evaluatr_result"
    )

    if (!is.null(by)) result$shuffled_by <- shuffled_by
  }

  message("Validation complete -- model: ", model_id,
          " | N = ", n_preds,
          " | Variables: ", paste(pred_vars, collapse = ", "))
  if (!is.null(by)) {
    message("Subgroup variable '", by, "' included in output.")
  }

  result
}


# ---- .generate_obfuscation_key() --------------------------------------------
# Generate a random 32-character lowercase hex obfuscation key.

.generate_obfuscation_key <- function() {
  paste0(sprintf("%02x", as.integer(sample.int(256L, 16L, replace = TRUE) - 1L)),
         collapse = "")
}


# ---- .obfuscate_coefficients() ----------------------------------------------
# R wrapper around the C++ obfuscation function.
# Takes a named numeric vector of real coefficients and an obfuscation key,
# returns a named numeric vector of obfuscated coefficients.
#
# @param real_coefficients Named numeric vector.
# @param obfuscation_key Character. 32-char hex string.
# @return Named numeric vector of obfuscated values.

.obfuscate_coefficients <- function(real_coefficients, obfuscation_key) {
  .obfuscate_coefficients_cpp(real_coefficients, obfuscation_key)
}
