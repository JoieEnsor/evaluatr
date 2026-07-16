# predict_secure_helpers.R -- R-side orchestration for the C++ secure engine
#
# All functions are internal (. prefix). They orchestrate the flow:
#   encoded_content (base64 string) -> metadata -> preprocessing -> design matrix
#   -> C++ prediction + shuffle -> evaluatr_result

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

  # Remove "(Intercept)" from variable list -- it is a column we generate
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


# ---- .decrypt_coefficients_in_json() ----------------------------------------
# Decrypts the AES-256-GCM coefficient payload and returns the JSON ready for
# the C++ engine. Returns encoded_content unchanged if not encrypted.
# The obfuscation key is NOT injected here -- C++ fetches it directly from
# Worker B using the GitHub token.
#
# @param encoded_content Character. Base64-encoded JSON (GitHub API format,
#   may contain embedded newlines).
# @param decryption_key Character. 64-char hex string (from Worker A).
# @return Character. Base64-encoded JSON with decrypted coefficients; no
#   obfuscation_key field (v1.1 format — C++ handles that via Worker B).

.decrypt_coefficients_in_json <- function(encoded_content, decryption_key) {
  # Decode the base64 GitHub content (strip embedded newlines first)
  json_raw  <- openssl::base64_decode(gsub("\n", "", encoded_content))
  json_list <- jsonlite::fromJSON(rawToChar(json_raw), simplifyVector = FALSE)

  # If not encrypted, return unchanged
  if (!identical(json_list$metadata$encryption, "aes256gcm")) {
    return(encoded_content)
  }

  key_raw    <- .hex_to_raw(decryption_key)
  iv_raw     <- openssl::base64_decode(json_list$encryption_iv)
  cipher_raw <- openssl::base64_decode(json_list$encrypted_coefficients)
  plain_raw  <- openssl::aes_gcm_decrypt(data = cipher_raw, key = key_raw,
                                         iv = iv_raw)

  # Substitute decrypted coefficients back; remove encryption fields
  json_list$coefficients           <- jsonlite::fromJSON(rawToChar(plain_raw),
                                                         simplifyVector = FALSE)
  json_list$encrypted_coefficients <- NULL
  json_list$encryption_iv          <- NULL
  json_list$metadata$encryption    <- NULL

  # Re-encode as base64 for the C++ engine
  new_json <- jsonlite::toJSON(json_list, auto_unbox = TRUE,
                               null = "null", pretty = FALSE, digits = 10)
  openssl::base64_encode(charToRaw(new_json))
}


# ---- .predict_secure() ------------------------------------------------------
# Main orchestrator -- calls C++ engine and packages result as evaluatr_result.
#
# @param encoded_content Character. Raw base64-encoded JSON from GitHub API.
# @param validation_data Data frame.
# @param outcome Character. Name of outcome column.
# @param by Character or NULL. Name of subgroup column.
# @param model_id Character. Used to look up keys in Worker B and populate model_info.
# @param decryption_key Character. 64-char hex string (from Worker A), or "" for
#   unencrypted models.
# @param github_token Character. Evaluator's GitHub PAT — passed into C++ for
#   Worker B authentication. Never stored in an R object beyond this call.
# @param repo_owner Character. GitHub repository owner.
# @param repo_name Character. GitHub repository name.
# @param validation_id Character. Id of the validations row Worker A created,
#   passed into C++ and on to Worker B so the row is marked completed once the
#   obfuscation key has been served. Empty string if unknown.
# @param worker_b_url Character. Base URL for Worker B. Defaults to production URL
#   read from getOption("evaluatr.obfuscation_service_url").
# @return An object of class "evaluatr_result".

.predict_secure <- function(encoded_content, validation_data, outcome,
                            by = NULL, model_id = "unknown",
                            decryption_key = "",
                            github_token = "",
                            repo_owner = "",
                            repo_name = "",
                            validation_id = "",
                            worker_b_url = getOption(
                              "evaluatr.obfuscation_service_url",
                              default = paste0(
                                "https://evaluatr-obfuscation-service",
                                ".joie-ensor.workers.dev"
                              )
                            )) {

  # Refuse to run if internal functions are being debugged.
  if (isdebugged(.predict_secure) ||
        isdebugged(.decrypt_coefficients_in_json) ||
        isdebugged(.hex_to_raw) ||
        isdebugged(.run_preprocessing)) {
    stop("evaluatr: debugging of internal security functions is not permitted.")
  }

  # C++ requires a string validation_id; coerce a missing/NULL value to "".
  if (is.null(validation_id) || length(validation_id) == 0) {
    validation_id <- ""
  }
  validation_id <- as.character(validation_id)

  # Decrypt and prepare encoded content if a decryption key was provided
  if (nzchar(decryption_key) && nchar(decryption_key) == 64) {
    encoded_content <- .decrypt_coefficients_in_json(
      encoded_content = encoded_content,
      decryption_key  = decryption_key
    )
  }

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

  # Step 3: Check for missing variables (after preprocessing)
  pred_vars <- variable_names[variable_names != "(Intercept)"]
  missing_vars <- setdiff(pred_vars, names(validation_data))
  if (length(missing_vars) > 0) {
    stop("Missing required variables in validation_data: ",
         paste(missing_vars, collapse = ", "))
  }

  # Step 4: Build design matrix. Cox has no intercept term; all other model
  # types need an intercept column at position 0 for C++ alignment.
  X <- .build_design_matrix(validation_data, variable_names,
                             has_intercept = (model_type != "cox"))

  # Step 5: Prepare vectors
  outcome_vec <- as.numeric(validation_data[[outcome]])
  by_vec      <- if (!is.null(by)) validation_data[[by]] else NULL

  # Step 6: Call C++ prediction engine
  # github_token, repo_owner, repo_name, and worker_b_url are passed into C++
  # so that Worker B can be called from within the compiled engine — the
  # obfuscation key and salts never appear as R objects.
  cpp_result <- .predict_from_encoded_cpp(
    encoded_content = encoded_content,
    model_type      = model_type,
    design_matrix   = X,
    outcome_vec     = outcome_vec,
    by_vec          = by_vec,
    model_params    = NULL,
    github_token    = github_token,
    repo_owner      = repo_owner,
    repo_name       = repo_name,
    model_id        = model_id,
    validation_id   = validation_id,
    worker_b_url    = worker_b_url
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
        prediction_matrix            = matrix(
          shuffled_predictions, ncol = 1,
          dimnames = list(NULL, "prediction")),
        model_info = list(
          model_id             = model_id,
          model_name           = model_name,
          model_type           = if (nchar(meta$outcome_type) > 0)
            meta$outcome_type else model_type,
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
          model_type           = if (nchar(meta$outcome_type) > 0)
            meta$outcome_type else model_type,
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


# ---- .hex_to_raw() ----------------------------------------------------------
# Convert a lowercase hex string to a raw vector.
# .hex_to_raw exists but is not exported; use base R instead.

.hex_to_raw <- function(hex_str) {
  n <- nchar(hex_str)
  pairs <- substring(hex_str, seq(1, n - 1, 2), seq(2, n, 2))
  as.raw(strtoi(pairs, 16L))
}


# ---- .generate_obfuscation_key() --------------------------------------------
# Generate a random 32-character lowercase hex obfuscation key.

.generate_obfuscation_key <- function() {
  bytes <- as.integer(sample.int(256L, 16L, replace = TRUE) - 1L)
  paste0(sprintf("%02x", bytes), collapse = "")
}


# ---- .generate_salt64() -----------------------------------------------------
# Generate a random 64-bit salt as a 16-character lowercase hex string.
# Used to produce per-model salt_a and salt_b at JSON creation time.
# These replace the compiled SALT_A / SALT_B constants in Phase 2.

.generate_salt64 <- function() {
  bytes <- as.integer(sample.int(256L, 8L, replace = TRUE) - 1L)
  paste0(sprintf("%02x", bytes), collapse = "")
}


# ---- .obfuscate_coefficients() ----------------------------------------------
# R wrapper around the C++ obfuscation function.
# Takes a named numeric vector of real coefficients, an obfuscation key, and
# the per-model salts; returns a named numeric vector of obfuscated coefficients.
#
# @param real_coefficients Named numeric vector.
# @param obfuscation_key Character. 32-char hex string.
# @param salt_a Character. 16-char hex string (per-model salt A).
# @param salt_b Character. 16-char hex string (per-model salt B).
# @return Named numeric vector of obfuscated values.

.obfuscate_coefficients <- function(real_coefficients, obfuscation_key,
                                    salt_a, salt_b) {
  .obfuscate_coefficients_cpp(
    real_coefficients, obfuscation_key, salt_a, salt_b
  )
}
