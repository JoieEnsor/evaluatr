# evaluatr: Secure Independent Evaluation of Clinical Prediction Models
# Core validation function -- v0.1.0
#
# GitHub fetch currently uses the pure-R curl implementation.
# The C++ libcurl version is preserved in inst/src/ for a future release
# once cross-platform static linking is resolved.

# Helper operator (internal)
`%||%` <- function(a, b) if (is.null(a)) b else a

# Internal: fetch model specification from GitHub API
.fetch_github_model <- function(api_url, token) {

  h <- curl::new_handle()
  curl::handle_setheaders(h,
    "Authorization" = paste("token", token),
    "Accept"        = "application/vnd.github.v3+json",
    "User-Agent"    = "evaluatr-r-package")
  curl::handle_setopt(h, timeout = 30, followlocation = TRUE, ssl_verifypeer = TRUE)

  tryCatch({
    response <- curl::curl_fetch_memory(api_url, handle = h)

    if (response$status_code != 200) {
      error_msg <- switch(as.character(response$status_code),
        "404" = "Model not found (404). Check repo_owner, repo_name, and model_id.",
        "401" = "Authentication failed (401). Check your GitHub token.",
        paste0("HTTP error: ", response$status_code)
      )
      return(list(error = error_msg, http_status = response$status_code))
    }

    api_response <- jsonlite::fromJSON(rawToChar(response$content))

    decoded_json <- if (api_response$encoding == "base64") {
      rawToChar(jsonlite::base64_dec(api_response$content))
    } else {
      api_response$content
    }

    model_json <- jsonlite::fromJSON(decoded_json)

    if (is.null(model_json$coefficients) ||
        is.null(model_json$prediction_function) ||
        is.null(model_json$metadata)) {
      return(list(error = "Missing required fields in model JSON",
                  http_status = response$status_code))
    }

    list(
      coefficients_json   = model_json$coefficients,
      prediction_function = model_json$prediction_function,
      metadata_json       = model_json$metadata,
      http_status         = response$status_code,
      success             = TRUE
    )

  }, error = function(e) {
    list(error = paste0("CURL error: ", e$message), http_status = 0)
  })
}


#' Secure Model Validation
#'
#' Retrieves a clinical prediction model specification from a developer's
#' private GitHub repository and computes predictions locally on the
#' evaluator's dataset, without exposing model coefficients or transmitting
#' patient data.
#'
#' @param repo_owner Character. GitHub username or organisation owning the
#'   model repository.
#' @param repo_name Character. Name of the GitHub repository containing the
#'   model specification.
#' @param model_id Character. Unique identifier for the model (must match the
#'   folder name under `models/` in the repository).
#' @param github_token Character. Fine-grained GitHub personal access token
#'   with read access to the repository. Provided by the model developer.
#' @param validation_data Data frame. The evaluator's local dataset, which
#'   must contain all predictor variables required by the model.
#' @param outcome Character. Name of the outcome variable in
#'   `validation_data`.
#' @param by Character or `NULL`. Optional name of a grouping variable in
#'   `validation_data` for subgroup analysis (e.g. `"sex"`). Must have at
#'   least two non-missing categories, each with \eqn{\geq 20} observations.
#'
#' @return A named list containing:
#' \describe{
#'   \item{`shuffled_outcome_predictions`}{Matrix of shuffled outcome-prediction
#'     pairs (and subgroup column if `by` is specified).}
#'   \item{`shuffled_outcomes`}{Numeric vector of shuffled outcomes.}
#'   \item{`shuffled_predictions`}{Numeric vector of shuffled predicted
#'     probabilities (binary/continuous models only).}
#'   \item{`shuffled_by`}{Vector of shuffled subgroup values (only present
#'     when `by` is specified).}
#'   \item{`prediction_matrix`}{Matrix of shuffled predicted probabilities
#'     (multinomial models: one column per outcome category).}
#'   \item{`model_info`}{List of model metadata including `model_id`,
#'     `model_name`, `model_type`, `version`, `required_variables`,
#'     `n_predictions`, and `validation_timestamp`.}
#' }
#'
#' @details
#' The function enforces a minimum dataset size of 20 observations (per
#' subgroup when `by` is used). Predictions are shuffled before being
#' returned so that they cannot be matched back to individual patient records,
#' preventing reverse-engineering of model coefficients and ensuring the
#' system cannot be used to make predictions for individual patients in
#' clinical practice.
#'
#' The `outcome` variable is not transmitted to GitHub; model coefficients
#' are retrieved but immediately cleared from memory after predictions are
#' computed.
#'
#' @examples
#' \dontrun{
#' # Basic usage with a binary outcome model
#' result <- secure_model_validation(
#'   repo_owner    = "developer-username",
#'   repo_name     = "my-models",
#'   model_id      = "sample_model_001",
#'   github_token  = Sys.getenv("GITHUB_PAT"),
#'   validation_data = my_data,
#'   outcome       = "event"
#' )
#'
#' # Subgroup analysis by sex
#' result_by_sex <- secure_model_validation(
#'   repo_owner    = "developer-username",
#'   repo_name     = "my-models",
#'   model_id      = "sample_model_001",
#'   github_token  = Sys.getenv("GITHUB_PAT"),
#'   validation_data = my_data,
#'   outcome       = "event",
#'   by            = "sex"
#' )
#' }
#'
#' @seealso [eval_performance()] for computing performance metrics
#'   from the returned object.
#'
#' @export
secure_model_validation <- function(repo_owner, repo_name, model_id,
                                    github_token, validation_data,
                                    outcome, by = NULL) {

  # ---- Input validation -------------------------------------------------------
  if (missing(repo_owner) || missing(repo_name) || missing(model_id) ||
      missing(github_token) || missing(validation_data) || missing(outcome)) {
    stop("All parameters are required.")
  }
  if (!is.data.frame(validation_data)) {
    stop("'validation_data' must be a data frame.")
  }
  if (!outcome %in% names(validation_data)) {
    stop("Outcome variable '", outcome, "' not found in validation_data.")
  }
  if (nrow(validation_data) < 21) {
    stop("Validation data must have more than 20 observations.")
  }

  # ---- Validate 'by' parameter ------------------------------------------------
  if (!is.null(by)) {
    if (!is.character(by) || length(by) != 1) {
      stop("'by' must be a single character string naming a column in validation_data.")
    }
    if (!by %in% names(validation_data)) {
      stop("Variable '", by, "' not found in validation_data.")
    }

    by_vec        <- validation_data[[by]]
    by_categories <- unique(by_vec[!is.na(by_vec)])

    if (length(by_categories) < 2) {
      stop("'by' variable '", by, "' must have at least 2 non-missing categories.")
    }
    for (cat in by_categories) {
      cat_idx      <- which(by_vec == cat & !is.na(by_vec))
      cat_outcomes <- validation_data[[outcome]][cat_idx]
      if (length(cat_outcomes) < 20) {
        stop("Category '", as.character(cat), "' in '", by,
             "' has fewer than 20 observations.")
      }
    }
  }

  # ---- Fetch model from GitHub ------------------------------------------------
  file_path <- paste0("models/", model_id, "/coefficients.json")
  api_url   <- paste0("https://api.github.com/repos/", repo_owner, "/",
                      repo_name, "/contents/", file_path)

  raw_result <- .fetch_github_model(api_url, github_token)

  if (!is.null(raw_result$error)) {
    stop(raw_result$error)
  }

  # ---- Reconstruct model object -----------------------------------------------
  coefficients_data <- raw_result$coefficients_json
  metadata          <- raw_result$metadata_json

  model_data <- list(
    model_type          = metadata$outcome_type %||% "unknown",
    coefficients        = coefficients_data,
    prediction_function = raw_result$prediction_function,
    metadata            = metadata,
    required_packages   = metadata$required_packages %||% NULL,
    model_parameters    = metadata$model_parameters  %||% NULL,
    baseline_survival   = metadata$baseline_survival  %||% NULL
  )

  # ---- Install any developer-specified packages -------------------------------
  if (!is.null(model_data$required_packages) &&
      length(model_data$required_packages) > 0) {
    for (pkg in model_data$required_packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        message("Installing required package: ", pkg)
        utils::install.packages(pkg, quiet = TRUE)
      }
    }
  }

  # ---- Identify required variables --------------------------------------------
  coeffs <- model_data$coefficients

  required_vars <- if (!is.null(model_data$metadata$variables)) {
    model_data$metadata$variables
  } else {
    names(coeffs)[names(coeffs) != "(Intercept)"]
  }

  missing_vars <- setdiff(required_vars, names(validation_data))
  if (length(missing_vars) > 0) {
    stop("Missing required variables in validation_data: ",
         paste(missing_vars, collapse = ", "))
  }

  # ---- Compute linear predictor (LP) ------------------------------------------
  LP <- rep(0, nrow(validation_data))
  if ("(Intercept)" %in% names(coeffs)) {
    LP <- LP + coeffs[["(Intercept)"]]
  }
  if (model_data$model_type != "multinomial") {
    for (var in required_vars) {
      LP <- LP + coeffs[[var]] * validation_data[[var]]
    }
  }

  # ---- Execute developer prediction function ----------------------------------
  # pred_env                <- new.env(parent = emptyenv())
  pred_env <- new.env(parent = baseenv())
  pred_env$LP             <- LP
  pred_env$validation_data <- validation_data

  for (coeff_name in names(coeffs)) {
    pred_env[[coeff_name]] <- coeffs[[coeff_name]]
  }
  if (!is.null(model_data$model_parameters)) {
    for (param_name in names(model_data$model_parameters)) {
      pred_env[[param_name]] <- model_data$model_parameters[[param_name]]
    }
  }
  if (!is.null(model_data$baseline_survival)) {
    pred_env$timepoints     <- model_data$baseline_survival$timepoints
    pred_env$survival_probs <- model_data$baseline_survival$survival_probs
  }

  predictions <- tryCatch(
    eval(parse(text = model_data$prediction_function), envir = pred_env),
    error = function(e) stop("Error in prediction function: ", e$message)
  )

  if (!is.matrix(predictions)) predictions <- as.matrix(predictions)

  # ---- Clear sensitive data from memory ---------------------------------------
  coeffs                      <- NULL
  model_data$coefficients     <- NULL
  model_data$model_parameters <- NULL
  model_data$prediction_function <- NULL
  pred_env                    <- NULL

  # ---- Build shuffled output --------------------------------------------------
  if (ncol(predictions) == 1) {
    # Binary / continuous outcome
    pred_vector <- predictions[, 1]

    outcome_pred_matrix <- if (!is.null(by)) {
      cbind(outcome    = validation_data[[outcome]],
            prediction = pred_vector,
            by         = validation_data[[by]])
    } else {
      cbind(outcome    = validation_data[[outcome]],
            prediction = pred_vector)
    }

    shuffled_indices <- sample(nrow(outcome_pred_matrix))
    shuffled_matrix  <- outcome_pred_matrix[shuffled_indices, ]

    result <- structure(
      list(
        shuffled_outcome_predictions = shuffled_matrix,
        shuffled_outcomes            = shuffled_matrix[, 1],
        shuffled_predictions         = shuffled_matrix[, 2],
        prediction_matrix            = predictions[shuffled_indices, , drop = FALSE],
        model_info = list(
          model_id           = model_id,
          model_name         = model_data$metadata$model_name,
          model_type         = model_data$model_type,
          version            = model_data$metadata$version,
          required_variables = required_vars,
          by_variable        = by,
          n_predictions      = length(pred_vector),
          prediction_columns = colnames(predictions),
          validation_timestamp = Sys.time()
        )
      ),
      class = "evaluatr_result"
    )
    if (!is.null(by)) result$shuffled_by <- shuffled_matrix[, 3]

  } else {
    # Multinomial outcome
    base_matrix <- if (!is.null(by)) {
      cbind(outcome = validation_data[[outcome]],
            by      = validation_data[[by]])
    } else {
      cbind(outcome = validation_data[[outcome]])
    }

    full_matrix      <- cbind(base_matrix, predictions)
    shuffled_indices <- sample(nrow(full_matrix))
    shuffled_matrix  <- full_matrix[shuffled_indices, ]

    pred_start_col             <- ncol(base_matrix) + 1
    shuffled_predictions_matrix <- shuffled_matrix[,
      pred_start_col:ncol(shuffled_matrix), drop = FALSE]

    result <- structure(
      list(
        shuffled_outcomes   = shuffled_matrix[, 1],
        prediction_matrix   = shuffled_predictions_matrix,
        full_shuffled_matrix = shuffled_matrix,
        model_info = list(
          model_id           = model_id,
          model_name         = model_data$metadata$model_name,
          model_type         = model_data$model_type,
          version            = model_data$metadata$version,
          required_variables = required_vars,
          by_variable        = by,
          n_predictions      = nrow(predictions),
          prediction_columns = colnames(predictions),
          validation_timestamp = Sys.time()
        )
      ),
      class = "evaluatr_result"
    )
    if (!is.null(by)) result$shuffled_by <- shuffled_matrix[, 2]
  }

  model_data <- NULL

  message("Validation complete -- model: ", model_id,
          " | N = ", nrow(predictions),
          " | Variables: ", paste(required_vars, collapse = ", "))
  if (!is.null(by)) {
    message("Subgroup variable '", by, "' included in output.")
  }

  return(result)
}
