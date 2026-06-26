# evaluatr: Secure Independent Evaluation of Clinical Prediction Models
# Core validation function -- v0.1.0

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

    # Return the raw base64 content for the C++ engine.
    # The GitHub Contents API always returns base64-encoded content; we pass
    # it directly to .predict_secure() which decodes, de-obfuscates, predicts,
    # shuffles, and wipes coefficients entirely inside C++.
    encoded_content <- api_response$content

    # Quick sanity check: the content field must be present
    if (is.null(encoded_content) || !nzchar(trimws(encoded_content))) {
      return(list(error = "Missing or empty content field in GitHub API response",
                  http_status = response$status_code))
    }

    list(
      encoded_content = encoded_content,
      http_status     = response$status_code,
      success         = TRUE
    )

  }, error = function(e) {
    list(error = paste0("CURL error: ", e$message), http_status = 0)
  })
}


#' Secure Model Validation
#'
#' Retrieves a clinical prediction model specification from a developer's
#' private GitHub repository and computes predictions locally on the
#' evaluator's dataset, without transmitting
#' patient data.
#'
#' @param repo_owner Character. GitHub username or organisation owning the
#'   model repository.
#' @param repo_name Character. Name of the GitHub repository containing the
#'   model specification.
#' @param model_id Character. Unique identifier for the model (must match the
#'   folder name under `models/` in the repository).
#' @param github_token Character. Fine-grained GitHub personal access token
#'   with read-only access to the repository. Provided by the model developer.
#' @param validation_data Data frame. The evaluator's local dataset, which
#'   must contain all predictor variables required by the model.
#' @param outcome Character. Name of the outcome variable in
#'   `validation_data`.
#' @param by Character or `NULL`. Optional name of a grouping variable in
#'   `validation_data` for subgroup analysis (e.g. `"study"`). Must have at
#'   least two non-missing categories, each meeting the minimum observation
#'   requirement.
#'
#' @return A named list of class `evaluatr_result` containing:
#' \describe{
#'   \item{`shuffled_outcome_predictions`}{Matrix of shuffled outcome-prediction
#'     pairs (and subgroup column if `by` is specified).}
#'   \item{`shuffled_outcomes`}{Numeric vector of shuffled outcomes.}
#'   \item{`shuffled_predictions`}{Numeric vector of shuffled predicted
#'     probabilities (binary models only).}
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
#' A minimum dataset size is enforced (per subgroup when `by` is used).
#' Predictions are shuffled before being returned so that they cannot be
#' matched back to individual patient records, preventing reverse-engineering
#' of model coefficients and ensuring the system cannot be used to make
#' predictions for individual patients in clinical practice.
#'
#' Patient data is never transmitted outside your R session. The validation event is logged by the evaluatr registry.
#'
#' To use this function you need four items from the model developer: their
#' GitHub username, repository name, model ID, and a time-limited access
#' token. See the evaluator guide vignette (\code{vignette("evaluator-guide",
#' package = "evaluatr")}) for a step-by-step walkthrough. Use
#' [list_registered_models()] to browse publicly registered models and find
#' developer contact details.
#'
#' @examples
#' \dontrun{
#' # Load the DVT example dataset included with the package
#' dvt_path <- system.file("extdata", "dvt_example.csv", package = "evaluatr")
#' dvt_data <- read.csv(dvt_path)
#'
#' # Use only studies 6-7 as the external validation set
#' dvt_val <- dvt_data[dvt_data$study %in% c(6, 7), ]
#'
#' # Run secure validation
#' # Replace the values below with those supplied by the model developer
#' result <- secure_model_validation(
#'   repo_owner      = "developer-username",
#'   repo_name       = "my-models",
#'   model_id        = "dvt_model_v1",
#'   github_token    = Sys.getenv("EVALUATR_TOKEN"),
#'   validation_data = dvt_val,
#'   outcome         = "dvt"
#' )
#'
#' # Compute performance metrics
#' perf <- eval_performance(result)
#' perf$metrics
#'
#' # Subgroup analysis by study
#' result_by_study <- secure_model_validation(
#'   repo_owner      = "developer-username",
#'   repo_name       = "my-models",
#'   model_id        = "dvt_model_v1",
#'   github_token    = Sys.getenv("EVALUATR_TOKEN"),
#'   validation_data = dvt_val,
#'   outcome         = "dvt",
#'   by              = "study"
#' )
#'
#' perf_by_study <- eval_performance(
#'   result_by_study,
#'   by = result_by_study$shuffled_by
#' )
#' perf_by_study$metrics
#' }
#'
#' @seealso [eval_performance()] for computing performance metrics from the
#'   returned object. [list_registered_models()] to browse publicly registered
#'   models.
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
  if (nrow(validation_data) < 50) {
    stop("Validation data must have at least 50 observations.")
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
      if (length(cat_outcomes) < 50) {
        stop("Category '", as.character(cat), "' in '", by,
             "' has fewer than 50 observations.")
      }
    }
  }

  # ---- Fetch model from GitHub ------------------------------------------------
  file_path <- paste0(model_id, "/", model_id, "_specification.json")
  api_url   <- paste0("https://api.github.com/repos/", repo_owner, "/",
                      repo_name, "/contents/", file_path)

  raw_result <- .fetch_github_model(api_url, github_token)

  if (!is.null(raw_result$error)) {
    stop(raw_result$error)
  }

  # ---- Retrieve decryption key and log validation event ----------------------
  # Worker A validates the GitHub token, logs the validation event, and returns
  # the AES-256 decryption key. The obfuscation key and per-model salts are
  # fetched directly from Worker B by the C++ engine in Phase 2 — they are
  # never returned to R.
  keys <- .fetch_decryption_key(
    model_id     = model_id,
    n            = nrow(validation_data),
    github_token = github_token,
    repo_owner   = repo_owner,
    repo_name    = repo_name
  )

  # ---- Run prediction engine -------------------------------------------------
  # github_token, repo_owner, repo_name are passed into C++ so that Worker B
  # is called from within the compiled engine — the obfuscation key and
  # per-model salts never appear as R objects.
  result <- .predict_secure(
    encoded_content = raw_result$encoded_content,
    validation_data = validation_data,
    outcome         = outcome,
    by              = by,
    model_id        = model_id,
    decryption_key  = keys$encryption_key,
    github_token    = github_token,
    repo_owner      = repo_owner,
    repo_name       = repo_name
  )

  return(result)
}
