# key_service.R -- Internal functions for evaluatr key service integration
#
# Base URL is read from getOption("evaluatr.key_service_url"), falling back
# to the production URL. Override in tests or local development:
#   options(evaluatr.key_service_url = "http://localhost:8000")

# ---- .key_service_url() -----------------------------------------------------
# Return the configured key service base URL (no trailing slash).

.key_service_url <- function() {
  url <- getOption("evaluatr.key_service_url",
                   default = "https://evaluatr-key-service.joie-ensor.workers.dev")
  sub("/+$", "", url)
}


# ---- .register_model_with_key_service() -------------------------------------
#
# Called by register_model() once per model.
# Returns a list with $encryption_key and $registered_at.
# Stops with an informative error on failure.
#
# Optional registry fields (developer_name, developer_email, model_description,
# public_listing) are forwarded to the key service for the public directory.

.register_model_with_key_service <- function(model_id, developer_id, model_name,
                                             obfuscation_key, salt_a, salt_b,
                                             registrant_relationship,
                                             developer_name    = NULL,
                                             developer_email   = NULL,
                                             model_description = NULL,
                                             public_listing    = TRUE,
                                             rate_limit_exempt = FALSE) {

  valid_relationships <- c("original_developer", "authorised_proxy", "independent")
  if (!registrant_relationship %in% valid_relationships) {
    stop("'registrant_relationship' must be one of: ",
         paste(valid_relationships, collapse = ", "), call. = FALSE)
  }

  base_url <- .key_service_url()
  endpoint <- paste0(base_url, "/register")

  body_list <- list(
    model_id                = jsonlite::unbox(model_id),
    developer_id            = jsonlite::unbox(developer_id),
    model_name              = jsonlite::unbox(model_name),
    obfuscation_key         = jsonlite::unbox(obfuscation_key),
    salt_a                  = jsonlite::unbox(salt_a),
    salt_b                  = jsonlite::unbox(salt_b),
    registrant_relationship = jsonlite::unbox(registrant_relationship),
    public_listing          = jsonlite::unbox(isTRUE(public_listing)),
    rate_limit_exempt       = jsonlite::unbox(isTRUE(rate_limit_exempt))
  )
  if (!is.null(developer_name) && nzchar(trimws(developer_name))) {
    body_list$developer_name <- jsonlite::unbox(trimws(developer_name))
  }
  if (!is.null(developer_email) && nzchar(trimws(developer_email))) {
    body_list$developer_email <- jsonlite::unbox(trimws(developer_email))
  }
  if (!is.null(model_description) && nzchar(trimws(model_description))) {
    body_list$model_description <- jsonlite::unbox(trimws(model_description))
  }

  body_json <- jsonlite::toJSON(body_list, auto_unbox = FALSE)

  h <- curl::new_handle()
  curl::handle_setheaders(h,
    "Content-Type" = "application/json",
    "User-Agent"   = paste0("evaluatr-r-package/",
                            utils::packageVersion("evaluatr"))
  )
  curl::handle_setopt(h,
    post        = TRUE,
    postfields  = body_json,
    timeout     = 30,
    ssl_verifypeer = TRUE
  )

  response <- tryCatch(
    curl::curl_fetch_memory(endpoint, handle = h),
    error = function(e) {
      stop("evaluatr key service unreachable at '", base_url, "'. ",
           "Check your network connection or set options(evaluatr.key_service_url = ...) ",
           "to point to a running instance.\nOriginal error: ", e$message,
           call. = FALSE)
    }
  )

  resp_body <- tryCatch(
    jsonlite::fromJSON(rawToChar(response$content)),
    error = function(e) list(error = paste0("Non-JSON response from key service: ",
                                            rawToChar(response$content)))
  )

  if (response$status_code == 409) {
    stop("evaluatr key service: model_id '", model_id, "' is already registered. ",
         "Use a different model_id or contact the package maintainer.",
         call. = FALSE)
  }

  if (response$status_code != 200) {
    err_msg <- if (!is.null(resp_body$error)) resp_body$error else
      paste0("HTTP ", response$status_code, " from key service")
    stop("evaluatr key service registration failed: ", err_msg, call. = FALSE)
  }

  if (is.null(resp_body$encryption_key) || nchar(resp_body$encryption_key) != 64) {
    stop("evaluatr key service returned an invalid encryption key. ",
         "Contact the package maintainer.", call. = FALSE)
  }

  list(
    encryption_key = resp_body$encryption_key,
    registered_at  = resp_body$registered_at
  )
}


# ---- list_registered_models() -----------------------------------------------

#' List models registered in the evaluatr public directory
#'
#' @description
#' Queries the evaluatr registry for all publicly listed models. Returns a
#' data frame you can use to find models available for validation and to
#' identify the developer contact for requesting a validation token.
#'
#' Only models registered with `public_listing = TRUE` (the default in
#' [register_model()]) appear in the results. By default only endorsed models
#' are returned; set `include_unendorsed = TRUE` to see all registered models.
#'
#' @param as_data_frame Logical. If `TRUE` (default) return a `data.frame`.
#'   If `FALSE` return the raw parsed list from the JSON response.
#' @param include_unendorsed Logical. If `FALSE` (default) only return models
#'   endorsed by the evaluatr registry. Set to `TRUE` to include all publicly
#'   listed models regardless of endorsement status.
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{model_id}{Unique model identifier (pass to
#'       [secure_model_validation()] as `model_id`).}
#'     \item{model_name}{Human-readable model name.}
#'     \item{developer_id}{Developer's evaluatr identifier.}
#'     \item{developer_name}{Developer's name (may be `NA` if not provided
#'       at registration).}
#'     \item{developer_email}{Contact email for requesting a validation token
#'       (may be `NA` if not provided at registration).}
#'     \item{model_description}{Free-text description of the model (may be
#'       `NA` if not provided at registration).}
#'     \item{registrant_relationship}{Registrant's declared relationship to
#'       the model: `"original_developer"`, `"authorised_proxy"`, or
#'       `"independent"`.}
#'     \item{endorsed}{Integer flag: `1` if the model has been endorsed by
#'       the evaluatr registry, `0` otherwise.}
#'   }
#'
#' @details
#' To validate a model, contact the developer at the listed email address and
#' request a time-limited access token. Once you have the token, pass it to
#' [secure_model_validation()] along with the `developer_id` as `repo_owner`,
#' the repository name the developer provides as `repo_name`, and the
#' `model_id` from this table.
#'
#' @examples
#' \dontrun{
#' # Browse endorsed models (default)
#' models <- list_registered_models()
#' print(models[, c("model_id", "model_name", "developer_email")])
#'
#' # Include unendorsed models
#' all_models <- list_registered_models(include_unendorsed = TRUE)
#'
#' # Find models by a specific developer
#' subset(models, developer_id == "JoieEnsor")
#' }
#'
#' @seealso [secure_model_validation()], [register_model()]
#' @export
list_registered_models <- function(as_data_frame = TRUE,
                                   include_unendorsed = FALSE) {

  base_url <- .key_service_url()
  endpoint <- paste0(base_url, "/models",
                     if (isTRUE(include_unendorsed)) "?include_unendorsed=true" else "")

  h <- curl::new_handle()
  curl::handle_setheaders(h,
    "User-Agent" = paste0("evaluatr-r-package/",
                          utils::packageVersion("evaluatr"))
  )
  curl::handle_setopt(h, timeout = 30, ssl_verifypeer = TRUE)

  response <- tryCatch(
    curl::curl_fetch_memory(endpoint, handle = h),
    error = function(e) {
      stop("evaluatr key service unreachable at '", base_url, "'. ",
           "Check your network connection or set ",
           "options(evaluatr.key_service_url = ...) ",
           "to point to a running instance.\nOriginal error: ", e$message,
           call. = FALSE)
    }
  )

  resp_body <- tryCatch(
    jsonlite::fromJSON(rawToChar(response$content)),
    error = function(e) {
      list(error = paste0("Non-JSON response from key service: ",
                          rawToChar(response$content)))
    }
  )

  if (response$status_code != 200) {
    err_msg <- if (!is.null(resp_body$error)) resp_body$error else
      paste0("HTTP ", response$status_code, " from key service")
    stop("evaluatr key service error: ", err_msg, call. = FALSE)
  }

  if (!isTRUE(as_data_frame)) {
    return(resp_body)
  }

  models <- resp_body$models
  expected_cols <- c("model_id", "model_name", "developer_id",
                     "developer_name", "developer_email",
                     "model_description", "registrant_relationship",
                     "endorsed")

  if (is.null(models) || length(models) == 0) {
    df <- as.data.frame(
      matrix(character(0), nrow = 0, ncol = length(expected_cols)),
      stringsAsFactors = FALSE
    )
    names(df) <- expected_cols
    return(df)
  }

  # fromJSON may return a data.frame (when all fields are present and non-NULL)
  # or a list of lists (when some fields are NULL). Normalise to data.frame via
  # row-by-row construction so NULL fields become NA_character_.
  if (is.data.frame(models)) {
    df <- models
  } else {
    rows <- lapply(models, function(m) {
      as.data.frame(
        lapply(m, function(v) if (is.null(v)) NA_character_ else as.character(v)),
        stringsAsFactors = FALSE
      )
    })
    df <- do.call(rbind, rows)
  }

  # Ensure expected columns exist (service may omit optional NULLs entirely)
  for (col in expected_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  # Flatten any residual list-columns (NULL entries from fromJSON data.frame path)
  for (col in expected_cols) {
    if (is.list(df[[col]])) {
      df[[col]] <- vapply(df[[col]],
                          function(v) if (is.null(v) || length(v) == 0)
                            NA_character_ else as.character(v[[1]]),
                          character(1))
    }
  }

  df[, expected_cols]
}


# ---- .fetch_decryption_key() ------------------------------------------------
#
# Called by secure_model_validation() after the GitHub fetch.
# Sends the evaluator's GitHub token to Worker A for validation.
# Returns a list with $encryption_key only (obfuscation key and salts are
# fetched directly from Worker B by the C++ engine in Phase 2).
# Stops with an informative error on failure.

.fetch_decryption_key <- function(model_id, n, github_token, repo_owner, repo_name) {

  base_url <- .key_service_url()
  endpoint <- paste0(base_url, "/key")

  body_json <- jsonlite::toJSON(
    list(
      model_id       = jsonlite::unbox(model_id),
      github_token   = jsonlite::unbox(github_token),
      repo_owner     = jsonlite::unbox(repo_owner),
      repo_name      = jsonlite::unbox(repo_name),
      pkg_version    = jsonlite::unbox(as.character(utils::packageVersion("evaluatr"))),
      r_version      = jsonlite::unbox(R.version.string),
      n_observations = jsonlite::unbox(as.integer(n))
    ),
    auto_unbox = FALSE
  )

  h <- curl::new_handle()
  curl::handle_setheaders(h,
    "Content-Type" = "application/json",
    "User-Agent"   = paste0("evaluatr-r-package/",
                            utils::packageVersion("evaluatr"))
  )
  curl::handle_setopt(h,
    post        = TRUE,
    postfields  = body_json,
    timeout     = 30,
    ssl_verifypeer = TRUE
  )

  response <- tryCatch(
    curl::curl_fetch_memory(endpoint, handle = h),
    error = function(e) {
      stop("evaluatr key service unreachable at '", base_url, "'. ",
           "Check your network connection or set options(evaluatr.key_service_url = ...) ",
           "to point to a running instance.\nOriginal error: ", e$message,
           call. = FALSE)
    }
  )

  resp_body <- tryCatch(
    jsonlite::fromJSON(rawToChar(response$content)),
    error = function(e) list(error = paste0("Non-JSON response from key service: ",
                                            rawToChar(response$content)))
  )

  if (response$status_code == 401) {
    stop("evaluatr key service: GitHub token validation failed. ",
         "Check that your token is valid and has read access to '",
         repo_owner, "/", repo_name, "'.",
         call. = FALSE)
  }

  if (response$status_code == 429) {
    stop("evaluatr key service: rate limit exceeded for this token and model. ",
         "Contact the model developer to obtain a new token.",
         call. = FALSE)
  }

  if (response$status_code == 404) {
    stop("evaluatr key service: model '", model_id, "' is not registered. ",
         "The developer must run register_model() before the model can be validated.",
         call. = FALSE)
  }

  if (response$status_code != 200) {
    err_msg <- if (!is.null(resp_body$error)) resp_body$error else
      paste0("HTTP ", response$status_code, " from key service")
    stop("evaluatr key service error: ", err_msg, call. = FALSE)
  }

  if (is.null(resp_body$encryption_key) || nchar(resp_body$encryption_key) != 64) {
    stop("evaluatr key service returned an invalid encryption key. ",
         "Contact the package maintainer.", call. = FALSE)
  }

  if (isFALSE(resp_body$endorsed)) {
    message("Note: this model has not been endorsed by the evaluatr registry.")
  }

  list(encryption_key = resp_body$encryption_key)
}
