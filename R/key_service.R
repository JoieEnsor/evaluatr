# key_service.R — Internal functions for evaluatr key service integration
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
# Called by generate_model_json() once per model.
# Returns a list with $encryption_key and $registered_at.
# Stops with an informative error on failure.

.register_model_with_key_service <- function(model_id, developer_id, model_name,
                                             obfuscation_key) {

  base_url <- .key_service_url()
  endpoint <- paste0(base_url, "/register")

  body_json <- jsonlite::toJSON(
    list(
      model_id        = jsonlite::unbox(model_id),
      developer_id    = jsonlite::unbox(developer_id),
      model_name      = jsonlite::unbox(model_name),
      obfuscation_key = jsonlite::unbox(obfuscation_key)
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


# ---- .fetch_decryption_key() ------------------------------------------------
#
# Called by secure_model_validation() after the GitHub fetch.
# Returns a list with $encryption_key and $obfuscation_key.
# Stops with an informative error on failure.

.fetch_decryption_key <- function(model_id, n) {

  base_url <- .key_service_url()
  endpoint <- paste0(base_url, "/key")

  body_json <- jsonlite::toJSON(
    list(
      model_id       = jsonlite::unbox(model_id),
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

  if (response$status_code == 404) {
    stop("evaluatr key service: model '", model_id, "' is not registered. ",
         "The developer must run generate_model_json() before the model can be validated.",
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
  if (is.null(resp_body$obfuscation_key) || nchar(resp_body$obfuscation_key) != 32) {
    stop("evaluatr key service returned an invalid obfuscation key. ",
         "Contact the package maintainer.", call. = FALSE)
  }

  list(
    encryption_key  = resp_body$encryption_key,
    obfuscation_key = resp_body$obfuscation_key
  )
}
