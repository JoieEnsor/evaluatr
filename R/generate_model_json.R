# generate_model_json.R -- Developer utility for creating obfuscated model JSON files
#
# Exports: generate_model_json()
# Internal helpers: .extract_from_glm(), .extract_from_coxph(),
#                   .extract_from_survreg(), .extract_from_multinom(),
#                   .validate_json_structure(), .assemble_model_json()

# ---- .extract_from_glm() ----------------------------------------------------
# Extract coefficients and metadata from a fitted glm object.
#
# @param model A fitted glm object (binomial family for logistic).
# @return Named list with model_type, coefficients, variables, outcome_type.

.extract_from_glm <- function(model) {
  if (!inherits(model, "glm")) {
    stop("Expected a glm object")
  }
  fam <- model$family$family
  if (!identical(fam, "binomial")) {
    stop("Only binomial (logistic) glm models are supported. Got family: ", fam)
  }
  coeffs    <- coef(model)
  all_names <- names(coeffs)
  pred_vars <- all_names[all_names != "(Intercept)"]

  list(
    model_type   = "logistic",
    coefficients = coeffs,
    variables    = pred_vars,
    outcome_type = "binary"
  )
}


# ---- .extract_from_coxph() --------------------------------------------------
# Extract coefficients and metadata from a fitted coxph object.
#
# @param model A fitted coxph object.
# @return Named list with model_type, coefficients, variables, outcome_type.
# Note: model_parameters (timepoints, baseline_survival) must be supplied
# by the caller via the generate_model_json() model_parameters argument.

.extract_from_coxph <- function(model) {
  if (!inherits(model, "coxph")) {
    stop("Expected a coxph object")
  }
  coeffs    <- coef(model)
  pred_vars <- names(coeffs)  # Cox has no intercept term

  list(
    model_type   = "cox",
    coefficients = coeffs,
    variables    = pred_vars,
    outcome_type = "survival"
  )
}


# ---- .extract_from_survreg() ------------------------------------------------
# Extract coefficients and shape parameter from a fitted survreg object.
#
# survreg uses an AFT parameterisation by default (dist = "weibull").
# The scale parameter from survreg corresponds to 1/shape in the Weibull.
#
# @param model A fitted survreg object.
# @return Named list with model_type, coefficients, variables, outcome_type,
#   and model_parameters (shape, parameterisation). Caller must supply
#   timepoints via generate_model_json() model_parameters argument and
#   those are merged in.

.extract_from_survreg <- function(model) {
  if (!inherits(model, "survreg")) {
    stop("Expected a survreg object")
  }
  dist <- model$dist
  if (!dist %in% c("weibull", "exponential")) {
    stop("Only Weibull (and exponential) survreg models are supported. Got dist: ", dist)
  }
  coeffs    <- coef(model)
  all_names <- names(coeffs)
  pred_vars <- all_names[all_names != "(Intercept)"]

  # survreg scale = 1/shape for Weibull
  shape_val <- if (dist == "exponential") 1.0 else 1.0 / model$scale

  list(
    model_type      = "weibull",
    coefficients    = coeffs,
    variables       = pred_vars,
    outcome_type    = "survival",
    extracted_shape = shape_val
  )
}


# ---- .extract_from_multinom() -----------------------------------------------
# Extract per-category coefficients from a fitted multinom object (nnet).
#
# multinom returns a coefficient matrix with one row per non-reference
# category. We restructure this into a named list of named vectors.
#
# @param model A fitted multinom object.
# @param reference_category Character. Name of the reference category.
#   If NULL, uses the first level of the response variable.
# @return Named list with model_type, coefficients (nested list),
#   variables, outcome_type, category_names, reference_category.

.extract_from_multinom <- function(model, reference_category = NULL) {
  if (!inherits(model, "multinom")) {
    stop("Expected a multinom object (from nnet package)")
  }

  coef_mat <- coef(model)

  # coef() for multinom returns a matrix (k-1 rows x p cols) when k >= 3
  # or a named vector when k == 2. Normalise to matrix.
  if (is.vector(coef_mat)) {
    # Two-category outcome -- only one non-reference row
    coef_mat <- matrix(coef_mat, nrow = 1,
                       dimnames = list(model$lev[2], names(coef_mat)))
  }

  non_ref_categories <- rownames(coef_mat)
  all_names          <- colnames(coef_mat)
  pred_vars          <- all_names[all_names != "(Intercept)"]

  # Build nested coefficient list: list(cat1 = c(...), cat2 = c(...))
  coeff_list <- vector("list", length(non_ref_categories))
  names(coeff_list) <- non_ref_categories
  for (cat in non_ref_categories) {
    coeff_list[[cat]] <- coef_mat[cat, ]
  }

  # Determine reference category
  if (is.null(reference_category)) {
    all_levels <- model$lev
    ref_cat    <- setdiff(all_levels, non_ref_categories)
    reference_category <- if (length(ref_cat) == 1) ref_cat else all_levels[1]
  }

  list(
    model_type         = "multinomial",
    coefficients       = coeff_list,
    variables          = pred_vars,
    outcome_type       = "multinomial",
    category_names     = non_ref_categories,
    reference_category = reference_category
  )
}


# ---- .validate_json_structure() ---------------------------------------------
# Validate the assembled JSON structure before writing.
#
# @param json_list Named list representing the JSON structure.
# @return Invisible TRUE, or stops with an informative error.

.validate_json_structure <- function(json_list) {
  # Phase 3b: encrypted format uses encrypted_coefficients instead of coefficients
  is_encrypted <- identical(json_list$metadata$encryption, "aes256gcm")

  if (is_encrypted) {
    # obfuscation_key is held by the key service -- not written to the JSON file
    required_top <- c("model_type", "encrypted_coefficients", "encryption_iv", "metadata")
  } else {
    required_top <- c("model_type", "obfuscation_key", "coefficients", "metadata")
  }
  missing_top  <- setdiff(required_top, names(json_list))
  if (length(missing_top) > 0) {
    stop("JSON structure missing required fields: ", paste(missing_top, collapse = ", "))
  }

  supported_types <- c("logistic", "cox", "weibull", "multinomial")
  if (!json_list$model_type %in% supported_types) {
    stop("Unsupported model_type: '", json_list$model_type, "'. ",
         "Must be one of: ", paste(supported_types, collapse = ", "))
  }

  meta <- json_list$metadata
  required_meta <- c("model_id", "model_name", "version", "outcome_type", "variables")
  missing_meta  <- setdiff(required_meta, names(meta))
  if (length(missing_meta) > 0) {
    stop("metadata missing required fields: ", paste(missing_meta, collapse = ", "))
  }
  if (length(meta$variables) == 0) {
    stop("metadata$variables must contain at least one predictor variable name")
  }

  # Validate coefficients non-empty (skip for encrypted format -- ciphertext is present instead)
  if (!is_encrypted) {
    if (json_list$model_type == "multinomial") {
      if (length(json_list$coefficients) == 0) {
        stop("multinomial model must have at least one non-reference category in coefficients")
      }
    } else {
      if (length(json_list$coefficients) == 0) {
        stop("coefficients must be non-empty")
      }
    }
  }

  # Cox / Weibull require model_parameters
  if (json_list$model_type %in% c("cox", "weibull")) {
    mp <- json_list$model_parameters
    if (is.null(mp)) {
      stop("Cox and Weibull models require model_parameters ",
           "(timepoints and baseline_survival for Cox; timepoints, shape, ",
           "and parameterisation for Weibull)")
    }
    if (is.null(mp$timepoints) || length(mp$timepoints) == 0) {
      stop("model_parameters must include timepoints for ", json_list$model_type, " models")
    }
    if (json_list$model_type == "cox") {
      if (is.null(mp$baseline_survival) || length(mp$baseline_survival) == 0) {
        stop("Cox model_parameters must include baseline_survival")
      }
      if (length(mp$timepoints) != length(mp$baseline_survival)) {
        stop("timepoints and baseline_survival must have the same length")
      }
    }
    if (json_list$model_type == "weibull") {
      if (is.null(mp$shape)) {
        stop("Weibull model_parameters must include shape")
      }
    }
  }

  invisible(TRUE)
}


# ---- .assemble_model_json() -------------------------------------------------
# Assemble the complete v1 JSON list structure from its components.
# Obfuscation is applied here.
#
# @param model_type Character.
# @param raw_coefficients Named numeric vector (flat) or named list (multinomial).
# @param obfuscation_key Character. 32-char hex key.
# @param preprocessing Character or NULL.
# @param model_parameters Named list or NULL.
# @param metadata Named list.
# @return Named list representing the complete JSON structure.

.assemble_model_json <- function(model_type, raw_coefficients, obfuscation_key,
                                 salt_a, salt_b,
                                 preprocessing, model_parameters, metadata,
                                 encrypted_b64 = NULL, iv_b64 = NULL) {

  if (is.null(encrypted_b64)) {
    # Unencrypted path: obfuscate and store plaintext coefficients
    if (model_type == "multinomial") {
      # Obfuscate each category independently using the same key.
      # The C++ de-obfuscation calls deobfuscate_coeffs() once per category,
      # each time seeding the PRNG fresh from the same key. We must match this:
      # obfuscate each category's coefficients as a separate call.
      categories <- names(raw_coefficients)
      obf_coefficients <- vector("list", length(categories))
      names(obf_coefficients) <- categories
      for (cat in categories) {
        obf_cat <- .obfuscate_coefficients(
          raw_coefficients[[cat]], obfuscation_key, salt_a, salt_b
        )
        obf_coefficients[[cat]] <- as.list(obf_cat)
      }
      coeff_field <- obf_coefficients
    } else {
      obf_vals    <- .obfuscate_coefficients(
        raw_coefficients, obfuscation_key, salt_a, salt_b
      )
      coeff_field <- as.list(obf_vals)
    }

    structure <- list(
      model_type       = model_type,
      obfuscation_key  = obfuscation_key,
      coefficients     = coeff_field,
      preprocessing    = preprocessing,
      model_parameters = model_parameters,
      metadata         = metadata
    )
  } else {
    # Phase 3b encrypted path: store ciphertext instead of plain coefficients.
    # obfuscation_key is NOT written to the JSON -- it is held exclusively by
    # the key service and returned at validation time alongside the AES key.
    structure <- list(
      model_type             = model_type,
      encrypted_coefficients = encrypted_b64,
      encryption_iv          = iv_b64,
      preprocessing          = preprocessing,
      model_parameters       = model_parameters,
      metadata               = metadata
    )
  }

  structure
}


# ---- generate_model_json() --------------------------------------------------

#' Generate an obfuscated JSON model specification file
#'
#' @description
#' Developer-facing utility that accepts a fitted R model object or a manual
#' coefficient specification and produces a correctly formatted, obfuscated
#' JSON file ready for upload to a private GitHub repository for use with
#' [secure_model_validation()].
#'
#' Coefficients are obfuscated using the Phase 1 C++ engine so that the values
#' stored in the JSON file are meaningless without the obfuscation key and the
#' compiled binary salt. The obfuscation key is embedded in the JSON file.
#'
#' @param model A fitted model object. Supported classes: `glm` (binomial
#'   family), `coxph`, `survreg` (Weibull/exponential), `multinom` (nnet).
#'   Exactly one of `model` or `coefficients` must be non-NULL.
#' @param coefficients A named numeric vector (for flat models) or named list
#'   of named numeric vectors (for multinomial, one entry per non-reference
#'   category). Exactly one of `model` or `coefficients` must be non-NULL.
#' @param model_type Character. Required when `coefficients` is supplied.
#'   One of `"logistic"`, `"cox"`, `"weibull"`, `"multinomial"`.
#' @param model_id Character. Unique identifier for this model (required).
#'   Must match the folder name in the GitHub repository (e.g. `"aki_v1"`).
#' @param developer_id Character. Your identifier (e.g. GitHub username). Used
#'   to attribute the model in the evaluatr key service registry (required).
#' @param model_name Character. Human-readable name (required).
#' @param outcome_type Character. One of `"binary"`, `"survival"`,
#'   `"multinomial"`. Required when `coefficients` is supplied; inferred
#'   automatically from model objects.
#' @param variables Character vector of predictor variable names (not including
#'   `"(Intercept)"`). Required when `coefficients` is supplied; inferred from
#'   model objects.
#' @param description Character. Free-text description of the model. Default `""`.
#' @param version Character. Version string. Default `"1.0"`.
#' @param preprocessing Character or NULL. R code string that will be evaluated
#'   on the evaluator's `validation_data` before prediction (e.g. to create
#'   dummy variables). Default `NULL`.
#' @param model_parameters Named list or NULL. Additional model parameters
#'   required for Cox and Weibull models:
#'   \itemize{
#'     \item Cox: `list(timepoints = c(...), baseline_survival = c(...))`
#'     \item Weibull: `list(timepoints = c(...), shape = <value>,
#'       parameterisation = "aft")` (shape is extracted automatically from
#'       `survreg` objects but can be overridden here).
#'   }
#' @param reference_category Character or NULL. For multinomial models: name of
#'   the reference category (the category whose coefficients are all zero).
#'   Required when `model_type = "multinomial"` and `coefficients` is supplied.
#'   Inferred automatically from `multinom` objects.
#' @param output_dir Character. Directory to write the JSON file. Created if it
#'   does not exist. Default `"."`.
#' @param output_filename Character or NULL. Filename for the JSON output.
#'   Default `"coefficients.json"`.
#'
#' @return The complete JSON structure as an R list (invisibly). The primary
#'   side effect is writing the JSON file to `{output_dir}/{output_filename}`.
#'
#' @details
#' The generated JSON follows the v1 format used by the C++ prediction engine:
#'
#' ```json
#' {
#'   "model_type": "logistic",
#'   "obfuscation_key": "<32-char hex>",
#'   "coefficients": { "(Intercept)": <obfuscated>, "age": <obfuscated> },
#'   "preprocessing": null,
#'   "model_parameters": null,
#'   "metadata": {
#'     "model_id": "...", "model_name": "...", "version": "1.0",
#'     "outcome_type": "binary", "variables": ["age"], "description": "..."
#'   }
#' }
#' ```
#'
#' Coefficient values are obfuscated using an affine transform seeded from the
#' obfuscation key and a compiled binary salt. The key is stored in the JSON
#' so the C++ engine can de-obfuscate at prediction time; it is NOT a secret,
#' but the compiled salt is. Coefficients cannot be recovered without the
#' compiled binary.
#'
#' @examples
#' \dontrun{
#' # Logistic regression from a fitted glm
#' model <- glm(AKI ~ creatinine_mean + bun_mean,
#'              data = mydata, family = binomial)
#' generate_model_json(
#'   model      = model,
#'   model_id   = "aki_v1",
#'   model_name = "AKI Prediction Model",
#'   output_dir = "models/aki_v1"
#' )
#'
#' # Logistic from manual coefficients
#' generate_model_json(
#'   coefficients = c("(Intercept)" = -1.25, age = 0.02, score = 0.8),
#'   model_type   = "logistic",
#'   model_id     = "manual_v1",
#'   model_name   = "Manual Model",
#'   outcome_type = "binary",
#'   variables    = c("age", "score"),
#'   output_dir   = "models/manual_v1"
#' )
#'
#' # Multinomial with preprocessing
#' model <- nnet::multinom(outcome ~ age + sex, data = mydata)
#' generate_model_json(
#'   model              = model,
#'   model_id           = "outcome_v1",
#'   model_name         = "Outcome Model",
#'   reference_category = "discharged",
#'   preprocessing      = "validation_data$sexfemale <- ifelse(validation_data$sex == 'female', 1, 0)",
#'   output_dir         = "models/outcome_v1"
#' )
#' }
#'
#' @importFrom stats coef
#' @export
generate_model_json <- function(
    model              = NULL,
    coefficients       = NULL,
    model_type         = NULL,
    model_id           = NULL,
    developer_id       = NULL,
    model_name         = NULL,
    outcome_type       = NULL,
    variables          = NULL,
    description        = "",
    version            = "1.0",
    preprocessing      = NULL,
    model_parameters   = NULL,
    reference_category = NULL,
    output_dir         = ".",
    output_filename    = NULL
) {

  # ---- Input validation -------------------------------------------------------
  if (is.null(model_id) || !nzchar(trimws(model_id))) {
    stop("model_id is required and must be a non-empty string")
  }
  if (is.null(developer_id) || !nzchar(trimws(developer_id))) {
    stop("developer_id is required and must be a non-empty string ",
         "(e.g. your GitHub username)")
  }
  if (is.null(model_name) || !nzchar(trimws(model_name))) {
    stop("model_name is required and must be a non-empty string")
  }
  if (!is.null(model) && !is.null(coefficients)) {
    stop("Provide either 'model' or 'coefficients', not both")
  }
  if (is.null(model) && is.null(coefficients)) {
    stop("One of 'model' or 'coefficients' must be provided")
  }

  # ---- Extract from model object or validate manual spec ----------------------
  if (!is.null(model)) {
    extracted <- if (inherits(model, "glm")) {
      .extract_from_glm(model)
    } else if (inherits(model, "coxph")) {
      .extract_from_coxph(model)
    } else if (inherits(model, "survreg")) {
      .extract_from_survreg(model)
    } else if (inherits(model, "multinom")) {
      .extract_from_multinom(model, reference_category)
    } else {
      stop("Unsupported model class: '", paste(class(model), collapse = ", "), "'. ",
           "Supported classes: glm (binomial), coxph, survreg, multinom")
    }

    raw_coefficients   <- extracted$coefficients
    model_type         <- extracted$model_type
    outcome_type       <- outcome_type %||% extracted$outcome_type
    variables          <- variables    %||% extracted$variables
    reference_category <- reference_category %||% extracted$reference_category

    # Merge extracted model_parameters (e.g. Weibull shape) with caller-supplied ones
    if (!is.null(extracted$extracted_shape)) {
      model_parameters <- model_parameters %||% list()
      if (is.null(model_parameters$shape)) {
        model_parameters$shape <- extracted$extracted_shape
      }
      if (is.null(model_parameters$parameterisation)) {
        model_parameters$parameterisation <- "aft"
      }
    }

  } else {
    # Manual coefficient path
    if (is.null(model_type) || !nzchar(trimws(model_type))) {
      stop("model_type is required when supplying manual coefficients")
    }
    if (!model_type %in% c("logistic", "cox", "weibull", "multinomial")) {
      stop("model_type must be one of: logistic, cox, weibull, multinomial")
    }
    if (is.null(outcome_type)) {
      outcome_type <- switch(model_type,
        logistic    = "binary",
        cox         = "survival",
        weibull     = "survival",
        multinomial = "multinomial"
      )
    }
    if (is.null(variables)) {
      stop("variables is required when supplying manual coefficients")
    }

    if (model_type == "multinomial") {
      # coefficients must be a named list of named vectors
      if (!is.list(coefficients) || is.null(names(coefficients))) {
        stop("For multinomial models, 'coefficients' must be a named list ",
             "(one entry per non-reference category)")
      }
      if (is.null(reference_category)) {
        stop("reference_category is required for multinomial models when ",
             "supplying manual coefficients")
      }
    }

    raw_coefficients <- coefficients
  }

  # ---- Ensure variables is set ------------------------------------------------
  if (is.null(variables) || length(variables) == 0) {
    stop("Could not determine predictor variable names. ",
         "Please supply the 'variables' argument explicitly.")
  }

  # ---- Early validation: Cox/Weibull model_parameters -------------------------
  # Check before registering with the key service so missing-parameter errors
  # fire immediately without making a network call.
  if (model_type %in% c("cox", "weibull")) {
    if (is.null(model_parameters) || is.null(model_parameters$timepoints) ||
        length(model_parameters$timepoints) == 0) {
      stop("Cox and Weibull models require model_parameters with timepoints")
    }
    if (model_type == "cox") {
      if (is.null(model_parameters$baseline_survival) ||
          length(model_parameters$baseline_survival) == 0) {
        stop("Cox model_parameters must include baseline_survival")
      }
      if (length(model_parameters$timepoints) !=
          length(model_parameters$baseline_survival)) {
        stop("timepoints and baseline_survival must have the same length")
      }
    }
    if (model_type == "weibull" && is.null(model_parameters$shape)) {
      stop("Weibull model_parameters must include shape")
    }
  }

  # ---- Generate obfuscation key and per-model salts ---------------------------
  obfuscation_key <- .generate_obfuscation_key()
  # Per-model salts: two independent random 64-bit values as 16-char hex strings.
  # Stored exclusively in the key service; never written to the JSON file.
  # In Phase 2, C++ reads these from Worker B at prediction time instead of
  # using the compiled SALT_A / SALT_B constants.
  salt_a <- .generate_salt64()
  salt_b <- .generate_salt64()

  # ---- Build metadata ---------------------------------------------------------
  metadata <- list(
    model_id     = model_id,
    model_name   = model_name,
    version      = version,
    outcome_type = outcome_type,
    variables    = as.list(variables),
    description  = description %||% ""
  )
  if (!is.null(reference_category) && model_type == "multinomial") {
    metadata$reference_category <- reference_category
  }

  # ---- Phase 3a/3b: Register model with key service --------------------------
  # Mandatory chokepoint: every model registration is logged.
  # The obfuscation_key and per-model salts are sent to the key service here
  # and NOT written to the JSON file. At validation time, Worker A returns the
  # encryption key (R side) and Worker B returns the obfuscation key + salts
  # (C++ side, Phase 2).
  registration <- .register_model_with_key_service(
    model_id        = model_id,
    developer_id    = developer_id,
    model_name      = model_name,
    obfuscation_key = obfuscation_key,
    salt_a          = salt_a,
    salt_b          = salt_b
  )

  # ---- Phase 3b: AES-256-GCM encrypt the obfuscated coefficients -------------
  # Obfuscate coefficients, serialise to JSON, then encrypt the JSON string.
  # The ciphertext + nonce are stored in the JSON; plaintext coefficients are not.
  key_raw <- .hex_to_raw(registration$encryption_key)

  coeff_json <- jsonlite::toJSON(
    if (model_type == "multinomial") {
      lapply(raw_coefficients, function(cat_coeffs) {
        as.list(.obfuscate_coefficients(
          cat_coeffs, obfuscation_key, salt_a, salt_b
        ))
      })
    } else {
      as.list(.obfuscate_coefficients(
        raw_coefficients, obfuscation_key, salt_a, salt_b
      ))
    },
    auto_unbox = TRUE,
    digits     = 10
  )

  iv         <- openssl::rand_bytes(12)   # 96-bit nonce for AES-GCM
  ciphertext <- openssl::aes_gcm_encrypt(
    data = charToRaw(coeff_json),
    key  = key_raw,
    iv   = iv
  )
  encrypted_b64 <- openssl::base64_encode(ciphertext)
  iv_b64        <- openssl::base64_encode(iv)

  # Add encryption marker to metadata
  metadata$encryption <- "aes256gcm"

  # ---- Assemble full JSON structure -------------------------------------------
  json_list <- .assemble_model_json(
    model_type       = model_type,
    raw_coefficients = raw_coefficients,
    obfuscation_key  = obfuscation_key,
    preprocessing    = preprocessing,
    model_parameters = model_parameters,
    metadata         = metadata,
    encrypted_b64    = encrypted_b64,
    iv_b64           = iv_b64
  )

  # ---- Validate ---------------------------------------------------------------
  .validate_json_structure(json_list)

  # ---- Write to file ----------------------------------------------------------
  filename <- output_filename %||% "coefficients.json"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_path <- file.path(output_dir, filename)

  json_str <- jsonlite::toJSON(json_list,
                               auto_unbox = TRUE,
                               null       = "null",
                               pretty     = TRUE,
                               digits     = 10)
  writeLines(json_str, output_path)

  # ---- Summary message --------------------------------------------------------
  n_coeffs <- if (model_type == "multinomial") {
    sum(sapply(raw_coefficients, length))
  } else {
    length(raw_coefficients)
  }

  message("evaluatr: model JSON written to '", output_path, "'")
  message("  model_id     : ", model_id)
  message("  developer_id : ", developer_id)
  message("  registered_at: ", registration$registered_at)
  message("  model_type   : ", model_type)
  message("  n_coeffs    : ", n_coeffs,
          if (model_type == "multinomial")
            paste0(" (across ", length(raw_coefficients), " non-reference categories)")
          else "")
  message("  variables   : ", paste(variables, collapse = ", "))
  if (!is.null(preprocessing) && nzchar(preprocessing)) {
    message("  preprocessing: <included>")
  }
  message("  obfuscation_key: <held by key service -- not stored in JSON>")
  message("  encryption     : aes256gcm")
  message("NOTE: Coefficients are AES-256-GCM encrypted (key held by key service) ",
          "and obfuscated (protected by compiled C++ binary salt). ",
          "Keep the JSON in a private repository.")

  invisible(json_list)
}
