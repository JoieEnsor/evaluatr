#' evaluatr: Secure Independent Evaluation of Clinical Prediction Models
#'
#' @description
#' The \pkg{evaluatr} package implements a two-component system for
#' independent external validation of clinical prediction algorithms,
#' designed to simultaneously satisfy:
#'
#' \itemize{
#'   \item **Scientific rigour** -- fully independent evaluators with
#'     complete control over their data and reporting.
#'   \item **Commercial viability** -- model coefficients never leave the
#'     developer's private GitHub repository.
#'   \item **Data sovereignty** -- patient data never leaves the evaluator's
#'     secure local environment.
#' }
#'
#' @section Workflow:
#' \enumerate{
#'   \item The model **developer** uses [register_model()] to create a
#'     protected model specification and registers the model with the evaluatr
#'     registry. The specification is uploaded to a private GitHub repository.
#'     The developer issues a time-limited, fine-grained access token to each
#'     evaluator.
#'   \item The **evaluator** calls [secure_model_validation()] with their
#'     local dataset and the developer-provided token. Predictions are
#'     computed entirely within the evaluator's R session; the output is a
#'     shuffled prediction-outcome matrix.
#'   \item The evaluator passes the result to [eval_performance()]
#'     to obtain discrimination, calibration, and utility metrics with
#'     optional bootstrap confidence intervals and diagnostic plots.
#' }
#'
#' @section Security:
#' Model coefficients are protected at rest and in transit; they are never
#' exposed as readable R objects. Predictions are cleared from memory once
#' returned. The returned matrix is row-shuffled so that predictions cannot
#' be matched back to individual patient records. A minimum dataset size of
#' 50 observations (per subgroup) is enforced to prevent near-individual
#' prediction and coefficient reverse-engineering. Every validation event is
#' logged by the evaluatr registry.
#'
#' @section Example datasets:
#' Three example datasets are included with the package:
#' \itemize{
#'   \item \code{dvt_example} -- simulated DVT diagnosis dataset, 9,805
#'     patients across seven studies (binary outcome). Loaded via
#'     \code{read.csv(system.file("extdata", "dvt_example.csv",
#'     package = "evaluatr"))}.
#'   \item \code{bc_survival_example} -- breast cancer relapse-free survival
#'     dataset, 2,232 patients with a pre-assigned development/validation
#'     split (survival outcome). Loaded via
#'     \code{read.csv(system.file("extdata", "bc_survival_example.csv",
#'     package = "evaluatr"))}.
#'   \item \code{adnex_sample} -- de-identified ADNEX ovarian cancer
#'     validation dataset, 1,500 patients (multinomial outcome). Loaded via
#'     \code{readRDS(system.file("extdata", "adnex_sample.rds",
#'     package = "evaluatr"))}.
#' }
#'
#' @references
#' Ensor J, Van Calster B, Wynants L, Perry BI. *A system for independent
#' evaluation of clinical predictive algorithms while preserving
#' implementation viability.* (manuscript in preparation)
#'
#' @keywords package
#' @useDynLib evaluatr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom curl new_handle handle_setheaders handle_setopt curl_fetch_memory
#' @importFrom jsonlite fromJSON toJSON base64_dec base64_enc
#' @importFrom ggplot2 ggplot aes geom_violin geom_jitter labs theme_classic theme element_text scale_fill_manual
#' @importFrom ResourceSelection hoslem.test
#' @importFrom rms rcs
#' @importFrom rmda decision_curve plot_decision_curve
#' @importFrom CalibrationCurves val.prob.ci.2
"_PACKAGE"

# Suppress R CMD check NOTEs for non-standard evaluation in ggplot2 aes() and
# rmda formula interfaces
utils::globalVariables(c("outcome", "prediction"))
