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
#'   \item The model **developer** uploads a JSON specification
#'     (coefficients + prediction code) to a private GitHub repository and
#'     issues a time-limited fine-grained access token to the evaluator.
#'   \item The **evaluator** calls [secure_model_validation()] with their
#'     local dataset and the developer-provided token. Predictions are
#'     computed locally; the output is a shuffled prediction-outcome matrix
#'     that prevents coefficient reverse-engineering.
#'   \item The evaluator passes the result to [eval_performance()]
#'     to obtain discrimination, calibration, and utility metrics with
#'     optional bootstrap confidence intervals and diagnostic plots.
#' }
#'
#' @section Security:
#' Model coefficients are retrieved from GitHub, used to compute predictions,
#' then immediately cleared from memory. The returned matrix is row-shuffled
#' so that predictions cannot be matched back to individual patient records.
#' A minimum dataset size of 20 observations (per subgroup) is enforced to
#' prevent both near-individual prediction and coefficient reverse-engineering.
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
#' @importFrom jsonlite fromJSON base64_dec base64_enc
#' @importFrom ggplot2 ggplot aes geom_violin geom_jitter labs theme_classic theme element_text scale_fill_manual
#' @importFrom ResourceSelection hoslem.test
#' @importFrom rms rcs
#' @importFrom rmda decision_curve plot_decision_curve
#' @importFrom CalibrationCurves val.prob.ci.2
"_PACKAGE"

# Suppress R CMD check NOTEs for non-standard evaluation in ggplot2 aes() and
# rmda formula interfaces
utils::globalVariables(c("outcome", "prediction"))
