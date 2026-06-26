#' Breast Cancer Survival Example Dataset
#'
#' A dataset of 2,232 breast cancer patients with relapse-free survival
#' follow-up, provided for use in package examples and vignettes. The dataset
#' includes a pre-assigned development/validation split and is suitable for
#' illustrating the evaluatr workflow with Cox proportional hazards or Weibull
#' survival models.
#'
#' @format A CSV file readable with [read.csv()], containing 2,232 rows and
#'   16 columns:
#' \describe{
#'   \item{id}{Integer. Patient identifier.}
#'   \item{year}{Integer. Year of surgery.}
#'   \item{rf}{Numeric. Relapse-free interval (months).}
#'   \item{rfi}{Integer. Relapse indicator (0 = censored, 1 = relapsed).}
#'   \item{age}{Numeric. Age at surgery (years).}
#'   \item{meno}{Integer. Menopausal status (0 = pre-menopausal, 1 = post-menopausal).}
#'   \item{size}{Integer. Tumour size category.}
#'   \item{nodes}{Integer. Number of positive lymph nodes.}
#'   \item{pr}{Numeric. Progesterone receptor level (fmol/L).}
#'   \item{er}{Numeric. Oestrogen receptor level (fmol/L).}
#'   \item{hormon}{Integer. Hormonal therapy (0 = no, 1 = yes).}
#'   \item{_st}{Integer. Survival analysis status variable (always 1).}
#'   \item{_d}{Integer. Survival analysis event indicator (same as \code{rfi}).}
#'   \item{_t}{Numeric. Survival time in years (derived from \code{rf}).}
#'   \item{_t0}{Numeric. Survival analysis time origin (always 0).}
#'   \item{val}{Integer. Sample split (0 = development, 1 = validation).}
#' }
#'
#' @details
#' The development set (\code{val == 0}) contains 1,546 patients with 974
#' relapse events (63\% event rate). The validation set (\code{val == 1})
#' contains 686 patients with 299 relapse events (44\% event rate).
#'
#' The dataset is intended to illustrate the evaluatr workflow for survival
#' models: fit a Cox proportional hazards model in the development set using
#' \pkg{survival}, register the model with [register_model()], then have
#' an independent evaluator run [secure_model_validation()] in the validation
#' set. Note that performance metric calculation via [eval_performance()]
#' currently supports binary outcomes only; survival model validation via
#' evaluatr produces predictions at specified time-points that can be evaluated
#' externally.
#'
#' Column names beginning with \code{_} (e.g. \code{_t}) are standard Stata
#' \code{stset} variables included for compatibility. In R, refer to these
#' columns using backtick notation: \code{bc_data$`_t`}.
#'
#' @source Dataset provided with the evaluatr package for illustrative
#'   purposes.
#'
#' @seealso [secure_model_validation()], [register_model()]
#'
#' @name bc_survival_example
#' @examples
#' bc_path <- system.file("extdata", "bc_survival_example.csv",
#'                        package = "evaluatr")
#' bc_data <- read.csv(bc_path)
#' dim(bc_data)
#' # [1] 2232   16
#' table(bc_data$val, bc_data$rfi)
NULL
