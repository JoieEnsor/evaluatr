#' DVT Example Dataset
#'
#' A simulated dataset of 9,805 patients from seven studies, provided for use
#' in package examples and vignettes. The research question is whether a
#' logistic regression model can diagnose deep vein thrombosis (DVT) from
#' clinical signs and simple laboratory results available at presentation.
#'
#' @format A CSV file readable with [read.csv()], containing 9,805 rows and
#'   14 columns:
#' \describe{
#'   \item{id}{Integer. Patient identifier.}
#'   \item{study}{Integer. Study identifier (1--7). Studies vary in size and
#'     DVT prevalence; studies 1--5 are suitable for model development and
#'     studies 6--7 for external validation.}
#'   \item{male}{Integer. Sex (0 = female, 1 = male).}
#'   \item{OCuse}{Integer. Oral contraceptive use (0 = no, 1 = yes).}
#'   \item{malignancy}{Integer. Active malignancy (0 = no, 1 = yes).}
#'   \item{surgery}{Integer. Recent surgery (0 = no, 1 = yes).}
#'   \item{ab_leg_trauma}{Integer. Recent leg trauma (0 = no, 1 = yes).}
#'   \item{vein_distension}{Integer. Distension of leg veins (0 = no, 1 = yes).}
#'   \item{calf_diff}{Integer. Calf circumference difference > 3 cm
#'     (0 = no, 1 = yes).}
#'   \item{ab_ddimer}{Integer. Abnormal D-dimer result (0 = normal, 1 = abnormal).}
#'   \item{pregnancy}{Integer. Recent pregnancy (0 = no, 1 = yes).}
#'   \item{immobile}{Integer. Recent immobility for > 24 hours (0 = no, 1 = yes).}
#'   \item{clotting}{Integer. Recent anticoagulant use for clotting
#'     (0 = no, 1 = yes).}
#'   \item{dvt}{Integer. Deep vein thrombosis diagnosis (0 = absent, 1 = present).}
#' }
#'
#' @details
#' The dataset spans seven studies with DVT prevalences ranging from 11\% to
#' 32\%. It is intended to illustrate the complete evaluatr workflow: fit a
#' logistic regression in a development subset, register the model with
#' [register_model()], upload the JSON to GitHub, then have an independent
#' evaluator run [secure_model_validation()] and [eval_performance()] in the
#' validation subset.
#'
#' A suggested split is studies 1--5 for development (n = 7,370) and studies
#' 6--7 for external validation (n = 2,435).
#'
#' This is a **simulated** dataset created for teaching and illustration
#' purposes only.
#'
#' @source Simulated dataset provided with the evaluatr package for
#'   illustrative purposes.
#'
#' @seealso [secure_model_validation()], [eval_performance()],
#'   [register_model()]
#'
#' @name dvt_example
#' @examples
#' dvt_path <- system.file("extdata", "dvt_example.csv", package = "evaluatr")
#' dvt_data <- read.csv(dvt_path)
#' dim(dvt_data)
#' # [1] 9805   14
#' table(dvt_data$study, dvt_data$dvt)
NULL
