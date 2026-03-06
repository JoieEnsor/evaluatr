#' MIMIC-III Sample Dataset
#'
#' A sample of 3,670 ICU admissions derived from the MIMIC-III clinical
#' database, included with evaluatr for use in package examples and vignettes.
#'
#' @format A data frame with 3,670 rows and 9 columns:
#' \describe{
#'   \item{gender}{Integer. Patient sex (0 = female, 1 = male).}
#'   \item{bicarbonate_mean}{Numeric. Mean serum bicarbonate (mEq/L) during
#'     ICU stay.}
#'   \item{creatinine_mean}{Numeric. Mean serum creatinine (mg/dL) during
#'     ICU stay.}
#'   \item{hemoglobin_mean}{Numeric. Mean haemoglobin (g/dL) during ICU stay.}
#'   \item{potassium_mean}{Numeric. Mean serum potassium (mEq/L) during ICU
#'     stay.}
#'   \item{bun_mean}{Numeric. Mean blood urea nitrogen (mg/dL) during ICU
#'     stay.}
#'   \item{sysbp_mean}{Numeric. Mean systolic blood pressure (mmHg) during
#'     ICU stay.}
#'   \item{spo2_mean}{Numeric. Mean peripheral oxygen saturation (\%) during
#'     ICU stay.}
#'   \item{AKI}{Integer. Acute kidney injury outcome (0 = no AKI, 1 = AKI).}
#' }
#'
#' @source Derived from the MIMIC-III clinical database
#'   (Johnson et al., 2016, \doi{10.1038/sdata.2016.35}).
#'   This sample is provided for illustrative purposes only.
#'
#' @examples
#' mimic_path <- system.file("extdata", "mimic_sample.csv", package = "evaluatr")
#' mimic_sample <- read.csv(mimic_path)
#' head(mimic_sample)
"mimic_sample"
