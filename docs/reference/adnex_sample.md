# ADNEX Validation Dataset

A de-identified validation dataset (N = 1,500) for use with the ADNEX
(Assessment of Different NEoplasias in the adneXa) model, included with
evaluatr for use in package examples and vignettes. Provided by Lasai
Barrenada, KU Leuven.

## Format

An RDS file containing a data frame with 1,500 rows and 33 columns. Key
columns are:

- age:

  Numeric. Patient age in years.

- ca125:

  Numeric. Serum CA-125 level (U/mL). Used in the ADNEX-with model only.

- max_diam_lesion:

  Numeric. Maximum diameter of the lesion (mm).

- prop_solid:

  Numeric. Proportion of the lesion that is solid (0-1 scale).

- locules_gt_10:

  Integer. Whether the number of locules exceeds 10 (0 = no, 1 = yes).

- papillary_count:

  Integer. Number of papillary projections (0-4+).

- acoustic_shadows:

  Integer. Presence of acoustic shadows on ultrasound (0 = absent, 1 =
  present).

- ascites:

  Integer. Presence of ascites (0 = absent, 1 = present).

- oncology_center:

  Integer. Whether the examination was performed at an oncology centre
  (0 = no, 1 = yes).

- outcome5:

  Integer. Five-category outcome classification: 1 = benign, 2 =
  borderline, 3 = stage I invasive, 4 = stage II-IV invasive, 5 =
  secondary metastatic.

- pbenw, pborw, pst1w, pst2_4w, pmetaw:

  Numeric. Reference predictions from the ADNEX-with-CA-125 model
  (developer-computed, for verification).

- pbenwo, pborwo, pst1wo, pst2_4wo, pmetawo:

  Numeric. Reference predictions from the ADNEX-without-CA-125 model
  (for verification).

## Source

Provided by Lasai Barrenada, Department of Public Health and Primary
Care, KU Leuven. De-identified for package distribution.

## Details

The dataset is loaded from `inst/extdata/adnex_sample.rds` using
[`readRDS()`](https://rdrr.io/r/base/readRDS.html). It is stored as an
RDS file rather than a lazy-loaded data object because it contains
grouped tibble metadata that requires the dplyr package to print
correctly.

The ADNEX model is described in: Van Calster B et al. (2014). Evaluating
the risk of ovarian cancer before surgery using the ADNEX model to
differentiate between benign, borderline, early and advanced stage
invasive, and secondary metastatic tumours. *BMJ* **349**: g5920.
[doi:10.1136/bmj.g5920](https://doi.org/10.1136/bmj.g5920)

## See also

[`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md),
the developer guide vignette.

## Examples

``` r
if (FALSE) { # \dontrun{
adnex_path <- system.file("extdata", "adnex_sample.rds",
                          package = "evaluatr")
adnex_data <- readRDS(adnex_path)
dim(adnex_data)
# [1] 1500   33
} # }
```
