# Breast Cancer Survival Example Dataset

A dataset of 2,232 breast cancer patients with relapse-free survival
follow-up, provided for use in package examples and vignettes. The
dataset includes a pre-assigned development/validation split and is
suitable for illustrating the evaluatr workflow with Cox proportional
hazards or Weibull survival models.

## Format

A CSV file readable with
[`read.csv()`](https://rdrr.io/r/utils/read.table.html), containing
2,232 rows and 16 columns:

- id:

  Integer. Patient identifier.

- year:

  Integer. Year of surgery.

- rf:

  Numeric. Relapse-free interval (months).

- rfi:

  Integer. Relapse indicator (0 = censored, 1 = relapsed).

- age:

  Numeric. Age at surgery (years).

- meno:

  Integer. Menopausal status (0 = pre-menopausal, 1 = post-menopausal).

- size:

  Integer. Tumour size category.

- nodes:

  Integer. Number of positive lymph nodes.

- pr:

  Numeric. Progesterone receptor level (fmol/L).

- er:

  Numeric. Oestrogen receptor level (fmol/L).

- hormon:

  Integer. Hormonal therapy (0 = no, 1 = yes).

- \_st:

  Integer. Survival analysis status variable (always 1).

- \_d:

  Integer. Survival analysis event indicator (same as `rfi`).

- \_t:

  Numeric. Survival time in years (derived from `rf`).

- \_t0:

  Numeric. Survival analysis time origin (always 0).

- val:

  Integer. Sample split (0 = development, 1 = validation).

## Source

Dataset provided with the evaluatr package for illustrative purposes.

## Details

The development set (`val == 0`) contains 1,546 patients with 974
relapse events (63\\ contains 686 patients with 299 relapse events (44\\

The dataset is intended to illustrate the evaluatr workflow for survival
models: fit a Cox proportional hazards model in the development set
using survival, register the model with
[`register_model()`](https://joieensor.github.io/evaluatr/reference/register_model.md),
then have an independent evaluator run
[`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md)
in the validation set. Note that performance metric calculation via
[`eval_performance()`](https://joieensor.github.io/evaluatr/reference/eval_performance.md)
currently supports binary outcomes only; survival model validation via
evaluatr produces predictions at specified time-points that can be
evaluated externally.

Column names beginning with `_` (e.g. `_t`) are standard Stata `stset`
variables included for compatibility. In R, refer to these columns using
backtick notation: `` bc_data$`_t` ``.

## See also

[`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md),
[`register_model()`](https://joieensor.github.io/evaluatr/reference/register_model.md)

## Examples

``` r
bc_path <- system.file("extdata", "bc_survival_example.csv",
                       package = "evaluatr")
bc_data <- read.csv(bc_path)
dim(bc_data)
#> [1] 2232   16
# [1] 2232   16
table(bc_data$val, bc_data$rfi)
#>    
#>       0   1
#>   0 572 974
#>   1 387 299
```
