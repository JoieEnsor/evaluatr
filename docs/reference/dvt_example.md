# DVT Example Dataset

A simulated dataset of 9,805 patients from seven studies, provided for
use in package examples and vignettes. The research question is whether
a logistic regression model can diagnose deep vein thrombosis (DVT) from
clinical signs and simple laboratory results available at presentation.

## Format

A CSV file readable with
[`read.csv()`](https://rdrr.io/r/utils/read.table.html), containing
9,805 rows and 14 columns:

- id:

  Integer. Patient identifier.

- study:

  Integer. Study identifier (1–7). Studies vary in size and DVT
  prevalence; studies 1–5 are suitable for model development and studies
  6–7 for external validation.

- male:

  Integer. Sex (0 = female, 1 = male).

- OCuse:

  Integer. Oral contraceptive use (0 = no, 1 = yes).

- malignancy:

  Integer. Active malignancy (0 = no, 1 = yes).

- surgery:

  Integer. Recent surgery (0 = no, 1 = yes).

- ab_leg_trauma:

  Integer. Recent leg trauma (0 = no, 1 = yes).

- vein_distension:

  Integer. Distension of leg veins (0 = no, 1 = yes).

- calf_diff:

  Integer. Calf circumference difference \> 3 cm (0 = no, 1 = yes).

- ab_ddimer:

  Integer. Abnormal D-dimer result (0 = normal, 1 = abnormal).

- pregnancy:

  Integer. Recent pregnancy (0 = no, 1 = yes).

- immobile:

  Integer. Recent immobility for \> 24 hours (0 = no, 1 = yes).

- clotting:

  Integer. Recent anticoagulant use for clotting (0 = no, 1 = yes).

- dvt:

  Integer. Deep vein thrombosis diagnosis (0 = absent, 1 = present).

## Source

Simulated dataset provided with the evaluatr package for illustrative
purposes.

## Details

The dataset spans seven studies with DVT prevalences ranging from 11\\
32\\ logistic regression in a development subset, register the model
with
[`register_model()`](https://joieensor.github.io/evaluatr/reference/register_model.md),
upload the JSON to GitHub, then have an independent evaluator run
[`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md)
and
[`eval_performance()`](https://joieensor.github.io/evaluatr/reference/eval_performance.md)
in the validation subset.

A suggested split is studies 1–5 for development (n = 7,370) and studies
6–7 for external validation (n = 2,435).

This is a **simulated** dataset created for teaching and illustration
purposes only.

## See also

[`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md),
[`eval_performance()`](https://joieensor.github.io/evaluatr/reference/eval_performance.md),
[`register_model()`](https://joieensor.github.io/evaluatr/reference/register_model.md)

## Examples

``` r
dvt_path <- system.file("extdata", "dvt_example.csv", package = "evaluatr")
dvt_data <- read.csv(dvt_path)
dim(dvt_data)
#> [1] 9805   14
# [1] 9805   14
table(dvt_data$study, dvt_data$dvt)
#>    
#>        0    1
#>   1 1022  273
#>   2  667  124
#>   3  919  109
#>   4 1395  361
#>   5 1838  662
#>   6  431  203
#>   7 1479  322
```
