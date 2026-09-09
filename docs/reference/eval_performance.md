# Calculate Performance Metrics from a Validation Result

Computes a comprehensive set of discrimination, calibration, and utility
performance metrics from the shuffled prediction-outcome matrix returned
by
[`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md).
Optionally generates diagnostic plots and bootstrap confidence
intervals.

## Usage

``` r
eval_performance(
  validation_result,
  generate_plots = TRUE,
  confidence_intervals = TRUE,
  n_boot = 200,
  plot_title = NULL,
  decision_threshold = 0.1,
  by = NULL,
  cal_args = list(),
  dca_args = list()
)
```

## Arguments

- validation_result:

  List. Output from
  [`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md).

- generate_plots:

  Logical. Whether to produce calibration, decision curve, and
  probability distribution plots. Default `TRUE`.

- confidence_intervals:

  Logical. Whether to compute 95% bootstrap confidence intervals.
  Default `TRUE`.

- n_boot:

  Integer. Number of bootstrap resamples. Default `200`. Increase to
  1000 for publication-quality confidence intervals.

- plot_title:

  Character or `NULL`. Optional title suffix added to plot headings.

- decision_threshold:

  Numeric. Probability threshold used for net benefit calculations in
  the decision curve. Default `0.1`. Set to the clinically relevant
  threshold for your outcome.

- by:

  Vector or `NULL`. Optional subgroup vector of the same length as the
  predictions (pass `result$shuffled_by` from the output of
  [`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md)
  with the `by` argument). When provided, metrics are computed
  separately for each level of this variable.

- cal_args:

  Named list of additional arguments controlling the calibration plot.
  Supported options:

  `g`

  :   Integer. Number of groups. Default `10`.

  `smooth`

  :   Character. Flexible calibration curve method: `"loess"` (default)
      or `"rcs"`.

  `CL.smooth`

  :   Whether to show the confidence band around the flexible curve:
      `"fill"` (default shaded band), `"line"`, or `FALSE` to suppress.

  `xlim`

  :   Numeric vector of length 2. X-axis limits. Default `c(0, 1)`.

  `ylim`

  :   Numeric vector of length 2. Y-axis limits. Default `c(0, 1)`.

- dca_args:

  Named list of additional arguments controlling the decision curve
  analysis. Supported options:

  `thresholds`

  :   Numeric vector. Threshold probabilities at which to evaluate net
      benefit. Default `seq(0, 1, by = 0.01)`.

  `standardize`

  :   Logical. If `TRUE`, plot standardised net benefit instead of net
      benefit. Default `FALSE`.

  `policy`

  :   Character. `"opt-in"` (default, treat high-risk patients) or
      `"opt-out"` (withhold treatment from low-risk patients).

## Value

A named list:

- `metrics`:

  Data frame of performance metrics. When `by` is supplied a `Subgroup`
  column is prepended.

- `plots`:

  List of plot objects: `calibration`, `decision_curve`, and
  `distribution`. When `by` is supplied, plots are organised by subgroup
  level: `plots[["level_name"]]$calibration`, etc.

- `model_info`:

  Metadata from `validation_result`.

- `decision_threshold`:

  The threshold value used.

- `subgroup_variable`:

  Name of the `by` variable (subgroup analysis only).

- `subgroup_categories`:

  Character vector of category names (subgroup analysis only).

## Details

Metrics computed include: AUROC/c-statistic, calibration intercept,
calibration slope, ECI, ICI, ECE, O:E ratio, Brier score, Nagelkerke
R-squared, Cox-Snell R-squared, McFadden R-squared, net benefit, and
standardised net benefit.

This function currently supports **binary outcomes** (0/1) only.
Multinomial support is planned for a future release.

## See also

[`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md),
[`list_registered_models()`](https://joieensor.github.io/evaluatr/reference/list_registered_models.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# ---- Whole-dataset evaluation ----------------------------------------------
dvt_path <- system.file("extdata", "dvt_example.csv", package = "evaluatr")
dvt_val  <- read.csv(dvt_path)
dvt_val  <- dvt_val[dvt_val$study %in% c(6, 7), ]

result <- secure_model_validation(
  repo_owner      = "JoieEnsor",
  repo_name       = "evaluatr-demo-models",
  model_id        = "dvt_logistic_v1",
  github_token    = "evaluatr-demo",
  validation_data = dvt_val,
  outcome         = "dvt"
)

perf <- eval_performance(
  result,
  n_boot             = 200,
  decision_threshold = 0.15
)
perf$metrics

# Access individual diagnostic plots
perf$plots$calibration    # calibration plot
perf$plots$decision_curve # decision curve analysis
perf$plots$distribution   # predicted probability distribution

# ---- Subgroup evaluation by study -----------------------------------------
result_by_study <- secure_model_validation(
  repo_owner      = "JoieEnsor",
  repo_name       = "evaluatr-demo-models",
  model_id        = "dvt_logistic_v1",
  github_token    = "evaluatr-demo",
  validation_data = dvt_val,
  outcome         = "dvt",
  by              = "study"
)

perf_by_study <- eval_performance(
  result_by_study,
  by                 = result_by_study$shuffled_by,
  n_boot             = 200,
  decision_threshold = 0.15
)

# Metrics table has a Subgroup column
perf_by_study$metrics

# Access study-specific plots
perf_by_study$plots[["6"]]$calibration
perf_by_study$plots[["7"]]$calibration

# ---- Customising the calibration plot -------------------------------------
perf_custom <- eval_performance(
  result,
  n_boot             = 200,
  decision_threshold = 0.15,
  cal_args = list(smooth = "rcs", g = 5),
  dca_args = list(standardize = TRUE)
)
} # }
```
