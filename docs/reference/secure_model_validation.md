# Secure Model Validation

Retrieves a clinical prediction model specification from a developer's
private GitHub repository and computes predictions locally on the
evaluator's dataset, without transmitting patient data.

## Usage

``` r
secure_model_validation(
  repo_owner,
  repo_name,
  model_id,
  github_token,
  validation_data,
  outcome,
  by = NULL
)
```

## Arguments

- repo_owner:

  Character. GitHub username or organisation owning the model
  repository.

- repo_name:

  Character. Name of the GitHub repository containing the model
  specification.

- model_id:

  Character. Unique identifier for the model (must match the folder name
  under `models/` in the repository).

- github_token:

  Character. Fine-grained GitHub personal access token with read-only
  access to the repository. Provided by the model developer.

- validation_data:

  Data frame. The evaluator's local dataset, which must contain all
  predictor variables required by the model.

- outcome:

  Character. Name of the outcome variable in `validation_data`.

- by:

  Character or `NULL`. Optional name of a grouping variable in
  `validation_data` for subgroup analysis (e.g. `"study"`). Must have at
  least two non-missing categories, each meeting the minimum observation
  requirement.

## Value

A named list of class `evaluatr_result` containing:

- `shuffled_outcome_predictions`:

  Matrix of shuffled outcome-prediction pairs (and subgroup column if
  `by` is specified).

- `shuffled_outcomes`:

  Numeric vector of shuffled outcomes.

- `shuffled_predictions`:

  Numeric vector of shuffled predicted probabilities (binary models
  only).

- `shuffled_by`:

  Vector of shuffled subgroup values (only present when `by` is
  specified).

- `prediction_matrix`:

  Matrix of shuffled predicted probabilities (multinomial models: one
  column per outcome category).

- `model_info`:

  List of model metadata including `model_id`, `model_name`,
  `model_type`, `version`, `required_variables`, `n_predictions`, and
  `validation_timestamp`.

## Details

A minimum dataset size is enforced (per subgroup when `by` is used).
Predictions are shuffled before being returned so that they cannot be
matched back to individual patient records, preventing
reverse-engineering of model coefficients and ensuring the system cannot
be used to make predictions for individual patients in clinical
practice.

Patient data is never transmitted outside your R session. The validation
event is logged by the evaluatr registry.

To use this function you need four items from the model developer: their
GitHub username, repository name, model ID, and a time-limited access
token. See the evaluator guide vignette
([`vignette("evaluator-guide", package = "evaluatr")`](https://joieensor.github.io/evaluatr/articles/evaluator-guide.md))
for a step-by-step walkthrough. Use
[`list_registered_models()`](https://joieensor.github.io/evaluatr/reference/list_registered_models.md)
to browse publicly registered models and find developer contact details.

## See also

[`eval_performance()`](https://joieensor.github.io/evaluatr/reference/eval_performance.md)
for computing performance metrics from the returned object.
[`list_registered_models()`](https://joieensor.github.io/evaluatr/reference/list_registered_models.md)
to browse publicly registered models.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the DVT example dataset included with the package
dvt_path <- system.file("extdata", "dvt_example.csv", package = "evaluatr")
dvt_data <- read.csv(dvt_path)

# Use only studies 6-7 as the external validation set
dvt_val <- dvt_data[dvt_data$study %in% c(6, 7), ]

# Run secure validation against the public evaluatr demo model. For your
# own models, replace these values with those supplied by the developer.
result <- secure_model_validation(
  repo_owner      = "JoieEnsor",
  repo_name       = "evaluatr-demo-models",
  model_id        = "dvt_logistic_v1",
  github_token    = "evaluatr-demo",
  validation_data = dvt_val,
  outcome         = "dvt"
)

# Compute performance metrics
perf <- eval_performance(result)
perf$metrics

# Subgroup analysis by study
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
  by = result_by_study$shuffled_by
)
perf_by_study$metrics
} # }
```
