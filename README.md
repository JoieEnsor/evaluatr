# evaluatr <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
<!-- badges: end -->

**evaluatr** enables independent external validation of clinical prediction
models while simultaneously protecting model intellectual property and patient
data privacy.

## The problem

Clinical prediction algorithms require open-science transparency for rigorous
independent evaluation, yet full publication of model coefficients removes
intellectual property protection, blocking the commercialisation route needed
to fund regulatory approval and clinical implementation.

## The solution

A two-component system:

| Component | Who controls it | What it contains |
|-----------|-----------------|-----------------|
| Developer side | Model developer | Coefficients + prediction code in a **private** GitHub repository |
| Evaluator side | Evaluator | This R package + their patient dataset |

The evaluator's data **never leaves** their environment. The developer's
coefficients **never appear** in the evaluator's R session. The output is a
row-shuffled prediction–outcome matrix that enables any performance metric
while preventing coefficient reverse-engineering.

## Installation

```r
# Requires devtools
devtools::install_github("JoieEnsor/evaluatr")
```

## Quick start

```r
library(evaluatr)

# Step 1: Run secure validation (token provided by model developer)
result <- secure_model_validation(
  repo_owner      = "model-developer",
  repo_name       = "clinical-models",
  model_id        = "my_model_v1",
  github_token    = Sys.getenv("EVALUATR_TOKEN"),
  validation_data = my_dataset,
  outcome         = "outcome_column"
)

# Step 2: Compute performance metrics
perf <- calculate_pmextval_metrics(
  result,
  decision_threshold   = 0.2,
  confidence_intervals = TRUE,
  n_boot               = 500
)

perf$metrics
```

## Supported model types

- Binary logistic regression
- Multinomial logistic regression
- Cox proportional hazards (with baseline survival in JSON)
- Any model expressible as a JSON specification + R prediction code string

## Citation

Ensor J, Van Calster B, Wynants L, Perry BI. *A system for independent
evaluation of clinical predictive algorithms while preserving implementation
viability.* (manuscript in preparation)

## License

GPL (≥ 3)
