# evaluatr

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
row-shuffled prediction–outcome matrix that enables calculation of any performance metric
while preventing ascertainment of individual predicted probabilities for informing patient care.

## Installation

```r
# Requires devtools
devtools::install_github("JoieEnsor/evaluatr")
```

## Try it now — no developer contact required

A small set of demo models is hosted on a public read-only repository so you
can run the full evaluation workflow immediately, without needing a real model
developer's private token:

```r
library(evaluatr)

dvt_path <- system.file("extdata", "dvt_example.csv", package = "evaluatr")
dvt_data <- read.csv(dvt_path)
dvt_val  <- dvt_data[dvt_data$study %in% c(6, 7), ]

result <- secure_model_validation(
  repo_owner      = "JoieEnsor",
  repo_name       = "evaluatr-demo-models",
  model_id        = "dvt_logistic_v1",
  github_token    = "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",
  validation_data = dvt_val,
  outcome         = "dvt"
)

perf <- eval_performance(
  result,
  decision_threshold   = 0.2,
  confidence_intervals = TRUE,
  n_boot               = 200
)

perf$metrics
```

More demo scripts covering Cox, Weibull, and multinomial models are included
under `system.file("extdata", package = "evaluatr")` — see
`vignette("evaluator-guide", package = "evaluatr")` for the full walkthrough.

## Quick start with your own model

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
perf <- eval_performance(
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
- Cox proportional hazards 
- Weibull accelerated failure time models
- [Get in touch](mailto:j.ensor@bham.ac.uk) or [raise a GitHub Issue](https://github.com/JoieEnsor/evaluatr/issues/new) if you'd like to use evaluatr with another model type

## Documentation

- `vignette("evaluator-guide", package = "evaluatr")` — for evaluators running
  a validation against someone else's model
- `vignette("developer-guide", package = "evaluatr")` — for developers
  registering their own model with the evaluatr key service

## Citing evaluatr

If you use evaluatr in your work, please cite the preprint describing the
system:

> Ensor, J., Van Calster, B., Barreñada, L., Wynants, L., & Perry, B. I.
> (2026). *A system for independent evaluation of clinical prediction models
> while preserving intellectual property and data privacy*. Zenodo.
> https://doi.org/10.5281/zenodo.20707721

BibTeX:

```bibtex
@misc{ensor2026evaluatr,
  author       = {Ensor, Joie and Van Calster, Ben and Barreñada, Lasai and Wynants, Laure and Perry, Benjamin I.},
  title        = {A system for independent evaluation of clinical prediction models while preserving intellectual property and data privacy},
  year         = {2026},
  publisher    = {Zenodo},
  doi          = {10.5281/zenodo.20707721},
  url          = {https://doi.org/10.5281/zenodo.20707721}
}
```

## License

GPL (≥ 3)
