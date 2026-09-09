# evaluatr: Secure Independent Evaluation of Clinical Prediction Models

The evaluatr package implements a two-component system for independent
external validation of clinical prediction algorithms, designed to
simultaneously satisfy:

- **Scientific rigour** – fully independent evaluators with complete
  control over their data and reporting.

- **Commercial viability** – model coefficients never leave the
  developer's private GitHub repository.

- **Data sovereignty** – patient data never leaves the evaluator's
  secure local environment.

## Workflow

1.  The model **developer** uses
    [`register_model()`](https://joieensor.github.io/evaluatr/reference/register_model.md)
    to create a protected model specification and registers the model
    with the evaluatr registry. The specification is uploaded to a
    private GitHub repository. The developer issues a time-limited,
    fine-grained access token to each evaluator.

2.  The **evaluator** calls
    [`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md)
    with their local dataset and the developer-provided token.
    Predictions are computed entirely within the evaluator's R session;
    the output is a shuffled prediction-outcome matrix.

3.  The evaluator passes the result to
    [`eval_performance()`](https://joieensor.github.io/evaluatr/reference/eval_performance.md)
    to obtain discrimination, calibration, and utility metrics with
    optional bootstrap confidence intervals and diagnostic plots.

## Security

Model coefficients are protected at rest and in transit; they are never
exposed as readable R objects. Predictions are cleared from memory once
returned. The returned matrix is row-shuffled so that predictions cannot
be matched back to individual patient records. A minimum dataset size of
50 observations (per subgroup) is enforced to prevent near-individual
prediction and coefficient reverse-engineering. Every validation event
is logged by the evaluatr registry.

## Example datasets

Three example datasets are included with the package:

- `dvt_example` – simulated DVT diagnosis dataset, 9,805 patients across
  seven studies (binary outcome). Loaded via
  `read.csv(system.file("extdata", "dvt_example.csv", package = "evaluatr"))`.

- `bc_survival_example` – breast cancer relapse-free survival dataset,
  2,232 patients with a pre-assigned development/validation split
  (survival outcome). Loaded via
  `read.csv(system.file("extdata", "bc_survival_example.csv", package = "evaluatr"))`.

- `adnex_sample` – de-identified ADNEX ovarian cancer validation
  dataset, 1,500 patients (multinomial outcome). Loaded via
  `readRDS(system.file("extdata", "adnex_sample.rds", package = "evaluatr"))`.

## References

Ensor J, Van Calster B, Barreñada L, Wynants L, Perry BI. *A system for
independent evaluation of clinical predictive algorithms while
preserving implementation viability.* (manuscript in preparation)

## See also

Useful links:

- <https://joieensor.github.io/evaluatr/>

- <https://github.com/JoieEnsor/evaluatr>

- Report bugs at <https://github.com/JoieEnsor/evaluatr/issues>

## Author

**Maintainer**: Joie Ensor <j.ensor@bham.ac.uk>
([ORCID](https://orcid.org/0000-0001-7481-0282))

Authors:

- Joie Ensor <j.ensor@bham.ac.uk>
  ([ORCID](https://orcid.org/0000-0001-7481-0282))

- Ben Van Calster ([ORCID](https://orcid.org/0000-0003-1613-7450))

- Lasai Barreñada ([ORCID](https://orcid.org/0000-0001-8020-0210))

- Laure Wynants ([ORCID](https://orcid.org/0000-0002-3037-122X))

- Benjamin I. Perry ([ORCID](https://orcid.org/0000-0002-1533-026X))
