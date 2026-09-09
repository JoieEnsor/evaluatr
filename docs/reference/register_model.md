# Register a model and generate its protected specification file

Developer-facing utility that accepts a fitted R model object or a
manual coefficient specification and produces a correctly formatted,
protected JSON file ready for upload to a GitHub repository for use with
[`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md).

The function also registers the model with the evaluatr registry, which
is required before any evaluator can validate the model. Registration
stores the keys needed to protect and later recover coefficient values;
those keys are never written to the JSON file itself.

## Usage

``` r
register_model(
  model = NULL,
  coefficients = NULL,
  model_type = NULL,
  model_id = NULL,
  developer_id = NULL,
  model_name = NULL,
  outcome_type = NULL,
  variables = NULL,
  description = "",
  version = "1.0",
  preprocessing = NULL,
  model_parameters = NULL,
  reference_category = NULL,
  registrant_relationship = NULL,
  developer_name = NULL,
  developer_email = NULL,
  public_listing = TRUE,
  rate_limit_exempt = FALSE,
  output_dir = "."
)
```

## Arguments

- model:

  A fitted model object. Supported classes: `glm` (binomial family),
  `coxph`, `survreg` (Weibull/exponential), `multinom` (nnet). Exactly
  one of `model` or `coefficients` must be non-NULL.

- coefficients:

  A named numeric vector (for flat models) or named list of named
  numeric vectors (for multinomial, one entry per non-reference
  category). Exactly one of `model` or `coefficients` must be non-NULL.

- model_type:

  Character. Required when `coefficients` is supplied. One of
  `"logistic"`, `"cox"`, `"weibull"`, `"multinomial"`.

- model_id:

  Character. Unique identifier for this model (required). Must match the
  folder name in the GitHub repository (e.g. `"dvt_v1"`). Choose a name
  that is meaningful and unlikely to conflict with other developers'
  models.

- developer_id:

  Character. Your identifier (e.g. GitHub username). Used to attribute
  the model in the evaluatr registry (required).

- model_name:

  Character. Human-readable name (required).

- outcome_type:

  Character. One of `"binary"`, `"survival"`, `"multinomial"`. Required
  when `coefficients` is supplied; inferred automatically from model
  objects.

- variables:

  Character vector of predictor variable names (not including
  `"(Intercept)"`). Required when `coefficients` is supplied; inferred
  from model objects.

- description:

  Character. Free-text description of the model. Default `""`.
  Recommended: include the outcome definition, predictor list, and the
  development dataset used.

- version:

  Character. Version string. Default `"1.0"`.

- preprocessing:

  Character or `NULL`. R code string that will be evaluated on the
  evaluator's `validation_data` before prediction (e.g. to create
  derived variables or dummy columns). Default `NULL`. The code must
  modify `validation_data` in place; it has access to that object and
  nothing else.

- model_parameters:

  Named list or `NULL`. Additional parameters required for Cox and
  Weibull models:

  - Cox: `list(timepoints = c(...), baseline_survival = c(...))`

  - Weibull:
    `list(timepoints = c(...), shape = <value>, parameterisation = "aft")`.
    The shape parameter is extracted automatically from `survreg`
    objects but can be overridden here.

- reference_category:

  Character or `NULL`. For multinomial models: name of the reference
  category (the category whose coefficients are all zero). Required when
  `model_type = "multinomial"` and `coefficients` is supplied. Inferred
  automatically from `multinom` objects.

- registrant_relationship:

  Character. Required. Your relationship to the model being registered.
  Must be one of `"original_developer"` (you created the model),
  `"authorised_proxy"` (you are registering on behalf of the original
  developer with their permission), or `"independent"` (you are
  registering a model developed by others). This is stored in the
  evaluatr registry and used for endorsement decisions.

- developer_name:

  Character or `NULL`. Your name (e.g. `"Jane Smith"`). Stored in the
  evaluatr registry so evaluators can identify the model maintainer.
  Optional but recommended.

- developer_email:

  Character or `NULL`. Contact email for evaluators who wish to request
  a validation token. Optional but strongly recommended if you want your
  model to be discoverable via
  [`list_registered_models()`](https://joieensor.github.io/evaluatr/reference/list_registered_models.md).

- public_listing:

  Logical. Whether to list this model in the public evaluatr registry
  (returned by
  [`list_registered_models()`](https://joieensor.github.io/evaluatr/reference/list_registered_models.md)).
  Default `TRUE`. Set to `FALSE` to keep the registration private while
  a paper is under review, for example.

- rate_limit_exempt:

  Logical. Whether this model is exempt from the key-fetch rate limit.
  Default `FALSE`. Set to `TRUE` only for demo/ teaching models intended
  for open repeated use. Also useful for fully open access models e.g.,
  specification already published open access.

- output_dir:

  Character. Directory to write the specification file. Created if it
  does not exist. Default `"."`. The file will be written as
  `{model_id}_specification.json` inside this directory.

## Value

The complete JSON structure as an R list (invisibly). The primary side
effect is writing the JSON file to
`file.path(output_dir, output_filename)`.

## Details

The generated file contains protected coefficient values and model
metadata. At validation time,
[`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md)
reads the file and computes predictions internally; the raw coefficient
values are never exposed as R objects in the evaluator's session.

**After running this function**, upload the specification file to your
private GitHub repository at the path
`<model_id>/<model_id>_specification.json`, then issue a time-limited,
fine-grained read-only access token to each evaluator. See the developer
guide vignette
([`vignette("developer-guide", package = "evaluatr")`](https://joieensor.github.io/evaluatr/articles/developer-guide.md))
for a complete walkthrough.

This function requires a network connection to the evaluatr registry. It
will fail if the registry is unreachable.

## Examples

``` r
if (FALSE) { # \dontrun{
# ---- Example 1: Logistic regression (DVT diagnosis model) ------------------
dvt_path <- system.file("extdata", "dvt_example.csv", package = "evaluatr")
dvt_data <- read.csv(dvt_path)

# Fit on development studies 1-5
dvt_dev <- dvt_data[dvt_data$study %in% 1:5, ]
dvt_fit <- glm(dvt ~ male + ab_ddimer + calf_diff + vein_distension +
                     malignancy + immobile,
               data = dvt_dev, family = binomial)

register_model(
  model          = dvt_fit,
  model_id       = "dvt_v1",
  developer_id   = "your-github-username",
  model_name     = "DVT Diagnosis Model v1",
  description    = "Logistic regression for DVT diagnosis, developed in studies 1-5.",
  developer_name  = "Your Name",
  developer_email = "you@institution.ac.uk",
  output_dir     = "dvt_v1"
)
# Upload dvt_v1/dvt_v1_specification.json to your private GitHub repository.

# ---- Example 2: Cox survival model (breast cancer relapse) ----------------
bc_path <- system.file("extdata", "bc_survival_example.csv",
                       package = "evaluatr")
bc_data <- read.csv(bc_path)
bc_dev  <- bc_data[bc_data$val == 0, ]

library(survival)
cox_fit <- coxph(Surv(rf, rfi) ~ age + nodes + pr + er + hormon,
                 data = bc_dev, x = TRUE)

# Baseline survival at 5 and 10 years (months: 60 and 120)
bh <- basehaz(cox_fit, centered = FALSE)
S0_60  <- exp(-bh$hazard[which.min(abs(bh$time - 60))])
S0_120 <- exp(-bh$hazard[which.min(abs(bh$time - 120))])

register_model(
  model            = cox_fit,
  model_id         = "bc_cox_v1",
  developer_id     = "your-github-username",
  model_name       = "Breast Cancer Relapse-Free Survival Model",
  description      = "Cox PH model for relapse-free survival, development set.",
  model_parameters = list(
    timepoints         = c(60, 120),
    baseline_survival  = c(S0_60, S0_120)
  ),
  developer_name  = "Your Name",
  developer_email = "you@institution.ac.uk",
  output_dir      = "bc_cox_v1"
)

# ---- Example 3: Manual coefficients (logistic) ----------------------------
register_model(
  coefficients = c("(Intercept)" = -3.2, ab_ddimer = 1.8,
                   calf_diff = 1.1, vein_distension = 0.9),
  model_type   = "logistic",
  model_id     = "dvt_manual_v1",
  developer_id = "your-github-username",
  model_name   = "DVT Manual Coefficients",
  outcome_type = "binary",
  variables    = c("ab_ddimer", "calf_diff", "vein_distension"),
  output_dir   = "dvt_manual_v1"
)
} # }
```
