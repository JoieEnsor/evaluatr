# List models registered in the evaluatr public directory

Queries the evaluatr registry for all publicly listed models. Returns a
data frame you can use to find models available for validation and to
identify the developer contact for requesting a validation token.

Only models registered with `public_listing = TRUE` (the default in
[`register_model()`](https://joieensor.github.io/evaluatr/reference/register_model.md))
appear in the results. By default only endorsed models are returned; set
`include_unendorsed = TRUE` to see all registered models.

## Usage

``` r
list_registered_models(as_data_frame = TRUE, include_unendorsed = FALSE)
```

## Arguments

- as_data_frame:

  Logical. If `TRUE` (default) return a `data.frame`. If `FALSE` return
  the raw parsed list from the JSON response.

- include_unendorsed:

  Logical. If `FALSE` (default) only return models endorsed by the
  evaluatr registry. Set to `TRUE` to include all publicly listed models
  regardless of endorsement status.

## Value

A `data.frame` with columns:

- model_id:

  Unique model identifier (pass to
  [`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md)
  as `model_id`).

- model_name:

  Human-readable model name.

- developer_id:

  Developer's evaluatr identifier.

- developer_name:

  Developer's name (may be `NA` if not provided at registration).

- developer_email:

  Contact email for requesting a validation token (may be `NA` if not
  provided at registration).

- model_description:

  Free-text description of the model (may be `NA` if not provided at
  registration).

- registrant_relationship:

  Registrant's declared relationship to the model:
  `"original_developer"`, `"authorised_proxy"`, or `"independent"`.

- endorsed:

  Integer flag: `1` if the model has been endorsed by the evaluatr
  registry, `0` otherwise.

## Details

To validate a model, contact the developer at the listed email address
and request a time-limited access token. Once you have the token, pass
it to
[`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md)
along with the `developer_id` as `repo_owner`, the repository name the
developer provides as `repo_name`, and the `model_id` from this table.

## See also

[`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md),
[`register_model()`](https://joieensor.github.io/evaluatr/reference/register_model.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Browse endorsed models (default)
models <- list_registered_models()
print(models[, c("model_id", "model_name", "developer_email")])

# Include unendorsed models
all_models <- list_registered_models(include_unendorsed = TRUE)

# Find models by a specific developer
subset(models, developer_id == "JoieEnsor")
} # }
```
