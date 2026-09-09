# Secure prediction pipeline

Secure prediction pipeline

## Usage

``` r
.predict_from_encoded_cpp(
  encoded_content,
  model_type,
  design_matrix,
  outcome_vec,
  by_vec,
  model_params,
  github_token,
  repo_owner,
  repo_name,
  model_id,
  validation_id,
  worker_b_url
)
```

## Arguments

- encoded_content:

  Character. Base64-encoded JSON from GitHub API.

- model_type:

  Character. Model type string from metadata.

- design_matrix:

  Numeric matrix.

- outcome_vec:

  Numeric vector. Outcome variable values.

- by_vec:

  Character/numeric vector (or R NULL). Subgroup variable.

- model_params:

  Named list (or NULL).

- github_token:

  Character. Evaluator's GitHub token.

- repo_owner:

  Character. GitHub repository owner.

- repo_name:

  Character. GitHub repository name.

- model_id:

  Character. Model identifier.

- validation_id:

  Character. Id of the validations row created by Worker A; forwarded to
  Worker B so the row can be marked completed.

- worker_b_url:

  Character. Key service URL.

## Value

A named R list with shuffled_outcomes, shuffled_pred_matrix, shuffled_by
(if by_vec non-null), is_single_col, has_by.
