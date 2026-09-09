# Changelog

## evaluatr 0.1.0

- Initial GitHub release.
- [`secure_model_validation()`](https://joieensor.github.io/evaluatr/reference/secure_model_validation.md):
  retrieves model from private GitHub repo and computes shuffled
  predictions locally.
- [`eval_performance()`](https://joieensor.github.io/evaluatr/reference/eval_performance.md):
  discrimination, calibration, and utility metrics with bootstrap CIs
  and diagnostic plots.
- Supports binary outcome models.
- Subgroup analysis via the `by` argument in both main functions.
