# Tests for secure_model_validation() input validation
# These tests cover argument checking and the 'by' parameter logic.
# They do NOT test GitHub API calls (which would require mocking or a live token).

test_that("secure_model_validation() requires all mandatory arguments", {
  df <- data.frame(x = rnorm(30), outcome = rbinom(30, 1, 0.5))

  expect_error(secure_model_validation(), "All parameters are required")
  expect_error(
    secure_model_validation(
      repo_owner = "owner", repo_name = "repo", model_id = "id",
      github_token = "tok", validation_data = df
      # outcome missing
    ),
    "All parameters are required"
  )
})

test_that("validation_data must be a data frame", {
  expect_error(
    secure_model_validation(
      repo_owner = "owner", repo_name = "repo", model_id = "id",
      github_token = "tok", validation_data = list(x = 1:30),
      outcome = "x"
    ),
    "must be a data frame"
  )
})

test_that("outcome must exist in validation_data", {
  df <- data.frame(x = rnorm(30), y = rbinom(30, 1, 0.5))

  expect_error(
    secure_model_validation(
      repo_owner = "owner", repo_name = "repo", model_id = "id",
      github_token = "tok", validation_data = df,
      outcome = "nonexistent"
    ),
    "not found in validation_data"
  )
})

test_that("validation_data must have more than 20 observations", {
  df_small <- data.frame(x = rnorm(20), outcome = rbinom(20, 1, 0.5))

  expect_error(
    secure_model_validation(
      repo_owner = "owner", repo_name = "repo", model_id = "id",
      github_token = "tok", validation_data = df_small,
      outcome = "outcome"
    ),
    "more than 20 observations"
  )

  # 21 rows should pass the size check (will fail later at GitHub fetch)
  df_ok <- data.frame(x = rnorm(21), outcome = rbinom(21, 1, 0.5))
  expect_error(
    secure_model_validation(
      repo_owner = "owner", repo_name = "repo", model_id = "id",
      github_token = "fake_token", validation_data = df_ok,
      outcome = "outcome"
    ),
    # Should get past size validation and fail at GitHub fetch
    "CURL error|HTTP error|Model not found|Authentication failed"
  )
})

test_that("'by' must be a single character string naming a column", {
  df <- data.frame(x = rnorm(30), outcome = rbinom(30, 1, 0.5),
                   group = rep(c("A", "B"), 15))


  expect_error(
    secure_model_validation(
      repo_owner = "owner", repo_name = "repo", model_id = "id",
      github_token = "tok", validation_data = df,
      outcome = "outcome", by = 42
    ),
    "single character string"
  )

  expect_error(
    secure_model_validation(
      repo_owner = "owner", repo_name = "repo", model_id = "id",
      github_token = "tok", validation_data = df,
      outcome = "outcome", by = c("group", "x")
    ),
    "single character string"
  )

  expect_error(
    secure_model_validation(
      repo_owner = "owner", repo_name = "repo", model_id = "id",
      github_token = "tok", validation_data = df,
      outcome = "outcome", by = "nonexistent"
    ),
    "not found in validation_data"
  )
})

test_that("'by' variable must have at least 2 non-missing categories", {
  df <- data.frame(x = rnorm(30), outcome = rbinom(30, 1, 0.5),
                   group = rep("A", 30))

  expect_error(
    secure_model_validation(
      repo_owner = "owner", repo_name = "repo", model_id = "id",
      github_token = "tok", validation_data = df,
      outcome = "outcome", by = "group"
    ),
    "at least 2 non-missing categories"
  )
})

test_that("each 'by' category must have at least 20 observations", {
  df <- data.frame(
    x       = rnorm(30),
    outcome = rbinom(30, 1, 0.5),
    group   = c(rep("A", 25), rep("B", 5))
  )

  expect_error(
    secure_model_validation(
      repo_owner = "owner", repo_name = "repo", model_id = "id",
      github_token = "tok", validation_data = df,
      outcome = "outcome", by = "group"
    ),
    "fewer than 20 observations"
  )
})
