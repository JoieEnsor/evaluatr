# Extract model metadata from base64-encoded JSON content

Decodes the base64 GitHub API content string and returns only metadata
(model type, variable names, preprocessing code). Does NOT return
coefficient values.

## Usage

``` r
.extract_model_metadata_cpp(encoded_content)
```

## Arguments

- encoded_content:

  Character. Base64-encoded JSON string from GitHub Contents API.

## Value

A named R list containing model_type, variable_names, model_name,
version, preprocessing, is_multinomial, category_names,
has_obfuscation_key, model_parameters_present.
