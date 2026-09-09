# Obfuscate coefficient values

Takes real coefficient values and returns obfuscated values using the
affine transform: stored = (real \* mult) + offset, where mult and
offset are deterministically generated from the obfuscation_key and
per-model salts. Used by the developer utility when creating model JSON
files.

## Usage

``` r
.obfuscate_coefficients_cpp(
  real_values,
  obfuscation_key,
  salt_a_hex,
  salt_b_hex
)
```

## Arguments

- real_values:

  Named numeric vector of real coefficient values.

- obfuscation_key:

  Character. 32-character hex string.

- salt_a_hex:

  Character. 16-character hex string (per-model salt A).

- salt_b_hex:

  Character. 16-character hex string (per-model salt B).

## Value

Named numeric vector of obfuscated values.
