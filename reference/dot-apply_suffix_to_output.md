# Apply Suffix to Incidence Output Columns - Internal

Renames incidence-related columns (n0_cases, n0_incidence, n1_cases,
etc.) by appending a suffix. Used to distinguish outputs for different
populations (e.g., u5 for under-5, all_ages, etc.).

## Usage

``` r
.apply_suffix_to_output(output, suffix)
```

## Arguments

- output:

  The output list from calc_incidence containing monthly and annual
  tibbles at each admin level.

- suffix:

  Character string to append to column names (e.g., "u5").

## Value

The output list with renamed columns.
