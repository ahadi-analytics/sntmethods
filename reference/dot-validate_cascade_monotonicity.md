# Validate Cascade Monotonicity - Internal

Checks that each incidence level is \>= the previous level. Violations
indicate potential data issues (e.g., reporting rate \> 100%).

## Usage

``` r
.validate_cascade_monotonicity(output, levels)
```

## Arguments

- output:

  The output list from calc_incidence.

- levels:

  Character vector of levels calculated.

## Value

Invisibly returns TRUE if valid, FALSE if violations found. Emits
warnings for any violations.
