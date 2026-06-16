# ACT Indicator Dictionary

Returns the full dictionary of ACT indicators with metadata. Each
indicator measures the proportion of febrile U5 children receiving ACT
within a specific subpopulation.

## Usage

``` r
act_dictionary()
```

## Value

Tibble with columns: indicator, indicator_code, indicator_title,
numerator_description, denominator_description, denominator_code,
cascade_step, requires_csb, requires_am.
