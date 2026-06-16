# SMC Indicator Dictionary

Returns the dictionary of SMC indicators with metadata. SMC coverage
measures the proportion of children under 5 who received seasonal
malaria chemoprevention during the malaria season, derived from DHS
Children's Recode (KR) variables hml43 (primary) or ml13g (fallback).

## Usage

``` r
smc_dictionary()
```

## Value

Tibble with columns: indicator, indicator_code, indicator_title,
numerator_description, denominator_description, denominator_code.
