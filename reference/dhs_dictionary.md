# Master DHS Indicator Dictionary

Returns a single consolidated tibble with every DHS indicator across all
domains (ACT, ITN, fever, etc.). Use this as a pre-analysis reference to
know what indicators are available, what DHS variables are needed, and
what each numerator/denominator represents.

## Usage

``` r
dhs_dictionary()
```

## Value

A tibble with one row per indicator and columns:

- domain:

  Topic area (e.g., "ACT", "ITN", "Fever")

- observation_unit:

  Unit of analysis (e.g., "Individual", "Household", "Person")

- dhs_recode:

  DHS file type needed (KR, IR, HR, PR, HR+PR)

- calc_function:

  R function to call

- indicator_code:

  Unique indicator identifier

- indicator:

  Short indicator name

- indicator_title:

  Full descriptive title

- numerator_code:

  Auto-derived numerator code (n\_\<indicator_code\>)

- numerator_description:

  What the numerator counts

- denominator_code:

  Short code for the denominator

- denominator_description:

  What the denominator counts

- eligibility:

  Who is eligible / inclusion criteria

- dhs_variables:

  Key DHS variables needed (per domain)

- notes:

  Additional context, caveats, or methodology notes

## Examples

``` r
if (FALSE) { # \dontrun{
# Browse all indicators
dhs_dictionary()

# Filter to a specific domain
dhs_dictionary() |> dplyr::filter(domain == "ITN")

# Find which DHS recode files you need
dhs_dictionary() |> dplyr::distinct(domain, dhs_recode, calc_function)
} # }
```
