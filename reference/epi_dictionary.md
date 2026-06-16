# EPI Indicator Dictionary

Returns the dictionary of EPI (Expanded Programme on Immunization)
indicators with metadata. Not all indicators will be available in every
survey; availability depends on the DHS variables present in the
dataset.

## Usage

``` r
epi_dictionary()
```

## Value

Tibble with columns: indicator, indicator_code, indicator_title,
numerator_description, denominator_description, denominator_code.
