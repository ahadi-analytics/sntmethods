# Build Output List with Monthly and Annual Aggregations - Internal

Creates a structured list with monthly and annual aggregations at each
admin level (adm0, adm1, adm2, adm3). Optionally includes facility-level
data with diagnostic flags.

## Usage

``` r
.build_output_list(
  df_admin,
  df_facility,
  scale_factor,
  has_adm3 = FALSE,
  return_facility = FALSE,
  cs_none_divisor = 2,
  group_by = NULL
)
```

## Arguments

- df_admin:

  Data frame with monthly incidence data at admin level

- df_facility:

  Data frame with monthly incidence data at facility level

- scale_factor:

  Numeric. Scale factor for incidence calculation

- has_adm3:

  Logical. Whether adm3 column exists

- return_facility:

  Logical. Whether to include facility-level data in output

- cs_none_divisor:

  Numeric. Divisor for cs_none in N5 calculation

## Value

Named list with monthly and annual components, and optionally facility
