# Calculate Effective Coverage of Case Management from DHS Data

Computes the effective coverage of case management as the product of two
survey-weighted proportions: \$\$Effective\\CM = CSB\\rate \times P(ACT
\mid antimalarial)\$\$

## Usage

``` r
calc_case_management_dhs(
  dhs_kr,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", age = "hw1",
    fever = "h22", alive = "b5", act = "ml13e"),
  region_var = NULL
)
```

## Arguments

- dhs_kr:

  DHS children's recode (KR) dataset (data.frame or tibble).

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Cluster/PSU ID (default: "v021")

  - `weight`: Survey weight (default: "v005")

  - `stratum`: Stratum variable (default: "v022")

  - `age`: Child's age in months (default: "hw1")

  - `fever`: Had fever in last 2 weeks (default: "h22")

  - `alive`: Child survival status (default: "b5")

  - `act`: Received ACT treatment (default: "ml13e")

- region_var:

  Optional column name in `dhs_kr` to use as grouping variable (e.g.,
  "v024" for region).

## Value

Named list of tibbles:

- `adm0`:

  National-level estimates (always present)

- `adm1`:

  Admin-1 estimates (when `region_var` provided)

Each tibble contains columns: survey_id, iso3, iso2, survey_type,
survey_year, adm0, adm1, type, geo_source, point, ci_l, ci_u, numerator,
denominator, indicator, indicator_code, numerator_description,
denominator_description, denominator_code.

## Details

where CSB rate is the care-seeking rate for fever among children under
5, and P(ACT \| antimalarial) is the proportion receiving ACT among
febrile children who received any antimalarial treatment.

Two variants are produced:

- `EFF_CM_ANY`: using any care-seeking (public or private)

- `EFF_CM_PUBLIC`: using public sector care-seeking only

Returns results in standardized long format with `list(adm0, adm1)`
structure.

The effective coverage indicator captures the probability that a febrile
child both seeks care AND receives ACT (given they receive any
antimalarial). CIs are approximated using the delta method assuming
independence: \$\$SE(A \times B) \approx \sqrt{A^2 \cdot SE(B)^2 + B^2
\cdot SE(A)^2}\$\$

The antimalarial denominator includes any child receiving at least one
drug from the `ml13` series (or `h37a-h` fallback for older surveys).
ACT is identified by `ml13e` (or `h37e` fallback).

## See also

[`case_management_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/case_management_dictionary.md)
for indicator definitions,
[`calc_csb_dhs_core()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_csb_dhs_core.md),
[`calc_act_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_act_dhs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- calc_case_management_dhs(
  dhs_kr = kr_data,
  region_var = "v024"
)
} # }
```
