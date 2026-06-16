# Calculate Core PfPR from DHS Data (Base Function)

Core function that estimates PfPR among children aged 6-59 months using
standard DHS methodology. Supports both Rapid Diagnostic Test (RDT) and
microscopy results. Produces cluster-level estimates when GPS data are
provided, or aggregates to administrative levels when GPS data are not
available. Survey strata are automatically detected using administrative
and urban/rural variables when available.

## Usage

``` r
calc_pfpr_dhs_core(
  dhs_pr,
  survey_vars = list(cluster = "hv021", weight = "hv005", stratum = "hv022", adm1 =
    "hv024", adm2 = NULL, age = "hc1", present = "hv103", mother = "hv042", rdt =
    "hml35", mic = "hml32"),
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_pr:

  DHS Person Records dataset in tidy format (data.frame or tibble).

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Primary sampling unit (default: "hv021"). Note: Use hv021
    (PSU) for proper survey design, not hv001 (cluster number).

  - `weight`: Survey weight (default: "hv005", divided by 1,000,000)

  - `stratum`: Explicit stratum variable if available (default: "hv022")

  - `adm1`: First administrative level (default: "hv024")

  - `adm2`: Second administrative level (default: NULL)

  - `age`: Child's age in months (default: "hc1")

  - `present`: Present in household (1=yes, default: "hv103")

  - `mother`: Mother listed in household (1=yes, default: "hv042")

  - `rdt`: RDT result (0=negative, 1=positive, default: "hml35")

  - `mic`: Microscopy result (0=neg, 1=pos, 6=other species, default:
    "hml32")

- gps_data:

  Optional DHS GPS dataset. If provided, results are cluster-level.

- gps_vars:

  Named list for GPS variables (cluster, lat, lon).

## Value

A tibble with PfPR estimates, confidence intervals, and sample sizes.
Columns depend on whether GPS data is provided (cluster-level vs
admin-level).

## Details

Note: Most users should use
[`calc_pfpr_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_pfpr_dhs.md)
instead, which provides additional spatial aggregation capabilities and
data dictionary support.

## Examples

``` r
# minimal example (structure only)
# pfpr <- calc_pfpr_dhs_core(
#   dhs_pr = pr_data,
#   survey_vars = list(
#     cluster = "hv021",
#     weight = "hv005",
#     stratum = "hv022",
#     adm1 = "hv024",
#     adm2 = NULL,
#     age = "hc1",
#     present = "hv103",
#     mother = "hv042",
#     rdt = "hml35",
#     mic = "hml32"
#   )
# )
```
