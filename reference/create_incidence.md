# Create an SNT Incidence Object from Tibble

User-facing wrapper to convert the output of
[`calc_incidence()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_incidence.md)
into an `snt_incidence` S3 object with metadata. This is useful for
tracking provenance, enabling custom print/summary methods, and
maintaining calculation metadata.

## Usage

``` r
create_incidence(data, scale = 1000, levels = c("N0", "N1", "N2", "N3"))
```

## Arguments

- data:

  Tibble output from
  [`calc_incidence()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_incidence.md).

- scale:

  Numeric. Scale factor used in calculation (default: 1000).

- levels:

  Character vector of incidence levels present in data (default: c("N0",
  "N1", "N2", "N3")). Will auto-detect from columns.

## Value

An object of class `snt_incidence`.

## Examples

``` r
# facility_data <- tibble::tibble(
#   hf_uid = "HF001",
#   adm1 = "RegionA",
#   adm2 = "DistrictX",
#   date = as.Date("2023-01-01"),
#   conf = 10,
#   test = 100,
#   pres = 5,
#   tpr = 0.10,
#   reprate = 0.85,
#   pop = 5000,
#   cs_public = 0.60,
#   cs_private = 0.25,
#   cs_none = 0.15
# )
#
# result_tbl <- calc_incidence(facility_data)
# result_obj <- create_incidence(result_tbl, scale = 1000)
# print(result_obj)
# summary(result_obj)
```
