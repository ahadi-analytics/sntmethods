# Calculate Severe Anemia Prevalence from DHS Data (Core Function)

Core function that estimates severe anemia prevalence (Hb \< 8.0 g/dL)
among children aged 6-59 months using standard DHS methodology. This
indicator represents clinically significant anemia requiring medical
attention.

## Usage

``` r
calc_severe_anemia_dhs_core(
  dhs_pr,
  survey_vars = list(cluster = "hv001", weight = "hv005", stratum = "hv022", adm1 =
    "hv024", adm2 = NULL, age = "hc1", hemoglobin = "hc56", hemoglobin_adj = "hw53",
    present = "hv103", mother = "hv042"),
  hb_threshold = 8,
  altitude_adjusted = TRUE,
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_pr:

  DHS Person Records dataset in tidy format (data.frame or tibble).

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Cluster ID (default: "hv001")

  - `weight`: Survey weight (default: "hv005", divided by 1,000,000)

  - `stratum`: Explicit stratum variable if available (default: "hv022")

  - `adm1`: First administrative level (default: "hv024")

  - `adm2`: Second administrative level (default: NULL)

  - `age`: Child's age in months (default: "hc1")

  - `hemoglobin`: Raw hemoglobin in tenths of g/dL (default: "hc56")

  - `hemoglobin_adj`: Altitude-adjusted hemoglobin (default: "hw53")

  - `present`: Present in household (1=yes, default: "hv103")

  - `mother`: Mother listed in household (1=yes, default: "hv042")

- hb_threshold:

  Hemoglobin threshold in g/dL for severe anemia (default: 8.0).
  Children with Hb \< threshold are classified as severely anemic.

- altitude_adjusted:

  Logical. If TRUE (default), uses altitude-adjusted hemoglobin variable
  (hw53). If FALSE, uses raw hemoglobin (hc56). WHO recommends altitude
  adjustment for surveys in regions above 1000m.

- gps_data:

  Optional DHS GPS dataset. If provided, results are cluster-level.

- gps_vars:

  Named list for GPS variables (cluster, lat, lon).

## Value

A tibble with severe anemia estimates, confidence intervals, and sample
sizes. Columns depend on whether GPS data is provided (cluster-level vs
admin-level).

## Details

Note: Most users should use
[`calc_severe_anemia_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_severe_anemia_dhs.md)
instead, which provides additional spatial aggregation capabilities and
data dictionary support.

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/anemia_dhs.yml>

DHS stores hemoglobin values in tenths of g/dL (e.g., 80 = 8.0 g/dL).
The function handles this conversion automatically.

Severe anemia (Hb \< 8.0 g/dL) is clinically significant and typically
requires medical intervention. This differs from:

- Any anemia: Hb \< 11.0 g/dL

- Moderate anemia: Hb 7.0-9.9 g/dL

- Mild anemia: Hb 10.0-10.9 g/dL

**Altitude Adjustment:** The WHO recommends adjusting hemoglobin values
for altitude to account for physiological adaptation to lower oxygen at
higher elevations. When `altitude_adjusted = TRUE`, the function uses
the pre-computed altitude- adjusted variable (hw53) from DHS. This is
particularly important for surveys in highland areas.

## Examples

``` r
# minimal example (structure only)
# anemia <- calc_severe_anemia_dhs_core(
#   dhs_pr = pr_data,
#   altitude_adjusted = TRUE  # Use altitude-adjusted Hb (default)
# )
#
# # Use raw hemoglobin (no altitude adjustment)
# anemia <- calc_severe_anemia_dhs_core(
#   dhs_pr = pr_data,
#   altitude_adjusted = FALSE
# )
```
