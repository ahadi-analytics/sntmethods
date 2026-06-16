# Prepare IPTp Data for MBG Analysis

Prepares cluster-level Intermittent Preventive Treatment in pregnancy
(IPTp) data for MBG analysis. Calculates both cumulative (1+, 2+, 3+)
and exclusive (exactly 1, exactly 2, exactly 3) dose categories.

## Usage

``` r
calc_iptp_mbg(
  dhs_ir,
  gps_data,
  indicators = c("iptp_1plus", "iptp_2plus", "iptp_3plus"),
  birth_window_months = 36,
  survey_vars = list(cluster = "v001", interview_date = "v008", birth_date = "b3_01",
    sp_doses = "ml1_1", sp_taken = "m49a_1"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- dhs_ir:

  DHS Individual Recode (IR) or Children's Recode (KR) dataset.

- gps_data:

  DHS GPS dataset with cluster coordinates.

- indicators:

  Character vector of indicators to calculate:

  - Cumulative:

    - "iptp_1plus": At least 1 dose

    - "iptp_2plus": At least 2 doses

    - "iptp_3plus": At least 3 doses (WHO recommendation)

    - "iptp_4plus": At least 4 doses (requires ml1_1 as sp_doses)

  - Exclusive:

    - "iptp_1only": Exactly 1 dose

    - "iptp_2only": Exactly 2 doses

    - "iptp_3only": Exactly 3 doses

  Default: c("iptp_1plus", "iptp_2plus", "iptp_3plus").

- birth_window_months:

  Months to look back for births. Default: 36.

- survey_vars:

  Named list mapping DHS variable names.

- gps_vars:

  Named list for GPS variable mapping.

## Value

A list of data.tables (one per indicator).

## Details

Supports both IR (Individual Recode) and KR (Children's Recode) formats.
The function automatically detects the file type and adjusts variable
names.

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/iptp_dhs.yml>

No ANC attendance restriction is applied; the denominator is all women
with a birth in the analysis window and a valid SP response
(`sp_doses <= 7`). The default `survey_vars$sp_doses = "ml1_1"` is the
dose count variable (0-7). If `ml1_1` is not available, the helper falls
back to `sp_taken` (`"m49a_1"`, binary 0/1), in which case only IPTp 1+
will produce meaningful results.

## See also

[`calc_iptp_dhs()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_iptp_dhs.md)
for survey-weighted estimates

## Examples

``` r
if (FALSE) { # \dontrun{
iptp_mbg <- calc_iptp_mbg(
  dhs_ir = ir_data,
  gps_data = gps_data,
  indicators = c("iptp_2plus", "iptp_3plus")
)
} # }
```
