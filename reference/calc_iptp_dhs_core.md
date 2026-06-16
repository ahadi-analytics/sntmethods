# Calculate Core IPTp Coverage from DHS Data

Core function that estimates IPTp (Intermittent Preventive Treatment in
pregnancy) coverage indicators from DHS Individual Recode (IR) data.
Implements standard DHS methodology for calculating IPTp indicators
among women with a recent birth. When GPS data is provided, produces
cluster-level results. Otherwise uses existing administrative variables
in the data.

## Usage

``` r
calc_iptp_dhs_core(
  dhs_ir,
  survey_vars = list(cluster = "v001", weight = "v005", stratum = "v022", adm1 = "v024",
    adm2 = NULL, interview_cmc = "v008", birth_cmc = "b3_01", birth_age_months =
    "b19_01", sp_taken = "m49a_1", sp_doses = "ml1_1"),
  birth_window_months = 24,
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE
)
```

## Arguments

- dhs_ir:

  DHS Individual Recode dataset (IR) in tidy format (data.frame or
  tibble).

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Cluster ID (default: "v001")

  - `weight`: Survey weight (default: "v005")

  - `stratum`: Stratum variable (default: "v022")

  - `adm1`: First administrative level (default: "v024")

  - `adm2`: Second administrative level (default: NULL)

  - `interview_cmc`: Interview date in CMC (default: "v008")

  - `birth_cmc`: Birth date of most recent child (default: "b3_01")

  - `birth_age_months`: Age of most recent child in months (default:
    "b19_01")

  - `sp_taken`: Whether took SP/Fansidar during pregnancy, 1=yes
    (default: "m49a_1")

  - `sp_doses`: Number of SP/Fansidar doses (default: "ml1_1")

- birth_window_months:

  Maximum age of most recent birth in months. Default 24 (DHS standard).
  Women with births older than this are excluded.

- gps_data:

  Optional DHS GPS dataset. If provided, results are cluster-level with
  coordinates.

- gps_vars:

  Named list for GPS variables (cluster, lat, lon).

- shapefile:

  Optional sf object with administrative boundaries for spatial
  aggregation.

- admin_level:

  Character vector of admin columns in shapefile (e.g., c("adm1",
  "adm2")). Auto-detected if NULL.

- join_nearest:

  Logical; if TRUE, assigns unmatched clusters to nearest polygon.
  Default TRUE.

## Value

Tibble with IPTp indicators by cluster (if GPS provided) or by existing
administrative levels, including IPTp 1+, 2+, 3+ coverage and confidence
intervals.

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/iptp_dhs.yml>
