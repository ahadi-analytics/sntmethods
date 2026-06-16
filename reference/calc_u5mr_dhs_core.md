# Calculate Under-5 Mortality Rate (U5MR) from DHS data using DHS.rates

core function that estimates under-5 mortality rate (U5MR) using the
DHS.rates::chmort() function following standard DHS methodology. when
gps and shapefile are provided, joins spatial data to assign admin
boundaries to each child record before calculating U5MR at the specified
admin level.

## Usage

``` r
calc_u5mr_dhs_core(
  dhs_kr,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", interview_date
    = "v008", birth_date = "b3", age_at_death = "b7"),
  period_years = 5,
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE
)
```

## Arguments

- dhs_kr:

  dhs children's recode (KR) dataset in tidy format (data.frame or
  tibble).

- survey_vars:

  named list mapping dhs variable names. required keys:

  - `cluster`: cluster id (default: "v021")

  - `weight`: survey weight (default: "v005")

  - `stratum`: stratum variable (default: "v022")

  - `interview_date`: date of interview (default: "v008")

  - `birth_date`: child's birth date (default: "b3")

  - `age_at_death`: age at death in months (default: "b7")

- period_years:

  years before survey to calculate rates (default: 5).

- gps_data:

  optional dhs gps dataset with cluster coordinates.

- gps_vars:

  named list for gps variables (cluster, lat, lon).

- shapefile:

  optional sf object with administrative boundaries.

- admin_level:

  character vector of admin columns from shapefile (for example,
  c("adm1", "adm2")). if NULL, uses existing admin variables in data.

- join_nearest:

  logical; if TRUE, assigns clusters outside polygons to nearest admin
  unit.

## Value

tibble with U5MR estimates by administrative level, with confidence
intervals and sample sizes.

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/u5mr_dhs.yml>
