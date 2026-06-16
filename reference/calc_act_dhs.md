# Calculate ACT Treatment Indicators from DHS Data

Computes the full set of (World Malaria Report) ACT treatment indicators
from DHS Children's Recode (KR) data. Returns survey-weighted
proportions with logit confidence intervals in standardized long format.

## Usage

``` r
calc_act_dhs(
  dhs_kr,
  dhs_kr_raw = NULL,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", age = "hw1",
    fever = "h22", alive = "b5", act = "ml13e", test = "ml13a"),
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE,
  dhs_pr = NULL,
  indicators = NULL,
  ci_method = "logit"
)
```

## Arguments

- dhs_kr:

  DHS Children's Recode (KR) dataset (data.frame or tibble). If read via
  [`dhs_read()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md),
  haven labels may be standardised. Supply `dhs_kr_raw` for accurate ACT
  variable detection.

- dhs_kr_raw:

  Optional un-standardised KR dataset (e.g., from
  [`arrow::read_parquet()`](https://arrow.apache.org/docs/r/reference/read_parquet.html))
  with original survey-specific haven labels. When provided, ACT and
  antimalarial variables are detected from its labels, and any extra
  variables (e.g., ml13aa, ml13da) are copied into the analysis dataset.
  This is the "two-pass" approach needed when
  [`dhs_read()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md)
  strips country-specific drug names.

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Cluster/PSU ID (default: "v021")

  - `weight`: Survey weight (default: "v005")

  - `stratum`: Stratum variable (default: "v022")

  - `age`: Child's age in months (default: "hw1")

  - `fever`: Had fever in last 2 weeks (default: "h22")

  - `alive`: Child alive (default: "b5")

  - `act`: ACT variable(s) (default: "ml13e"; auto-detected from labels)

  - `test`: Test-positive filter variable (default: "ml13a")

- region_var:

  Optional column name for subnational grouping (e.g., "v024").

- gps_data:

  Optional DHS GE (Geographic) dataset with cluster coordinates.

- gps_vars:

  Named list for GE variables: cluster, lat, lon.

- shapefile:

  Optional sf object with administrative boundaries.

- admin_level:

  Character vector of admin columns from shapefile.

- join_nearest:

  Logical; if TRUE, assigns unmatched clusters to nearest admin unit.
  Default: TRUE.

- dhs_pr:

  Optional DHS Person Recode (PR) for febrile RDT indicators.

- indicators:

  Character vector of indicator names to compute. If NULL (default),
  computes all indicators from
  [`act_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/act_dictionary.md).

- ci_method:

  Method for confidence intervals. Default: "logit".

## Value

Named list of tibbles, one per admin level:

- `adm0`:

  National-level estimates (always present)

- `adm1`:

  Admin-1 estimates (when `region_var` or shapefile used)

- `adm2`:

  Admin-2 estimates (when shapefile with adm2 used)

Each tibble contains columns:

- `survey_id`: Survey identifier (e.g., "TG2017DHS")

- `iso3`: ISO 3166-1 alpha-3 country code (e.g., "TGO")

- `iso2`: DHS 2-letter country code (e.g., "TG")

- `survey_type`: Survey type ("DHS", "MIS", or "AIS")

- `survey_year`: Survey year (integer)

- `adm0`: Country name in UPPERCASE (e.g., "TOGO")

- `adm1`, `adm2`: Admin names in UPPERCASE (subnational tabs only)

- `type`: Analysis type ("survey_weighted")

- `geo_source`: Source of admin names ("survey" or "gps")

- `point`: Survey-weighted proportion (0-1 scale)

- `ci_l`, `ci_u`: Lower and upper 95\\

- `numerator`: Unweighted numerator count (has_act == 1 in filtered
  subgroup)

- `denominator`: Unweighted denominator count (condition-filtered
  subgroup size)

- `indicator`: Indicator name in Title Case (e.g., "Act Antimalarial")

- `indicator_code`: Short indicator code (e.g., "act_am")

- `numerator_description`: Description of numerator

- `denominator_description`: Description of denominator

- `denominator_code`: Short code for the denominator subpopulation

## Details

Computes up to 12 ACT indicators following DHS methodology. Each
indicator measures the proportion of febrile U5 children receiving ACT
within a specific subpopulation defined by care-seeking behaviour and
antimalarial receipt. See
[`act_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/act_dictionary.md)
for the full indicator list.

The function uses three internal helpers:

- `.prepare_act_data()` for ACT variable detection and febrile U5
  filtering

- `.classify_csb_from_h32()` for care-seeking behaviour classification

- Antimalarial composite built from ml13/h37 drug series

## See also

[`act_dictionary()`](https://ahadi-analytics.github.io/sntmethods/reference/act_dictionary.md)
for indicator definitions,
[`calc_act_mbg()`](https://ahadi-analytics.github.io/sntmethods/reference/calc_act_mbg.md)
for cluster-level MBG inputs

## Examples

``` r
if (FALSE) { # \dontrun{
# Two-pass approach (recommended): read standardised + raw
kr <- sntmethods::dhs_read(path, file_type = "KR", ...)
kr_raw <- arrow::read_parquet(parquet_path)
act <- calc_act_dhs(dhs_kr = kr, dhs_kr_raw = kr_raw)

# By region
act <- calc_act_dhs(dhs_kr = kr, dhs_kr_raw = kr_raw, region_var = "v024")

# With GE spatial join
act <- calc_act_dhs(
  dhs_kr = kr, dhs_kr_raw = kr_raw,
  gps_data = ge_data,
  shapefile = admin_sf,
  admin_level = "adm1"
)

# Subset of indicators
act <- calc_act_dhs(
  dhs_kr = kr, dhs_kr_raw = kr_raw,
  indicators = c("ACT_ANTIMALARIAL", "ACT_PUBLIC_ANTIMALARIAL")
)
} # }
```
