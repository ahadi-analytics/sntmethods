# Calculate IPTp Coverage from DHS Data (standardized long-format output)

Computes IPTp 1+/2+/3+/4+ and exact-dose 1/2/3 coverage indicators
nationally and optionally by subnational region, returning the
standardized `list(adm0, adm1)` output.

## Usage

``` r
calc_iptp_dhs(
  dhs_ir,
  survey_vars = list(cluster = "v001", weight = "v005", stratum = "v022", adm1 = "v024",
    adm2 = NULL, interview_cmc = "v008", birth_cmc = "b3_01", birth_age_months =
    "b19_01", sp_taken = "m49a_1", sp_doses = "ml1_1"),
  birth_window_months = 24,
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE,
  ci_method = "logit"
)
```

## Arguments

- dhs_ir:

  DHS Individual Recode dataset (IR) in tidy format.

- survey_vars:

  Named list mapping DHS variable names.

- birth_window_months:

  Maximum age of most recent birth in months. Default 24 (DHS standard).

- region_var:

  Optional column name (character string) in `dhs_ir` to use as the
  subnational grouping variable (e.g., `"v024"` for region).

- gps_data:

  Optional DHS GE (GPS) cluster dataset used to attach admin-unit labels
  when `shapefile` is supplied. Default `NULL`.

- gps_vars:

  Named list mapping cluster/lat/lon column names in `gps_data`.
  Defaults to the standard DHS GE names (`DHSCLUST`, `LATNUM`,
  `LONGNUM`).

- shapefile:

  Optional `sf` polygon dataset whose attributes carry admin labels for
  the cluster-to-admin spatial join. When `NULL` (default) the spatial
  join step is skipped.

- admin_level:

  Character vector of admin column names in `shapefile` to retain (e.g.
  `c("adm1", "adm2")`). Default `NULL` (use all).

- join_nearest:

  Logical. If `TRUE` (default), clusters that fall outside any polygon
  are re-assigned to the nearest polygon. If `FALSE`, unmatched clusters
  are left as `NA`.

- ci_method:

  CI method for svyciprop. Default: "logit".

## Value

Named list with `adm0` tibble and optionally `adm1` tibble in
standardized long format.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
result <- calc_iptp_dhs(dhs_ir = ir_data)

# With subnational estimates
result <- calc_iptp_dhs(
  dhs_ir = ir_data,
  region_var = "v024"
)
} # }
```
