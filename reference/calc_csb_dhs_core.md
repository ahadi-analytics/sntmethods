# Calculate Care-Seeking Behavior from DHS Data ( Methodology)

Estimates care-seeking behavior for febrile children under 5 using the
WHO World Malaria Report methodology with overlapping indicators.

## Usage

``` r
calc_csb_dhs_core(
  dhs_kr,
  survey_vars = list(cluster = "v021", weight = "v005", stratum = "v022", age = "hw1",
    fever = "h22", alive = "b5"),
  csb_priority_method = c("all", "first", "public", "private"),
  source_config = NULL,
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM"),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE,
  custom_csb_indicator = NULL
)
```

## Arguments

- dhs_kr:

  DHS children's recode (KR) dataset in tidy format (data.frame or
  tibble).

- survey_vars:

  Named list mapping DHS variable names. Required keys:

  - `cluster`: Cluster ID (default: "v021")

  - `weight`: Survey weight (default: "v005")

  - `stratum`: Stratum variable (default: "v022")

  - `age`: Child's age in months (default: "hw1")

  - `fever`: Had fever in last 2 weeks (default: "h22")

  - `alive`: Child survival status (default: "b5"). NOTE: methodology
    assumes filtering to living children (b5 == 1) is done upstream.
    This function does NOT filter by alive status.

- csb_priority_method:

  Character, one of "all" (default), "first", "public", or "private".
  Controls how overlapping care-seeking records are resolved so each
  child is assigned to at most one sector.

  - `"all"`: WHO default. Overlaps allowed; csb_public + csb_private may
    exceed 100%.

  - `"first"`: Take the first h32 source (alphabetical order) visited
    per child.

  - `"public"`: Public/CHW priority when a child sought both sectors.

  - `"private"`: Private priority when a child sought both sectors.

  With non-`"all"` values, csb_public + csb_private + csb_none sums to
  100%.

- source_config:

  **Deprecated**. No longer used. Legacy parameter for backwards
  compatibility. Named list with:

  - `public`: Character vector of h32 codes for public sector

  - `private`: Character vector of h32 codes for private sector

  - `excluded`: Character vector of h32 codes to exclude

- region_var:

  Optional column name (character string) in `dhs_kr` to use as the
  grouping variable (e.g., `"v024"` for region). When provided, this
  takes precedence over GPS/shapefile-based grouping and the column
  appears first in the output.

- gps_data:

  Optional DHS GPS dataset with cluster coordinates.

- gps_vars:

  Named list for GPS variables (cluster, lat, lon).

- shapefile:

  Optional sf object with administrative boundaries.

- admin_level:

  Character vector of admin columns from shapefile (e.g., c("adm1",
  "adm2")). If NULL, uses existing admin variables in data.

- join_nearest:

  Logical; if TRUE, assigns clusters outside polygons to nearest admin
  unit.

- custom_csb_indicator:

  Optional named list defining a user-specified, mutually-exclusive
  care-seeking partition fitted in addition to the built-in CSB
  indicators. When supplied, three derived indicators are produced:
  `<name>_dhis` (sought care at any user-listed DHIS source),
  `<name>_nondhis` (sought care at any user-listed non-DHIS source and
  not at any DHIS source), and `<name>_untreat` (did not seek care at
  any user-listed source). The list must have four character fields:
  `name` (alphanumeric prefix matching pattern `^csb_[a-z0-9_]+$`),
  `dhis_locs`, `nondhis_locs`, and `untreat_locs`. Each `*_locs` vector
  may contain either **h32 variable names** (e.g. `"h32a"`, `"h32e"`)
  which are matched as columns in `dhs_kr`, or **haven label strings**
  which are matched case-insensitively against the `label` attribute of
  each h32 column. The two styles can be mixed in the same vector.
  Variable-name matches take precedence over label matches, which is
  useful when two h32 columns share an identical haven label (e.g.
  `h32e` and `h32n` both labelled "comm.health wrkr"). The custom triple
  is always mutually exclusive at the child level (priority
  `dhis > nondhis > untreat`). Default: NULL (disabled).

## Value

Tibble with CSB estimates by administrative level, with confidence
intervals and sample sizes.

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/csb_dhs.yml>

This function implements the WHO World Malaria Report methodology for
care-seeking behavior analysis.

**5-Category Classification:**

- `public`: Government health facilities (hospitals, health centers,
  posts)

- `chw`: Community health workers (often NGO sector in DHS-8)

- `private_formal`: Private hospitals, clinics, and doctors

- `private_informal`: Traditional practitioners and other informal
  sources

- `pharmacy`: Pharmacies and drug shops

**Derived Indicators (OVERLAPPING):** These indicators are NOT mutually
exclusive. A child can be counted in multiple categories if they visited
multiple source types.

- `dhs_csb_public`: Public sector care (public OR chw)

- `dhs_csb_private`: Any private sector care (private_formal OR
  private_informal OR pharmacy)

- `dhs_csb_trained`: Trained provider (public OR private_formal OR
  pharmacy)

- `dhs_csb_any`: Any treatment sought (public OR private)

- `dhs_csb_none`: No treatment sought (NOT any)

**Important:** Only `dhs_csb_any` and `dhs_csb_none` are mutually
exclusive. The equation `dhs_csb_any + dhs_csb_none = 1` always holds.
However, `dhs_csb_public + dhs_csb_private + dhs_csb_none` may exceed
1.0 when children visit both public and private sources.

## References

WHO. World Malaria Report. Geneva: World Health Organization.
<https://www.who.int/teams/global-malaria-programme/reports>
