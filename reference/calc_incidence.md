# Calculate Malaria Incidence from Routine Health Facility Data (N0-N5)

Calculates malaria incidence at the admin-month level (e.g.,
district-month) using a structured cascade framework (N0 through N5) to
adjust for testing gaps, reporting incompleteness, and care-seeking
behavior. Facility-level data is aggregated to admin level before
calculating incidence. Returns a validated dataset with all incidence
levels, quality flags, and source tracking.

## Usage

``` r
calc_incidence(
  data,
  levels = c("N0", "N1", "N2", "N3"),
  hf_var = "hf_uid",
  adm0_var = NULL,
  adm1_var = "adm1",
  adm2_var = "adm2",
  adm3_var = NULL,
  date_var = "date",
  year_var = "year",
  pop_var = "pop",
  conf_var = "conf",
  test_var = "test",
  pres_var = "pres",
  tpr_var = "tpr",
  reprate_var = "reprate",
  cs_public_var = "cs_public",
  cs_private_var = "cs_private",
  cs_none_var = "cs_none",
  cs_none_divisor = 2,
  rate_multiplier = 1000,
  scale_factor = lifecycle::deprecated(),
  include_flags = FALSE,
  suffix = NULL,
  group_by = NULL,
  return_facility = FALSE
)
```

## Arguments

- data:

  Routine health facility data at facility-month level (data.frame or
  tibble). Must contain one row per facility per month.

- levels:

  Character vector specifying which incidence levels to calculate
  (default: `c("N0", "N1", "N2", "N3")`). Can specify subset like
  `c("N0", "N1")` or include N4/N5 with
  `c("N0", "N1", "N2", "N3", "N4", "N5")`.

- hf_var:

  Column name for health facility unique identifier (default: "hf_uid").
  Set to `NULL` when data is already aggregated at the admin level and
  has no facility identifier.

- adm0_var:

  Column name for national/country level. If NULL (default), creates a
  single "country" value for all records.

- adm1_var:

  Column name for first administrative level/region (default: "adm1").

- adm2_var:

  Column name for second administrative level/district (default:
  "adm2").

- adm3_var:

  Column name for third administrative level/sub-district. If NULL
  (default), adm3 column is not included in output.

- date_var:

  Column name for date of reporting period (default: "date").

- year_var:

  Column name for year (default: "year"). Used for year-level
  aggregation in summary output.

- pop_var:

  Column name for population denominator (default: "pop"). Must be
  present in `data`.

- conf_var:

  Column name for number of confirmed malaria cases (default: "conf").

- test_var:

  Column name for number of individuals tested (default: "test").

- pres_var:

  Column name for number of presumed cases (default: "pres").

- tpr_var:

  Column name for test positivity rate (default: "tpr"). Must be present
  in `data` for N1/N2/N3 calculations.

- reprate_var:

  Column name for reporting rate (default: "reprate"). Must be present
  in `data` for N2/N3 calculations.

- cs_public_var:

  Column name for proportion seeking care at public facilities (default:
  "cs_public"). Required for N3.

- cs_private_var:

  Column name for proportion seeking care at private facilities
  (default: "cs_private"). Required for N3.

- cs_none_var:

  Column name for proportion seeking no care (default: "cs_none").
  Required for N3/N4/N5.

- cs_none_divisor:

  Numeric. Divisor applied to cs_none for N5 calculation (default: 2).
  N5 uses `cs_none / cs_none_divisor` instead of `cs_none`, producing a
  more conservative estimate than N4.

- rate_multiplier:

  Denominator for incidence rate calculation (default: 1000, for cases
  per 1,000 population).

- scale_factor:

  Deprecated. Use `rate_multiplier` instead.

- include_flags:

  Logical; if `TRUE`, includes all quality flag columns in the output.
  If `FALSE` (default), returns only the core incidence variables
  without flags.

- suffix:

  Character string to append to incidence column names (default: NULL).
  If provided (e.g., `suffix = "u5"`), output columns will be named
  `n0_cases_u5`, `n0_incidence_u5`, etc. Useful for distinguishing
  outputs for different population groups.

- group_by:

  Character vector of additional column names to group by (e.g. facility
  type, urban/rural). These columns are preserved through aggregation
  and appear in output. Default NULL.

- return_facility:

  Logical; if `TRUE`, includes facility-level data in the output with
  diagnostic flags. If `FALSE` (default), returns only aggregated
  admin-level data. Useful for investigating data quality issues before
  aggregation (e.g., why n1_cases \< n0_cases).

## Value

A named list with components:

- monthly:

  A named list with tibbles at each admin level (N0-N5):

  - `adm0`: National level monthly incidence (N0-N5)

  - `adm1`: First admin level (region) monthly incidence (N0-N5)

  - `adm2`: Second admin level (district) monthly incidence (N0-N5)

  - `adm3`: Third admin level monthly incidence (N0-N5, if `adm3_var`
    provided)

- annual:

  A named list with tibbles at each admin level (N0-N5):

  - `adm0`: National level annual incidence (N0-N5)

  - `adm1`: First admin level (region) annual incidence (N0-N5)

  - `adm2`: Second admin level (district) annual incidence (N0-N5)

  - `adm3`: Third admin level annual incidence (N0-N5, if `adm3_var`
    provided)

- facility:

  (Only if `return_facility = TRUE`) A named list with tibbles:

  - `monthly`: Facility-month level data

  - `annual`: Facility-year level data (if applicable)

Monthly and annual tibbles contain:

- ID columns: `adm0`, `adm1`, `adm2`, `adm3` (depending on level)

- Time columns: `year`, `month`, `date` (monthly) or `year` (annual)

- `pop`: Population denominator (summed)

- `conf`: Confirmed cases (summed)

- `test`, `pres`, `tpr`: Testing data (summed, if N1+ calculated)

- `reprate`: Reporting rate (summed, if N2+ calculated)

- `cs_public`, `cs_private`, `cs_none`: Care-seeking proportions (if N3+
  calculated)

- `n0_cases`, `n0_incidence`: N0 (crude)

- `n1_cases`, `n1_incidence`: N1 (testing-adjusted)

- `n2_cases`, `n2_incidence`: N2 (reporting-adjusted)

- `n3_cases`, `n3_incidence`: N3 (care-seeking-adjusted)

- `n4_cases`, `n4_incidence`: N4 (public + non-seekers)

- `n5_cases`, `n5_incidence`: N5 (conservative non-seekers)

## Details

The incidence cascade framework:

- **N0 (Crude Incidence)**:

  - n0_cases = conf

  - n0_incidence = (n0_cases / pop) \* rate_multiplier

- **N1 (Testing-Adjusted)**:

  - n1_cases = n0_cases + (pres \* tpr)

  - n1_incidence = (n1_cases / pop) \* rate_multiplier

- **N2 (Reporting-Adjusted)**:

  - n2_cases = n1_cases / reprate

  - n2_incidence = (n2_cases / pop) \* rate_multiplier

- **N3 (Care-Seeking-Adjusted)**:

  - n3_cases = n2_cases + (n2_cases \* cs_private/cs_public) + (n2_cases
    \* cs_none/cs_public)

  - n3_incidence = (n3_cases / pop) \* rate_multiplier

- **N4 (Public + Non-Seekers Adjusted)**:

  - n4_cases = n2_cases \* (1 + cs_none/cs_public)

  - n4_incidence = (n4_cases / pop) \* rate_multiplier

  - **Note**: Excludes private sector adjustment. Use when private data
    is already captured in DHIS2 or private burden is negligible.

  - N4 is always between N2 and N3 in magnitude (N2 \< N4 \< N3).

- **N5 (Conservative Non-Seekers Adjusted)**:

  - n5_cases = n2_cases \* (1 + (cs_none/cs_none_divisor)/cs_public)

  - n5_incidence = (n5_cases / pop) \* rate_multiplier

  - **Note**: Like N4 but divides the non-care-seeking proportion by
    `cs_none_divisor` (default 2) for a more conservative estimate.

  - N5 is always between N2 and N4 in magnitude (N2 \< N5 \< N4).

Each level builds on the previous, with N3 representing the most
complete estimate of true community-level malaria incidence. N4 provides
a conservative alternative to N3 that excludes private sector. N5
provides an even more conservative estimate by reducing the non-seeker
adjustment.

## Examples

``` r
# Example workflow: Calculate TPR first, then incidence
# library(tibble)
#
# # Step 1: Calculate TPR
# facility_data <- tibble(
#   hf_uid = c("HF001", "HF002", "HF003"),
#   adm1 = rep("RegionA", 3),
#   adm2 = rep("DistrictX", 3),
#   date = as.Date(c("2023-01-01", "2023-02-01", "2023-01-01")),
#   conf = c(10, 15, 12),
#   test = c(100, 120, 80),
#   pres = c(5, 8, 10)
# )
#
# tpr_data <- calc_tpr(facility_data)
#
# # Step 2: Add population, reporting rate, and care-seeking data
# tpr_data$pop <- c(5000, 6000, 4500)
# tpr_data$reprate <- c(0.85, 0.90, 0.80)
# tpr_data$cs_public <- 0.60
# tpr_data$cs_private <- 0.25
# tpr_data$cs_none <- 0.15
#
# # Step 3: Calculate incidence (all levels N0-N3)
# result <- calc_incidence(tpr_data)
#
# # Access monthly facility-level data
# result$monthly$hf
#
# # Access monthly district-level data
# result$monthly$adm2
#
# # Access annual facility-level data
# result$annual$hf
#
# # Access annual national-level data
# result$annual$adm0
```
