# Read a DHS recode from a hive-partitioned parquet archive

Reads a single DHS / MIS recode from a parquet archive that is laid out
using AHADI's Hive partitioning convention (see *Directory layout*
below). This is the function
[`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md)
and the AHADI example scripts use internally; it is **not** a
general-purpose DHS reader.

## Usage

``` r
dhs_read(
  path,
  survey_id = NULL,
  file_type = NULL,
  country_code = NULL,
  survey_year = NULL,
  survey_type = NULL,
  verbose = TRUE
)
```

## Arguments

- path:

  Root parquet directory (must follow the *Directory layout* above).

- survey_id:

  Optional survey ID (e.g. `"KEKR8A"`). Matches the `survey_id=...`
  partition.

- file_type:

  DHS recode code: one of `"PR"`, `"HR"`, `"IR"`, `"KR"`, `"GE"`,
  `"BR"`, `"MR"`, `"WI"`. Required.

- country_code:

  DHS two-letter country code (e.g. `"TG"`).

- survey_year:

  Survey year (e.g. `2017`).

- survey_type:

  DHS survey type (e.g. `"DHS"`, `"MIS"`). Applied as a post-read filter
  (not a partition column).

- verbose:

  Logical; print progress messages? Default `TRUE`.

## Value

A tibble of filtered DHS records with `haven` labels preserved.

## When to use `dhs_read()`

Use `dhs_read()` only when you have a parquet archive that follows the
layout below. The archive is what makes multi-country, multi-year,
multi-recode discovery
(`dhs_read(file_type = "GE", country_code = ...)`) and the
[`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md)
workflow possible.

If you have a **single DHS file** (`.dta`, `.csv`, `.rds`, `.sav`, ...),
you do **not** need `dhs_read()` or a parquet archive at all. Read the
file with
[`sntutils::read()`](https://ahadi-analytics.github.io/sntutils/reference/read.html)
(or
[`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html))
and pass the resulting data frame straight to any `calc_*_dhs()`
function:

    kr <- sntutils::read("TGKR81FL.DTA")   # or .csv, .rds, .sav, ...
    ge <- sntutils::read("TGGE8AFL.dta")
    fever <- calc_fever_dhs(dhs_kr = kr, gps_data = ge,
                            shapefile = shp_admin,
                            admin_level = c("adm0", "adm1"))

Every `calc_*_dhs()` estimator accepts a plain data frame - the recode-
specific reader is just convenience.

## Directory layout (Hive partitioning)

`dhs_read()` expects `path` to be the root of a directory tree
partitioned in this order, with one parquet file per survey at the leaf:

    path/
      file_type=GE/
        country_code=TG/
          survey_year=2017/
            survey_id=TGGE8I/
              data.parquet
      file_type=KR/
        country_code=TG/
          survey_year=2017/
            survey_id=TGKR81/
              data.parquet
      file_type=PR/...

Partition keys are read literally as `file_type=...`,
`country_code=...`, `survey_year=...`, `survey_id=...`. Allowed
`file_type` values are `PR`, `HR`, `IR`, `KR`, `GE`, `BR`, `MR`, `WI`.
`country_code` is the DHS two-letter code (e.g. `TG`, `BU`, `KE`).

## Building your own parquet archive

To use `dhs_read()` (and therefore
[`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md))
on your own data, convert each raw DHS recode into a parquet file at the
matching leaf path. A minimal recipe per file:

    library(arrow); library(haven); library(fs)

    raw <- haven::read_dta("TGKR81FL.DTA")          # preserves labels
    raw$file_type    <- "KR"
    raw$country_code <- "TG"
    raw$survey_year  <- 2017L
    raw$survey_id    <- "TGKR81"
    raw$survey_type  <- "DHS"                       # or "MIS"

    leaf <- fs::path("path/to/parquet",
                     "file_type=KR",
                     "country_code=TG",
                     "survey_year=2017",
                     "survey_id=TGKR81")
    fs::dir_create(leaf)
    arrow::write_parquet(raw, fs::path(leaf, "data.parquet"))

Repeat per recode (GE/PR/HR/KR/IR) and per survey. Keep `haven` labels
on the columns - `dhs_read()` and the indicator functions rely on them.

## Behaviour notes

- When `country_code` and `survey_year` (and optionally `survey_id`)
  identify a single survey, `dhs_read()` calls
  [`arrow::read_parquet()`](https://arrow.apache.org/docs/r/reference/read_parquet.html)
  directly so haven labels and survey-specific variables are preserved.

- When the filter spans multiple surveys, it falls back to
  [`arrow::open_dataset()`](https://arrow.apache.org/docs/r/reference/open_dataset.html)
  which standardises labels and drops variables absent in some surveys.
  For indicator work, prefer the single-survey path.

- Defensive deduplication runs on standard recode keys to guarantee one
  row per respondent unit (some DHS parquet files contain duplicate
  rows).

## See also

[`sntutils::read()`](https://ahadi-analytics.github.io/sntutils/reference/read.html)
for reading a single DHS file directly,
[`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md)
for the multi-survey pipeline that builds on top of this archive layout.
