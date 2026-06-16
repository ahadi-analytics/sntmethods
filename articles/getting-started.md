# Get started

## What is `sntmethods`?

`sntmethods` is an R package developed by AHADI that turns raw survey
and routine health data into the sub-national estimates used to plan
malaria control. It is the analytical companion to
[`sntutils`](https://ahadi-analytics.github.io/sntutils/): where
`sntutils` reads, cleans, and harmonises data, `sntmethods` runs the
epidemiological methods on top of it.

The package is organised around **three workflows**, each covered by its
own article:

- **[DHS survey
  analysis](https://ahadi-analytics.github.io/sntmethods/articles/dhs-survey-analysis.md)** -
  survey-weighted, design-correct estimates for 100+ indicators across
  16 domains, straight from DHS/MIS microdata.
- **[Spatial modeling
  (MBG)](https://ahadi-analytics.github.io/sntmethods/articles/spatial-modeling.md)** -
  model-based geostatistics that turns cluster-level survey data into
  continuous raster surfaces and population-weighted admin estimates.
- **[Routine data: incidence &
  TPR](https://ahadi-analytics.github.io/sntmethods/articles/routine-incidence.md)** -
  malaria incidence from health-facility data via the N0-N5 cascade,
  test positivity with structured fallbacks, and trend detection.

Two ideas run through all three:

- **Long-format, admin-stratified outputs.** Every estimator returns
  tidy tibbles keyed by admin level (`adm0`, `adm1`, `adm2`) with
  `point`, `ci_l`, `ci_u`, `numerator`, `denominator`, and indicator
  metadata - never a nested list you have to unpack.
- **Documented methods.** Each indicator family ships a machine-readable
  data dictionary and a methodology spec under
  [`inst/methods/`](https://github.com/ahadi-analytics/sntmethods/tree/master/inst/methods).
  See the [Methodology &
  conventions](https://ahadi-analytics.github.io/sntmethods/articles/methodology.md)
  article.

**New to Subnational Tailoring?** The [AHADI SNT Code
Library](https://ahadi-analytics.github.io/snt-code-library/) is the
methodology-level companion - country examples and the analytical
reasoning behind every step these functions automate.

## Install

``` r

# install pak if needed
install.packages("pak")

# install sntmethods from GitHub (pulls sntutils automatically)
pak::pkg_install("ahadi-analytics/sntmethods")
```

Or with `remotes`:

``` r

remotes::install_github("ahadi-analytics/sntmethods")
```

### System dependencies (spatial features)

The MBG and spatial functions use `sf` and `terra`, which require GDAL,
GEOS, and PROJ:

``` bash
# macOS
brew install gdal geos proj

# Ubuntu/Debian
sudo apt-get install -y libgdal-dev libgeos-dev libproj-dev libudunits2-dev
```

### MBG dependencies (optional)

Spatial modeling additionally needs INLA (from its own repository) and
the `mbg` engine. These are only required if you run the [MBG
workflow](https://ahadi-analytics.github.io/sntmethods/articles/spatial-modeling.md):

``` r

install.packages(
  "INLA",
  repos = c(
    INLA = "https://inla.r-inla-download.org/R/stable",
    CRAN = "https://cloud.r-project.org"
  )
)

remotes::install_github("ihmeuw/mbg")
```

The DHS-survey and routine-data workflows have no spatial dependencies -
you can use them without INLA, `mbg`, or even `sf`.

## Reading DHS data: two paths

`sntmethods` does **not** lock you into a specific reader. Every
`calc_*_dhs()` function takes a plain data frame, so you have two paths:

- **Path A - I have one or a few DHS files (`.dta`, `.csv`, `.rds`,
  …).** Use
  [`sntutils::read()`](https://ahadi-analytics.github.io/sntutils/) (or
  [`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html))
  to load each file and pass it straight to the indicator. No special
  directory layout, no archive, no parquet.

  ``` r

  library(sntmethods)
  library(sntutils)

  hr <- sntutils::read("data/BUHR62FL.DTA")  # any of .dta .csv .rds .sav ...
  pr <- sntutils::read("data/BUPR62FL.DTA")

  itn <- calc_itn_dhs(dhs_hr = hr, dhs_pr = pr)
  ```

  This is the right path for most analysts and is what we recommend if
  you are working with a single country / survey.

- **Path B - I want to discover and read across an archive of many
  surveys.** Build a hive-partitioned parquet archive in AHADI’s layout
  (see
  [`?dhs_read`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.html))
  and use
  [`dhs_read()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md)
  to query it by `file_type` / `country_code` / `survey_year` /
  `survey_type`. This is the layout
  [`run_mbg_pipeline()`](https://ahadi-analytics.github.io/sntmethods/reference/run_mbg_pipeline.md)
  consumes.

  ``` r

  hr <- dhs_read(path = "path/to/parquet", file_type = "HR",
                 country_code = "BU", survey_year = 2016)
  pr <- dhs_read(path = "path/to/parquet", file_type = "PR",
                 country_code = "BU", survey_year = 2016)
  itn <- calc_itn_dhs(dhs_hr = hr, dhs_pr = pr)
  ```

[`dhs_read()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md)
is **not** a general DHS file reader - it is an archive query tool. If
you only have a handful of files, stay on Path A. The only reason to
take Path B is to enable cross-survey discovery and the [MBG
pipeline](https://ahadi-analytics.github.io/sntmethods/articles/spatial-modeling.md).

The output is identical in both cases:

``` r

itn$adm1
#> # a long-format tibble: indicator, indicator_code, point, ci_l, ci_u,
#> #   numerator, denominator, adm1, survey_year, ...
```

Every `calc_*_dhs()` function follows the same shape, so once you have
read one indicator you have read them all. Continue with the [DHS survey
analysis](https://ahadi-analytics.github.io/sntmethods/articles/dhs-survey-analysis.md)
article.

## Where to next?

| If you want to… | Read |
|----|----|
| Compute survey indicators from DHS/MIS microdata | [DHS survey analysis](https://ahadi-analytics.github.io/sntmethods/articles/dhs-survey-analysis.md) |
| Produce continuous maps / model-based estimates | [Spatial modeling (MBG)](https://ahadi-analytics.github.io/sntmethods/articles/spatial-modeling.md) |
| Estimate incidence or TPR from routine data | [Routine data](https://ahadi-analytics.github.io/sntmethods/articles/routine-incidence.md) |
| Detect trends in monthly time series | [Trend analysis](https://ahadi-analytics.github.io/sntmethods/articles/trend-analysis.md) |
| Understand the methods and naming conventions | [Methodology & conventions](https://ahadi-analytics.github.io/sntmethods/articles/methodology.md) |
| Look up a specific function | [Reference](https://ahadi-analytics.github.io/sntmethods/reference/index.html) |

\`\`\`
