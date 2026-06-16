# Generate All Map Types for Indicator

Generates multiple map types for a single indicator.

## Usage

``` r
generate_all_maps(
  indicator_name,
  mbg_raster = NULL,
  adm1_estimates = NULL,
  adm2_estimates = NULL,
  cluster_data = NULL,
  adm1_sf,
  adm2_sf,
  path_output,
  country_iso3,
  survey_year
)
```

## Arguments

- indicator_name:

  Indicator name.

- mbg_raster:

  MBG prediction raster (mean).

- adm1_estimates:

  ADM1-level survey estimates.

- adm2_estimates:

  ADM2-level MBG estimates (as sf).

- cluster_data:

  Cluster-level data.

- adm1_sf:

  ADM1 boundaries.

- adm2_sf:

  ADM2 boundaries.

- path_output:

  Output directory.

- country_iso3:

  Country code.

- survey_year:

  Survey year.

## Value

List of file paths to saved maps.
