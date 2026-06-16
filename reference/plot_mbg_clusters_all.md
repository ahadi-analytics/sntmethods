# Plot All MBG Cluster Maps

Creates cluster-level maps for all indicators in the output from any
calc\_\*\_mbg() function. Optionally saves each plot to disk.

## Usage

``` r
plot_mbg_clusters_all(
  mbg_results,
  adm0_sf = NULL,
  adm_sf = NULL,
  country_name = NULL,
  survey_year = NULL,
  output_dir = NULL,
  file_prefix = "mbg_clusters",
  width = 10,
  height = 9,
  dpi = 320,
  ...
)
```

## Arguments

- mbg_results:

  Named list of data.tables from any calc\_\*\_mbg() function.

- adm0_sf:

  sf object with country boundary (optional).

- adm_sf:

  sf object with admin boundaries to overlay (optional).

- country_name:

  Country name for plot titles. Default: NULL.

- survey_year:

  Survey year for plot titles. Default: NULL.

- output_dir:

  Directory to save plots. If NULL, plots are returned but not saved.
  Default: NULL.

- file_prefix:

  Prefix for output filenames. Default: "mbg_clusters".

- width:

  Plot width in inches. Default: 10.

- height:

  Plot height in inches. Default: 9.

- dpi:

  Plot resolution. Default: 320.

- ...:

  Additional arguments passed to
  [`plot_mbg_clusters()`](https://ahadi-analytics.github.io/sntmethods/reference/plot_mbg_clusters.md).

## Value

A named list of ggplot2 objects (one per indicator).

## Examples

``` r
if (FALSE) { # \dontrun{
# Works with any MBG indicator
itn_results <- calc_itn_mbg(hr_data, pr_data, gps_data)
plots <- plot_mbg_clusters_all(
  itn_results,
  adm0_sf = country_boundary,
  country_name = "Burundi",
  survey_year = 2021,
  output_dir = "outputs/"
)
} # }
```
