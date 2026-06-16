# Plot MBG Cluster Map

Creates a map of DHS cluster-level estimates from any MBG indicator,
with points colored by proportion and sized by sample size.

## Usage

``` r
plot_mbg_clusters(
  mbg_data,
  adm0_sf = NULL,
  adm_sf = NULL,
  title = "Indicator by DHS Cluster",
  subtitle = NULL,
  caption = NULL,
  legend_label = "Proportion",
  point_alpha = 0.9,
  point_size_range = c(0.8, 5),
  palette = "heat"
)
```

## Arguments

- mbg_data:

  Data.table or data.frame from any calc\_\*\_mbg() function containing
  columns: cluster_id, x, y, indicator, samplesize.

- adm0_sf:

  sf object with country boundary (optional).

- adm_sf:

  sf object with admin boundaries to overlay (optional).

- title:

  Plot title. Default: "Indicator by DHS Cluster".

- subtitle:

  Plot subtitle. Default: NULL.

- caption:

  Plot caption. Default: NULL.

- legend_label:

  Legend label for proportion scale. Default: "Proportion".

- point_alpha:

  Point transparency. Default: 0.9.

- point_size_range:

  Size range for points. Default: c(0.8, 5).

- palette:

  Color palette - either "heat" (yellow-red) or "blue" (YlGnBu).
  Default: "heat".

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
itn_data <- calc_itn_mbg(hr_data, pr_data, gps_data)
plot_mbg_clusters(
  itn_data[["itn_access"]],
  adm0_sf = country_boundary,
  title = "ITN Access by DHS Cluster"
)
} # }
```
