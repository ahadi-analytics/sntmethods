# Join DHS GPS Coordinates to Person Records Data

Safely merges cluster-level GPS coordinates from a DHS Geographic
dataset onto Person Records (PR) data. All individuals within the same
cluster will receive the same coordinates.

## Usage

``` r
join_dhs_coords(
  pr_data,
  gps_data,
  pr_vars = list(cluster = "hv001"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
)
```

## Arguments

- pr_data:

  DHS Person Records dataset (data.frame or tibble). Can be raw PR data
  or a processed subset that already includes `cluster_id`.

- gps_data:

  DHS GPS dataset (data.frame or tibble) containing cluster coordinates
  from the Geographic (GE) file.

- pr_vars:

  Named list specifying the cluster variable in PR data. Must include
  `cluster` if `cluster_id` column is not already present.

- gps_vars:

  Named list mapping GPS variable names. Must include:

  - `cluster`: Cluster ID variable (default: "DHSCLUST")

  - `lat`: Latitude variable (default: "LATNUM")

  - `lon`: Longitude variable (default: "LONGNUM")

## Value

PR dataset with `cluster_id`, `lat`, and `lon` columns added. Records
without matching GPS coordinates will have NA values for lat/lon.

## Details

Methodology:
<https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/pfpr_dhs.yml>

## Examples

``` r
pr_data <- tibble::tibble(hv001 = c(1, 1, 2), child = 1:3)
gps_data <- tibble::tibble(
  DHSCLUST = c(1, 2),
  LATNUM   = c(8.1, 7.9),
  LONGNUM  = c(-11.0, -10.8)
)

join_dhs_coords(
  pr_data = pr_data,
  gps_data = gps_data,
  pr_vars = list(cluster = "hv001"),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
)
#> # A tibble: 3 × 5
#>   hv001 child cluster_id   lat   lon
#>   <dbl> <int>      <dbl> <dbl> <dbl>
#> 1     1     1          1   8.1 -11  
#> 2     1     2          1   8.1 -11  
#> 3     2     3          2   7.9 -10.8
```
