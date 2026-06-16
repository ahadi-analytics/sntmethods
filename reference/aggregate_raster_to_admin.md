# Aggregate Raster to Administrative Level

Calculates population-weighted or simple mean of raster values within
administrative unit polygons.

## Usage

``` r
aggregate_raster_to_admin(
  raster,
  admin_sf,
  pop_raster = NULL,
  method = "weighted_mean",
  fun = NULL
)
```

## Arguments

- raster:

  SpatRaster to aggregate.

- admin_sf:

  sf object with admin boundaries.

- pop_raster:

  Optional population raster for weighting.

- method:

  Aggregation method: "mean", "weighted_mean", "sum".

- fun:

  Custom aggregation function (overrides method).

## Value

Data frame with admin unit values.
