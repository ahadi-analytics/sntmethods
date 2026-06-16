# Generate Indicator Map

Creates various types of maps for indicator visualization.

## Usage

``` r
generate_indicator_map(
  estimate_data = NULL,
  boundaries,
  indicator_col,
  map_type = "adm2",
  cluster_data = NULL,
  raster = NULL,
  title = NULL,
  palette = "YlGnBu",
  reverse_palette = FALSE,
  legend_title = "Indicator",
  show_values = FALSE
)
```

## Arguments

- estimate_data:

  Data frame with estimates and geometry, or sf object.

- boundaries:

  sf object with admin boundaries.

- indicator_col:

  Name of indicator column to map.

- map_type:

  Type of map:

  - "adm1": ADM1 choropleth with ADM2 boundaries

  - "adm1_clusters": ADM1 choropleth with cluster points

  - "raster": Pixel-level raster map

  - "adm2": ADM2 choropleth from raster aggregation

  - "adm2_clusters": ADM2 choropleth with cluster points

  - "raster_clusters": Raster with cluster points

- cluster_data:

  Optional data frame with cluster locations and values.

- raster:

  Optional SpatRaster for raster maps.

- title:

  Map title.

- palette:

  Color palette. Default: "YlGnBu".

- reverse_palette:

  Logical. Reverse palette direction.

- legend_title:

  Legend title.

- show_values:

  Logical. Show values on admin units.

## Value

A ggplot2 or tmap object.
