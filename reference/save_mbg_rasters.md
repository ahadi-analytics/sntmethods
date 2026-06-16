# Save MBG Rasters

Saves MBG prediction rasters (mean, lower, upper) to files.

## Usage

``` r
save_mbg_rasters(
  cell_predictions,
  indicator_name,
  survey_year,
  path,
  country_iso3,
  format = "tif"
)
```

## Arguments

- cell_predictions:

  List with cell_pred_mean, cell_pred_lower, cell_pred_upper rasters
  from MBG.

- indicator_name:

  Name of the indicator (e.g., "itn_access").

- survey_year:

  Survey year.

- path:

  Output directory path.

- country_iso3:

  Three-letter country code.

- format:

  Raster format. Default: "tif".

## Value

Named list of output file paths.
