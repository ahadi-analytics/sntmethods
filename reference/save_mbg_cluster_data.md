# Save MBG Cluster Data

Saves cluster-level MBG data to CSV files for later reuse. Works with
output from any calc\_\*\_mbg() function. Each indicator is saved as a
separate file containing cluster coordinates, sample sizes, and values.

## Usage

``` r
save_mbg_cluster_data(
  mbg_results,
  output_dir,
  file_prefix = "mbg_cluster_data",
  country_iso3 = NULL,
  survey_year = NULL
)
```

## Arguments

- mbg_results:

  Named list of data.tables from any calc\_\*\_mbg() function.

- output_dir:

  Directory to save CSV files.

- file_prefix:

  Prefix for output filenames. Default: "mbg_cluster_data".

- country_iso3:

  Optional ISO3 country code to include in filename.

- survey_year:

  Optional survey year to include in filename.

## Value

Invisibly returns a character vector of saved file paths.

## Details

Each saved CSV contains:

- cluster_id: DHS cluster identifier

- x: Longitude

- y: Latitude

- indicator: Numerator count (for MBG input)

- samplesize: Denominator count (for MBG input)

- n_positive: Numerator (alias)

- n_tested: Denominator (alias)

- prop_raw: Raw proportion (indicator / samplesize)

## Examples

``` r
if (FALSE) { # \dontrun{
# Works with any MBG indicator
itn_results <- calc_itn_mbg(hr_data, pr_data, gps_data)
save_mbg_cluster_data(itn_results, "data/clusters", country_iso3 = "BDI")

pfpr_results <- calc_pfpr_mbg(pr_data, gps_data)
save_mbg_cluster_data(pfpr_results, "data/clusters", country_iso3 = "BDI")

anc_results <- calc_anc_mbg(ir_data, gps_data)
save_mbg_cluster_data(anc_results, "data/clusters", country_iso3 = "BDI")
} # }
```
