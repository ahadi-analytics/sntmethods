# Create Data Dictionary for DHS Raw Datasets

Generates a comprehensive data dictionary from DHS raw datasets,
extracting variable names, labels, types, unique value counts, and
missing data percentages. This function is particularly useful for
exploring and documenting DHS survey datasets.

## Usage

``` r
make_dhs_raw_dictionary(data)
```

## Arguments

- data:

  A data frame or tibble containing DHS survey data with labeled columns
  (typically from Haven-imported SPSS/Stata files).

## Value

A tibble with the following columns:

- var_name:

  Character. Variable name as it appears in the dataset

- var_label:

  Character. Human-readable label from the variable's label attribute,
  or empty string if no label exists

- var_type:

  Character. R data type(s) of the variable, comma-separated if multiple
  classes

- n_unique:

  Integer. Number of unique non-missing values

- pct_missing:

  Numeric. Percentage of missing values, rounded to 2 decimal places

## Details

This function is designed to work with labeled data typically found in
DHS datasets imported from SPSS or Stata files using the haven package.
It safely handles variables without labels and provides a quick overview
of data quality and structure.

The function extracts:

- Variable labels from the "label" attribute

- Data types using the class() function

- Unique value counts excluding NA values

- Missing data percentages as a quality metric

## See also

[`dhs_read`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md)
for loading DHS parquet datasets

## Examples

``` r
if (FALSE) { # \dontrun{
# Load a DHS dataset
pr_data <- dhs_read(
  path = dhs_data_path("01_data/parquet"),
  file_type = "PR",
  country_code = "KE",
  survey_year = 2022
)

# Create data dictionary
dict <- make_dhs_raw_dictionary(pr_data)

# View first few entries
head(dict)

# Filter to see variables with high missing rates
dict |>
  dplyr::filter(pct_missing > 50) |>
  dplyr::arrange(desc(pct_missing))

# Find malaria-related variables
dict |>
  dplyr::filter(grepl("malaria|fever|net", var_label, ignore.case = TRUE))
} # }
```
