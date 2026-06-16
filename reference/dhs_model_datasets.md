# Get DHS Model Dataset URLs and Metadata

Returns a data frame containing metadata and download URLs for all DHS
model datasets. These are example datasets provided by DHS for testing
and development purposes.

## Usage

``` r
dhs_model_datasets()
```

## Value

A data frame with the following columns:

- FileFormat:

  Description of the file format (e.g., "Stata dataset (.dta)")

- FileSize:

  File size in bytes (NA for model datasets)

- DatasetType:

  Type of dataset ("Survey Datasets", "GPS Datasets", or "Survey Final
  Reports")

- SurveyNum:

  Survey number (NA for model datasets)

- SurveyId:

  Survey ID (NA for model datasets)

- FileType:

  Type of recode file (e.g., "Births Recode", "Geographic Data")

- FileDateLastModified:

  Last modification date (NA for model datasets)

- SurveyYearLabel:

  Survey year label (NA for model datasets)

- SurveyType:

  Survey type (always "DHS" for model datasets)

- SurveyYear:

  Survey year ("ModelDatasetSurveyYear" for model datasets)

- DHS_CountryCode:

  Country code (always "ZZ" for model datasets)

- FileName:

  Name of the downloadable file

- CountryName:

  Country name ("ModelDatasetCountry" for model datasets)

- URLS:

  Full download URL for the dataset

## Details

The DHS model datasets include:

- Survey final reports (PDF format in English and French)

- Survey data recodes: BR (Births), CR (Couples), HR (Household), IR
  (Individual), KR (Children), MR (Men), PR (Household Member), AR (HIV
  Test Results)

- Geographic datasets (shapefiles and CSV)

- Regional and subregional boundary files

Each recode is available in multiple formats:

- Stata (.dta)

- SPSS (.sav)

- SAS (.sas7bdat)

- Flat ASCII (.dat)

- Hierarchical ASCII (.dat) - only for IR, MR, and AR recodes

## Examples

``` r
# Get all model dataset URLs
model_data <- dhs_model_datasets()

# Filter for Stata datasets only
stata_urls <- model_data[grep("Stata", model_data$FileFormat), ]

# Get HIV test results datasets
hiv_urls <- model_data[model_data$FileType == "HIV Test Results Recode", ]

# Get geographic datasets
geo_urls <- model_data[model_data$DatasetType == "GPS Datasets", ]
```
