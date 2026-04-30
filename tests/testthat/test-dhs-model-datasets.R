test_that("dhs_model_datasets returns correct structure", {
  df <- dhs_model_datasets()

  # Check it returns a data frame
  expect_s3_class(df, "data.frame")

  # Check expected number of rows (41 total files)
  expect_equal(nrow(df), 41)

  # Check all expected columns are present
  expected_cols <- c(
    "FileFormat", "FileSize", "DatasetType", "SurveyNum", "SurveyId",
    "FileType", "FileDateLastModified", "SurveyYearLabel", "SurveyType",
    "SurveyYear", "DHS_CountryCode", "FileName", "CountryName", "URLS"
  )
  expect_equal(names(df), expected_cols)

  # Check column types
  expect_type(df$FileFormat, "character")
  expect_type(df$FileName, "character")
  expect_type(df$URLS, "character")
  expect_type(df$FileSize, "integer")

  # Check all DHS_CountryCode are "ZZ" (model dataset indicator)
  expect_true(all(df$DHS_CountryCode == "ZZ"))

  # Check all CountryName are "ModelDatasetCountry"
  expect_true(all(df$CountryName == "ModelDatasetCountry"))
})

test_that("dhs_model_datasets has correct dataset types", {
  df <- dhs_model_datasets()

  # Check dataset type distribution
  type_counts <- table(df$DatasetType)
  expect_equal(as.numeric(type_counts["Survey Final Reports"]), 2)
  expect_equal(as.numeric(type_counts["Survey Datasets"]), 35)
  expect_equal(as.numeric(type_counts["GPS Datasets"]), 4)
})

test_that("dhs_model_datasets URLs are correctly formatted", {
  df <- dhs_model_datasets()

  # All URLs should start with the base URL
  base_url <- "https://dhsprogram.com/data/model_data/"
  expect_true(all(grepl(paste0("^", base_url), df$URLS)))

  # Check AR (HIV) files use /hiv/ path
  ar_files <- df[df$FileType == "HIV Test Results Recode", ]
  expect_true(all(grepl("/model_data/hiv/", ar_files$URLS)))
  expect_equal(nrow(ar_files), 5)

  # Check standard recodes use /dhs/ path
  standard_recodes <- c("Births Recode", "Couples' Recode", "Household Recode",
                       "Individual Recode", "Children's Recode", "Men's Recode",
                       "Household Member Recode")
  std_files <- df[df$FileType %in% standard_recodes, ]
  expect_true(all(grepl("/model_data/dhs/", std_files$URLS)))
  expect_equal(nrow(std_files), 30)

  # Check GPS files use /gps/ path
  gps_files <- df[df$DatasetType == "GPS Datasets", ]
  expect_true(all(grepl("/model_data/gps/", gps_files$URLS)))
  expect_equal(nrow(gps_files), 4)

  # Check tables use /tables/ path
  table_files <- df[df$DatasetType == "Survey Final Reports", ]
  expect_true(all(grepl("/model_data/tables/", table_files$URLS)))
  expect_equal(nrow(table_files), 2)
})

test_that("dhs_model_datasets has correct file formats", {
  df <- dhs_model_datasets()

  # Check Stata files (dt suffix before .zip)
  stata_files <- df[grepl("Stata", df$FileFormat), ]
  expect_true(all(grepl("dt\\.zip$", stata_files$FileName)))

  # Check SPSS files (sv suffix before .zip)
  spss_files <- df[grepl("SPSS", df$FileFormat), ]
  expect_true(all(grepl("sv\\.zip$", spss_files$FileName)))

  # Check SAS files (sd suffix before .zip)
  sas_files <- df[grepl("SAS", df$FileFormat), ]
  expect_true(all(grepl("sd\\.zip$", sas_files$FileName)))

  # Check Flat ASCII files (fl suffix before .zip)
  flat_files <- df[grepl("Flat ASCII", df$FileFormat), ]
  expect_true(all(grepl("fl\\.zip$", flat_files$FileName)))

  # Check Hierarchical ASCII files (only IR, MR, AR have these)
  hier_files <- df[grepl("Hierarchical", df$FileFormat), ]
  expect_equal(nrow(hier_files), 3)
  expect_true(all(hier_files$FileName %in% c("zzir62.zip", "zzmr61.zip", "zzar61.zip")))
})

test_that("dhs_model_datasets has all recode types", {
  df <- dhs_model_datasets()

  # Expected file types and their counts
  expected_types <- list(
    "Survey - Final Report" = 2,
    "Births Recode" = 4,
    "Couples' Recode" = 4,
    "Household Recode" = 4,
    "Individual Recode" = 5,  # Has hierarchical
    "Children's Recode" = 4,
    "Men's Recode" = 5,  # Has hierarchical
    "Household Member Recode" = 4,
    "HIV Test Results Recode" = 5,  # Has hierarchical
    "Geographic Data" = 2,
    "Regional Boundaries" = 1,
    "Subregional Boundaries" = 1
  )

  actual_counts <- table(df$FileType)

  for (type in names(expected_types)) {
    expect_equal(
      as.numeric(actual_counts[type]),
      expected_types[[type]],
      info = paste("FileType:", type)
    )
  }
})

test_that("dhs_model_datasets examples work", {
  # Test that the examples in the documentation work
  model_data <- dhs_model_datasets()

  # Filter for Stata datasets only
  stata_urls <- model_data[grep("Stata", model_data$FileFormat), ]
  expect_true(nrow(stata_urls) > 0)
  expect_true(all(grepl("dt\\.zip", stata_urls$FileName)))

  # Get HIV test results datasets
  hiv_urls <- model_data[model_data$FileType == "HIV Test Results Recode", ]
  expect_equal(nrow(hiv_urls), 5)

  # Get geographic datasets
  geo_urls <- model_data[model_data$DatasetType == "GPS Datasets", ]
  expect_equal(nrow(geo_urls), 4)
})

test_that("dhs_model_datasets URLs are accessible", {
  skip_if_offline()
  skip_on_cran()  # Don't run on CRAN to avoid excessive HTTP requests

  df <- dhs_model_datasets()

  # Test a sample of URLs to verify they're accessible
  # We'll test one from each category
  test_urls <- c(
    df[df$FileName == "zzbr62dt.zip", "URLS"],  # Standard DHS recode
    df[df$FileName == "zzar61dt.zip", "URLS"],   # HIV recode
    df[df$FileName == "zzge61fl.zip", "URLS"]    # GPS dataset
  )

  for (url in test_urls) {
    # Use HEAD request to check if URL is accessible without downloading
    response <- httr::HEAD(url)
    expect_equal(
      httr::status_code(response),
      200,
      info = paste("URL not accessible:", url)
    )
  }
})