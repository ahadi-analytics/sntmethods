#' Get DHS Model Dataset URLs and Metadata
#'
#' Returns a data frame containing metadata and download URLs for all DHS model datasets.
#' These are example datasets provided by DHS for testing and development purposes.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{FileFormat}{Description of the file format (e.g., "Stata dataset (.dta)")}
#'   \item{FileSize}{File size in bytes (NA for model datasets)}
#'   \item{DatasetType}{Type of dataset ("Survey Datasets", "GPS Datasets", or "Survey Final Reports")}
#'   \item{SurveyNum}{Survey number (NA for model datasets)}
#'   \item{SurveyId}{Survey ID (NA for model datasets)}
#'   \item{FileType}{Type of recode file (e.g., "Births Recode", "Geographic Data")}
#'   \item{FileDateLastModified}{Last modification date (NA for model datasets)}
#'   \item{SurveyYearLabel}{Survey year label (NA for model datasets)}
#'   \item{SurveyType}{Survey type (always "DHS" for model datasets)}
#'   \item{SurveyYear}{Survey year ("ModelDatasetSurveyYear" for model datasets)}
#'   \item{DHS_CountryCode}{Country code (always "ZZ" for model datasets)}
#'   \item{FileName}{Name of the downloadable file}
#'   \item{CountryName}{Country name ("ModelDatasetCountry" for model datasets)}
#'   \item{URLS}{Full download URL for the dataset}
#' }
#'
#' @details
#' The DHS model datasets include:
#' \itemize{
#'   \item Survey final reports (PDF format in English and French)
#'   \item Survey data recodes: BR (Births), CR (Couples), HR (Household),
#'         IR (Individual), KR (Children), MR (Men), PR (Household Member),
#'         AR (HIV Test Results)
#'   \item Geographic datasets (shapefiles and CSV)
#'   \item Regional and subregional boundary files
#' }
#'
#' Each recode is available in multiple formats:
#' \itemize{
#'   \item Stata (.dta)
#'   \item SPSS (.sav)
#'   \item SAS (.sas7bdat)
#'   \item Flat ASCII (.dat)
#'   \item Hierarchical ASCII (.dat) - only for IR, MR, and AR recodes
#' }
#'
#' @examples
#' # Get all model dataset URLs
#' model_data <- dhs_model_datasets()
#'
#' # Filter for Stata datasets only
#' stata_urls <- model_data[grep("Stata", model_data$FileFormat), ]
#'
#' # Get HIV test results datasets
#' hiv_urls <- model_data[model_data$FileType == "HIV Test Results Recode", ]
#'
#' # Get geographic datasets
#' geo_urls <- model_data[model_data$DatasetType == "GPS Datasets", ]
#'
#' @export
dhs_model_datasets <- function() {

  # Create the data frame with all model datasets
  df <- data.frame(
    FileFormat = c(
      # Tables (2)
      "Full report tables (PDF)", "Tableaux complets (PDF)",
      # BR (4)
      "Stata dataset (.dta)", "Flat ASCII data (.dat)",
      "SAS dataset (.sas7bdat)", "SPSS dataset (.sav)",
      # CR (4)
      "Stata dataset (.dta)", "Flat ASCII data (.dat)",
      "SAS dataset (.sas7bdat)", "SPSS dataset (.sav)",
      # HR (4)
      "Stata dataset (.dta)", "Flat ASCII data (.dat)",
      "SAS dataset (.sas7bdat)", "SPSS dataset (.sav)",
      # IR (5)
      "Hierarchical ASCII data (.dat)", "Stata dataset (.dta)",
      "Flat ASCII data (.dat)", "SAS dataset (.sas7bdat)",
      "SPSS dataset (.sav)",
      # KR (4)
      "Stata dataset (.dta)", "Flat ASCII data (.dat)",
      "SAS dataset (.sas7bdat)", "SPSS dataset (.sav)",
      # MR (5)
      "Hierarchical ASCII data (.dat)", "Stata dataset (.dta)",
      "Flat ASCII data (.dat)", "SAS dataset (.sas7bdat)",
      "SPSS dataset (.sav)",
      # PR (4)
      "Stata dataset (.dta)", "Flat ASCII data (.dat)",
      "SAS dataset (.sas7bdat)", "SPSS dataset (.sav)",
      # AR (5)
      "Hierarchical ASCII data (.dat)", "Stata dataset (.dta)",
      "Flat ASCII data (.dat)", "SAS dataset (.sas7bdat)",
      "SPSS dataset (.sav)",
      # GPS (4)
      "Geographic data (shapefile)", "Geographic data (CSV)",
      "Geographic data (shapefile)", "Geographic data (shapefile)"
    ),
    FileSize = NA_integer_,
    DatasetType = c(
      rep("Survey Final Reports", 2),
      rep("Survey Datasets", 35),
      rep("GPS Datasets", 4)
    ),
    SurveyNum = NA,
    SurveyId = NA,
    FileType = c(
      rep("Survey - Final Report", 2),
      rep("Births Recode", 4),
      rep("Couples' Recode", 4),
      rep("Household Recode", 4),
      rep("Individual Recode", 5),
      rep("Children's Recode", 4),
      rep("Men's Recode", 5),
      rep("Household Member Recode", 4),
      rep("HIV Test Results Recode", 5),
      "Geographic Data", "Geographic Data",
      "Regional Boundaries", "Subregional Boundaries"
    ),
    FileDateLastModified = NA,
    SurveyYearLabel = NA,
    SurveyType = "DHS",
    SurveyYear = "ModelDatasetSurveyYear",
    DHS_CountryCode = "ZZ",
    FileName = c(
      "zzfulltables.zip", "zztableauxcomplets.zip",
      "zzbr62dt.zip", "zzbr62fl.zip", "zzbr62sd.zip", "zzbr62sv.zip",
      "zzcr61dt.zip", "zzcr61fl.zip", "zzcr61sd.zip", "zzcr61sv.zip",
      "zzhr62dt.zip", "zzhr62fl.zip", "zzhr62sd.zip", "zzhr62sv.zip",
      "zzir62.zip",   "zzir62dt.zip", "zzir62fl.zip", "zzir62sd.zip", "zzir62sv.zip",
      "zzkr62dt.zip", "zzkr62fl.zip", "zzkr62sd.zip", "zzkr62sv.zip",
      "zzmr61.zip",   "zzmr61dt.zip", "zzmr61fl.zip", "zzmr61sd.zip", "zzmr61sv.zip",
      "zzpr62dt.zip", "zzpr62fl.zip", "zzpr62sd.zip", "zzpr62sv.zip",
      "zzar61.zip",   "zzar61dt.zip", "zzar61fl.zip", "zzar61sd.zip", "zzar61sv.zip",
      "zzge61fl.zip", "zzgc61fl.zip", "regional.zip", "subregional.zip"
    ),
    CountryName = "ModelDatasetCountry",
    stringsAsFactors = FALSE
  )

  # Build URLs based on the path segment used by the DHS site:
  #   tables/  -> Full reports
  #   dhs/     -> BR, CR, HR, IR, KR, MR, PR
  #   hiv/     -> AR
  #   gps/     -> GE, GC, regional, subregional
  df$URLS <- paste0(
    "https://dhsprogram.com/data/model_data/",
    c(
      rep("tables", 2),
      rep("dhs",   4 + 4 + 4 + 5 + 4 + 5 + 4),  # BR..PR = 30
      rep("hiv",   5),                          # AR
      rep("gps",   4)                           # GE, GC, regional, subregional
    ),
    "/", df$FileName
  )

  return(df)
}