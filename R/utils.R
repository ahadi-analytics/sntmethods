#' Build OS-specific path to AHADI OneDrive shared library
#'
#' @description
#' Constructs file paths to resources in the AHADI OneDrive shared library,
#' handling OS-specific differences in OneDrive folder structures. This function
#' automatically detects the operating system and searches for the appropriate
#' OneDrive shared library location.
#'
#' @param relative Character string specifying the relative path within the base
#'   directory. If NULL (default), returns the base directory path.
#' @param org Character string specifying the organization name used in the
#'   OneDrive shared folder name. Defaults to
#'   "AppliedHealthAnalyticsforDeliveryandInnovationInc".
#' @param library Character string specifying the shared library name.
#'   Defaults to "AHADI Information - technical".
#' @param base Character string specifying the base path within the library.
#'   Defaults to "Documentation per topic/data/dhs_data".
#'
#' @return Character string containing the full file path to the requested
#'   resource in the OneDrive shared library.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Detects the operating system (macOS or Windows)
#'   \item Searches OS-specific locations for OneDrive shared folders
#'   \item Locates the specified shared library folder
#'   \item Constructs the full path by combining library, base, and relative
#'   paths
#' }
#'
#' On macOS, the function searches in `~/Library/CloudStorage`.
#' On Windows, it searches in the user's OneDrive folder and user directory.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get the base DHS data directory
#' base_path <- ahadi_path()
#'
#' # Get path to a specific file
#' file_path <- ahadi_path("surveys/2020/survey_data.csv")
#'
#' # Use custom library or base path
#' custom_path <- ahadi_path(
#'   relative = "my_file.xlsx",
#'   library = "Another Library",
#'   base = "different/base/path"
#' )
#' }
#'
#' @seealso
#' \code{\link[base]{file.path}} for path construction,
#' \code{\link[base]{Sys.info}} for system information
#'
ahadi_path <- function(
  relative = NULL,
  org = "AppliedHealthAnalyticsforDeliveryandInnovationInc",
  library = "AHADI Information - technical",
  base = "Documentation per topic/data/dhs_data"
) {
  # detect OS
  os <- Sys.info()[["sysname"]]

  # OS-specific OneDrive search roots
  if (os == "Darwin") {
    candidates <- c("~/Library/CloudStorage")
  } else if (os == "Windows") {
    user <- Sys.getenv("USERNAME")
    candidates <- c(
      file.path("C:/Users", user, "OneDrive"),
      file.path("C:/Users", user)
    )
  } else {
    cli::cli_abort("Unsupported OS: {os}")
  }

  candidates <- candidates |>
  path.expand() |>
  unique()

  # folder name used by OneDrive Shared Libraries
  shared_folder <- paste0("OneDrive-SharedLibraries-", org)

  shared_root <- NULL

  for (c in candidates) {
    if (!dir.exists(c)) {
      next
    }

    subs <- list.files(c, full.names = TRUE, recursive = FALSE)

    hit <- subs[basename(subs) == shared_folder]

    if (length(hit) > 0) {
      shared_root <- hit[1]
      break
    }
  }

  if (is.null(shared_root)) {
    cli::cli_abort("OneDrive Shared Library '{shared_folder}' not found.")
  }

  # go into the shared library folder
  library_root <- file.path(shared_root, library)

  if (!dir.exists(library_root)) {
    cli::cli_abort(
      "Shared Library found but folder '{library}' not found."
    )
  }

  # go into the base folder
  base_root <- file.path(library_root, base)

  if (!dir.exists(base_root)) {
    cli::cli_abort(
      "Base folder '{base}' not found inside '{library}'."
    )
  }

  # if no relative path provided, return base_root
  if (is.null(relative)) {
    return(base_root)
  }

  # otherwise return full resolved path
  final_path <- file.path(base_root, relative)

  return(final_path)
}


#' Filter DHS Parquet Dataset (Arrow)
#'
#' @param path Root parquet directory
#' @param survey_id Optional survey ID (e.g., "KEKR8A")
#' @param file_type Optional file type (PR, IR, KR, GE)
#' @param country_code Optional two-letter DHS country code
#' @param survey_year Optional numeric survey year
#' @param survey_type Optional DHS survey type (e.g., "DHS", "MIS")
#'
#' @return A tibble of filtered DHS records
#' @export
dhs_read <- function(
  path,
  survey_id = NULL,
  file_type = NULL,
  country_code = NULL,
  survey_year = NULL,
  survey_type = NULL
) {
  # -------------------------------------------
  # Validate file_type (now mandatory)
  # -------------------------------------------
  if (is.null(file_type)) {
    cli::cli_abort("`file_type` must be provided (PR, IR, KR, GE etc...).")
  }

  file_type <- toupper(file_type)

  allowed <- c("PR", "HR", "IR", "KR", "GE", "BR", "MR", "WI")
  if (!file_type %in% allowed) {
    cli::cli_abort(
      "Invalid `file_type` '{file_type}'. Must be one of: {allowed}."
    )
  }

  # -------------------------------------------
  # Construct path to file_type folder
  # -------------------------------------------
  ft_path <- fs::path(path, paste0("file_type=", file_type))

  # shorten printed path
  short_path <- ft_path
  home_dir <- fs::path_home()
  short_path <- gsub(home_dir, "~", short_path, fixed = TRUE)

  if (nchar(short_path) > 70) {
    parts <- unlist(strsplit(short_path, "/"))
    short_path <- paste0(
      parts[1],
      "/.../",
      paste(utils::tail(parts, 3), collapse = "/")
    )
  }

  if (!fs::dir_exists(ft_path)) {
    cli::cli_abort("Directory does not exist: {short_path}")
  }

  cli::cli_h1("Loading DHS parquet dataset")
  cli::cli_inform("File type: {file_type}")
  cli::cli_inform("Path: {short_path}")

  # -------------------------------------------
  # Open dataset only for specific file_type
  # -------------------------------------------
  cli::cli_inform("Opening Arrow dataset...")

  suppressWarnings(
    ds <- arrow::open_dataset(
      ft_path,
      # hive_style = TRUE
    )
  )

  # -------------------------------------------
  # Apply filters lazily
  # -------------------------------------------
  applied_filters <- list()

  # then file_type
  suppressWarnings(
    if (!is.null(file_type)) {
      ds <- ds |> dplyr::filter(file_type == !!file_type)
      applied_filters <- c(
        applied_filters,
        paste0("file_type = ", file_type)
      )
    }
  )

  # then country_code
  suppressWarnings(
    if (!is.null(country_code)) {
      ds <- ds |> dplyr::filter(country_code == !!country_code)
      applied_filters <- c(
        applied_filters,
        paste0("country_code = ", country_code)
      )
    }
  )

  # then survey_year
  suppressWarnings(
    if (!is.null(survey_year)) {
      ds <- ds |> dplyr::filter(survey_year == !!survey_year)
      applied_filters <- c(
        applied_filters,
        paste0("survey_year = ", survey_year)
      )
    }
  )

  # survey_id last (lowest-level directory)
  suppressWarnings(
    if (!is.null(survey_id)) {
      ds <- ds |> dplyr::filter(survey_id == !!survey_id)
      applied_filters <- c(
        applied_filters,
        paste0("survey_id = ", survey_id)
      )
    }
  )

  # survey_type (NOT partitioned, so filter last)
  if (!is.null(survey_type)) {
    ds <- ds |> dplyr::filter(survey_type == !!survey_type)
    applied_filters <- c(applied_filters, paste0("survey_type = ", survey_type))
  }

  # Report filters
  if (length(applied_filters) == 0) {
    cli::cli_alert_warning("No filters supplied. Returning whole dataset.")
  } else {
    cli::cli_inform("Applied filters:")
    purrr::walk(applied_filters, ~ cli::cli_li(.x))
  }

  # -------------------------------------------
  # Collect (loads only matching partitions)
  # -------------------------------------------
  cli::cli_inform("Collecting data...")

  suppressWarnings(
    out <- ds |>
      dplyr::collect() |>
      janitor::remove_empty(which = c("rows", "cols"))
  )

  n <- nrow(out)
  cli::cli_inform("Rows loaded: {format(n, big.mark = ',')}")

  if (n == 0) {
    cli::cli_alert_warning("Filter returned zero rows")
  } else {
    cli::cli_alert_success("Data loaded successfully")
  }

  return(out)
}

#' Create Data Dictionary for DHS Raw Datasets
#'
#' @description
#' Generates a comprehensive data dictionary from DHS raw datasets, extracting
#' variable names, labels, types, unique value counts, and missing data
#' percentages. This function is particularly useful for exploring and
#' documenting DHS survey datasets.
#'
#' @param data A data frame or tibble containing DHS survey data with labeled
#'   columns (typically from Haven-imported SPSS/Stata files).
#'
#' @return A tibble with the following columns:
#'   \describe{
#'     \item{var_name}{Character. Variable name as it appears in the dataset}
#'     \item{var_label}{Character. Human-readable label from the variable's
#'       label attribute, or empty string if no label exists}
#'     \item{var_type}{Character. R data type(s) of the variable,
#'       comma-separated if multiple classes}
#'     \item{n_unique}{Integer. Number of unique non-missing values}
#'     \item{pct_missing}{Numeric. Percentage of missing values, rounded to
#'       2 decimal places}
#'   }
#'
#' @details
#' This function is designed to work with labeled data typically found in DHS
#' datasets imported from SPSS or Stata files using the haven package. It
#' safely handles variables without labels and provides a quick overview of
#' data quality and structure.
#'
#' The function extracts:
#' - Variable labels from the "label" attribute
#' - Data types using the class() function
#' - Unique value counts excluding NA values
#' - Missing data percentages as a quality metric
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load a DHS dataset
#' pr_data <- dhs_read(
#'   path = ahadi_path("01_data/parquet"),
#'   file_type = "PR",
#'   country_code = "KE",
#'   survey_year = 2022
#' )
#'
#' # Create data dictionary
#' dict <- make_dhs_raw_dictionary(pr_data)
#'
#' # View first few entries
#' head(dict)
#'
#' # Filter to see variables with high missing rates
#' dict |>
#'   dplyr::filter(pct_missing > 50) |>
#'   dplyr::arrange(desc(pct_missing))
#'
#' # Find malaria-related variables
#' dict |>
#'   dplyr::filter(grepl("malaria|fever|net", var_label, ignore.case = TRUE))
#' }
#'
#' @seealso
#' \code{\link{dhs_read}} for loading DHS parquet datasets
#'
make_dhs_raw_dictionary <- function(data) {
  # extract variable names
  var_names <- names(data)

  # extract labels safely
  var_labels <- purrr::map_chr(var_names, function(v) {
    attr(data[[v]], "label") |>
      dplyr::coalesce("") # replace NULL with ""
  })

  # extract types
  var_types <- purrr::map_chr(var_names, function(v) {
    class(data[[v]]) |> paste(collapse = ", ")
  })

  # number of unique values
  var_nunique <- purrr::map_int(var_names, function(v) {
    dplyr::n_distinct(data[[v]], na.rm = TRUE)
  })

  # percent missing
  var_missing <- purrr::map_dbl(var_names, function(v) {
    mean(is.na(data[[v]])) * 100
  })

  # assemble dictionary
  tibble::tibble(
    var_name = var_names,
    var_label = var_labels,
    var_type = var_types,
    n_unique = var_nunique,
    pct_missing = round(var_missing, 2)
  )
}
