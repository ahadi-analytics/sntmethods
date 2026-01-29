#' Resolve path inside AHADI OneDrive shared library
#'
#' @description
#' `ahadi_path()` builds a robust, OS-aware path to a file or folder inside
#' the AHADI OneDrive "Shared Library". It:
#'
#' * detects the OS (Windows or macOS)
#' * scans common OneDrive roots (including CloudStorage on macOS)
#' * finds the "Shared Libraries" folder for the given organisation
#' * fuzzy-matches the requested library name
#' * navigates to a given base folder inside that library
#' * optionally joins a relative path
#'
#' The function caches the resolved library root in an R option so that
#' subsequent calls are fast. Use `refresh = TRUE` to force re-detection.
#'
#' @param relative character. Relative path inside `base`. If `NULL`, the
#'   function returns the resolved `base` folder path.
#' @param org character. Organisation name as it appears in OneDrive shared
#'   library naming. This is cleaned internally for matching. Default is
#'   "Applied Health Analytics for Delivery and Innovation Inc".
#' @param library character. Display name of the OneDrive shared library.
#'   Example: "AHADI Information - technical".
#' @param base character. Base folder inside the library. Example:
#'   "Documentation per topic/data/dhs_data".
#' @param refresh logical. If `TRUE`, ignores any cached location and forces
#'   a fresh search.
#' @param verbose logical. If `TRUE`, prints diagnostic messages about how
#'   the path was resolved.
#'
#' @return
#' A character scalar giving an absolute path. If `relative` is `NULL`, this
#' is the resolved `base` path. Otherwise it is `file.path(base, relative)`.
#'
#' @details
#' The detection logic follows several steps:
#'
#' * detect OS using `Sys.info()[["sysname"]]`
#' * construct a list of candidate OneDrive root folders
#' * within each root, look for folders whose names suggest a OneDrive
#'   "Shared Libraries" mount
#' * inside those, fuzzy match the requested organisation and library names
#' * construct and validate the requested `base` folder
#'
#' Matching is done on "cleaned" names (lowercase, no spaces or punctuation).
#' This makes the function resilient to cosmetic differences in naming such
#' as "AHADI Information - technical" vs "AHADI information (technical)".
#'
#' If the function cannot find a matching organisation, library, or base
#' folder, it fails with `cli::cli_abort()` and prints the closest matches
#' it could see.
#'
#' @examples
#' \dontrun{
#' # get the base DHS data directory
#' base_path <- ahadi_path()
#'
#' # get a specific file inside the base folder
#' file_path <- ahadi_path("surveys/2020/survey_data.csv")
#'
#' # use a different library and base folder
#' other_path <- ahadi_path(
#'   relative = "my_file.xlsx",
#'   library = "AHADI Operations",
#'   base = "SOPs"
#' )
#'
#' # force re-detection if the OneDrive setup changed
#' ahadi_path(refresh = TRUE, verbose = TRUE)
#' }
#'
#' @importFrom utils head
#' @export
ahadi_path <- function(
  relative = NULL,
  org = "Applied Health Analytics for Delivery and Innovation Inc",
  library = "AHADI Information - technical",
  base = "Documentation per topic/data/dhs_data",
  refresh = FALSE,
  verbose = FALSE
) {
  # get OS
  os_name <- Sys.info()[["sysname"]]

  # validate OS
  if (!os_name %in% c("Darwin", "Windows")) {
    cli::cli_abort(
      "Unsupported operating system: {os_name}. Only macOS and Windows
      are supported."
    )
  }

  # clean reference strings for matching
  org_clean <- ahadi_clean_string(org)
  library_clean <- ahadi_clean_string(library)

  # try cached library root first
  library_root <- NULL

  if (!isTRUE(refresh)) {
    cache <- getOption("ahadi.onedrive.cache", default = NULL)

    if (!is.null(cache)) {
      if (
        is.list(cache) &&
          all(
            c("library_root", "org_clean", "library_clean") %in%
              names(cache)
          )
      ) {
        if (
          identical(cache$org_clean, org_clean) &&
            identical(cache$library_clean, library_clean) &&
            dir.exists(cache$library_root)
        ) {
          library_root <- cache$library_root

          if (isTRUE(verbose)) {
            cli::cli_inform(
              c(
                "i" = "Using cached OneDrive library root:",
                " " = "{library_root}"
              )
            )
          }
        }
      }
    }
  }

  # if no cache, detect library root
  if (is.null(library_root)) {
    if (isTRUE(verbose)) {
      cli::cli_inform("Detecting OneDrive shared library on {os_name}...")
    }

    # get OneDrive roots by OS
    roots <- ahadi_find_onedrive_roots(os_name = os_name, verbose = verbose)

    if (length(roots) == 0L) {
      cli::cli_abort(
        c(
          "Could not find any OneDrive root folders.",
          "i" = "Check that OneDrive is installed and that the shared
                 library is synced locally.",
          "i" = "If this is a fresh machine, open OneDrive and make sure
                 the AHADI library is added and synced."
        )
      )
    }

    # find organisation-level shared library root
    shared_root <- ahadi_find_shared_root(
      roots = roots,
      org_clean = org_clean,
      verbose = verbose
    )

    # find specific library folder inside shared root
    library_root <- ahadi_find_library_root(
      shared_root = shared_root,
      library_clean = library_clean,
      verbose = verbose
    )

    # cache result
    options(
      ahadi.onedrive.cache = list(
        library_root = library_root,
        org_clean = org_clean,
        library_clean = library_clean
      )
    )
  }

  # construct base root
  base_root <- file.path(library_root, base)

  if (!dir.exists(base_root)) {
    # list some candidates to assist debugging
    parent_dir <- dirname(base_root)

    if (dir.exists(parent_dir)) {
      siblings <- list.dirs(
        parent_dir,
        full.names = FALSE,
        recursive = FALSE
      )
    } else {
      siblings <- character(0L)
    }

    cli::cli_abort(
      c(
        "Base folder '{base}' not found inside library.",
        "i" = "Expected path: {base_root}",
        if (length(siblings) > 0L) {
          c(
            "i" = "Folders that do exist under {parent_dir}:",
            " " = paste0("  - ", head(siblings, 20L), collapse = "\n")
          )
        } else {
          "i" = "Parent directory does not exist or has no visible
                 subfolders."
        }
      )
    )
  }

  if (is.null(relative)) {
    if (isTRUE(verbose)) {
      cli::cli_inform(
        "Returning base folder: {base_root}"
      )
    }

    return(base_root)
  }

  # construct final path
  final_path <- file.path(base_root, relative)

  if (isTRUE(verbose)) {
    cli::cli_inform(
      c(
        "i" = "Returning path inside library:",
        " " = "{final_path}"
      )
    )
  }

  return(final_path)
}

# ---- helpers: string cleaning and matching ----------------------------------

#' Clean string for fuzzy matching
#'
#' Removes spaces and punctuation and converts to lowercase.
#'
#' @noRd
ahadi_clean_string <- function(x) {
  if (is.null(x)) {
    return("")
  }

  # ensure character
  x_chr <- as.character(x)

  # convert to lowercase and strip non-alphanumeric characters
  x_chr |>
    tolower() |>
    gsub("[^a-z0-9]", "", x = _) |>
    trimws()
}

#' Fuzzy match a single target against candidate names
#'
#' Returns the index of the best match or `NA_integer_` if none found.
#'
#' @noRd
ahadi_fuzzy_match <- function(
  candidates,
  target_clean
) {
  if (length(candidates) == 0L) {
    return(NA_integer_)
  }

  cand_clean <- ahadi_clean_string(candidates)

  # direct equality first
  eq_idx <- which(cand_clean == target_clean)

  if (length(eq_idx) >= 1L) {
    return(eq_idx[1L])
  }

  # substring both ways
  contains_idx <- which(
    grepl(target_clean, cand_clean, fixed = TRUE) |
      grepl(cand_clean, target_clean, fixed = TRUE)
  )

  if (length(contains_idx) >= 1L) {
    return(contains_idx[1L])
  }

  # no clear match
  NA_integer_
}

# ---- helpers: root discovery ------------------------------------------------

#' Find candidate OneDrive root folders
#'
#' @noRd
ahadi_find_onedrive_roots <- function(
  os_name,
  verbose = FALSE
) {
  # candidate roots differ by OS
  if (os_name == "Darwin") {
    candidates <- c(
      "~/Library/CloudStorage",
      "~/OneDrive",
      "~"
    )
  } else {
    # Windows
    user <- Sys.getenv("USERNAME", unset = NA_character_)

    if (is.na(user) || user == "") {
      cli::cli_abort("Could not determine Windows username from USERNAME.")
    }

    candidates <- c(
      file.path("C:/Users", user, "OneDrive"),
      file.path("C:/Users", user, "OneDrive - AHADI"),
      file.path(
        "C:/Users",
        user,
        "OneDrive - Applied Health Analytics
                for Delivery and Innovation Inc"
      ),
      file.path("C:/Users", user, "Library", "CloudStorage"),
      file.path("C:/Users", user)
    )
  }

  # normalise and drop non-existent
  roots <- candidates |>
    path.expand() |>
    unique()
  roots <- roots[dir.exists(roots)]

  if (isTRUE(verbose)) {
    cli::cli_inform(
      c(
        "i" = "Candidate OneDrive roots detected:",
        " " = paste0("  - ", roots, collapse = "\n")
      )
    )
  }

  roots
}

# ---- helpers: shared library root ------------------------------------------

#' Find the shared library root matching an organisation
#'
#' @noRd
ahadi_find_shared_root <- function(
  roots,
  org_clean,
  verbose = FALSE
) {
  # patterns that often appear in OneDrive shared library mounts
  shared_patterns <- c(
    "onedrive-sharedlibraries",
    "onedrivesharedlibraries",
    "sharedlibraries",
    "sharepoint"
  )

  candidate_hits <- character(0L)

  for (root in roots) {
    # get top-level dirs
    entries <- list.dirs(
      root,
      full.names = TRUE,
      recursive = FALSE
    )

    if (length(entries) == 0L) {
      next
    }

    base_names <- basename(entries)
    base_clean <- ahadi_clean_string(base_names)

    is_shared_like <- vapply(
      shared_patterns,
      function(pat) grepl(pat, base_clean, fixed = TRUE),
      logical(length(base_clean))
    )
    is_shared_like <- apply(is_shared_like, 1L, any)

    org_like <- grepl(org_clean, base_clean, fixed = TRUE) |
      grepl(org_clean, dirname(base_clean), fixed = TRUE)

    hits <- entries[is_shared_like | org_like]

    if (length(hits) > 0L) {
      candidate_hits <- c(candidate_hits, hits)
    }
  }

  candidate_hits <- unique(candidate_hits)

  if (length(candidate_hits) == 0L) {
    cli::cli_abort(
      c(
        "Could not find a OneDrive shared library root for organisation
        '{org_clean}'.",
        "i" = "Checked the following roots:",
        " " = paste0("  - ", roots, collapse = "\n")
      )
    )
  }

  # if multiple, pick the one whose cleaned name best matches org_clean
  base_names <- basename(candidate_hits)
  idx <- ahadi_fuzzy_match(base_names, org_clean)

  if (is.na(idx)) {
    # fall back to first, but inform
    chosen <- candidate_hits[1L]

    if (isTRUE(verbose)) {
      cli::cli_inform(
        c(
          "!" = "Multiple candidate shared roots found but no strong
                 fuzzy match to organisation.",
          "i" = "Using the first candidate: {chosen}",
          "i" = "Candidates:",
          " " = paste0("  - ", candidate_hits, collapse = "\n")
        )
      )
    }
  } else {
    chosen <- candidate_hits[idx]

    if (isTRUE(verbose)) {
      cli::cli_inform(
        c(
          "i" = "Using shared library root:",
          " " = "{chosen}"
        )
      )
    }
  }

  chosen
}

# ---- helpers: library root --------------------------------------------------

#' Find the specific library inside the shared root
#'
#' @noRd
ahadi_find_library_root <- function(
  shared_root,
  library_clean,
  verbose = FALSE
) {
  # list immediate subfolders, which should include individual libraries
  libraries <- list.dirs(
    shared_root,
    full.names = TRUE,
    recursive = FALSE
  )

  if (length(libraries) == 0L) {
    cli::cli_abort(
      c(
        "No subfolders found inside shared library root '{shared_root}'.",
        "i" = "Expected to find at least one library."
      )
    )
  }

  library_names <- basename(libraries)
  idx <- ahadi_fuzzy_match(library_names, library_clean)

  if (is.na(idx)) {
    cli::cli_abort(
      c(
        "Could not find a library matching '{library_clean}' inside
        '{shared_root}'.",
        "i" = "Available libraries:",
        " " = paste0("  - ", library_names, collapse = "\n")
      )
    )
  }

  chosen <- libraries[idx]

  if (isTRUE(verbose)) {
    cli::cli_inform(
      c(
        "i" = "Using library folder:",
        " " = "{chosen}"
      )
    )
  }

  chosen
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


#' Convert DHS Century Month Code to Date
#'
#' @description
#' Converts DHS Century Month Code (CMC) values to R Date objects.
#' CMC is a date coding system used by DHS where CMC = (year - 1900) * 12 + month.
#'
#' @param cmc Numeric vector of CMC values.
#'
#' @return Vector of Date objects (mid-month: day 15).
#'
#' @details
#' The DHS Century Month Code (CMC) encodes dates as the number of months since
#' December 1899. The formula is: CMC = (year - 1900) * 12 + month.
#'
#' For example:
#' - CMC 1 = January 1900
#' - CMC 1324 = January 2010
#' - CMC 1404 = August 2016
#'
#' @examples
#' \dontrun{
#' cmc_to_date(1)     # 1900-01-15
#' cmc_to_date(1324)  # 2010-01-15
#' cmc_to_date(1404)  # 2016-08-15
#' }
#'
#' @export
cmc_to_date <- function(cmc) {
  if (any(cmc <= 0 | cmc > 1500, na.rm = TRUE)) {
    cli::cli_warn("Some CMC values are out of expected range (1-1500)")
  }

  year <- floor((cmc - 1) / 12) + 1900
  month <- ((cmc - 1) %% 12) + 1
  as.Date(paste(year, month, 15, sep = "-"))
}
