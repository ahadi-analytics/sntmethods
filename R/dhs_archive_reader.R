# =============================================================================
# DHS parquet archive: reader family  (changes C1-C4, C7)
#
# Complete, final versions of every function added or rewritten in
# R/utils.R. Paste over the corresponding block; nothing else in utils.R
# changes.
#
#   .dhs_index_cache        session cache for the leaf index
#   .dhs_parse_hive_keys()  parse key=value at any depth
#   .dhs_walk_parquet()     resilient recursive listing  (Windows fix)
#   .dhs_leaf_index()       index leaves instead of constructing paths
#   .dhs_leaf_identity()    read year / survey_type out of a leaf
#   dhs_archive_surveys()   EXPORTED - one row per survey
#   .dhs_confirm()          y/n gate
#   .dhs_content_matches()  a GE leaf must really hold coordinates
#   .dhs_dedupe_rows()      + GE / GC keys
#   dhs_read()              rewritten
#
# dhs_read() and .dhs_dedupe_rows() have MOVED here from utils.R - the
# companion utils.R in this set has them removed, so there is exactly one
# definition of each. Do not keep both.
# =============================================================================


# Session cache so the pipeline's repeated dhs_read() calls scan the tree
# once per file_type rather than once per call.
.dhs_index_cache <- new.env(parent = emptyenv())

#' Parse hive `key=value` segments from a relative path
#'
#' Keys are read wherever they appear, at any depth and in any order, so a
#' leaf carrying an extra partition level (e.g. `survey_type=MIS/`) is
#' still described correctly.
#'
#' @param rel_path Character path relative to the `file_type=` folder.
#' @return Named list of partition values.
#' @keywords internal
#' @noRd
.dhs_parse_hive_keys <- function(rel_path) {
  parts <- unlist(strsplit(rel_path, "/", fixed = TRUE))
  kv <- parts[grepl("^[A-Za-z_][A-Za-z_0-9]*=", parts)]
  if (length(kv) == 0) {
    return(list())
  }
  stats::setNames(
    as.list(sub("^[^=]+=", "", kv)),
    sub("=.*$", "", kv)
  )
}

#' Recursively list parquet files, skipping unreadable branches
#'
#' @description
#' `fs::dir_ls(recurse = TRUE)` aborts the entire walk when any single
#' directory cannot be opened. On Windows that happens for real: the path
#' limit is 260 characters and `fs` enforces it whatever the
#' `LongPathsEnabled` registry key says. Arrow writes
#' `__HIVE_DEFAULT_PARTITION__` (26 characters) for any `NA` partition
#' value, so two of those under a deep OneDrive root exceed the limit on
#' their own.
#'
#' Such a leaf is unreadable by every tool, not only this one, so it is
#' reported and skipped rather than allowed to fail the whole query.
#'
#' @param dir Directory to walk.
#' @param depth Current recursion depth (internal).
#' @param max_depth Guard against pathological trees.
#' @return Character vector of parquet file paths.
#' @keywords internal
#' @noRd
.dhs_walk_parquet <- function(dir, depth = 0L, max_depth = 8L) {
  if (depth > max_depth) {
    return(character())
  }

  entries <- tryCatch(
    fs::dir_ls(dir, recurse = FALSE, all = FALSE),
    error = function(e) {
      cli::cli_alert_warning(
        "Skipping unreadable path ({nchar(dir)} chars): {.path {dir}}"
      )
      character()
    }
  )

  if (length(entries) == 0) {
    return(character())
  }

  # per entry, not vectorised: one unreadable path must not decide the
  # classification of its siblings
  is_dir <- vapply(
    as.character(entries),
    function(p) isTRUE(tryCatch(fs::is_dir(p), error = function(e) FALSE)),
    logical(1),
    USE.NAMES = FALSE
  )

  files <- entries[
    !is_dir & grepl("[.]parquet$", entries, ignore.case = TRUE)
  ]

  subs <- unlist(lapply(
    entries[is_dir],
    .dhs_walk_parquet,
    depth = depth + 1L,
    max_depth = max_depth
  ))

  c(as.character(files), as.character(subs))
}


#' Empty leaf index with the right column types
#'
#' @keywords internal
#' @noRd
.dhs_empty_index <- function() {
  tibble::tibble(
    path = character(),
    country_code = character(),
    survey_type = character(),
    survey_year = integer(),
    survey_id = character(),
    n_keys = integer()
  )
}


#' Index the parquet leaves under one `file_type=` folder
#'
#' @description
#' Walks the folder once and returns one row per parquet file with the
#' partition values parsed out of its path. Replaces path construction:
#' `dhs_read()` previously built
#' `file_type=X/country_code=Y/survey_year=Z` by string concatenation and
#' silently found nothing when the archive's layout differed (an extra
#' partition level, a differently formatted year). Indexing what is
#' actually on disk removes that whole class of failure.
#'
#' @param ft_path Path to a `file_type=` folder.
#' @param country_code Optional DHS two-letter country code. When the
#'   matching `country_code=` folder exists the scan starts there, which
#'   is both faster and isolates the query from unreadable leaves
#'   elsewhere in the archive.
#' @param refresh Logical; bypass the session cache.
#' @return A tibble with `path`, `country_code`, `survey_type`,
#'   `survey_year`, `survey_id`, `n_keys`.
#' @keywords internal
#' @noRd
.dhs_leaf_index <- function(ft_path, country_code = NULL, refresh = FALSE) {
  # Scope the scan to one country when we know it. Beyond being faster on
  # a multi-country archive, this stops an unreadable leaf in another
  # country's tree from failing an unrelated query.
  scan_root <- ft_path

  if (!is.null(country_code)) {
    cc_path <- fs::path(
      ft_path,
      paste0("country_code=", toupper(country_code))
    )
    if (fs::dir_exists(cc_path)) {
      scan_root <- cc_path
    }
  }

  key <- paste0(as.character(scan_root), "|", as.character(ft_path))

  if (!isTRUE(refresh) && !is.null(.dhs_index_cache[[key]])) {
    return(.dhs_index_cache[[key]])
  }

  files <- .dhs_walk_parquet(scan_root)

  if (length(files) == 0) {
    idx <- .dhs_empty_index()
    .dhs_index_cache[[key]] <- idx
    return(idx)
  }

  # Drop leaves the operating system cannot open. On Windows the limit is
  # 260 characters; arrow cannot read such a file either, so excluding it
  # here loses nothing that was ever reachable.
  path_max <- if (identical(.Platform$OS.type, "windows")) 259L else 4095L
  too_long <- nchar(files) > path_max

  if (any(too_long)) {
    cli::cli_alert_warning(
      "Skipping {sum(too_long)} leaf{?s} over the {path_max}-character \\
       path limit"
    )
    files <- files[!too_long]
  }

  if (length(files) == 0) {
    idx <- .dhs_empty_index()
    .dhs_index_cache[[key]] <- idx
    return(idx)
  }

  # Relative paths by string surgery, not fs::path_rel(): every fs path
  # function normalises through path_tidy(), which itself enforces the
  # 260-character limit and errors on anything longer.
  prefix <- paste0(gsub("\\\\", "/", as.character(ft_path)), "/")
  rel <- gsub("\\\\", "/", as.character(files))
  rel <- ifelse(
    startsWith(rel, prefix),
    substring(rel, nchar(prefix) + 1L),
    rel
  )

  keys <- lapply(rel, .dhs_parse_hive_keys)

  pick <- function(nm) {
    vapply(
      keys,
      function(k) {
        v <- k[[nm]]
        if (is.null(v) || length(v) == 0) {
          return(NA_character_)
        }
        v <- as.character(v[1])
        # arrow writes this literal for any NA partition value
        if (identical(v, "__HIVE_DEFAULT_PARTITION__")) NA_character_ else v
      },
      character(1)
    )
  }

  idx <- tibble::tibble(
    path = as.character(files),
    country_code = toupper(pick("country_code")),
    survey_type = toupper(pick("survey_type")),
    # a survey_year partition is sometimes written as "2019-20"; the first
    # four digits are the survey's start year
    survey_year = suppressWarnings(
      as.integer(substr(pick("survey_year"), 1, 4))
    ),
    survey_id = toupper(pick("survey_id")),
    n_keys = vapply(keys, length, integer(1))
  )

  .dhs_index_cache[[key]] <- idx
  idx
}


#' Read survey identity fields from one parquet leaf
#'
#' @description
#' Column projection only - the file is never materialised. Returns the
#' survey year as recorded in the data (`DHSYEAR` for GPS exports,
#' `hv007` / `v007` year of interview for the household and individual
#' recodes), the in-file `survey_type` when present, and the survey type
#' and year parsed from `SurveyID` (e.g. `"RW2023MIS"`), which GE and GC
#' extracts carry.
#'
#' @param leaf_path Path to a single parquet file.
#' @return A one-row tibble.
#' @keywords internal
#' @noRd
.dhs_leaf_identity <- function(leaf_path) {
  empty <- tibble::tibble(
    n_rows = NA_integer_,
    year_data = NA_integer_,
    type_column = NA_character_,
    type_surveyid = NA_character_,
    year_surveyid = NA_integer_,
    readable = FALSE
  )

  ds <- tryCatch(arrow::open_dataset(leaf_path), error = function(e) NULL)
  if (is.null(ds)) {
    return(empty)
  }

  cols <- names(ds)

  pull_col <- function(nm) {
    tryCatch(
      ds |>
        dplyr::select(dplyr::all_of(nm)) |>
        dplyr::collect() |>
        dplyr::pull(1),
      error = function(e) NULL
    )
  }

  # --- year from the data ---------------------------------------------
  year_data <- NA_integer_
  year_col <- intersect(c("DHSYEAR", "MISYEAR", "hv007", "v007"), cols)

  if (length(year_col) > 0) {
    vals <- suppressWarnings(as.integer(as.vector(pull_col(year_col[1]))))
    # two-digit years of interview (hv007 = 92) belong to the 1900s
    vals <- ifelse(!is.na(vals) & vals < 100L, vals + 1900L, vals)
    vals <- vals[!is.na(vals) & vals > 1900L & vals < 2100L]
    if (length(vals) > 0) {
      year_data <- as.integer(min(vals))
    }
  }

  # --- survey_type from the in-file column ----------------------------
  type_column <- NA_character_

  if ("survey_type" %in% cols) {
    vals <- as.character(pull_col("survey_type"))
    vals <- vals[!is.na(vals) & nzchar(vals) & toupper(vals) != "NA"]
    if (length(vals) > 0) {
      type_column <- toupper(vals[1])
    }
  }

  # --- survey_type and year from SurveyID -----------------------------
  type_surveyid <- NA_character_
  year_surveyid <- NA_integer_

  if ("SurveyID" %in% cols) {
    vals <- as.character(pull_col("SurveyID"))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) > 0) {
      m <- regmatches(
        toupper(vals[1]),
        regexec("^([A-Z]{2})([0-9]{4})([A-Z]{3})$", toupper(vals[1]))
      )[[1]]
      if (length(m) == 4L) {
        year_surveyid <- as.integer(m[3])
        type_surveyid <- m[4]
      }
    }
  }

  tibble::tibble(
    n_rows = tryCatch(as.integer(nrow(ds)), error = function(e) NA_integer_),
    year_data = year_data,
    type_column = type_column,
    type_surveyid = type_surveyid,
    year_surveyid = year_surveyid,
    readable = TRUE
  )
}


#' List the surveys present in a DHS parquet archive
#'
#' @description
#' Builds one row per survey by indexing the archive's leaves and reading
#' their identity fields, rather than by reading a whole recode into
#' memory and calling `distinct()` on it.
#'
#' Survey type is resolved in a fixed order and the source is reported, so
#' a caller can see how each label was arrived at:
#'
#' 1. the `survey_type=` partition value;
#' 2. the in-file `survey_type` column (absent whenever survey_type was
#'    used as a partition key, which is why it is second);
#' 3. the `SurveyID` field carried by GE and GC extracts.
#'
#' A survey whose type resolves to none of these is returned with
#' `survey_type = NA` and `type_source = "unresolved"`. It is never
#' silently relabelled `"DHS"`: doing so produces a survey that is
#' discovered and then loads zero rows for every other recode, because the
#' `survey_type` filter does apply to PR/HR/KR/IR.
#'
#' @param path Root parquet directory.
#' @param country_code DHS two-letter country code.
#' @param file_type Recode used for discovery. Defaults to `"GE"`, since a
#'   survey without GPS cannot be modelled.
#' @param refresh_index Logical; rescan the archive.
#'
#' @return A tibble with `DHSYEAR`, `survey_type`, `type_source`,
#'   `survey_year_path`, `survey_id`, `n_rows`, `n_leaves`, `path`.
#' @export
dhs_archive_surveys <- function(
  path,
  country_code,
  file_type = "GE",
  refresh_index = FALSE
) {
  .check_pkg(
    c("arrow", "fs"),
    reason = "to index a DHS parquet archive"
  )

  ft_path <- fs::path(path, paste0("file_type=", toupper(file_type)))

  if (!fs::dir_exists(ft_path)) {
    cli::cli_abort("Directory does not exist: {.path {ft_path}}")
  }

  idx <- .dhs_leaf_index(
    ft_path,
    country_code = country_code,
    refresh = refresh_index
  )
  idx <- idx[!is.na(idx$country_code) &
               idx$country_code %in% toupper(country_code), ]

  if (nrow(idx) == 0) {
    cli::cli_abort(
      "No {toupper(file_type)} leaves for {toupper(country_code)}"
    )
  }

  identity <- purrr::list_rbind(lapply(idx$path, .dhs_leaf_identity))

  leaves <- dplyr::bind_cols(idx, identity) |>
    dplyr::mutate(
      DHSYEAR = dplyr::coalesce(
        .data$year_data, .data$year_surveyid, .data$survey_year
      ),
      resolved_type = dplyr::coalesce(
        .data$survey_type, .data$type_column, .data$type_surveyid
      ),
      type_source = dplyr::case_when(
        !is.na(.data$survey_type) ~ "partition",
        !is.na(.data$type_column) ~ "column",
        !is.na(.data$type_surveyid) ~ "SurveyID",
        TRUE ~ "unresolved"
      )
    )

  # Resolve type at survey level, not leaf level. A survey is often held
  # twice - one copy carrying survey_type in the path, another carrying it
  # only in the file, or not at all. Grouping on type_source would report
  # those as separate surveys, so a type found on any leaf of a year is
  # first inherited by the year's other leaves. Inheriting only when the
  # year holds exactly one known type keeps a genuine DHS + MIS pair in
  # the same year distinct.
  priority <- c("partition", "column", "SurveyID", "inherited", "unresolved")

  leaves <- leaves |>
    dplyr::group_by(.data$DHSYEAR) |>
    dplyr::mutate(
      .types = list(unique(stats::na.omit(.data$resolved_type))),
      .one_type = vapply(
        .data$.types,
        function(x) if (length(x) == 1L) x[[1]] else NA_character_,
        character(1)
      ),
      type_source = dplyr::if_else(
        is.na(.data$resolved_type) & !is.na(.data$.one_type),
        "inherited",
        .data$type_source
      ),
      resolved_type = dplyr::coalesce(.data$resolved_type, .data$.one_type)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-".types", -".one_type")

  leaves |>
    dplyr::group_by(.data$DHSYEAR, .data$resolved_type) |>
    dplyr::summarise(
      type_source = priority[min(match(unique(.data$type_source), priority))],
      survey_year_path = paste(
        sort(unique(.data$survey_year)), collapse = "/"
      ),
      survey_id = paste(
        sort(unique(stats::na.omit(.data$survey_id))), collapse = "/"
      ),
      n_rows = max(.data$n_rows, na.rm = TRUE),
      n_leaves = dplyr::n(),
      path = .data$path[1],
      .groups = "drop"
    ) |>
    dplyr::rename(survey_type = "resolved_type") |>
    dplyr::filter(!is.na(.data$DHSYEAR)) |>
    dplyr::arrange(.data$DHSYEAR, .data$survey_type)
}


#' Ask the user to confirm before continuing
#'
#' @param question Text shown before the prompt.
#' @param default Value returned when the session is not interactive.
#' @return `TRUE` to continue, `FALSE` to stop.
#' @keywords internal
#' @noRd
.dhs_confirm <- function(question, default = TRUE) {
  if (!interactive()) {
    cli::cli_alert_warning(
      "Non-interactive session: continuing without confirmation."
    )
    return(default)
  }

  repeat {
    answer <- tolower(trimws(
      readline(paste0(question, " [y/n]: "))
    ))
    if (answer %in% c("y", "yes")) {
      return(TRUE)
    }
    if (answer %in% c("n", "no")) {
      return(FALSE)
    }
    cli::cli_alert_warning("Please answer {.val y} or {.val n}.")
  }
}


#' Check that a leaf holds the columns its file_type promises
#'
#' @description
#' DHS geospatial covariate extracts (`GC`) name the GPS dataset they were
#' derived from in a `GPS_Dataset` column, e.g. `RWGE8AFL`. If anything
#' derives a recode type from that string rather than from the file name,
#' the covariates land in the `GE` slot: 170 rows, one per cluster,
#' plausible in every respect except that they carry no coordinates. The
#' pipeline then discovers a survey it cannot geolocate.
#'
#' This check is cheap - a schema read, no data - and turns that failure
#' into a named error at load time.
#'
#' @param x Data frame read from a leaf, or an Arrow dataset.
#' @param file_type Declared recode type.
#' @return `TRUE` when the content matches, `FALSE` otherwise.
#' @keywords internal
#' @noRd
.dhs_content_matches <- function(x, file_type) {
  required <- switch(
    as.character(file_type),
    GE = c("DHSCLUST", "LATNUM", "LONGNUM"),
    GC = c("DHSCLUST"),
    PR = c("hv001", "hv103"),
    HR = c("hv001"),
    IR = c("v001"),
    KR = c("v001"),
    BR = c("v001"),
    MR = c("mv001"),
    NULL
  )

  if (is.null(required)) {
    return(TRUE)
  }

  all(required %in% names(x))
}


#' Defensive de-duplication on standard DHS row keys
#'
#' @param x A data frame read from one or more parquet leaves.
#' @param file_type DHS recode code.
#' @param verbose Logical; report removals?
#' @return `x` with duplicate respondent rows removed.
#' @keywords internal
#' @noRd
.dhs_dedupe_rows <- function(x, file_type, verbose = TRUE) {
  if (is.null(file_type) || length(file_type) != 1L) {
    return(x)
  }
  # Dedupe keys deliberately exclude `survey_id` because some parquet
  # partitions contain a duplicate copy of every respondent with
  # `survey_id = NA` alongside the canonical row with the populated
  # `survey_id`. Including `survey_id` in the keys would treat those as
  # distinct and the duplicates would survive. Using just the natural
  # DHS within-survey keys (combined with the country_code / survey_year
  # filter already applied upstream) collapses NA-survey_id duplicates
  # onto the canonical row.
  #
  # GE (and GC) are keyed on DHSCLUST: one row per sampled cluster. Before
  # this key existed, duplicated GPS leaves passed through untouched and
  # every cluster could enter a model twice, silently doubling the
  # effective sample size in the binomial likelihood.
  dedupe_keys <- switch(
    as.character(file_type),
    KR = c("caseid", "bidx"),
    BR = c("caseid", "bidx"),
    IR = c("caseid"),
    MR = c("mcaseid"),
    PR = c("hhid", "hvidx"),
    HR = c("hhid"),
    AR = c("hhid", "hvidx"),
    CR = c("caseid"),
    GE = c("DHSCLUST"),
    GC = c("DHSCLUST"),
    NULL
  )
  if (is.null(dedupe_keys)) {
    return(x)
  }
  if (!all(dedupe_keys %in% names(x))) {
    return(x)
  }
  # Prefer rows with non-NA `survey_id` when both versions of a duplicate
  # exist, so the canonical row survives the `distinct()` call.
  if ("survey_id" %in% names(x)) {
    x <- x[order(is.na(x[["survey_id"]])), , drop = FALSE]
  }
  n_before <- nrow(x)
  x <- dplyr::distinct(
    x,
    dplyr::across(dplyr::all_of(dedupe_keys)),
    .keep_all = TRUE
  )
  n_after <- nrow(x)
  if (isTRUE(verbose) && n_after < n_before) {
    cli::cli_alert_warning(
      paste0(
        "Removed ", format(n_before - n_after, big.mark = ","),
        " duplicate row(s) on keys ",
        paste0("'", dedupe_keys, "'", collapse = ", "),
        " (file_type = '", file_type, "')."
      )
    )
  }
  x
}


#' Read a DHS recode from a hive-partitioned parquet archive
#'
#' Reads a single DHS / MIS recode from a parquet archive that is laid out
#' using AHADI's Hive partitioning convention (see *Directory layout* below).
#' This is the function `run_mbg_pipeline()` and the AHADI example scripts use
#' internally; it is **not** a general-purpose DHS reader.
#'
#' @section When to use `dhs_read()`:
#'
#' Use `dhs_read()` only when you have a parquet archive that follows the
#' layout below. The archive is what makes multi-country, multi-year,
#' multi-recode discovery (`dhs_read(file_type = "GE", country_code = ...)`)
#' and the `run_mbg_pipeline()` workflow possible.
#'
#' If you have a **single DHS file** (`.dta`, `.csv`, `.rds`, `.sav`, ...),
#' you do **not** need `dhs_read()` or a parquet archive at all. Read the
#' file with [sntutils::read()] (or `haven::read_dta()`) and pass the
#' resulting data frame straight to any `calc_*_dhs()` function:
#'
#' ```r
#' kr <- sntutils::read("TGKR81FL.DTA")   # or .csv, .rds, .sav, ...
#' ge <- sntutils::read("TGGE8AFL.dta")
#' fever <- calc_fever_dhs(dhs_kr = kr, gps_data = ge,
#'                         shapefile = shp_admin,
#'                         admin_level = c("adm0", "adm1"))
#' ```
#'
#' Every `calc_*_dhs()` estimator accepts a plain data frame - the recode-
#' specific reader is just convenience.
#'
#' @section Directory layout (Hive partitioning):
#'
#' `dhs_read()` expects `path` to be the root of a directory tree partitioned
#' in this order, with one parquet file per survey at the leaf:
#'
#' ```
#' path/
#'   file_type=GE/
#'     country_code=TG/
#'       survey_year=2017/
#'         survey_id=TGGE8I/
#'           data.parquet
#'   file_type=KR/
#'     country_code=TG/
#'       survey_year=2017/
#'         survey_id=TGKR81/
#'           data.parquet
#'   file_type=PR/...
#' ```
#'
#' Partition keys are read literally as `file_type=...`, `country_code=...`,
#' `survey_year=...`, `survey_id=...`. Allowed `file_type` values are
#' `PR`, `HR`, `IR`, `KR`, `GE`, `BR`, `MR`, `WI`. `country_code` is the
#' DHS two-letter code (e.g. `TG`, `BU`, `KE`).
#'
#' @section Building your own parquet archive:
#'
#' To use `dhs_read()` (and therefore `run_mbg_pipeline()`) on your own data,
#' convert each raw DHS recode into a parquet file at the matching leaf path.
#' A minimal recipe per file:
#'
#' ```r
#' library(arrow); library(haven); library(fs)
#'
#' raw <- haven::read_dta("TGKR81FL.DTA")          # preserves labels
#' raw$file_type    <- "KR"
#' raw$country_code <- "TG"
#' raw$survey_year  <- 2017L
#' raw$survey_id    <- "TGKR81"
#' raw$survey_type  <- "DHS"                       # or "MIS"
#'
#' leaf <- fs::path("path/to/parquet",
#'                  "file_type=KR",
#'                  "country_code=TG",
#'                  "survey_year=2017",
#'                  "survey_id=TGKR81")
#' fs::dir_create(leaf)
#' arrow::write_parquet(raw, fs::path(leaf, "data.parquet"))
#' ```
#'
#' Repeat per recode (GE/PR/HR/KR/IR) and per survey. Keep `haven` labels on
#' the columns - `dhs_read()` and the indicator functions rely on them.
#'
#' @section Behaviour notes:
#'
#' - When `country_code` and `survey_year` (and optionally `survey_id`)
#'   identify a single survey, `dhs_read()` calls `arrow::read_parquet()`
#'   directly so haven labels and survey-specific variables are preserved.
#' - When the filter spans multiple surveys, it falls back to
#'   `arrow::open_dataset()` which standardises labels and drops variables
#'   absent in some surveys. For indicator work, prefer the single-survey path.
#' - Defensive deduplication runs on standard recode keys to guarantee one
#'   row per respondent unit (some DHS parquet files contain duplicate rows).
#'   `GE` and `GC` are keyed on `DHSCLUST`.
#' - Leaves are discovered by indexing the archive rather than by constructing
#'   a path, so partitions carrying extra or reordered `key=value` levels are
#'   read correctly, and an unreadable branch is skipped with a warning
#'   instead of failing the query.
#' - When several `survey_id` partitions match one survey, all are read and
#'   de-duplicated rather than silently taking the first.
#'
#' @param path Root parquet directory (must follow the *Directory layout*
#'   above).
#' @param survey_id Optional survey ID (e.g. `"KEKR8A"`). Matches the
#'   `survey_id=...` partition.
#' @param file_type DHS recode code: one of `"PR"`, `"HR"`, `"IR"`, `"KR"`,
#'   `"GE"`, `"GC"`, `"BR"`, `"MR"`, `"WI"`. Required.
#' @param country_code DHS two-letter country code (e.g. `"TG"`).
#' @param survey_year Survey year (e.g. `2017`).
#' @param survey_type DHS survey type (e.g. `"DHS"`, `"MIS"`). Applied first
#'   against the `survey_type=` partition value, then against the in-file
#'   column where it survives into the file. Honoured for `"GE"`.
#' @param verbose Logical; print progress messages? Default `TRUE`.
#' @param year_tolerance Integer. How far the requested `survey_year` may sit
#'   from a leaf's `survey_year=` partition and still match. Defaults to `1`:
#'   surveys spanning a calendar boundary are filed inconsistently (a 2014-15
#'   survey may sit under `survey_year=2015` while `DHSYEAR` says 2014). Exact
#'   matches always win; the tolerance applies only when nothing matches
#'   exactly, and the fallback is reported. Set `0` for strict matching.
#' @param refresh_index Logical. Rescan the archive instead of reusing the
#'   session-cached leaf index. Needed only when files are added mid-session.
#'
#' @return A tibble of filtered DHS records with `haven` labels preserved.
#'
#' @seealso [sntutils::read()] for reading a single DHS file directly,
#'   [run_mbg_pipeline()] for the multi-survey pipeline that builds on top
#'   of this archive layout.
#' @export
dhs_read <- function(
  path,
  survey_id = NULL,
  file_type = NULL,
  country_code = NULL,
  survey_year = NULL,
  survey_type = NULL,
  verbose = TRUE,
  year_tolerance = 1L,
  refresh_index = FALSE
) {
  .check_pkg(
    c("arrow", "fs", "janitor"),
    reason = "to read DHS parquet datasets in `dhs_read()`"
  )

  # -------------------------------------------
  # Validate file_type (mandatory)
  # -------------------------------------------
  if (is.null(file_type)) {
    cli::cli_abort("`file_type` must be provided (PR, IR, KR, GE etc...).")
  }

  file_type <- toupper(file_type)

  # NOTE (change C7, included here because it is one token): "GC" admits
  # the DHS geospatial covariate extracts. They are readable and joinable
  # on DHSCLUST, and are candidate MBG predictors. They carry no
  # coordinates and are never a substitute for GE.
  allowed <- c("PR", "HR", "IR", "KR", "GE", "GC", "BR", "MR", "WI")
  if (!file_type %in% allowed) {
    cli::cli_abort(
      "Invalid `file_type` '{file_type}'. Must be one of: {allowed}."
    )
  }

  ft_path <- fs::path(path, paste0("file_type=", file_type))

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

  if (verbose) {
    cli::cli_h1("Loading DHS parquet dataset")
    cli::cli_inform("File type: {file_type}")
    cli::cli_inform("Path: {short_path}")
  }

  # -------------------------------------------
  # Resolve leaves from the index
  # -------------------------------------------
  idx <- .dhs_leaf_index(
    ft_path,
    country_code = country_code,
    refresh = refresh_index
  )

  if (nrow(idx) == 0) {
    cli::cli_alert_warning("No parquet files under {short_path}")
    return(tibble::tibble())
  }

  hits <- idx

  if (!is.null(country_code)) {
    wanted <- toupper(as.character(country_code))
    hits <- hits[!is.na(hits$country_code) & hits$country_code %in% wanted, ]
  }

  if (!is.null(survey_id)) {
    wanted <- toupper(as.character(survey_id))
    hits <- hits[!is.na(hits$survey_id) & hits$survey_id %in% wanted, ]
  }

  # survey_type at leaf level: keep leaves whose partition says the right
  # thing, and leaves that say nothing (their in-file column is filtered
  # after the read). This is what lets GE be separated by survey type.
  if (!is.null(survey_type)) {
    wanted <- toupper(as.character(survey_type))
    hits <- hits[is.na(hits$survey_type) | hits$survey_type %in% wanted, ]
  }

  exact_year <- TRUE

  if (!is.null(survey_year)) {
    wanted <- as.integer(survey_year)
    gap <- vapply(
      hits$survey_year,
      function(y) {
        if (is.na(y)) return(NA_integer_)
        as.integer(min(abs(y - wanted)))
      },
      integer(1)
    )
    keep <- !is.na(gap) & gap <= as.integer(year_tolerance)
    hits <- hits[keep, ]
    gap <- gap[keep]

    if (nrow(hits) > 0 && any(gap == 0L)) {
      hits <- hits[gap == 0L, ]
    } else if (nrow(hits) > 0) {
      exact_year <- FALSE
      hits <- hits[gap == min(gap), ]
    }
  }

  if (nrow(hits) == 0) {
    if (verbose) {
      cli::cli_alert_warning(
        "No {file_type} leaf matched the requested filters"
      )
    }
    return(tibble::tibble())
  }

  if (isFALSE(exact_year) && verbose) {
    cli::cli_alert_info(
      paste0(
        "Requested survey_year {survey_year}; using leaves filed as ",
        "{paste(sort(unique(hits$survey_year)), collapse = ', ')}"
      )
    )
  }

  # -------------------------------------------
  # Read
  # -------------------------------------------
  # A direct read per leaf preserves haven labels and survey-specific
  # columns. open_dataset() is used only when the request genuinely spans
  # several surveys, where a unified schema is unavoidable.
  surveys_matched <- unique(
    paste(hits$country_code, hits$survey_year, hits$survey_id, sep = "|")
  )

  use_direct <- length(surveys_matched) <= 1L

  if (use_direct) {
    if (verbose) {
      cli::cli_alert_info(
        "Direct parquet read ({nrow(hits)} file{?s}), labels preserved"
      )
    }
    parts <- lapply(hits$path, function(f) {
      tryCatch(
        arrow::read_parquet(f),
        error = function(e) {
          cli::cli_alert_warning(
            "Unreadable leaf {.path {f}}: {conditionMessage(e)}"
          )
          NULL
        }
      )
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]

    if (length(parts) == 0) {
      return(tibble::tibble())
    }

    out <- if (length(parts) == 1L) {
      parts[[1]]
    } else {
      dplyr::bind_rows(parts)
    }
  } else {
    if (verbose) {
      cli::cli_inform(
        "Request spans {length(surveys_matched)} surveys; opening dataset"
      )
    }
    suppressWarnings(
      out <- arrow::open_dataset(unique(hits$path)) |>
        dplyr::collect()
    )
  }

  # -------------------------------------------
  # Post-read filters
  # -------------------------------------------
  # The in-file survey_type column is authoritative when present. Note it
  # is absent whenever survey_type was used as a partition key, which is
  # why the leaf-level filter above runs first.
  if (!is.null(survey_type) && "survey_type" %in% names(out)) {
    wanted <- toupper(as.character(survey_type))
    known <- !is.na(out$survey_type) & toupper(out$survey_type) != ""
    out <- out[!known | toupper(out$survey_type) %in% wanted, , drop = FALSE]
  }

  suppressWarnings(
    out <- janitor::remove_empty(out, which = "rows")
  )

  out <- .dhs_dedupe_rows(out, file_type, verbose = verbose)

  n <- nrow(out)
  if (verbose) {
    cli::cli_inform("Rows loaded: {format(n, big.mark = ',')}")
    if (n == 0) {
      cli::cli_alert_warning("Filter returned zero rows")
    } else {
      cli::cli_alert_success("Data loaded successfully")
    }
  }

  out
}
