#' Prepare U5MR Data for MBG Analysis
#'
#' Prepares cluster-level Under-5 Mortality Rate (U5MR) data for MBG analysis
#' using `DHS.rates::chmort()` for the mortality calculation. This follows the
#' standard DHS synthetic-cohort life-table methodology with 8 age segments.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/u5mr_dhs.yml}
#'
#' `DHS.rates::chmort()` computes childhood mortality rates (NNMR, PNNMR, IMR,
#' CMR, U5MR) using the standard DHS synthetic-cohort approach. When called with
#' `Class = "v001"` (cluster ID), it produces per-cluster U5MR estimates. The
#' function uses 8 age segments (0-1, 1-3, 3-6, 6-12, 12-24, 24-36, 36-48,
#' 48-60 months) and applies partial-exposure weighting at period boundaries.
#'
#' Because `chmort()` internally computes a design effect (DEFT) via
#' [survey::svydesign()], and per-cluster subsets contain only a single PSU,
#' this function creates synthetic PSU and strata columns with two pseudo-PSUs
#' per cluster. Uniform weights are applied so that the cluster-level rates are
#' unweighted -- appropriate for MBG, which handles spatial smoothing and
#' uncertainty internally.
#'
#' **Important:** The `indicator` and `samplesize` columns are used by MBG to
#' model the death proportion (indicator/samplesize). The MBG pipeline
#' automatically converts model outputs to "per 1,000" units to match
#' epidemiological standards and the scale of the `u5mr` column.
#'
#' @param dhs_br DHS Birth Recode (BR) dataset. Must contain the standard DHS
#'   variables needed by `DHS.rates::chmort()`: cluster ID (v001), interview
#'   date in CMC (v008), child's date of birth in CMC (b3), and child's age at
#'   death in months (b7).
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param period_years Number of years to look back for the mortality reference
#'   period. Default: 5 (standard DHS 5-year window). Passed to
#'   `DHS.rates::chmort()` as `Period = period_years * 12`.
#' @param survey_vars Named list mapping DHS variable names. Keys:
#'   \itemize{
#'     \item cluster: Cluster ID variable (default: "v001")
#'     \item interview_date: Date of interview in CMC (default: "v008")
#'     \item birth_date: Child's date of birth in CMC (default: "b3")
#'     \item age_at_death: Child's age at death in months (default: "b7")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list with one data.table named "u5mr" containing:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Estimated number of deaths (derived from U5MR and exposure)
#'     \item samplesize: Number of births exposed in the 0-60 month window
#'     \item x: Longitude
#'     \item y: Latitude
#'     \item u5mr: U5MR per 1,000 live births
#'   }
#'   Returns NULL if required variables are missing or data is insufficient.
#'
#' @examples
#' \dontrun{
#' u5mr_mbg <- calc_u5mr_mbg(
#'   dhs_br = br_data,
#'   gps_data = gps_data
#' )
#' # Access combined U5MR
#' u5mr_mbg$u5mr
#' }
#'
#' @seealso [calc_u5mr_dhs()] for survey-weighted estimates
#' @export
calc_u5mr_mbg <- function(
  dhs_br,
  gps_data,
  period_years = 5,
  survey_vars = list(
    cluster        = "v001",
    interview_date = "v008",
    birth_date     = "b3",
    age_at_death   = "b7"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat     = "LATNUM",
    lon     = "LONGNUM"
  )
) {
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("data.table"),
    reason = "for `calc_u5mr_mbg()`"
  )

  # ---- Check DHS.rates availability ----

  if (!requireNamespace("DHS.rates", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg DHS.rates} is required but not installed.",
        "i" = "Install it with: {.code install.packages('DHS.rates')}"
      )
    )
  }

  # ---- Input validation ----

  if (!is.data.frame(dhs_br)) {
    cli::cli_abort("{.arg dhs_br} must be a data.frame or tibble.")
  }

  if (nrow(dhs_br) == 0) {
    cli::cli_abort("{.arg dhs_br} is empty.")
  }

  if (!is.data.frame(gps_data)) {
    cli::cli_abort("{.arg gps_data} must be a data.frame or tibble.")
  }

  # Check required BR columns
  required_vars <- unlist(survey_vars)
  missing_vars <- setdiff(required_vars, names(dhs_br))

  if (length(missing_vars) > 0) {
    cli::cli_warn(
      "U5MR column(s) not found in BR data: {.var {missing_vars}}; U5MR not available for this survey"
    )
    return(NULL)
  }

  # Guard: if age_at_death is entirely NA, U5MR cannot be estimated
  age_death_var <- survey_vars$age_at_death
  if (all(is.na(dhs_br[[age_death_var]]))) {
    cli::cli_warn(
      "{.var {age_death_var}} is entirely NA; U5MR cannot be estimated - skipping"
    )
    return(NULL)
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare BR data for chmort() ----

  cluster_var <- survey_vars$cluster
  br_prepped <- .prepare_br_for_chmort(dhs_br, cluster_var)

  cli::cli_alert_info(
    "BR data: {format(nrow(br_prepped), big.mark = ',')} birth records ",
    "across {length(unique(br_prepped[[cluster_var]]))} clusters"
  )

  # ---- Call DHS.rates::chmort() per cluster ----

  cli::cli_alert_info(
    "Calculating cluster-level U5MR using {.fn DHS.rates::chmort} ",
    "(Period = {period_years * 12} months)"
  )

  # chmort() uses cat() for progress messages; suppress with capture.output()
  mort_results <- tryCatch({
      invisible(utils::capture.output({
        mort_raw <- DHS.rates::chmort(
          Data.Name         = br_prepped,
          Strata            = "v022",
          Cluster           = "v021",
          Weight            = "v005",
          Date_of_interview = survey_vars$interview_date,
          Date_of_birth     = survey_vars$birth_date,
          Age_at_death      = survey_vars$age_at_death,
          Period            = period_years * 12,
          Class             = cluster_var
        )
      }))
      mort_raw
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Error in {.fn DHS.rates::chmort}",
          "x" = e$message,
          "i" = "Check that BR data has valid variables: {.var {unlist(survey_vars)}}"
        )
      )
    }
  )

  # ---- Extract U5MR from chmort output ----

  # chmort() with Class returns a data.frame with columns: Class, R, N, WN
  # Row names follow the pattern: NNMR, PNNMR, IMR, CMR, U5MR for the first

  # class, then NNMR1, PNNMR1, ..., U5MR1 for the second, etc.

  mort_df <- as.data.frame(mort_results)

  if (!"Class" %in% names(mort_df) || !"R" %in% names(mort_df)) {
    cli::cli_abort(
      c(
        "Unexpected output format from {.fn DHS.rates::chmort}.",
        "i" = "Expected columns {.val Class}, {.val R}, {.val N} in output."
      )
    )
  }

  u5mr_rows <- mort_df[grepl("^U5MR", rownames(mort_df)), , drop = FALSE]

  if (nrow(u5mr_rows) == 0) {
    cli::cli_warn("No U5MR rows found in {.fn chmort} output; skipping")
    return(NULL)
  }

  # Build cluster-level results
  # R  = U5MR per 1,000 live births
  # N  = exposure (number of births in the 0-60 month window)
  # WN = weighted exposure (equal to N here since weights are uniform)
  u5mr_data <- data.frame(
    cluster_id = as.integer(as.character(u5mr_rows$Class)),
    u5mr       = as.numeric(u5mr_rows$R),
    samplesize = as.integer(u5mr_rows$N),
    stringsAsFactors = FALSE
  )

  # Drop clusters with NaN/NA U5MR (too few births for a valid estimate)
  na_mask <- is.na(u5mr_data$u5mr) | is.nan(u5mr_data$u5mr)
  if (any(na_mask)) {
    n_na <- sum(na_mask)
    cli::cli_alert_warning(
      "Dropping {n_na} cluster(s) with undefined U5MR (insufficient births)"
    )
    u5mr_data <- u5mr_data[!na_mask, ]
  }

  if (nrow(u5mr_data) == 0) {
    cli::cli_warn("No clusters with valid U5MR estimates; skipping")
    return(NULL)
  }

  # Derive approximate death count from rate and exposure
  # U5MR = deaths/exposed * 1000, so deaths ~ round(U5MR/1000 * exposed)
  u5mr_data$indicator <- as.integer(round(
    u5mr_data$u5mr / 1000 * u5mr_data$samplesize
  ))

  # ---- Join GPS coordinates ----

  u5mr_data <- dplyr::inner_join(u5mr_data, gps_clean, by = "cluster_id")

  if (nrow(u5mr_data) == 0) {
    cli::cli_warn(
      "No clusters matched between mortality results and GPS data; skipping"
    )
    return(NULL)
  }

  # ---- Format final output ----

  u5mr_output <- u5mr_data |>
    dplyr::transmute(
      cluster_id,
      indicator,
      samplesize,
      x,
      y,
      u5mr
    )

  n_clusters   <- nrow(u5mr_output)
  total_deaths <- sum(u5mr_output$indicator, na.rm = TRUE)
  total_exposed <- sum(u5mr_output$samplesize, na.rm = TRUE)
  mean_u5mr    <- round(mean(u5mr_output$u5mr, na.rm = TRUE), 1)

  cli::cli_alert_success(
    "U5MR: {n_clusters} clusters, {total_deaths} total deaths, ",
    "mean U5MR = {mean_u5mr} per 1,000 live births"
  )

  list(
    u5mr = data.table::as.data.table(u5mr_output)
  )
}


#' Prepare BR Data for Cluster-Level chmort()
#'
#' Creates synthetic survey-design columns required by `DHS.rates::chmort()`
#' when computing per-cluster mortality rates. Since `chmort()` internally
#' calls `DEFT()` which requires at least 2 PSUs per stratum -- even when
#' DEFT is not used in the output -- this function creates 2 pseudo-PSUs
#' per cluster and assigns uniform weights.
#'
#' @param dhs_br DHS Birth Recode dataset.
#' @param cluster_var Name of the cluster ID variable (default: "v001").
#'
#' @return A data.frame with synthetic v005, v021, v022 columns suitable
#'   for `chmort(Class = cluster_var)`.
#'
#' @noRd
.prepare_br_for_chmort <- function(dhs_br, cluster_var = "v001") {
  br <- as.data.frame(dhs_br)

  # Strip haven labels to avoid issues with DHS.rates internals
  for (col in names(br)) {
    if (inherits(br[[col]], "haven_labelled")) {
      br[[col]] <- as.vector(br[[col]])
    }
  }

  # Sort by cluster for deterministic PSU assignment
  br <- br[order(br[[cluster_var]]), ]

  # Create 2 pseudo-PSUs per cluster by alternating rows.
  # This satisfies svydesign() which requires >= 2 PSUs per stratum.
  br$v021 <- paste0(
    br[[cluster_var]], "_",
    stats::ave(
      rep(1L, nrow(br)),
      br[[cluster_var]],
      FUN = function(x) ((seq_along(x) - 1L) %% 2L) + 1L
    )
  )

  # Each cluster is its own stratum (since we want per-cluster rates)
  br$v022 <- br[[cluster_var]]

  # Uniform weights: MBG uses unweighted cluster-level rates.
  # DHS.rates divides v005 by 1e6, so v005 = 1e6 gives weight = 1.
  br$v005 <- 1e6

  # Drop clusters with only 1 birth (cannot form 2 pseudo-PSUs)
  cluster_counts <- table(br[[cluster_var]])
  singleton_clusters <- names(cluster_counts)[cluster_counts < 2]

  if (length(singleton_clusters) > 0) {
    cli::cli_alert_warning(
      "Dropping {length(singleton_clusters)} cluster(s) with < 2 births ",
      "(cannot compute mortality rate)"
    )
    br <- br[!br[[cluster_var]] %in% as.integer(singleton_clusters), ]
  }

  br
}
