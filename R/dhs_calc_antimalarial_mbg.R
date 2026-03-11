#' Prepare Antimalarial Treatment Data for MBG Analysis
#'
#' Prepares cluster-level antimalarial treatment data for MBG analysis.
#' Uses a dictionary-driven approach matching the indicator codes from
#' \code{\link{calc_antimalarial_dhs}}.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/antimalarial_dhs.yml}
#'
#' All dictionary-based indicators share the same data preparation pipeline:
#' \enumerate{
#'   \item Filter to febrile U5 children (via \code{.prepare_antimalarial_data()})
#'   \item Classify care-seeking sectors if needed (via
#'     \code{.classify_csb_from_h32()})
#'   \item Apply per-indicator filters and aggregate to cluster-level counts
#' }
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate.
#'   See \code{.antimalarial_mbg_dictionary()} for the full list of
#'   standardized indicator codes. Default: \code{"antimalarial"}.
#' @param survey_vars Named list mapping DHS variable names:
#'   \itemize{
#'     \item \code{cluster}: Cluster ID (default: "v001")
#'     \item \code{age}: Child's age in months (default: "hw1")
#'     \item \code{fever}: Fever in last 2 weeks (default: "h22")
#'   }
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A named list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count (children receiving antimalarial)
#'     \item samplesize: Denominator count (febrile U5 children)
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @examples
#' \dontrun{
#' am_mbg <- calc_antimalarial_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("antimalarial", "antimalarial_public")
#' )
#' }
#'
#' @seealso [calc_antimalarial_dhs()] for survey-weighted estimates,
#'   [calc_act_mbg()] for ACT-specific treatment
#' @export
calc_antimalarial_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = "antimalarial",
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  # Validate indicators against dictionary
  dict <- .antimalarial_mbg_dictionary()
  dict_names <- vapply(dict, `[[`, character(1), "name")
  invalid <- setdiff(indicators, dict_names)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare base data using shared helpers ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  am_data <- tryCatch(
    .prepare_antimalarial_data(
      dhs_kr = dhs_kr,
      survey_vars = survey_vars,
      include_survey_vars = FALSE
    ),
    error = function(e) {
      cli::cli_alert_warning(conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(am_data)) return(list())

  if (all(is.na(am_data$has_antimalarial))) {
    cli::cli_alert_warning("All antimalarial variables are NA")
    return(list())
  }

  # ---- Determine which dictionary entries are requested ----

  dict_specs <- dict[vapply(dict, function(d) d$name %in% indicators, logical(1))]

  # ---- Conditional CSB enrichment (only when needed) ----

  needs_csb <- any(vapply(
    dict_specs,
    function(s) !is.null(s$csb_filter),
    logical(1)
  ))

  if (needs_csb) {
    am_data <- tryCatch(
      .classify_csb_from_h32(am_data),
      error = function(e) {
        cli::cli_alert_warning(
          "CSB classification failed: {conditionMessage(e)}"
        )
        NULL
      }
    )
    if (is.null(am_data)) {
      # Fall back: re-prepare without CSB, skip CSB-dependent indicators
      am_data <- tryCatch(
        .prepare_antimalarial_data(
          dhs_kr = dhs_kr,
          survey_vars = survey_vars,
          include_survey_vars = FALSE
        ),
        error = function(e) {
          cli::cli_alert_warning(conditionMessage(e))
          return(NULL)
        }
      )
      if (is.null(am_data)) return(list())
      needs_csb <- FALSE
    }
  }

  # ---- Dictionary-driven indicator loop ----

  results <- list()

  for (spec in dict_specs) {
    # Skip CSB-filtered indicators if CSB enrichment failed
    if (!is.null(spec$csb_filter) && !needs_csb) {
      cli::cli_alert_warning(
        "Skipping {.val {spec$name}}: CSB classification not available"
      )
      next
    }

    filtered <- am_data

    # Apply CSB filter if specified
    if (!is.null(spec$csb_filter)) {
      col <- spec$csb_filter
      if (!col %in% names(filtered)) next
      filtered <- filtered[
        !is.na(filtered[[col]]) & filtered[[col]] == 1, ,
        drop = FALSE
      ]
    }

    # Filter to non-NA outcome
    filtered <- filtered[!is.na(filtered[[spec$outcome]]), , drop = FALSE]
    if (nrow(filtered) == 0) {
      cli::cli_alert_warning(
        "No data for {.val {spec$name}} — skipping"
      )
      next
    }

    # Build binary outcome
    filtered$.binary <- as.integer(filtered[[spec$outcome]] == 1)

    dt <- .aggregate_to_mbg_clusters(
      individual_data = filtered,
      indicator_col = ".binary",
      gps_clean = gps_clean,
      result_name = spec$name
    )

    if (!is.null(dt)) {
      results[[spec$name]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_alert_warning("No valid antimalarial MBG data could be prepared")
  }

  results
}


#' Antimalarial MBG Indicator Dictionary
#'
#' Returns the full set of standardized indicator specifications for
#' cluster-level antimalarial MBG output. Each entry defines the outcome
#' variable and an optional CSB filter column.
#'
#' @return List of named lists with fields:
#'   \code{name}, \code{outcome}, \code{csb_filter}.
#' @noRd
.antimalarial_mbg_dictionary <- function() {
  list(
    list(
      name = "antimalarial",
      outcome = "has_antimalarial",
      csb_filter = NULL
    ),
    list(
      name = "antimalarial_public",
      outcome = "has_antimalarial",
      csb_filter = "csb_public"
    )
  )
}


#' Prepare Single Antimalarial Indicator for MBG
#'
#' Convenience wrapper around [calc_antimalarial_mbg()] to prepare antimalarial
#' treatment data for MBG analysis.
#'
#' @inheritParams calc_antimalarial_mbg
#' @param indicator Single indicator name. Default: "antimalarial".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_antimalarial_mbg <- function(
  dhs_kr,
  gps_data,
  indicator = "antimalarial",
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_antimalarial_mbg(
    dhs_kr = dhs_kr,
    gps_data = gps_data,
    indicators = indicator,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  if (length(result) == 0) {
    cli::cli_abort("No data returned for indicator {.val {indicator}}")
  }

  result[[1]]
}
