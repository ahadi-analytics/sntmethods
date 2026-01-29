#' Prepare IPTp Data for MBG Analysis
#'
#' Prepares cluster-level Intermittent Preventive Treatment in pregnancy (IPTp)
#' data for MBG analysis. Calculates both cumulative (1+, 2+, 3+) and
#' exclusive (exactly 1, exactly 2, exactly 3) dose categories.
#'
#' @param dhs_ir DHS Individual Recode dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item Cumulative:
#'     \itemize{
#'       \item "1plus": At least 1 dose
#'       \item "2plus": At least 2 doses
#'       \item "3plus": At least 3 doses (WHO recommendation)
#'     }
#'     \item Exclusive:
#'     \itemize{
#'       \item "1only": Exactly 1 dose
#'       \item "2only": Exactly 2 doses
#'       \item "3only": Exactly 3 doses
#'     }
#'   }
#'   Default: c("1plus", "2plus", "3plus").
#' @param birth_window_months Months to look back for births. Default: 36.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per indicator).
#'
#' @details
#' IPTp coverage is measured using m49a (SP/Fansidar during pregnancy).
#' The denominator is women with a live birth in the specified window
#' who attended ANC.
#'
#' @examples
#' \dontrun{
#' iptp_mbg <- calc_iptp_mbg(
#'   dhs_ir = ir_data,
#'   gps_data = gps_data,
#'   indicators = c("2plus", "3plus")
#' )
#' }
#'
#' @seealso [calc_iptp_dhs()] for survey-weighted estimates
#' @export
calc_iptp_mbg <- function(
  dhs_ir,
  gps_data,
  indicators = c("1plus", "2plus", "3plus"),
  birth_window_months = 36,
  survey_vars = list(
    cluster = "v001",
    interview_date = "v008",
    birth_date = "b3_01",
    sp_doses = "m49a_1"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  if (!is.data.frame(dhs_ir)) {
    cli::cli_abort("`dhs_ir` must be a data.frame or tibble")
  }

  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  valid_indicators <- c("1plus", "2plus", "3plus", "1only", "2only", "3only")
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # Check SP variable
  if (!survey_vars$sp_doses %in% names(dhs_ir)) {
    cli::cli_abort(
      c(
        "SP doses variable {.var {survey_vars$sp_doses}} not found",
        "i" = "IPTp data may not be available in this survey"
      )
    )
  }

  # ---- Prepare GPS data ----

  gps_clean <- gps_data |>
    dplyr::transmute(
      cluster_id = .data[[gps_vars$cluster]],
      x = as.numeric(.data[[gps_vars$lon]]),
      y = as.numeric(.data[[gps_vars$lat]])
    ) |>
    dplyr::filter(!is.na(x), !is.na(y), x != 0, y != 0) |>
    dplyr::distinct()

  cli::cli_alert_info(
    "GPS data: {nrow(gps_clean)} clusters with valid coordinates"
  )

  # ---- Prepare IR data ----

  ir <- dhs_ir |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      interview_cmc = .data[[survey_vars$interview_date]],
      birth_cmc = .data[[survey_vars$birth_date]],
      sp_doses = .data[[survey_vars$sp_doses]]
    ) |>
    dplyr::filter(
      !is.na(birth_cmc),
      !is.na(interview_cmc)
    ) |>
    dplyr::mutate(
      months_since_birth = interview_cmc - birth_cmc
    ) |>
    dplyr::filter(
      months_since_birth >= 0,
      months_since_birth <= birth_window_months
    )

  # Filter valid SP responses
  # In DHS: 0=None, 1-7=number of doses, 90+=don't know/other
  ir <- ir |>
    dplyr::filter(
      !is.na(sp_doses),
      sp_doses <= 7  # Valid dose counts
    )

  if (nrow(ir) == 0) {
    cli::cli_abort("No eligible women with valid IPTp data found")
  }

  cli::cli_alert_info(
    "IR data: {format(nrow(ir), big.mark = ',')} women with births in last ",
    "{birth_window_months} months"
  )

  # ---- Calculate IPTp indicators ----

  ir <- ir |>
    dplyr::mutate(
      # Cumulative
      has_1plus = as.integer(sp_doses >= 1),
      has_2plus = as.integer(sp_doses >= 2),
      has_3plus = as.integer(sp_doses >= 3),

      # Exclusive
      has_1only = as.integer(sp_doses == 1),
      has_2only = as.integer(sp_doses == 2),
      has_3only = as.integer(sp_doses == 3)
    )

  # ---- Aggregate to cluster level ----

  results <- list()

  indicator_map <- list(
    `1plus` = "has_1plus",
    `2plus` = "has_2plus",
    `3plus` = "has_3plus",
    `1only` = "has_1only",
    `2only` = "has_2only",
    `3only` = "has_3only"
  )

  result_names <- list(
    `1plus` = "iptp_1plus",
    `2plus` = "iptp_2plus",
    `3plus` = "iptp_3plus",
    `1only` = "iptp_1only",
    `2only` = "iptp_2only",
    `3only` = "iptp_3only"
  )

  for (ind in indicators) {
    var_name <- indicator_map[[ind]]
    result_name <- result_names[[ind]]

    cluster_data <- ir |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(.data[[var_name]], na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[[result_name]] <- data.table::as.data.table(cluster_data)

    cli::cli_alert_success(
      "{result_name}: {nrow(cluster_data)} clusters"
    )
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  results
}


#' Prepare Single IPTp Indicator for MBG
#'
#' @inheritParams calc_iptp_mbg
#' @param doses Minimum doses for cumulative indicator (1, 2, or 3).
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_iptp_mbg <- function(
  dhs_ir,
  gps_data,
  doses = 3,
  birth_window_months = 36,
  survey_vars = list(
    cluster = "v001",
    interview_date = "v008",
    birth_date = "b3_01",
    sp_doses = "m49a_1"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  indicator_name <- paste0(doses, "plus")

  result <- calc_iptp_mbg(
    dhs_ir = dhs_ir,
    gps_data = gps_data,
    indicators = indicator_name,
    birth_window_months = birth_window_months,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result[[1]]
}
