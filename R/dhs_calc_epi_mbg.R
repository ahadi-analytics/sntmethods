#' Prepare EPI (Vaccination) Data for MBG Analysis
#'
#' Prepares cluster-level vaccination coverage data for MBG analysis.
#' Calculates coverage for standard EPI vaccines plus malaria vaccine.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/epi_dhs.yml}
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of vaccines to calculate:
#'   \itemize{
#'     \item "bcg": BCG vaccine
#'     \item "dpt1", "dpt2", "dpt3": DPT doses 1-3 (falls back to pentavalent)
#'     \item "polio0": OPV birth dose
#'     \item "polio1", "polio2", "polio3": Polio doses 1-3
#'     \item "measles1", "measles2": Measles doses 1-2
#'     \item "vita1", "vita2": Vitamin A doses 1-2
#'     \item "malaria": Malaria vaccine (RTS,S/R21)
#'     \item "penta1", "penta2", "penta3": Pentavalent doses 1-3
#'     \item "pneumo1", "pneumo2", "pneumo3": Pneumococcal doses 1-3
#'     \item "rota1", "rota2", "rota3": Rotavirus doses 1-3
#'     \item "ipv": Inactivated Polio Vaccine
#'     \item "hepb0": Hepatitis B birth dose
#'     \item "yellowfever": Yellow Fever vaccine
#'     \item "any": Any vaccination (h10 >= 1)
#'     \item "never_vaccinated": Zero-dose (h10 == 0)
#'     \item "fully_vaccinated": Basic fully vaccinated
#'   }
#'   Default: c("bcg", "dpt3", "measles1").
#' @param age_min_months Minimum age in months (default: 12).
#' @param age_max_months Maximum age in months (default: 23).
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per vaccine).
#'
#' @details
#' Standard EPI target age is 12-23 months (children who have had time to
#' complete basic vaccination schedule). Malaria vaccine (RTS,S) is measured
#' by h68 variable.
#'
#' @examples
#' \dontrun{
#' epi_mbg <- calc_epi_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("bcg", "dpt3", "measles1")
#' )
#' }
#'
#' @export
calc_epi_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = c("bcg", "dpt3", "measles1"),
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    bcg = "h2",
    polio0 = "h0",
    dpt1 = "h3", dpt2 = "h4", dpt3 = "h5",
    polio1 = "h6", polio2 = "h7", polio3 = "h8",
    measles1 = "h9", measles2 = "h9a",
    vita1 = "h33", vita2 = "h33a",
    malaria = "h68",
    penta1 = "h51", penta2 = "h52", penta3 = "h53",
    pneumo1 = "h54", pneumo2 = "h55", pneumo3 = "h56",
    rota1 = "h57", rota2 = "h58", rota3 = "h59",
    ipv = "h60", hepb0 = "h50", yellowfever = "h61",
    any = "h10"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  # ---- Input validation ----

  valid_indicators <- c(
    "bcg", "dpt1", "dpt2", "dpt3",
    "polio0", "polio1", "polio2", "polio3",
    "measles1", "measles2",
    "vita1", "vita2",
    "malaria",
    "penta1", "penta2", "penta3",
    "pneumo1", "pneumo2", "pneumo3",
    "rota1", "rota2", "rota3",
    "ipv", "hepb0", "yellowfever",
    "any", "never_vaccinated",
    "fully_vaccinated"
  )
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # ---- Prepare GPS data ----

  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare KR data ----

  kr <- .prepare_epi_data(
    dhs_kr, survey_vars,
    age_min_months = age_min_months,
    age_max_months = age_max_months,
    include_survey_vars = FALSE
  )

  # ---- Calculate vaccine indicators ----

  results <- list()

  for (ind in indicators) {
    if (ind == "fully_vaccinated") {
      # Check that vax_fully_vaccinated was created by the helper
      if (!"vax_fully_vaccinated" %in% names(kr)) {
        cli::cli_alert_warning(
          "Cannot calculate fully_vaccinated - missing required vaccine variables"
        )
        next
      }

      result <- .aggregate_to_mbg_clusters(
        kr, "vax_fully_vaccinated", gps_clean, "epi_fully_vaccinated"
      )
      if (!is.null(result)) results[["epi_fully_vaccinated"]] <- result

    } else {
      # Single vaccine indicator
      vax_col <- paste0("vax_", ind)

      if (!vax_col %in% names(kr)) {
        cli::cli_alert_warning(
          "Vaccine column {.var {vax_col}} for {ind} not available in prepared data"
        )
        next
      }

      result_name <- paste0("epi_", ind)
      result <- .aggregate_to_mbg_clusters(kr, vax_col, gps_clean, result_name)
      if (!is.null(result)) results[[result_name]] <- result
    }
  }

  if (length(results) == 0) return(NULL)

  results
}


#' Prepare Single EPI Indicator for MBG
#'
#' @inheritParams calc_epi_mbg
#' @param vaccine Single vaccine name. Default: "measles1".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_epi_mbg <- function(
  dhs_kr,
  gps_data,
  vaccine = "measles1",
  age_min_months = 12,
  age_max_months = 23,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    bcg = "h2",
    polio0 = "h0",
    dpt1 = "h3", dpt2 = "h4", dpt3 = "h5",
    polio1 = "h6", polio2 = "h7", polio3 = "h8",
    measles1 = "h9", measles2 = "h9a",
    vita1 = "h33", vita2 = "h33a",
    malaria = "h68",
    penta1 = "h51", penta2 = "h52", penta3 = "h53",
    pneumo1 = "h54", pneumo2 = "h55", pneumo3 = "h56",
    rota1 = "h57", rota2 = "h58", rota3 = "h59",
    ipv = "h60", hepb0 = "h50", yellowfever = "h61",
    any = "h10"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_epi_mbg(
    dhs_kr = dhs_kr,
    gps_data = gps_data,
    indicators = vaccine,
    age_min_months = age_min_months,
    age_max_months = age_max_months,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result[[1]]
}
