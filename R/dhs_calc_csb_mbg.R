#' Prepare Care-Seeking Behavior Data for MBG Analysis
#'
#' Prepares cluster-level care-seeking behavior data for MBG analysis.
#' Calculates proportions of febrile children who sought care at various
#' source types.
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset.
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param indicators Character vector of indicators to calculate:
#'   \itemize{
#'     \item "any": Sought care anywhere
#'     \item "public": Sought care at public facility
#'     \item "private": Sought care at private facility
#'     \item "trained": Sought care from trained provider
#'     \item "none": Did not seek care
#'     \item "act": Received ACT treatment (among fever cases)
#'     \item "act_tested": Received ACT among those tested positive
#'   }
#'   Default: c("any", "public", "private", "none").
#' @param csb_classification Data frame with h32 variable to category mapping.
#'   Must have columns `variable` and `csb`. If NULL, uses default WMR classification.
#' @param survey_vars Named list mapping DHS variable names.
#' @param gps_vars Named list for GPS variable mapping.
#'
#' @return A list of data.tables (one per indicator), each with columns:
#'   \itemize{
#'     \item cluster_id: Cluster identifier
#'     \item indicator: Numerator count
#'     \item samplesize: Denominator count
#'     \item x: Longitude
#'     \item y: Latitude
#'   }
#'
#' @details
#' This function uses KR data on children under 5 who had fever in the last
#' 2 weeks. Care-seeking is determined using h32 variables.
#'
#' Note: Care-seeking indicators (except "none") are NOT mutually exclusive.
#' A child can appear in both "public" and "private" if they visited both.
#'
#' @examples
#' \dontrun{
#' csb_mbg <- calc_csb_mbg(
#'   dhs_kr = kr_data,
#'   gps_data = gps_data,
#'   indicators = c("public", "none")
#' )
#' }
#'
#' @seealso [calc_csb_dhs()] for survey-weighted estimates
#' @export
calc_csb_mbg <- function(
  dhs_kr,
  gps_data,
  indicators = c("any", "public", "private", "none"),
  csb_classification = NULL,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22",
    act = "ml13e"
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

  valid_indicators <- c("any", "public", "private", "trained", "none", "act", "act_tested")
  invalid <- setdiff(indicators, valid_indicators)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid indicators: {.val {invalid}}")
  }

  # Use default classification if not provided
  if (is.null(csb_classification)) {
    csb_classification <- .default_csb_classification_mbg()
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

  # ---- Prepare KR data ----

  # Find available h32 variables
  available_h32 <- grep("^h32[a-z0-9]+$", names(dhs_kr), value = TRUE)

  if (length(available_h32) == 0) {
    cli::cli_abort("No h32 treatment-seeking variables found in data")
  }

  # Filter classification to available variables
  csb_classification <- csb_classification |>
    dplyr::filter(variable %in% available_h32)

  # Get h32 columns that exist in the data
  h32_cols <- intersect(csb_classification$variable, names(dhs_kr))

  kr <- dhs_kr |>
    dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
    dplyr::transmute(
      cluster_id = .data[[survey_vars$cluster]],
      age = .data[[survey_vars$age]],
      fever = .data[[survey_vars$fever]],
      received_act = if (survey_vars$act %in% names(dhs_kr)) {
        .data[[survey_vars$act]]
      } else {
        NA_real_
      },
      # Include h32 columns in the same transmute to preserve row alignment
      dplyr::across(dplyr::all_of(h32_cols), ~ .)
    )

  # Filter to U5 children with fever
  kr_fever <- kr |>
    dplyr::filter(
      age >= 0,
      age <= 59,
      fever == 1
    )

  if (nrow(kr_fever) == 0) {
    cli::cli_abort("No eligible children with fever found")
  }

  cli::cli_alert_info(
    "KR data: {format(nrow(kr_fever), big.mark = ',')} children under 5 with fever"
  )

  # ---- Create care-seeking indicators ----

  # Convert h32 columns to binary (visited or not)
  # h32 columns are already in kr_fever from the transmute above
  h32_binary <- kr_fever |>
    dplyr::select(dplyr::all_of(h32_cols)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::if_else(. == 1, 1L, 0L, missing = 0L)))

  # Map to categories
  public_vars <- csb_classification$variable[csb_classification$csb == "public"]
  chw_vars <- csb_classification$variable[csb_classification$csb == "chw"]
  private_formal_vars <- csb_classification$variable[csb_classification$csb == "private_formal"]
  private_informal_vars <- csb_classification$variable[csb_classification$csb == "private_informal"]
  pharmacy_vars <- csb_classification$variable[csb_classification$csb == "pharmacy"]

  # Calculate derived indicators
  kr_fever$csb_public <- rowSums(
    h32_binary[, intersect(c(public_vars, chw_vars), names(h32_binary)), drop = FALSE],
    na.rm = TRUE
  ) > 0

  kr_fever$csb_private <- rowSums(
    h32_binary[, intersect(c(private_formal_vars, private_informal_vars, pharmacy_vars), names(h32_binary)), drop = FALSE],
    na.rm = TRUE
  ) > 0

  kr_fever$csb_trained <- rowSums(
    h32_binary[, intersect(c(public_vars, chw_vars, private_formal_vars, pharmacy_vars), names(h32_binary)), drop = FALSE],
    na.rm = TRUE
  ) > 0

  kr_fever$csb_any <- as.integer(kr_fever$csb_public | kr_fever$csb_private)
  kr_fever$csb_none <- as.integer(!kr_fever$csb_any)

  # ACT treatment
  if (!all(is.na(kr_fever$received_act))) {
    kr_fever$has_act <- as.integer(kr_fever$received_act == 1)
  } else {
    kr_fever$has_act <- NA_integer_
  }

  # ---- Calculate cluster-level indicators ----

  results <- list()

  if ("any" %in% indicators) {
    any_cluster <- kr_fever |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(csb_any, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[["csb_any"]] <- data.table::as.data.table(any_cluster)
    cli::cli_alert_success("csb_any: {nrow(any_cluster)} clusters")
  }

  if ("public" %in% indicators) {
    public_cluster <- kr_fever |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(csb_public, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[["csb_public"]] <- data.table::as.data.table(public_cluster)
    cli::cli_alert_success("csb_public: {nrow(public_cluster)} clusters")
  }

  if ("private" %in% indicators) {
    private_cluster <- kr_fever |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(csb_private, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[["csb_private"]] <- data.table::as.data.table(private_cluster)
    cli::cli_alert_success("csb_private: {nrow(private_cluster)} clusters")
  }

  if ("trained" %in% indicators) {
    trained_cluster <- kr_fever |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(csb_trained, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[["csb_trained"]] <- data.table::as.data.table(trained_cluster)
    cli::cli_alert_success("csb_trained: {nrow(trained_cluster)} clusters")
  }

  if ("none" %in% indicators) {
    none_cluster <- kr_fever |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(csb_none, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[["csb_none"]] <- data.table::as.data.table(none_cluster)
    cli::cli_alert_success("csb_none: {nrow(none_cluster)} clusters")
  }

  if ("act" %in% indicators && !all(is.na(kr_fever$has_act))) {
    act_cluster <- kr_fever |>
      dplyr::filter(!is.na(has_act)) |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        indicator = sum(has_act, na.rm = TRUE),
        samplesize = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::inner_join(gps_clean, by = "cluster_id") |>
      dplyr::filter(samplesize > 0)

    results[["csb_act"]] <- data.table::as.data.table(act_cluster)
    cli::cli_alert_success("csb_act: {nrow(act_cluster)} clusters")
  }

  if (length(results) == 0) {
    cli::cli_abort("No valid MBG data could be prepared")
  }

  results
}


#' Default CSB Classification for MBG
#'
#' @return Data frame with variable and csb columns
#' @noRd
.default_csb_classification_mbg <- function() {
  data.frame(
    variable = c(
      "h32a", "h32b", "h32c", "h32d", "h32e", "h32f", "h32g", "h32h", "h32i",
      "h32na", "h32nb", "h32nc", "h32nd", "h32ne",
      "h32j", "h32k", "h32l", "h32m",
      "h32s", "h32t", "h32u",
      "h32n", "h32o", "h32p", "h32q", "h32r"
    ),
    csb = c(
      rep("public", 9),
      rep("chw", 5),
      rep("private_formal", 4),
      rep("private_informal", 3),
      rep("pharmacy", 5)
    ),
    stringsAsFactors = FALSE
  )
}


#' Prepare Single CSB Indicator for MBG
#'
#' @inheritParams calc_csb_mbg
#' @param indicator Single indicator name. Default: "public".
#'
#' @return A data.table with columns: cluster_id, indicator, samplesize, x, y
#' @export
prep_csb_mbg <- function(
  dhs_kr,
  gps_data,
  indicator = "public",
  csb_classification = NULL,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22",
    act = "ml13e"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  result <- calc_csb_mbg(
    dhs_kr = dhs_kr,
    gps_data = gps_data,
    indicators = indicator,
    csb_classification = csb_classification,
    survey_vars = survey_vars,
    gps_vars = gps_vars
  )

  result[[1]]
}
