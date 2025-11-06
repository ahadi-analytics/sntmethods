#' Join DHS GPS coordinates to PR data
#'
#' Safely merges cluster-level coordinates from DHS GPS dataset onto PR data.
#'
#' @param pr_data DHS PR dataset (data.frame or tibble).
#' @param gps_data DHS GPS dataset (data.frame or tibble).
#' @param pr_vars Named list; must include `cluster`.
#' @param gps_vars Named list; must include `cluster`, `lat`, `lon`.
#'
#' @return PR dataset with `lat`, `lon` added.
#' @export
join_dhs_coords <- function(
  pr_data,
  gps_data,
  pr_vars = list(cluster = "hv001"),
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
) {
  # checks
  needed_pr <- c("cluster")
  needed_gps <- c("cluster", "lat", "lon")
  if (!all(needed_pr %in% names(pr_vars))) {
    cli::cli_abort("`pr_vars` must include: {needed_pr}.")
  }
  if (!all(needed_gps %in% names(gps_vars))) {
    cli::cli_abort("`gps_vars` must include: {needed_gps}.")
  }

  pr_core <- pr_data |>
    dplyr::mutate(cluster_id = .data[[pr_vars$cluster]]) |>
    dplyr::select(dplyr::everything(), cluster_id)

  gps_core <- gps_data |>
    dplyr::transmute(
      cluster_id = .data[[gps_vars$cluster]],
      lat = as.numeric(.data[[gps_vars$lat]]),
      lon = as.numeric(.data[[gps_vars$lon]])
    ) |>
    dplyr::distinct(cluster_id, .keep_all = TRUE)

  out <- pr_core |>
    dplyr::left_join(gps_core, by = "cluster_id")

  na_share <- mean(is.na(out$lat) | is.na(out$lon))
  if (is.finite(na_share) && na_share > 0.2) {
    cli::cli_alert_warning(
      "More than {round(100 * na_share, 1)}% of PR records lack coords after join."
    )
  }

  out
}

#' Calculate PfPR (RDT & Microscopy) from DHS PR data
#'
#' Estimates malaria prevalence (PfPR) among children aged 6–59 months.
#' Returns either cluster-level results (with coordinates if available) or
#' aggregated by adm1/adm2 if coordinates are missing. Includes denominators.
#'
#' @param dhs_pr DHS PR dataset (tibble or data.frame).
#' @param survey_vars Named list mapping DHS vars:
#'   list(
#'     cluster = "hv021", stratum = "hv022", weight = "hv005",
#'     adm1 = "v024", adm2 = "sdist",
#'     age = "hc1", present = "hv103", mother = "hv042",
#'     rdt = "hml35", mic = "hml32"
#'   )
#' @param gps_data Optional DHS GPS dataset; if provided, joins coordinates.
#' @param gps_vars Named list for GPS vars (if gps_data provided).
#'
#' @return Tibble with PfPR estimates (RDT + Microscopy, with denominators).
#' @export
calc_pfpr_dhs <- function(
  dhs_pr,
  survey_vars = list(
    cluster = "hv021",
    stratum = "hv022",
    weight = "hv005",
    adm1 = "v024",
    adm2 = "sdist",
    age = "hc1",
    present = "hv103",
    mother = "hv042",
    rdt = "hml35",
    mic = "hml32"
  ),
  gps_data = NULL,
  gps_vars = list(cluster = "DHSCLUST", lat = "LATNUM", lon = "LONGNUM")
) {
  # -- checks ---------------------------------------------------------------
  needed <- c(
    "cluster",
    "stratum",
    "weight",
    "age",
    "present",
    "mother",
    "rdt",
    "mic"
  )
  if (!all(needed %in% names(survey_vars))) {
    cli::cli_abort("`survey_vars` must include: {needed}.")
  }

  # -- step 1: prepare ------------------------------------------------------
  pr <- dhs_pr |>
    dplyr::mutate(
      survey_weight = .data[[survey_vars$weight]] / 1e6,
      age = .data[[survey_vars$age]],
      present = .data[[survey_vars$present]],
      mother = .data[[survey_vars$mother]],
      rdt_res = .data[[survey_vars$rdt]],
      mic_res = .data[[survey_vars$mic]],
      adm1 = if ("adm1" %in% names(survey_vars)) {
        haven::as_factor(.data[[survey_vars$adm1]]) |> toupper()
      } else {
        NA
      },
      adm2 = if ("adm2" %in% names(survey_vars)) {
        haven::as_factor(.data[[survey_vars$adm2]]) |> toupper()
      } else {
        NA
      }
    ) |>
    dplyr::mutate(
      tested_rdt = dplyr::if_else(
        present == 1 &
          mother == 1 &
          age >= 6 &
          age <= 59 &
          rdt_res %in% c(0, 1),
        1,
        0,
        missing = NA_real_
      ),
      tested_mic = dplyr::if_else(
        present == 1 &
          mother == 1 &
          age >= 6 &
          age <= 59 &
          mic_res %in% c(0, 1, 6),
        1,
        0,
        missing = NA_real_
      ),
      rdt_pos = dplyr::case_when(
        present == 1 & mother == 1 & age >= 6 & age <= 59 & rdt_res == 1 ~ 1,
        present == 1 & mother == 1 & age >= 6 & age <= 59 & rdt_res == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      mic_pos = dplyr::case_when(
        present == 1 & mother == 1 & age >= 6 & age <= 59 & mic_res == 1 ~ 1,
        present == 1 &
          mother == 1 &
          age >= 6 &
          age <= 59 &
          mic_res %in% c(0, 6) ~ 0,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::select(
      cluster_id = survey_vars$cluster,
      stratum_id = survey_vars$stratum,
      survey_weight,
      adm1,
      adm2,
      tested_rdt,
      tested_mic,
      rdt_pos,
      mic_pos
    )

  # attach GPS if available
  if (!is.null(gps_data)) {
    pr <- join_dhs_coords(
      pr_data = pr,
      gps_data = gps_data,
      pr_vars = list(cluster = survey_vars$cluster),
      gps_vars = gps_vars
    )
  }

  # -- step 2: survey designs -----------------------------------------------
  des_rdt <- survey::svydesign(
    ids = ~cluster_id,
    strata = ~stratum_id,
    weights = ~survey_weight,
    data = dplyr::filter(pr, tested_rdt == 1),
    nest = TRUE
  )
  des_mic <- survey::svydesign(
    ids = ~cluster_id,
    strata = ~stratum_id,
    weights = ~survey_weight,
    data = dplyr::filter(pr, tested_mic == 1),
    nest = TRUE
  )

  # -- step 3: grouping -----------------------------------------------------
  if (!is.null(gps_data) && all(c("lat", "lon") %in% names(pr))) {
    group_vars <- c("cluster_id", "lat", "lon")
  } else {
    group_vars <- c("adm1", "adm2")
  }

  # -- step 4: estimates ----------------------------------------------------
  pfpr_rdt <- survey::svyby(
    ~rdt_pos,
    by = pr[, group_vars, drop = FALSE],
    design = des_rdt,
    FUN = survey::svymean,
    vartype = "ci",
    keep.names = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      pfpr_rdt = rdt_pos,
      pfpr_rdt_low = ci_l,
      pfpr_rdt_upp = ci_u
    ) |>
    dplyr::mutate(
      pfpr_rdt = round(pfpr_rdt * 100, 1),
      pfpr_rdt_low = round(pfpr_rdt_low * 100, 1),
      pfpr_rdt_upp = round(pfpr_rdt_upp * 100, 1)
    )

  pfpr_mic <- survey::svyby(
    ~mic_pos,
    by = pr[, group_vars, drop = FALSE],
    design = des_mic,
    FUN = survey::svymean,
    vartype = "ci",
    keep.names = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      pfpr_mic = mic_pos,
      pfpr_mic_low = ci_l,
      pfpr_mic_upp = ci_u
    ) |>
    dplyr::mutate(
      pfpr_mic = round(pfpr_mic * 100, 1),
      pfpr_mic_low = round(pfpr_mic_low * 100, 1),
      pfpr_mic_upp = round(pfpr_mic_upp * 100, 1)
    )

  # -- step 5: denominators -------------------------------------------------
  denom_rdt <- survey::svyby(
    ~ tested_rdt + rdt_pos,
    by = pr[, group_vars, drop = FALSE],
    design = des_rdt,
    FUN = survey::svytotal,
    keep.names = TRUE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      n_tested_rdt = tested_rdt,
      n_pos_rdt = rdt_pos
    )

  denom_mic <- survey::svyby(
    ~ tested_mic + mic_pos,
    by = pr[, group_vars, drop = FALSE],
    design = des_mic,
    FUN = survey::svytotal,
    keep.names = TRUE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      n_tested_mic = tested_mic,
      n_pos_mic = mic_pos
    )

  # -- step 6: join all -----------------------------------------------------
  pfpr_final <- pfpr_rdt |>
    dplyr::left_join(pfpr_mic, by = group_vars) |>
    dplyr::left_join(denom_rdt, by = group_vars) |>
    dplyr::left_join(denom_mic, by = group_vars)

  return(pfpr_final)
}
