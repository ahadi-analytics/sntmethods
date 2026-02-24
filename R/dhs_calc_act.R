#' Calculate ACT Treatment from DHS Data
#'
#' Estimates ACT (Artemisinin-based Combination Therapy) treatment coverage
#' among febrile children under 5 using survey-weighted methods.
#'
#' @details
#' Methodology: \url{https://github.com/ahadi-analytics/sntmethods/blob/master/inst/methods/act_dhs.yml}
#'
#' @param dhs_kr DHS children's recode (KR) dataset (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster/PSU ID (default: "v021")
#'     \item `weight`: Survey weight (default: "v005")
#'     \item `stratum`: Stratum variable (default: "v022")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Had fever in last 2 weeks (default: "h22")
#'     \item `act`: Received ACT treatment (default: "ml13e")
#'     \item `test`: Filter variable for act_tested denominator (default: "ml13a").
#'       NOTE: ml13a is chloroquine in standard DHS; verify meaning per survey.
#'   }
#' @param region_var Optional column name in `dhs_kr` to use as grouping
#'   variable (e.g., "v024" for region). Takes precedence over GPS/shapefile.
#' @param gps_data Optional DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns from shapefile
#'   (e.g., c("adm1", "adm2")).
#' @param join_nearest Logical; if TRUE, assigns clusters outside polygons
#'   to nearest admin unit. Default: TRUE.
#' @param dhs_pr Optional DHS Person Recode (PR) dataset. When provided,
#'   two additional indicators are computed: `dhs_febrile_rdt_pos` (RDT
#'   positivity rate among febrile children with a valid test result) and
#'   `dhs_febrile_rdt_pos_act` (ACT coverage among febrile RDT-positive
#'   children). Requires PR to contain hml35 and linkage via hv001/hv002/hvidx.
#'
#' @return Tibble with ACT estimates by grouping level, including:
#'   \itemize{
#'     \item Grouping variables (region, admin level, or national)
#'     \item `dhs_act`: Proportion receiving ACT among febrile children
#'     \item `dhs_act_low`, `dhs_act_upp`: 95\% confidence interval
#'     \item `dhs_act_tested`: Proportion receiving ACT among test-positive
#'     \item `dhs_act_tested_low`, `dhs_act_tested_upp`: 95\% CI
#'     \item `dhs_n_fever`: Number of febrile children
#'     \item `dhs_n_tested`: Number of test-positive children
#'   }
#'
#' @details
#' This function calculates two ACT treatment indicators:
#' \itemize{
#'   \item \strong{ACT coverage}: Proportion of febrile U5 children who
#'     received ACT treatment (ml13e == 1)
#'   \item \strong{ACT among tested}: Proportion receiving ACT among
#'     children where ml13a == 1. NOTE: ml13a is chloroquine in standard
#'     DHS; verify meaning per survey.
#' }
#'
#' Survey weighting uses the standard DHS design (clusters, strata, weights).
#'
#' @examples
#' \dontrun{
#' act_results <- calc_act_dhs(
#'   dhs_kr = kr_data,
#'   region_var = "v024"
#' )
#' }
#'
#' @seealso [calc_act_mbg()] for cluster-level MBG inputs,
#'   [calc_csb_dhs()] for care-seeking behavior
#' @export
calc_act_dhs <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    act = "ml13e",
    test = "ml13a"
  ),
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  ),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE,
  dhs_pr = NULL
) {
  # ---- 1. Input validation ----

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }

  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Check required survey variables
  needed <- unlist(survey_vars[c("cluster", "weight", "stratum", "age", "fever")])
  missing_vars <- setdiff(needed, names(dhs_kr))

  if (length(missing_vars) > 0) {
    cli::cli_abort(c(
      "Required variables not found: {.var {missing_vars}}",
      "i" = "Check your survey_vars mapping"
    ))
  }

  # Check ACT variable (ml13e preferred; fall back to h37e for older surveys)
  has_act_var <- survey_vars$act %in% names(dhs_kr)
  has_test_var <- survey_vars$test %in% names(dhs_kr)

  if (!has_act_var) {
    if ("h37e" %in% names(dhs_kr)) {
      cli::cli_alert_info(
        "ACT variable {.var {survey_vars$act}} not found; will use {.var h37e} (artemisinin combination for fever/cough)"
      )
    } else {
      cli::cli_abort(c(
        "ACT variable {.var {survey_vars$act}} not found in data (also tried {.var h37e})",
        "i" = "Check your survey_vars mapping"
      ))
    }
  }

  # Validate region_var if provided
  if (!is.null(region_var)) {
    if (!is.character(region_var) || length(region_var) != 1) {
      cli::cli_abort("`region_var` must be a single character string.")
    }
    if (!region_var %in% names(dhs_kr)) {
      cli::cli_abort(c(
        "Column {.var {region_var}} not found in `dhs_kr`.",
        "i" = "Available columns: {.var {head(names(dhs_kr), 10)}}..."
      ))
    }
  }

  # ---- 2. Prepare base dataset ----

  kr_fever <- .prepare_act_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = TRUE
  )

  # Create binary ACT-among-tested indicator
  has_tested_data <- has_test_var && !all(is.na(kr_fever$test_positive))
  if (has_tested_data) {
    kr_fever <- kr_fever |>
      dplyr::mutate(
        has_act_tested = dplyr::if_else(
          test_positive == 1 & received_act == 1, 1, 0,
          missing = NA_real_
        )
      )
  }

  # ---- 3. Spatial join if GPS + shapefile provided ----

  class_var <- NULL

  if (!is.null(region_var)) {
    class_var <- region_var
    cli::cli_alert_info("Using {.var {region_var}} as grouping variable")
  } else if (!is.null(gps_data) && !is.null(shapefile)) {
    cli::cli_alert_info("Joining GPS coordinates and administrative boundaries")

    if (!requireNamespace("sf", quietly = TRUE)) {
      cli::cli_abort("Package 'sf' is required for spatial operations")
    }

    gps_clean <- gps_data |>
      dplyr::select(
        cluster_id = !!gps_vars$cluster,
        lat = !!gps_vars$lat,
        lon = !!gps_vars$lon
      ) |>
      dplyr::distinct()

    kr_fever <- kr_fever |>
      dplyr::left_join(gps_clean, by = "cluster_id")

    clusters_sf <- kr_fever |>
      dplyr::select(cluster_id, lat, lon) |>
      dplyr::distinct() |>
      dplyr::filter(!is.na(lat), !is.na(lon)) |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

    shapefile <- shapefile |>
      sf::st_transform(4326) |>
      sf::st_make_valid()

    if (is.null(admin_level)) {
      available_admins <- names(shapefile)[grep("^adm[0-9]+$", names(shapefile))]
      if (length(available_admins) == 0) {
        cli::cli_abort("No admin columns (adm0, adm1, adm2, etc.) found in shapefile")
      }
      admin_level <- available_admins
    }

    cluster_admin <- sf::st_join(
      clusters_sf,
      shapefile[, c(admin_level, "geometry")],
      join = sf::st_within,
      left = TRUE
    )

    if (join_nearest) {
      unmatched <- is.na(cluster_admin[[admin_level[1]]])
      if (any(unmatched)) {
        nearest_idx <- sf::st_nearest_feature(cluster_admin[unmatched, ], shapefile)
        for (col in admin_level) {
          if (col %in% names(shapefile)) {
            cluster_admin[unmatched, col] <- shapefile[[col]][nearest_idx]
          }
        }
      }
    }

    cluster_admin_df <- sf::st_drop_geometry(cluster_admin)
    kr_fever <- kr_fever |>
      dplyr::left_join(cluster_admin_df, by = "cluster_id")

    if (length(admin_level) > 1) {
      kr_fever$admin_class <- apply(
        kr_fever[, admin_level, drop = FALSE], 1, paste, collapse = "_"
      )
      class_var <- "admin_class"
    } else {
      class_var <- admin_level[1]
    }
  } else if (!is.null(region_var)) {
    class_var <- region_var
  } else if ("v024" %in% names(kr_fever)) {
    class_var <- "v024"
    cli::cli_alert_info("Using v024 (region) as grouping variable")
  }

  # ---- 4. Set up survey design ----

  use_strata <- dplyr::n_distinct(kr_fever$stratum_id) > 1

  if (use_strata) {
    survey_options <- options(survey.lonely.psu = "certainty")
    on.exit(options(survey_options), add = TRUE)

    des <- survey::svydesign(
      ids = ~cluster_id,
      strata = ~stratum_id,
      weights = ~survey_weight,
      data = kr_fever,
      nest = TRUE
    )
  } else {
    des <- survey::svydesign(
      ids = ~cluster_id,
      weights = ~survey_weight,
      data = kr_fever,
      nest = TRUE
    )
  }

  # ---- 5. Calculate ACT indicators ----

  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
  } else {
    group_formula <- ~1
  }

  # ACT among febrile children
  if (!is.null(class_var)) {
    act_results <- tryCatch({
      survey::svyby(
        ~has_act,
        by = group_formula,
        design = des,
        FUN = survey::svymean,
        vartype = "ci",
        na.rm = TRUE,
        keep.names = FALSE
      ) |>
        tibble::as_tibble()
    }, error = function(e) {
      if (grepl("has only one PSU", e$message)) {
        des_no_strata <- survey::svydesign(
          ids = ~cluster_id, weights = ~survey_weight,
          data = kr_fever, nest = TRUE
        )
        survey::svyby(
          ~has_act, by = group_formula, design = des_no_strata,
          FUN = survey::svymean, vartype = "ci", na.rm = TRUE,
          keep.names = FALSE
        ) |> tibble::as_tibble()
      } else {
        stop(e)
      }
    })
  } else {
    act_mean <- survey::svymean(~has_act, design = des, na.rm = TRUE)
    act_ci <- stats::confint(act_mean)

    act_results <- tibble::tibble(
      level = "National",
      has_act = as.numeric(act_mean["has_act"]),
      `ci_l.has_act` = act_ci["has_act", 1],
      `ci_u.has_act` = act_ci["has_act", 2]
    )
  }

  # Normalize CI column names (svyby uses ci_l/ci_u for single-variable formulas)
  names(act_results)[names(act_results) == "ci_l"] <- "ci_l.has_act"
  names(act_results)[names(act_results) == "ci_u"] <- "ci_u.has_act"

  # Rename ACT columns
  act_results <- act_results |>
    dplyr::rename(
      dhs_act = has_act,
      dhs_act_low = `ci_l.has_act`,
      dhs_act_upp = `ci_u.has_act`
    )

  # ACT among test-positive children (if data available)
  if (has_tested_data) {
    # Subset design to test-positive children
    kr_tested <- kr_fever |>
      dplyr::filter(test_positive == 1, !is.na(received_act))

    if (nrow(kr_tested) > 0 && dplyr::n_distinct(kr_tested$cluster_id) > 1) {
      use_strata_tested <- dplyr::n_distinct(kr_tested$stratum_id) > 1

      if (use_strata_tested) {
        des_tested <- survey::svydesign(
          ids = ~cluster_id, strata = ~stratum_id,
          weights = ~survey_weight, data = kr_tested, nest = TRUE
        )
      } else {
        des_tested <- survey::svydesign(
          ids = ~cluster_id, weights = ~survey_weight,
          data = kr_tested, nest = TRUE
        )
      }

      act_tested_indicator <- kr_tested |>
        dplyr::mutate(
          got_act = dplyr::if_else(received_act == 1, 1, 0, missing = NA_real_)
        )

      if (use_strata_tested) {
        des_tested <- survey::svydesign(
          ids = ~cluster_id, strata = ~stratum_id,
          weights = ~survey_weight, data = act_tested_indicator, nest = TRUE
        )
      } else {
        des_tested <- survey::svydesign(
          ids = ~cluster_id, weights = ~survey_weight,
          data = act_tested_indicator, nest = TRUE
        )
      }

      if (!is.null(class_var) && class_var %in% names(act_tested_indicator)) {
        tested_results <- tryCatch({
          survey::svyby(
            ~got_act,
            by = group_formula,
            design = des_tested,
            FUN = survey::svymean,
            vartype = "ci",
            na.rm = TRUE,
            keep.names = FALSE
          ) |> tibble::as_tibble()
        }, error = function(e) {
          cli::cli_alert_warning("Could not calculate act_tested by group: {e$message}")
          NULL
        })
      } else {
        tested_mean <- survey::svymean(~got_act, design = des_tested, na.rm = TRUE)
        tested_ci <- stats::confint(tested_mean)

        tested_results <- tibble::tibble(
          level = "National",
          got_act = as.numeric(tested_mean["got_act"]),
          `ci_l.got_act` = tested_ci["got_act", 1],
          `ci_u.got_act` = tested_ci["got_act", 2]
        )
      }

      if (!is.null(tested_results)) {
        # Normalize CI column names for single-variable svyby
        names(tested_results)[names(tested_results) == "ci_l"] <- "ci_l.got_act"
        names(tested_results)[names(tested_results) == "ci_u"] <- "ci_u.got_act"

        tested_results <- tested_results |>
          dplyr::rename(
            dhs_act_tested = got_act,
            dhs_act_tested_low = `ci_l.got_act`,
            dhs_act_tested_upp = `ci_u.got_act`
          )

        # Merge tested results into main results
        join_by <- if (!is.null(class_var)) class_var else "level"
        act_results <- act_results |>
          dplyr::left_join(
            tested_results |> dplyr::select(
              dplyr::all_of(join_by),
              dhs_act_tested, dhs_act_tested_low, dhs_act_tested_upp
            ),
            by = join_by
          )
      }
    } else {
      cli::cli_alert_warning(
        "Too few test-positive children for act_tested estimates"
      )
    }
  }

  # ---- 6. Calculate sample sizes ----

  if (!is.null(class_var)) {
    sample_sizes <- kr_fever |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(
        dhs_n_fever = dplyr::n(),
        dhs_n_act = sum(has_act == 1, na.rm = TRUE),
        dhs_n_tested = sum(test_positive == 1, na.rm = TRUE),
        .groups = "drop"
      )

    act_results <- act_results |>
      dplyr::left_join(sample_sizes, by = class_var)
  } else {
    act_results$dhs_n_fever <- nrow(kr_fever)
    act_results$dhs_n_act <- sum(kr_fever$has_act == 1, na.rm = TRUE)
    act_results$dhs_n_tested <- sum(kr_fever$test_positive == 1, na.rm = TRUE)
  }

  # ---- 6.5. Febrile RDT indicators (if dhs_pr provided) ----

  if (!is.null(dhs_pr)) {
    kr_merged <- .merge_kr_pr_febrile(kr_fever = kr_fever, dhs_pr = dhs_pr)

    if (!is.null(kr_merged)) {
      use_strata_rdt <- dplyr::n_distinct(kr_merged$stratum_id) > 1
      if (use_strata_rdt) {
        des_rdt <- survey::svydesign(
          ids = ~cluster_id, strata = ~stratum_id,
          weights = ~survey_weight, data = kr_merged, nest = TRUE
        )
      } else {
        des_rdt <- survey::svydesign(
          ids = ~cluster_id, weights = ~survey_weight,
          data = kr_merged, nest = TRUE
        )
      }

      # febrile_rdt_pos: RDT positivity rate among febrile children with valid test
      if (!is.null(class_var)) {
        rdt_pos_results <- tryCatch({
          survey::svyby(
            ~has_rdt_pos, by = group_formula, design = des_rdt,
            FUN = survey::svymean, vartype = "ci", na.rm = TRUE,
            keep.names = FALSE
          ) |> tibble::as_tibble()
        }, error = function(e) {
          cli::cli_alert_warning("Could not calculate febrile_rdt_pos by group: {e$message}")
          NULL
        })
      } else {
        rdt_mean <- survey::svymean(~has_rdt_pos, design = des_rdt, na.rm = TRUE)
        rdt_ci   <- stats::confint(rdt_mean)
        rdt_pos_results <- tibble::tibble(
          level              = "National",
          has_rdt_pos        = as.numeric(rdt_mean["has_rdt_pos"]),
          `ci_l.has_rdt_pos` = rdt_ci["has_rdt_pos", 1],
          `ci_u.has_rdt_pos` = rdt_ci["has_rdt_pos", 2]
        )
      }

      if (!is.null(rdt_pos_results)) {
        names(rdt_pos_results)[names(rdt_pos_results) == "ci_l"] <- "ci_l.has_rdt_pos"
        names(rdt_pos_results)[names(rdt_pos_results) == "ci_u"] <- "ci_u.has_rdt_pos"

        rdt_pos_results <- rdt_pos_results |>
          dplyr::rename(
            dhs_febrile_rdt_pos     = has_rdt_pos,
            dhs_febrile_rdt_pos_low = `ci_l.has_rdt_pos`,
            dhs_febrile_rdt_pos_upp = `ci_u.has_rdt_pos`
          ) |>
          dplyr::mutate(
            dhs_febrile_rdt_pos     = round(dhs_febrile_rdt_pos, 2),
            dhs_febrile_rdt_pos_low = pmax(0, round(dhs_febrile_rdt_pos_low, 2)),
            dhs_febrile_rdt_pos_upp = pmin(1, round(dhs_febrile_rdt_pos_upp, 2))
          )

        join_by_rdt <- if (!is.null(class_var)) class_var else "level"
        act_results <- act_results |>
          dplyr::left_join(
            rdt_pos_results |> dplyr::select(
              dplyr::all_of(join_by_rdt),
              dhs_febrile_rdt_pos, dhs_febrile_rdt_pos_low, dhs_febrile_rdt_pos_upp
            ),
            by = join_by_rdt
          )
      }

      # febrile_rdt_pos_act: ACT coverage among febrile RDT-positive children
      kr_rdt_pos <- kr_merged |>
        dplyr::filter(has_rdt_pos == 1, !is.na(received_act))

      if (nrow(kr_rdt_pos) > 0 && dplyr::n_distinct(kr_rdt_pos$cluster_id) > 1) {
        use_strata_rpa <- dplyr::n_distinct(kr_rdt_pos$stratum_id) > 1
        if (use_strata_rpa) {
          des_rpa <- survey::svydesign(
            ids = ~cluster_id, strata = ~stratum_id,
            weights = ~survey_weight, data = kr_rdt_pos, nest = TRUE
          )
        } else {
          des_rpa <- survey::svydesign(
            ids = ~cluster_id, weights = ~survey_weight,
            data = kr_rdt_pos, nest = TRUE
          )
        }

        if (!is.null(class_var) && class_var %in% names(kr_rdt_pos)) {
          rpa_results <- tryCatch({
            survey::svyby(
              ~has_act, by = group_formula, design = des_rpa,
              FUN = survey::svymean, vartype = "ci", na.rm = TRUE,
              keep.names = FALSE
            ) |> tibble::as_tibble()
          }, error = function(e) {
            cli::cli_alert_warning("Could not calculate febrile_rdt_pos_act by group: {e$message}")
            NULL
          })
        } else {
          rpa_mean <- survey::svymean(~has_act, design = des_rpa, na.rm = TRUE)
          rpa_ci   <- stats::confint(rpa_mean)
          rpa_results <- tibble::tibble(
            level          = "National",
            has_act        = as.numeric(rpa_mean["has_act"]),
            `ci_l.has_act` = rpa_ci["has_act", 1],
            `ci_u.has_act` = rpa_ci["has_act", 2]
          )
        }

        if (!is.null(rpa_results)) {
          names(rpa_results)[names(rpa_results) == "ci_l"] <- "ci_l.has_act"
          names(rpa_results)[names(rpa_results) == "ci_u"] <- "ci_u.has_act"

          rpa_results <- rpa_results |>
            dplyr::rename(
              dhs_febrile_rdt_pos_act     = has_act,
              dhs_febrile_rdt_pos_act_low = `ci_l.has_act`,
              dhs_febrile_rdt_pos_act_upp = `ci_u.has_act`
            ) |>
            dplyr::mutate(
              dhs_febrile_rdt_pos_act     = round(dhs_febrile_rdt_pos_act, 2),
              dhs_febrile_rdt_pos_act_low = pmax(0, round(dhs_febrile_rdt_pos_act_low, 2)),
              dhs_febrile_rdt_pos_act_upp = pmin(1, round(dhs_febrile_rdt_pos_act_upp, 2))
            )

          join_by_rpa <- if (!is.null(class_var)) class_var else "level"
          act_results <- act_results |>
            dplyr::left_join(
              rpa_results |> dplyr::select(
                dplyr::all_of(join_by_rpa),
                dhs_febrile_rdt_pos_act, dhs_febrile_rdt_pos_act_low, dhs_febrile_rdt_pos_act_upp
              ),
              by = join_by_rpa
            )
        }
      } else {
        cli::cli_alert_warning("Too few RDT-positive children for febrile_rdt_pos_act estimates")
      }

      # Sample sizes for RDT indicators
      if (!is.null(class_var)) {
        rdt_sizes <- kr_merged |>
          dplyr::group_by(.data[[class_var]]) |>
          dplyr::summarise(
            dhs_n_febrile_rdt     = dplyr::n(),
            dhs_n_febrile_rdt_pos = sum(has_rdt_pos == 1, na.rm = TRUE),
            .groups = "drop"
          )
        act_results <- act_results |>
          dplyr::left_join(rdt_sizes, by = class_var)
      } else {
        act_results$dhs_n_febrile_rdt     <- nrow(kr_merged)
        act_results$dhs_n_febrile_rdt_pos <- sum(kr_merged$has_rdt_pos == 1, na.rm = TRUE)
      }
    }
  }

  # ---- 7. Format results ----

  # Round proportions
  act_cols <- names(act_results)[grepl("^dhs_act", names(act_results))]
  act_results <- act_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(act_cols[!grepl("^dhs_n_", act_cols)]),
        ~ round(.x, 2)
      )
    )

  # Clamp CIs to [0, 1]
  act_results <- act_results |>
    dplyr::mutate(
      dplyr::across(dplyr::matches("_low$"), ~ pmax(0, .)),
      dplyr::across(dplyr::matches("_upp$"), ~ pmin(1, .))
    )

  # Ensure count columns are integers
  count_cols <- intersect(
    c("dhs_n_fever", "dhs_n_act", "dhs_n_tested", "dhs_n_febrile_rdt", "dhs_n_febrile_rdt_pos"),
    names(act_results)
  )
  act_results <- act_results |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(count_cols), ~ as.integer(round(.x)))
    )

  # Split admin_class back if needed
  if (!is.null(class_var) && class_var == "admin_class" &&
      !is.null(admin_level) && length(admin_level) > 1) {
    admin_splits <- stringr::str_split(
      act_results$admin_class, "_", simplify = TRUE
    )
    for (i in seq_along(admin_level)) {
      act_results[[admin_level[i]]] <- admin_splits[, i]
    }
  }

  # Remove temporary columns
  act_results <- act_results |>
    dplyr::select(-dplyr::any_of(c("admin_class", "level")))

  tibble::as_tibble(act_results)
}
