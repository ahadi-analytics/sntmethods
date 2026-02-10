#' Calculate Care-Seeking Behavior from DHS Data (WMR Methodology)
#'
#' Estimates care-seeking behavior for febrile children under 5 using
#' the WHO World Malaria Report (WMR) methodology with overlapping indicators.
#'
#' @param dhs_kr DHS children's recode (KR) dataset in tidy format
#'   (data.frame or tibble).
#' @param survey_vars Named list mapping DHS variable names. Required keys:
#'   \itemize{
#'     \item `cluster`: Cluster ID (default: "v021")
#'     \item `weight`: Survey weight (default: "v005")
#'     \item `stratum`: Stratum variable (default: "v022")
#'     \item `age`: Child's age in months (default: "hw1")
#'     \item `fever`: Had fever in last 2 weeks (default: "h22")
#'     \item `alive`: Child survival status (default: "b5"). NOTE: WMR
#'       methodology assumes filtering to living children (b5 == 1) is done
#'       upstream. This function does NOT filter by alive status.
#'   }
#' @param csb_classification Data frame specifying h32 variable to CSB category
#'   mapping. Must have columns:
#'   \itemize{
#'     \item `variable`: h32 variable name (e.g., "h32a", "h32j")
#'     \item `csb`: Category - one of: "public", "chw", "private_formal",
#'       "private_informal", "pharmacy"
#'   }
#'   If NULL, uses default WMR classification. See Details for category
#'   meanings.
#' @param source_config **Deprecated**. Use `csb_classification` instead.
#'   Legacy parameter for backwards compatibility. Named list with:
#'   \itemize{
#'     \item `public`: Character vector of h32 codes for public sector
#'     \item `private`: Character vector of h32 codes for private sector
#'     \item `excluded`: Character vector of h32 codes to exclude
#'   }
#' @param region_var Optional column name (character string) in `dhs_kr` to
#'   use as the grouping variable (e.g., `"v024"` for region). When provided,
#'   this takes precedence over GPS/shapefile-based grouping and the column
#'   appears first in the output.
#' @param gps_data Optional DHS GPS dataset with cluster coordinates.
#' @param gps_vars Named list for GPS variables (cluster, lat, lon).
#' @param shapefile Optional sf object with administrative boundaries.
#' @param admin_level Character vector of admin columns from shapefile
#'   (e.g., c("adm1", "adm2")). If NULL, uses existing admin variables
#'   in data.
#' @param join_nearest Logical; if TRUE, assigns clusters outside polygons
#'   to nearest admin unit.
#'
#' @return Tibble with CSB estimates by administrative level, with
#'   confidence intervals and sample sizes.
#'
#' @details
#' This function implements the WHO World Malaria Report (WMR) methodology
#' for care-seeking behavior analysis.
#'
#' \strong{WMR 5-Category Classification:}
#' \itemize{
#'   \item `public`: Government health facilities (hospitals, health
#'     centers, posts)
#'   \item `chw`: Community health workers (often NGO sector in DHS-8)
#'   \item `private_formal`: Private hospitals, clinics, and doctors
#'   \item `private_informal`: Traditional practitioners and other
#'     informal sources
#'   \item `pharmacy`: Pharmacies and drug shops
#' }
#'
#' \strong{Derived Indicators (OVERLAPPING):}
#' These indicators are NOT mutually exclusive. A child can be counted in
#' multiple categories if they visited multiple source types.
#' \itemize{
#'   \item `dhs_csb_public`: Public sector care (public OR chw)
#'   \item `dhs_csb_private`: Any private sector care (private_formal OR
#'     private_informal OR pharmacy)
#'   \item `dhs_csb_trained`: Trained provider (public OR private_formal
#'     OR pharmacy)
#'   \item `dhs_csb_any`: Any treatment sought (public OR private)
#'   \item `dhs_csb_none`: No treatment sought (NOT any)
#' }
#'
#' \strong{Important:} Only `dhs_csb_any` and `dhs_csb_none` are mutually
#' exclusive. The equation `dhs_csb_any + dhs_csb_none = 1` always holds.
#' However, `dhs_csb_public + dhs_csb_private + dhs_csb_none` may exceed
#' 1.0 when children visit both public and private sources.
#'
#' @references
#' WHO. World Malaria Report. Geneva: World Health Organization.
#' \url{https://www.who.int/teams/global-malaria-programme/reports}
#'
#' @export
calc_csb_dhs_core <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    alive = "b5"
  ),
  csb_classification = NULL,
  source_config = NULL,
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  ),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE
) {
  # ---- 1. Input validation ---------------------------------------------------

  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble.")
  }

  if (nrow(dhs_kr) == 0) {
    cli::cli_abort("`dhs_kr` is empty.")
  }

  # Check required survey variables
  needed <- unlist(
    survey_vars[
      c(
        "cluster",
        "weight",
        "stratum",
        "age",
        "fever"
      )
    ]
  )

  missing_vars <- setdiff(needed, names(dhs_kr))

  if (length(missing_vars) > 0) {
    cli::cli_abort(
      c(
        "Required variables not found: {.var {missing_vars}}",
        "i" = "Check your survey_vars mapping"
      )
    )
  }

  # Validate region_var if provided
  if (!is.null(region_var)) {
    if (!is.character(region_var) || length(region_var) != 1) {
      cli::cli_abort(
        "`region_var` must be a single character string."
      )
    }
    if (!region_var %in% names(dhs_kr)) {
      cli::cli_abort(
        c(
          "Column {.var {region_var}} not found in `dhs_kr`.",
          "i" = "Available columns: {.var {head(names(dhs_kr), 10)}}..."
        )
      )
    }
    if (!is.null(gps_data) || !is.null(shapefile)) {
      cli::cli_alert_warning(
        "`region_var` provided with GPS/shapefile; `region_var` takes precedence"
      )
    }
  }

  # Auto-detect available h32 treatment source variables
  # Pattern includes digits for DHS-8 NGO sector variables (h32na, h32nb, etc.)
  available_h32 <- grep("^h32[a-z0-9]+$", names(dhs_kr), value = TRUE)

  if (length(available_h32) == 0) {
    cli::cli_abort(
      c(
        "No h32 treatment-seeking variables found in data.",
        "i" = "Expected variables like h32a, h32b, h32c, etc.",
        "i" = "Check your DHS KR data includes care-seeking variables"
      )
    )
  }

  cli::cli_alert_info(
    "Detected {length(available_h32)} h32 source variables"
  )

  # ---- 2. Handle classification parameter ------------------------------------

  # Handle backwards compatibility and defaults
  if (!is.null(source_config) && is.null(csb_classification)) {
    # Legacy source_config provided - convert to classification
    cli::cli_alert_warning(
      "source_config is deprecated. Use csb_classification instead."
    )
    csb_classification <- .convert_source_config(source_config)
    cli::cli_alert_info(
      "Converted source_config to csb_classification format"
    )
  } else if (is.null(csb_classification)) {
    # Neither provided - use WMR default
    csb_classification <- .default_csb_classification()
    cli::cli_alert_info(
      "Using default WMR csb_classification"
    )
  }

  # Validate csb_classification
  if (!is.data.frame(csb_classification)) {
    cli::cli_abort("`csb_classification` must be a data.frame")
  }

  if (!all(c("variable", "csb") %in% names(csb_classification))) {
    cli::cli_abort(
      c(
        "`csb_classification` must have columns: variable, csb",
        "i" = "Got columns: {.var {names(csb_classification)}}"
      )
    )
  }

  valid_csb_values <- c(
    "public", "chw", "private_formal",
    "private_informal", "pharmacy"
  )
  invalid_csb <- setdiff(unique(csb_classification$csb), valid_csb_values)

  if (length(invalid_csb) > 0) {
    cli::cli_abort(
      c(
        "Invalid csb values: {.val {invalid_csb}}",
        "i" = "Valid values: {.val {valid_csb_values}}"
      )
    )
  }

  # Filter classification to only include variables present in data
  csb_classification <- csb_classification |>
    dplyr::filter(variable %in% available_h32)

  if (nrow(csb_classification) == 0) {
    class_vars <- unique(csb_classification$variable)
    cli::cli_abort(
      c(
        "No h32 variables from csb_classification found in data.",
        "i" = "Available h32 variables: {.var {available_h32}}",
        "i" = "Classification variables: {.var {class_vars}}"
      )
    )
  }

  # Log which categories are available
  categories_found <- unique(csb_classification$csb)
  cli::cli_alert_info(
    "CSB categories found: {paste(categories_found, collapse = ', ')}"
  )

  for (cat in categories_found) {
    cat_vars <- csb_classification$variable[csb_classification$csb == cat]
    cli::cli_alert_info(
      "  {cat} ({length(cat_vars)}): {paste(cat_vars, collapse = ', ')}"
    )
  }

  # ---- 3. Prepare base dataset ----------------------------------------------

  # Check if alive variable exists
  has_alive <- !is.null(survey_vars$alive) &&
    survey_vars$alive %in% names(dhs_kr)

  kr <- dhs_kr |>
    dplyr::mutate(
      cluster_id = .data[[survey_vars$cluster]],
      survey_weight = .data[[survey_vars$weight]] / 1e6,
      stratum_id = .data[[survey_vars$stratum]],
      age_months = .data[[survey_vars$age]],
      had_fever = .data[[survey_vars$fever]],
      child_alive = if (has_alive) .data[[survey_vars$alive]] else NA_real_
    )

  # Filter to children under 5 with valid fever data
  kr_eligible <- kr |>
    dplyr::filter(
      age_months >= 0,
      age_months <= 59,
      had_fever %in% c(0, 1)
    )

  if (nrow(kr_eligible) == 0) {
    cli::cli_abort(
      "No eligible children (0-59 months) with valid fever data found."
    )
  }

  # Filter to children who had fever
  kr_fever <- kr_eligible |>
    dplyr::filter(had_fever == 1)

  if (nrow(kr_fever) == 0) {
    cli::cli_abort(
      "No children with fever in the last 2 weeks found in the dataset."
    )
  }

  cli::cli_alert_info(
    paste0(
      "Found {format(nrow(kr_fever), big.mark = ',')} children with fever ",
      "out of {format(nrow(kr_eligible), big.mark = ',')} eligible children"
    )
  )

  # ---- 4. Create care-seeking indicators (WMR methodology) -----------------
  # This implements the WHO World Malaria Report methodology:
  # 1. Reshape h32 variables to long format

  # 2. Join to classification table
  # 3. Aggregate to 5 base categories per child
  # 4. Create derived overlapping indicators

  # Add row ID for joining back after reshape
  kr_fever <- kr_fever |>
    dplyr::mutate(.row_id = dplyr::row_number())

  # Get h32 columns that are in our classification
  h32_cols <- intersect(csb_classification$variable, names(kr_fever))

  # Reshape h32 to long format and join to classification
  kr_long <- kr_fever |>
    dplyr::select(.row_id, dplyr::all_of(h32_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(h32_cols),
      names_to = "variable",
      values_to = "visited"
    ) |>
    dplyr::left_join(
      csb_classification |> dplyr::select(variable, csb),
      by = "variable"
    ) |>
    dplyr::filter(visited == 1)  # Keep only sources that were visited

  # Aggregate to base categories per child
  if (nrow(kr_long) > 0) {
    base_cats <- kr_long |>
      dplyr::group_by(.row_id, csb) |>
      dplyr::summarise(visited = 1L, .groups = "drop") |>
      tidyr::pivot_wider(
        names_from = csb,
        values_from = visited,
        values_fill = 0L,
        names_prefix = "has_"
      )

    # Join back to main data
    kr_fever <- kr_fever |>
      dplyr::left_join(base_cats, by = ".row_id")
  }

  # Ensure all 5 base categories exist (even if no children visited them)
  base_category_cols <- c(
    "has_public", "has_chw", "has_private_formal",
    "has_private_informal", "has_pharmacy"
  )

  for (col in base_category_cols) {
    if (!col %in% names(kr_fever)) {
      kr_fever[[col]] <- 0L
    }
    # Replace NA with 0 (children who had fever but visited no sources)
    kr_fever[[col]] <- tidyr::replace_na(kr_fever[[col]], 0L)
  }

  # Create derived indicators (WMR methodology)
  kr_fever <- kr_fever |>
    dplyr::mutate(
      # csb_public = public OR chw
      csb_public = as.numeric(has_public == 1 | has_chw == 1),

      # csb_private = private_formal OR private_informal OR pharmacy
      csb_private = as.numeric(
        has_private_formal == 1 |
        has_private_informal == 1 |
        has_pharmacy == 1
      ),

      # csb_private_formal_pha = private_formal OR pharmacy
      csb_private_formal_pha = as.numeric(
        has_private_formal == 1 |
        has_pharmacy == 1
      ),

      # csb_any_treatment = csb_public OR csb_private
      csb_any_treatment = as.numeric(csb_public == 1 | csb_private == 1),

      # csb_no_treatment = NOT(csb_any_treatment)
      csb_no_treatment = as.numeric(csb_any_treatment == 0),

      # csb_trained_provider = csb_public OR csb_private_formal_pha
      csb_trained_provider = as.numeric(
        csb_public == 1 |
        csb_private_formal_pha == 1
      )
    )

  # Verify mathematical invariant: any + none must equal 1
  stopifnot(
    all(kr_fever$csb_any_treatment + kr_fever$csb_no_treatment == 1)
  )

  # Log distribution for diagnostic purposes (unweighted)
  n_total <- nrow(kr_fever)
  n_public <- sum(kr_fever$csb_public, na.rm = TRUE)
  n_private <- sum(kr_fever$csb_private, na.rm = TRUE)
  n_none <- sum(kr_fever$csb_no_treatment, na.rm = TRUE)
  n_trained <- sum(kr_fever$csb_trained_provider, na.rm = TRUE)

  cli::cli_alert_info(
    paste0(
      "Care-seeking (unweighted): ",
      "public={round(n_public/n_total*100, 1)}%, ",
      "private={round(n_private/n_total*100, 1)}%, ",
      "none={round(n_none/n_total*100, 1)}%, ",
      "trained={round(n_trained/n_total*100, 1)}%"
    )
  )

  # ---- 5. Join GPS and shapefile if provided --------------------------------

  class_var <- NULL

  if (!is.null(region_var)) {
    class_var <- region_var
    cli::cli_alert_info("Using {.var {region_var}} as grouping variable")
  } else if (!is.null(gps_data) && !is.null(shapefile)) {
    cli::cli_alert_info(
      "Joining GPS coordinates and administrative boundaries"
    )

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
      dplyr::left_join(
        gps_clean,
        by = "cluster_id"
      )

    clusters_sf <- kr_fever |>
      dplyr::select(
        cluster_id,
        lat,
        lon
      ) |>
      dplyr::distinct() |>
      dplyr::filter(
        !is.na(lat),
        !is.na(lon)
      ) |>
      sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326
      )

    shapefile <- shapefile |>
      sf::st_transform(4326) |>
      sf::st_make_valid()

    if (is.null(admin_level)) {
      available_admins <- names(shapefile)[
        grep("^adm[0-9]+$", names(shapefile))
      ]

      if (length(available_admins) == 0) {
        cli::cli_abort(
          "No admin columns (adm0, adm1, adm2, etc.) found in shapefile"
        )
      }

      admin_level <- available_admins

      cli::cli_alert_info(
        "Using admin levels: {paste(admin_level, collapse = ', ')}"
      )
    }

    missing_cols <- setdiff(admin_level, names(shapefile))

    if (length(missing_cols) > 0) {
      cli::cli_abort(
        "Admin columns not found in shapefile: ",
        "{paste(missing_cols, collapse = ', ')}"
      )
    }

    admin_name_cols <- paste0(admin_level, "_name")
    admin_name_cols <- admin_name_cols[
      admin_name_cols %in% names(shapefile)
    ]
    all_admin_cols <- c(admin_level, admin_name_cols)

    cluster_admin <- sf::st_join(
      clusters_sf,
      shapefile[, c(all_admin_cols, "geometry")],
      join = sf::st_within,
      left = TRUE
    )

    if (join_nearest) {
      unmatched <- is.na(cluster_admin[[admin_level[1]]])

      if (any(unmatched)) {
        n_unmatched <- format(sum(unmatched), big.mark = ",")
        cli::cli_alert_info(
          "Assigning {n_unmatched} clusters to nearest admin units"
        )

        nearest_idx <- sf::st_nearest_feature(
          cluster_admin[unmatched, ],
          shapefile
        )

        for (col in all_admin_cols) {
          if (col %in% names(shapefile)) {
            cluster_admin[unmatched, col] <- shapefile[[col]][
              nearest_idx
            ]
          }
        }
      }
    }

    cluster_admin_df <- sf::st_drop_geometry(cluster_admin)

    kr_fever <- kr_fever |>
      dplyr::left_join(
        cluster_admin_df,
        by = "cluster_id"
      )

    if (length(admin_level) > 1) {
      kr_fever$admin_class <- apply(
        kr_fever[, admin_level, drop = FALSE],
        1,
        paste,
        collapse = "_"
      )
      class_var <- "admin_class"
    } else {
      class_var <- admin_level[1]
    }
  } else if (!is.null(shapefile)) {
    cli::cli_alert_warning(
      "Shapefile provided without GPS data; using existing admin vars"
    )

    existing_admins <- c("v024", "v025", "sdist")
    found_admin <- existing_admins[
      existing_admins %in% names(kr_fever)
    ][1]

    if (!is.na(found_admin)) {
      class_var <- found_admin
      cli::cli_alert_info(
        "Using {.var {found_admin}} as grouping variable"
      )
    }
  } else if (!is.null(gps_data)) {
    cli::cli_alert_info(
      "GPS provided without shapefile; calculating cluster-level CSB"
    )
    class_var <- "cluster_id"
  } else {
    if ("v024" %in% names(kr_fever)) {
      class_var <- "v024"
      cli::cli_alert_info(
        "Using v024 (region) as grouping variable"
      )
    }
  }

  # ---- 6. Set up survey design ----------------------------------------------

  if (!is.null(class_var)) {
    cli::cli_alert_info(
      "Calculating CSB by {.var {class_var}}"
    )
  } else {
    cli::cli_alert_info(
      "Calculating national-level CSB"
    )
  }

  # Check for single-PSU strata
  strata_check <- kr_fever |>
    dplyr::group_by(stratum_id) |>
    dplyr::summarise(
      n_clusters = dplyr::n_distinct(cluster_id),
      .groups = "drop"
    )

  single_psu_strata <- sum(strata_check$n_clusters == 1)

  if (single_psu_strata > 0) {
    n_strata <- format(single_psu_strata, big.mark = ",")
    cli::cli_alert_info(
      "Found {n_strata} strata with single PSU; using certainty option"
    )
  }

  use_strata <- dplyr::n_distinct(kr_fever$stratum_id) > 1

  if (use_strata) {
    # Set survey options to handle single-PSU strata
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

  # ---- 7. Calculate care-seeking indicators ---------------------------------
  # Use the WMR derived indicators for survey estimation

  # Determine grouping
  if (!is.null(class_var)) {
    group_formula <- stats::as.formula(paste("~", class_var))
  } else {
    group_formula <- ~1
  }

  # Formula for svyby using WMR indicators
  indicator_formula <- ~ csb_public + csb_private +
    csb_no_treatment + csb_trained_provider

  # Calculate proportions
  if (!is.null(class_var)) {
    # Additional check for single-cluster groups when grouping by admin level
    if (class_var != "cluster_id") {
      group_check <- kr_fever |>
        dplyr::group_by(.data[[class_var]]) |>
        dplyr::summarise(
          n_clusters = dplyr::n_distinct(cluster_id),
          .groups = "drop"
        )

      single_cluster_groups <- sum(group_check$n_clusters == 1)

      if (single_cluster_groups > 0) {
        cli::cli_alert_warning(
          paste0(
            "{single_cluster_groups} admin unit(s) have only one cluster; ",
            "variance estimates may be unreliable"
          )
        )
      }
    }

    csb_results <- tryCatch({
      survey::svyby(
        indicator_formula,
        by = group_formula,
        design = des,
        FUN = survey::svymean,
        vartype = "ci",
        keep.names = FALSE
      ) |>
        tibble::as_tibble()
    }, error = function(e) {
      if (grepl("has only one PSU", e$message)) {
        cli::cli_alert_warning(
          "Single PSU issue persists; trying with adjusted survey design"
        )
        # Fall back to ignoring strata
        des_no_strata <- survey::svydesign(
          ids = ~cluster_id,
          weights = ~survey_weight,
          data = kr_fever,
          nest = TRUE
        )
        survey::svyby(
          indicator_formula,
          by = group_formula,
          design = des_no_strata,
          FUN = survey::svymean,
          vartype = "ci",
          keep.names = FALSE
        ) |>
          tibble::as_tibble()
      } else {
        stop(e)
      }
    })
  } else {
    csb_means <- survey::svymean(
      indicator_formula,
      design = des
    )
    csb_ci <- stats::confint(csb_means)

    csb_results <- tibble::tibble(
      level = "National",
      csb_public = as.numeric(csb_means["csb_public"]),
      `ci_l.csb_public` = csb_ci["csb_public", 1],
      `ci_u.csb_public` = csb_ci["csb_public", 2],
      csb_private = as.numeric(csb_means["csb_private"]),
      `ci_l.csb_private` = csb_ci["csb_private", 1],
      `ci_u.csb_private` = csb_ci["csb_private", 2],
      csb_no_treatment = as.numeric(csb_means["csb_no_treatment"]),
      `ci_l.csb_no_treatment` = csb_ci["csb_no_treatment", 1],
      `ci_u.csb_no_treatment` = csb_ci["csb_no_treatment", 2],
      csb_trained_provider = as.numeric(csb_means["csb_trained_provider"]),
      `ci_l.csb_trained_provider` = csb_ci["csb_trained_provider", 1],
      `ci_u.csb_trained_provider` = csb_ci["csb_trained_provider", 2]
    )
  }

  # ---- 8. Calculate sample sizes --------------------------------------------

  if (!is.null(class_var)) {
    sample_sizes <- kr_fever |>
      dplyr::group_by(.data[[class_var]]) |>
      dplyr::summarise(
        dhs_n_fever = dplyr::n(),
        dhs_n_public = sum(csb_public == 1, na.rm = TRUE),
        dhs_n_private = sum(csb_private == 1, na.rm = TRUE),
        dhs_n_none = sum(csb_no_treatment == 1, na.rm = TRUE),
        dhs_n_trained = sum(csb_trained_provider == 1, na.rm = TRUE),
        .groups = "drop"
      )

    csb_results <- csb_results |>
      dplyr::left_join(
        sample_sizes,
        by = class_var
      )
  } else {
    csb_results$dhs_n_fever <- nrow(kr_fever)
    csb_results$dhs_n_public <- sum(
      kr_fever$csb_public == 1, na.rm = TRUE
    )
    csb_results$dhs_n_private <- sum(
      kr_fever$csb_private == 1, na.rm = TRUE
    )
    csb_results$dhs_n_none <- sum(
      kr_fever$csb_no_treatment == 1, na.rm = TRUE
    )
    csb_results$dhs_n_trained <- sum(
      kr_fever$csb_trained_provider == 1, na.rm = TRUE
    )
  }

  # ---- 9. Format results -----------------------------------------------------

  # Rename columns to standard output format
  csb_results <- csb_results |>
    dplyr::rename(
      dhs_csb_public = csb_public,
      dhs_csb_public_low = `ci_l.csb_public`,
      dhs_csb_public_upp = `ci_u.csb_public`,
      dhs_csb_private = csb_private,
      dhs_csb_private_low = `ci_l.csb_private`,
      dhs_csb_private_upp = `ci_u.csb_private`,
      dhs_csb_none = csb_no_treatment,
      dhs_csb_none_low = `ci_l.csb_no_treatment`,
      dhs_csb_none_upp = `ci_u.csb_no_treatment`,
      dhs_csb_trained = csb_trained_provider,
      dhs_csb_trained_low = `ci_l.csb_trained_provider`,
      dhs_csb_trained_upp = `ci_u.csb_trained_provider`
    )

  # Round proportions (keep as 0-1 scale, not percentages)
  csb_cols <- names(csb_results)[grepl("^dhs_csb_", names(csb_results))]

  csb_results <- csb_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(csb_cols),
        ~ round(.x, 2)
      )
    )

  # Derive "any care" = 1 - none (by construction: any + none == 1)
  csb_results <- csb_results |>
    dplyr::mutate(
      dhs_csb_any = 1 - dhs_csb_none,
      # CI is inverted: low of "any" = 1 - high of "none"
      dhs_csb_any_low = 1 - dhs_csb_none_upp,
      dhs_csb_any_upp = 1 - dhs_csb_none_low
    )

  # Ensure confidence intervals stay within [0, 1]
  csb_results <- csb_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("_low$"),
        ~ pmax(0, .)
      ),
      dplyr::across(
        dplyr::matches("_upp$"),
        ~ pmin(1, .)
      )
    )

  # Split admin_class back into individual admin columns if needed
  if (!is.null(class_var) && class_var == "admin_class" && length(admin_level) > 1) {
    admin_splits <- stringr::str_split(
      csb_results$admin_class,
      "_",
      simplify = TRUE
    )

    for (i in seq_along(admin_level)) {
      csb_results[[admin_level[i]]] <- admin_splits[, i]
    }
  }

  # Add admin name columns if available
  if (!is.null(shapefile) && !is.null(admin_level)) {
    admin_name_cols <- paste0(admin_level, "_name")
    admin_name_cols <- admin_name_cols[
      admin_name_cols %in% names(shapefile)
    ]

    if (length(admin_name_cols) > 0) {
      admin_lookup <- sf::st_drop_geometry(shapefile) |>
        dplyr::select(
          dplyr::all_of(c(admin_level, admin_name_cols))
        ) |>
        dplyr::distinct()

      csb_results <- csb_results |>
        dplyr::left_join(
          admin_lookup,
          by = intersect(names(csb_results), admin_level)
        )
    }
  } else {
    admin_name_cols <- character(0)
  }

  # Reorder columns - simplified to essential columns only
  col_order <- c(
    region_var,
    admin_level,
    admin_name_cols,
    "dhs_n_fever",
    "dhs_csb_any",
    "dhs_csb_any_low",
    "dhs_csb_any_upp",
    "dhs_csb_public",
    "dhs_csb_public_low",
    "dhs_csb_public_upp",
    "dhs_csb_private",
    "dhs_csb_private_low",
    "dhs_csb_private_upp",
    "dhs_csb_trained",
    "dhs_csb_trained_low",
    "dhs_csb_trained_upp",
    "dhs_csb_none",
    "dhs_csb_none_low",
    "dhs_csb_none_upp"
  )

  col_order <- intersect(col_order, names(csb_results))

  # Ensure count columns are integers
  count_cols <- c(
    "dhs_n_fever", "dhs_n_public", "dhs_n_private",
    "dhs_n_none", "dhs_n_trained"
  )
  count_cols <- intersect(count_cols, names(csb_results))

  csb_results <- csb_results |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(count_cols),
        ~ as.integer(round(.x))
      )
    )

  # Exclude admin_class from final output (keep sample sizes though)
  exclude_cols <- c("admin_class")
  other_cols <- setdiff(names(csb_results), c(col_order, exclude_cols))

  csb_results <- csb_results |>
    dplyr::select(
      dplyr::all_of(c(col_order, other_cols))
    )

  tibble::as_tibble(csb_results)
}

#' Default WMR CSB classification
#'
#' Returns the default WHO World Malaria Report classification mapping
#' h32 variables to CSB categories.
#'
#' @return Data frame with columns: variable, csb
#' @noRd
.default_csb_classification <- function() {
  data.frame(
    variable = c(
      # Public sector (government facilities)
      "h32a", "h32b", "h32c", "h32d", "h32e", "h32f", "h32g", "h32h", "h32i",
      # CHW / NGO sector (DHS-8 added h32na-h32ne)
      "h32na", "h32nb", "h32nc", "h32nd", "h32ne",
      # Private formal (private hospitals, clinics, doctors)
      "h32j", "h32k", "h32l", "h32m",
      # Private informal (traditional practitioners, other)
      "h32s", "h32t", "h32u",
      # Pharmacy (pharmacies, drug shops)
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

#' Convert legacy source_config to csb_classification
#'
#' @param source_config Named list with public, private, excluded vectors
#' @return Data frame with columns: variable, csb
#' @noRd
.convert_source_config <- function(source_config) {
  result <- dplyr::bind_rows(
    if (length(source_config$public) > 0) {
      data.frame(
        variable = source_config$public,
        csb = "public",
        stringsAsFactors = FALSE
      )
    },
    if (length(source_config$private) > 0) {
      # In legacy mode, all private sources are treated as private_formal
      # This maintains backwards compatibility with the old behavior
      data.frame(
        variable = source_config$private,
        csb = "private_formal",
        stringsAsFactors = FALSE
      )
    }
  )

  if (nrow(result) == 0) {
    cli::cli_abort(
      "source_config must have at least one public or private source"
    )
  }

  result
}

#' Extract metadata from DHS KR dataset for care-seeking analysis
#'
#' Internal function to extract survey metadata from DHS children's recode
#' data. Looks for standard DHS metadata columns and extracts key survey
#' information relevant to care-seeking behavior analysis.
#'
#' @param dhs_kr DHS children's recode dataset.
#' @param survey_vars Named list of survey variable mappings.
#'
#' @return List containing survey metadata.
#' @noRd
extract_dhs_metadata_csb <- function(
  dhs_kr,
  survey_vars = NULL
) {
  metadata <- list()

  # Extract country code
  if ("v000" %in% names(dhs_kr)) {
    metadata$country_code <- unique(dhs_kr$v000)[1]
  } else if ("country_code" %in% names(dhs_kr)) {
    metadata$country_code <- unique(dhs_kr$country_code)[1]
  } else {
    metadata$country_code <- NA_character_
  }

  # Extract survey year
  if ("v007" %in% names(dhs_kr)) {
    metadata$survey_year <- unique(dhs_kr$v007)[1]
  } else if ("survey_year" %in% names(dhs_kr)) {
    metadata$survey_year <- unique(dhs_kr$survey_year)[1]
  } else {
    metadata$survey_year <- NA_integer_
  }

  # Extract survey ID
  if ("survey_id" %in% names(dhs_kr)) {
    metadata$survey_id <- unique(dhs_kr$survey_id)[1]
  } else if ("v000" %in% names(dhs_kr)) {
    metadata$survey_id <- unique(dhs_kr$v000)[1]
  } else {
    metadata$survey_id <- NA_character_
  }

  metadata$survey_type <- "DHS"
  metadata$file_type <- "KR"

  metadata$total_records <- nrow(dhs_kr)

  # Count clusters
  cluster_var <- if (!is.null(survey_vars$cluster)) {
    survey_vars$cluster
  } else {
    "v021"
  }

  if (cluster_var %in% names(dhs_kr)) {
    metadata$total_clusters <- length(
      unique(dhs_kr[[cluster_var]])
    )
  }

  # Count children with fever
  fever_var <- if (!is.null(survey_vars$fever)) {
    survey_vars$fever
  } else {
    "h22"
  }

  if (fever_var %in% names(dhs_kr)) {
    age_var <- if (!is.null(survey_vars$age)) {
      survey_vars$age
    } else {
      "hw1"
    }

    if (age_var %in% names(dhs_kr)) {
      eligible <- dhs_kr[[age_var]] >= 0 & dhs_kr[[age_var]] <= 59
      metadata$total_eligible_children <- sum(eligible, na.rm = TRUE)
      metadata$total_fever_cases <- sum(
        eligible & dhs_kr[[fever_var]] == 1,
        na.rm = TRUE
      )
    } else {
      metadata$total_fever_cases <- sum(
        dhs_kr[[fever_var]] == 1,
        na.rm = TRUE
      )
    }
  }

  metadata$processed_date <- Sys.Date()
  metadata$processed_time <- Sys.time()

  metadata$analysis_type <- "CSB (Care-Seeking Behavior)"
  metadata$methodology <- "WHO World Malaria Report (WMR)"
  metadata$age_group <- "0-59 months"
  metadata$condition <- "Fever in last 2 weeks"

  # Detect available h32 source variables
  # Pattern includes digits for DHS-8 NGO sector variables (h32na, h32nb, etc.)
  available_h32 <- grep("^h32[a-z0-9]+$", names(dhs_kr), value = TRUE)
  metadata$h32_sources_detected <- available_h32
  metadata$n_h32_sources <- length(available_h32)

  # Check if alive variable is available
  metadata$has_alive_var <- !is.null(survey_vars$alive) &&
    survey_vars$alive %in% names(dhs_kr)

  metadata$variable_mapping <- survey_vars

  metadata
}

#' Calculate Care-Seeking Behavior from DHS Data (WMR Methodology)
#'
#' Main function for calculating care-seeking behavior (CSB)
#' from DHS children's recode data following the WHO World Malaria Report
#' methodology. Supports spatial aggregation using administrative boundary
#' shapefiles to calculate CSB at any administrative level.
#' Returns both data and a data dictionary.
#'
#' This is a convenience wrapper around calc_csb_dhs_core() that also extracts
#' survey metadata and builds a data dictionary.
#'
#' @inheritParams calc_csb_dhs_core
#'
#' @return List with:
#'   \itemize{
#'     \item `data`: Tibble with CSB estimates by admin level
#'     \item `dict`: Data dictionary from sntutils::build_dictionary()
#'     \item `metadata`: List with survey metadata
#'   }
#'
#' @details
#' See calc_csb_dhs_core() for full details on the WMR methodology, including:
#' \itemize{
#'   \item The 5-category classification system
#'   \item How derived indicators are calculated
#'   \item How to configure country-specific source mappings
#' }
#'
#' @examples
#' # Example with default WMR classification
#' # csb_results <- calc_csb_dhs(
#' #   dhs_kr = kr_data,
#' #   gps_data = gps_data,
#' #   shapefile = admin_shapefile,
#' #   admin_level = c("adm1")
#' # )
#' #
#' # # Example with custom classification (country-specific)
#' # my_classification <- data.frame(
#' #   variable = c("h32a", "h32b", "h32c", "h32j", "h32k", "h32n"),
#' #   csb = c("public", "public", "chw",
#' #           "private_formal", "pharmacy", "pharmacy")
#' # )
#' # csb_results <- calc_csb_dhs(
#' #   dhs_kr = kr_data,
#' #   csb_classification = my_classification
#' # )
#' #
#' # # Access the data
#' # csb_data <- csb_results$data
#' #
#' # # Access the dictionary
#' # csb_dict <- csb_results$dict
#' #
#' # # Access the metadata
#' # csb_metadata <- csb_results$metadata
#'
#' @export
calc_csb_dhs <- function(
  dhs_kr,
  survey_vars = list(
    cluster = "v021",
    weight = "v005",
    stratum = "v022",
    age = "hw1",
    fever = "h22",
    alive = "b5"
  ),
  csb_classification = NULL,
  source_config = NULL,
  region_var = NULL,
  gps_data = NULL,
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  ),
  shapefile = NULL,
  admin_level = NULL,
  join_nearest = TRUE
) {
  # Extract metadata
  metadata <- extract_dhs_metadata_csb(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars
  )

  # Calculate CSB using core function
  csb_data <- calc_csb_dhs_core(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    csb_classification = csb_classification,
    source_config = source_config,
    region_var = region_var,
    gps_data = gps_data,
    gps_vars = gps_vars,
    shapefile = shapefile,
    admin_level = admin_level,
    join_nearest = join_nearest
  )

  # Return list with data, dictionary, and metadata
  list(
    data = csb_data,
    dict = sntutils::build_dictionary(csb_data),
    metadata = metadata
  )
}

#' Aggregate CSB to administrative levels
#'
#' Helper to aggregate CSB results to administrative levels using a shapefile.
#' Performs spatial joins and calculates weighted averages by administrative
#' unit.
#'
#' @param csb_results CSB results with coordinates.
#' @param shapefile sf object with administrative boundaries.
#' @param admin_level Character vector of admin levels to aggregate to.
#' @param weighted Logical. If TRUE (default), uses sample size as weights.
#'
#' @return sf object with aggregated CSB by administrative level.
#'
#' @export
aggregate_csb_admin <- function(
  csb_results,
  shapefile,
  admin_level = c("adm1"),
  weighted = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort("Package 'sf' is required for spatial operations")
  }

  # Convert to sf if needed
  if (!inherits(csb_results, "sf")) {
    if (!all(c("lat", "lon") %in% names(csb_results))) {
      cli::cli_abort(
        "csb_results must have lat and lon columns for spatial join"
      )
    }

    csb_sf <- csb_results |>
      sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326,
        remove = FALSE
      )
  } else {
    csb_sf <- csb_results
  }

  # Prepare shapefile
  shapefile <- shapefile |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  # Spatial join
  joined <- sf::st_join(
    csb_sf,
    shapefile[, c(admin_level, "geometry")],
    join = sf::st_within,
    left = TRUE
  )

  # Handle unmatched clusters
  unmatched <- is.na(joined[[admin_level[1]]])

  if (any(unmatched)) {
    nearest_idx <- sf::st_nearest_feature(
      joined[unmatched, ],
      shapefile
    )

    for (col in admin_level) {
      joined[unmatched, col] <- shapefile[[col]][nearest_idx]
    }
  }

  # Convert to data frame for aggregation
  joined_df <- sf::st_drop_geometry(joined)

  # Aggregate
  if (weighted && "dhs_n_fever" %in% names(joined_df)) {
    # Weighted aggregation
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(admin_level)
        )
      ) |>
      dplyr::summarise(
        dhs_csb_any = if ("dhs_csb_any" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_any,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_csb_public = if ("dhs_csb_public" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_public,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_csb_private = if ("dhs_csb_private" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_private,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_csb_trained = if ("dhs_csb_trained" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_trained,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_csb_none = if ("dhs_csb_none" %in% names(joined_df)) {
          stats::weighted.mean(
            dhs_csb_none,
            w = dhs_n_fever,
            na.rm = TRUE
          )
        } else NA_real_,
        dhs_n_fever = sum(
          dhs_n_fever,
          na.rm = TRUE
        ),
        dhs_n_public = if ("dhs_n_public" %in% names(joined_df)) {
          sum(dhs_n_public, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_private = if ("dhs_n_private" %in% names(joined_df)) {
          sum(dhs_n_private, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_trained = if ("dhs_n_trained" %in% names(joined_df)) {
          sum(dhs_n_trained, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_none = if ("dhs_n_none" %in% names(joined_df)) {
          sum(dhs_n_none, na.rm = TRUE)
        } else NA_integer_,
        .groups = "drop"
      )
  } else {
    # Simple average
    aggregated <- joined_df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(admin_level)
        )
      ) |>
      dplyr::summarise(
        dhs_csb_any = if ("dhs_csb_any" %in% names(joined_df)) {
          mean(dhs_csb_any, na.rm = TRUE)
        } else NA_real_,
        dhs_csb_public = if ("dhs_csb_public" %in% names(joined_df)) {
          mean(dhs_csb_public, na.rm = TRUE)
        } else NA_real_,
        dhs_csb_private = if ("dhs_csb_private" %in% names(joined_df)) {
          mean(dhs_csb_private, na.rm = TRUE)
        } else NA_real_,
        dhs_csb_trained = if ("dhs_csb_trained" %in% names(joined_df)) {
          mean(dhs_csb_trained, na.rm = TRUE)
        } else NA_real_,
        dhs_csb_none = if ("dhs_csb_none" %in% names(joined_df)) {
          mean(dhs_csb_none, na.rm = TRUE)
        } else NA_real_,
        dhs_n_fever = sum(
          dhs_n_fever,
          na.rm = TRUE
        ),
        dhs_n_public = if ("dhs_n_public" %in% names(joined_df)) {
          sum(dhs_n_public, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_private = if ("dhs_n_private" %in% names(joined_df)) {
          sum(dhs_n_private, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_trained = if ("dhs_n_trained" %in% names(joined_df)) {
          sum(dhs_n_trained, na.rm = TRUE)
        } else NA_integer_,
        dhs_n_none = if ("dhs_n_none" %in% names(joined_df)) {
          sum(dhs_n_none, na.rm = TRUE)
        } else NA_integer_,
        .groups = "drop"
      )
  }

  # Round percentages
  aggregated <- aggregated |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("dhs_csb_"),
        ~ round(.x, 1)
      )
    )

  # Detect and preserve admin name columns
  admin_name_cols <- paste0(admin_level, "_name")
  admin_name_cols <- admin_name_cols[
    admin_name_cols %in% names(shapefile)
  ]
  all_admin_cols <- c(admin_level, admin_name_cols)

  # Join back with shapefile geometry
  result_with_geometry <- shapefile |>
    dplyr::select(
      dplyr::all_of(all_admin_cols)
    ) |>
    dplyr::distinct() |>
    dplyr::left_join(
      aggregated,
      by = admin_level
    )

  result_with_geometry
}
