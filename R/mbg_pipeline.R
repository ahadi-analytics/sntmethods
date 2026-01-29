#' MBG Indicator Pipeline
#'
#' Main orchestration function for processing DHS indicators through MBG
#' and generating outputs for SNT analysis.
#'
#' @name mbg_pipeline
#' @keywords internal
NULL


#' Run MBG Indicator Pipeline
#'
#' Orchestrates the full MBG processing pipeline for DHS indicators:
#' 1. Discovers available surveys
#' 2. Loads survey data
#' 3. Prepares cluster-level data for each indicator
#' 4. Runs MBG models (if enabled)
#' 5. Generates outputs (rasters, CSVs, maps)
#'
#' @param country_iso3 Three-letter ISO country code (e.g., "bdi" for Burundi).
#' @param country_iso2 Two-letter DHS country code (e.g., "BU" for Burundi).
#'   If NULL (default), derived automatically from `country_iso3` using
#'   the `countrycode` package.
#' @param adm0_sf sf object with country boundary.
#' @param adm1_sf sf object with ADM1 boundaries.
#' @param adm2_sf sf object with ADM2 boundaries.
#' @param pop_rasters Population raster input. Can be one of:
#'   \itemize{
#'     \item Named list of file paths to TIF files, with years as names
#'       (e.g., `list("2016" = "path/to/pop_2016.tif")`)
#'     \item Named list of already-loaded SpatRaster objects
#'     \item Single directory path containing TIF files with years in filenames
#'       (function will search for files matching `*{year}*.tif`)
#'   }
#' @param path_dhs_parquet Path to DHS parquet archive.
#' @param path_output Output directory path.
#' @param survey_year Survey year(s) to process. Can be:
#'   \itemize{
#'     \item NULL: Process ALL available surveys with GPS data
#'     \item Single integer: Process only that year (e.g., 2016)
#'     \item Integer vector: Process specific years (e.g., c(2012, 2016))
#'   }
#' @param survey_type Survey type ("DHS" or "MIS"). Default: "DHS".
#' @param indicators Character vector of indicator categories to process:
#'   \itemize{
#'     \item "pfpr": Parasite prevalence
#'     \item "itn": ITN ownership/access/use
#'     \item "irs": IRS coverage
#'     \item "anc": ANC attendance
#'     \item "csb": Care-seeking behavior
#'     \item "anemia": Anemia prevalence
#'     \item "iptp": IPTp doses
#'     \item "epi": EPI vaccination
#'     \item "u5mr": Under-5 mortality
#'     \item "smc": SMC receipt
#'   }
#' @param run_mbg Logical. If TRUE, runs MBG models. Default: TRUE.
#' @param save_rasters Logical. If TRUE, saves output rasters. Default: TRUE.
#' @param generate_maps Logical. If TRUE, generates maps. Default: TRUE.
#' @param verbose Logical. If TRUE, prints detailed progress. Default: TRUE.
#'
#' @return A list containing:
#'   \itemize{
#'     \item final_dataset: Combined ADM2 dataset with all indicators
#'     \item adm1_estimates: Survey-weighted ADM1 estimates (if available)
#'     \item mbg_estimates: MBG ADM2 predictions (if MBG was run)
#'     \item cluster_data: Raw cluster-level data
#'     \item raster_paths: Paths to saved rasters
#'     \item survey_metadata: Survey collection dates and metadata
#'   }
#'
#' @examples
#' \dontrun{
#' results <- run_mbg_indicator_pipeline(
#'   country_iso3 = "bdi",
#'   adm0_sf = adm0,
#'   adm1_sf = adm1,
#'   adm2_sf = adm2,
#'   pop_rasters = list("2016" = "/path/to/bdi_ppp_2016.tif"),
#'   path_dhs_parquet = "path/to/parquet",
#'   path_output = "path/to/output",
#'   survey_year = 2016,
#'   indicators = c("pfpr", "itn", "csb")
#' )
#' }
#'
#' @export
run_mbg_indicator_pipeline <- function(
  country_iso3,
  adm0_sf,
  adm1_sf,
  adm2_sf,
  pop_rasters,
  path_dhs_parquet,
  path_output,
  country_iso2 = NULL,
  survey_year = NULL,
  survey_type = "DHS",
  indicators = c("pfpr", "itn", "irs", "anc", "csb", "anemia", "iptp"),
  run_mbg = TRUE,
  save_rasters = TRUE,
  generate_maps = TRUE,
  verbose = TRUE
) {

  # Check for required spatial packages
  .check_spatial_pkg("mbg", "run_mbg_indicator_pipeline")
  .check_spatial_pkg("sf", "run_mbg_indicator_pipeline")
  .check_spatial_pkg("terra", "run_mbg_indicator_pipeline")
  .check_spatial_pkg("fs", "run_mbg_indicator_pipeline")
  .check_spatial_pkg("countrycode", "run_mbg_indicator_pipeline")

  # ---- Setup ----

  cli::cli_h1("MBG Indicator Pipeline: {toupper(country_iso3)}")

  # Derive DHS country code from ISO3 if not provided
  if (is.null(country_iso2)) {
    country_iso2 <- .get_dhs_country_code(country_iso3)
    cli::cli_alert_info(
      "Derived DHS country code: {.val {country_iso2}} from ISO3: {.val {country_iso3}}"
    )
  }

  fs::dir_create(path_output)

  results <- list(
    final_dataset = NULL,
    adm1_estimates = list(),
    mbg_estimates = list(),
    cluster_data = list(),
    raster_paths = list(),
    survey_metadata = list()
  )

  # ---- Find surveys using dhs_read() ----

  cli::cli_h2("Discovering available surveys")

  # Use dhs_read to find available survey years with GPS data
  gps_check <- tryCatch({
    dhs_read(
      path = path_dhs_parquet,
      file_type = "GE",
      survey_type = survey_type,
      country_code = country_iso2
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "Could not query DHS parquet archive for GPS data",
      "i" = "Error: {e$message}"
    ))
  })

  if (is.null(gps_check) || nrow(gps_check) == 0) {
    cli::cli_abort("No surveys with GPS data found for {country_iso2}")
  }

  # Extract available years from DHSYEAR column
  available_years <- gps_check |>
    dplyr::pull(DHSYEAR) |>
    unique() |>
    sort()

  cli::cli_alert_success(
    "Found GPS data for {length(available_years)} survey(s): {paste(available_years, collapse = ', ')}"
  )

  # Determine which years to process
  if (is.null(survey_year)) {
    # Process ALL available surveys with GPS
    years_to_process <- available_years
    cli::cli_alert_info(
      "Processing ALL {length(years_to_process)} survey(s)"
    )
  } else {
    # Process specific year(s)
    years_to_process <- as.integer(survey_year)

    # Validate requested years exist
    missing_years <- setdiff(years_to_process, available_years)
    if (length(missing_years) > 0) {
      cli::cli_abort(c(
        "Requested survey year(s) not found with GPS data: {paste(missing_years, collapse = ', ')}",
        "i" = "Available years with GPS: {paste(available_years, collapse = ', ')}"
      ))
    }

    cli::cli_alert_info(
      "Processing {length(years_to_process)} survey(s): {paste(years_to_process, collapse = ', ')}"
    )
  }

  # Determine which file types we need based on indicators
  file_types_needed <- c("GE")  # Always need GPS

  file_type_map <- list(
    pfpr = "PR",
    itn = c("HR", "PR"),
    irs = "HR",
    anc = "IR",
    csb = "KR",
    anemia = "PR",
    iptp = "IR",
    epi = "KR",
    u5mr = "BR",
    smc = "KR"
  )

  for (ind in indicators) {
    if (ind %in% names(file_type_map)) {
      file_types_needed <- c(file_types_needed, file_type_map[[ind]])
    }
  }

  file_types_needed <- unique(file_types_needed)

  cli::cli_alert_info(
    "Required file types: {paste(file_types_needed, collapse = ', ')}"
  )

  # Store metadata
  results$survey_metadata$country <- country_iso2
  results$survey_metadata$country_iso3 <- country_iso3
  results$survey_metadata$survey_type <- survey_type
  results$survey_metadata$years_processed <- years_to_process
  results$survey_metadata$available_years <- available_years

  # ---- Process each survey year ----

  all_year_results <- list()

  for (current_year in years_to_process) {
    cli::cli_h2("Processing survey year: {current_year}")

    year_key <- as.character(current_year)

    # ---- Load survey data for this year using dhs_read() ----

    cli::cli_alert_info("Loading survey data for {current_year}...")

    survey_data <- list()

    for (ft in file_types_needed) {
      tryCatch({
        ft_data <- dhs_read(
          path = path_dhs_parquet,
          file_type = ft,
          survey_type = survey_type,
          country_code = country_iso2,
          survey_year = current_year
        )
        if (!is.null(ft_data) && nrow(ft_data) > 0) {
          survey_data[[ft]] <- ft_data
          cli::cli_alert_info(
            "  {ft}: {format(nrow(ft_data), big.mark = ',')} records"
          )
        }
      }, error = function(e) {
        cli::cli_alert_warning("Could not load {ft}: {e$message}")
      })
    }

    if (length(survey_data) == 0) {
      cli::cli_alert_danger("No data could be loaded for {current_year}")
      cli::cli_alert_warning("Skipping year {current_year} due to load failure")
      next
    }

    # Report what was loaded
    loaded_types <- names(survey_data)
    cli::cli_alert_success(
      "Loaded {length(loaded_types)} file types: {paste(loaded_types, collapse = ', ')}"
    )

    # ---- Get population raster for this year ----

    pop_rast <- tryCatch({
      .load_population_raster(
        pop_rasters = pop_rasters,
        target_year = current_year,
        country_iso3 = country_iso3
      )
    }, error = function(e) {
      cli::cli_alert_warning(
        "Could not load population raster for {current_year}: {e$message}"
      )
      NULL
    })

    # ---- Align CRS ----

    if (!is.null(pop_rast)) {
      crs_master <- terra::crs(pop_rast)
      adm0_aligned <- sf::st_transform(adm0_sf, crs_master)
      adm1_aligned <- sf::st_transform(adm1_sf, crs_master)
      adm2_aligned <- sf::st_transform(adm2_sf, crs_master)
    } else {
      # Use a default CRS if no population raster
      adm0_aligned <- adm0_sf
      adm1_aligned <- adm1_sf
      adm2_aligned <- adm2_sf
    }

    # ---- Process indicators for this year ----

    gps_data <- survey_data$GE

    # Validate GPS data is available
    if (is.null(gps_data) || nrow(gps_data) == 0) {
      cli::cli_alert_danger(
        "GPS data (GE file) not loaded or empty for {current_year}"
      )
      cli::cli_alert_warning("Skipping year {current_year} - GPS data required")
      next
    }

    year_results <- list(
      cluster_data = list(),
      mbg_estimates = list(),
      raster_paths = list()
    )

    for (ind_category in indicators) {
      cli::cli_h3("Processing: {ind_category}")

      tryCatch({
        ind_results <- .process_indicator_category(
          category = ind_category,
          survey_data = survey_data,
          gps_data = gps_data,
          adm0_sf = adm0_aligned,
          adm1_sf = adm1_aligned,
          adm2_sf = adm2_aligned,
          pop_rast = pop_rast,
          path_output = path_output,
          country_iso3 = country_iso3,
          survey_year = current_year,
          run_mbg = run_mbg,
          save_rasters = save_rasters,
          generate_maps = generate_maps,
          verbose = verbose
        )

        # Store results for this indicator
        for (name in names(ind_results$cluster_data)) {
          year_results$cluster_data[[name]] <- ind_results$cluster_data[[name]]
        }

        for (name in names(ind_results$mbg_estimates)) {
          year_results$mbg_estimates[[name]] <- ind_results$mbg_estimates[[name]]
        }

        for (name in names(ind_results$raster_paths)) {
          year_results$raster_paths[[name]] <- ind_results$raster_paths[[name]]
        }

      }, error = function(e) {
        cli::cli_alert_danger(
          "Failed to process {ind_category} for {current_year}: {e$message}"
        )
      })
    }

    # ---- Build dataset for this year ----

    year_dataset <- .build_final_dataset(
      adm2_sf = adm2_aligned,
      mbg_estimates = year_results$mbg_estimates,
      cluster_data = year_results$cluster_data,
      survey_year = current_year,
      country_iso3 = country_iso3,
      country_iso2 = country_iso2
    )

    # Apply smart rounding
    year_dataset <- .round_mbg_output(year_dataset)

    # ---- Save outputs for this year ----

    output_basename <- glue::glue("{country_iso3}_dhs_mbg_indicators_{current_year}")

    sntutils::write_snt_data(
      data = year_dataset,
      path = path_output,
      name = output_basename,
      formats = c("csv", "xlsx")
    )
    cli::cli_alert_success("Saved: {.file {output_basename}}")

    # Store results for this year
    all_year_results[[year_key]] <- list(
      dataset = year_dataset,
      cluster_data = year_results$cluster_data,
      mbg_estimates = year_results$mbg_estimates,
      raster_paths = year_results$raster_paths
    )

    # Also add to main results structure
    results$cluster_data[[year_key]] <- year_results$cluster_data
    results$mbg_estimates[[year_key]] <- year_results$mbg_estimates
    results$raster_paths[[year_key]] <- year_results$raster_paths

  }  # End loop over years

  # ---- Build combined final dataset ----

  cli::cli_h2("Building combined dataset")

  # Combine all years into one dataset with year column
  combined_datasets <- lapply(names(all_year_results), function(yr) {
    df <- all_year_results[[yr]]$dataset
    if (!is.null(df) && nrow(df) > 0) {
      df$survey_year <- as.integer(yr)
      df
    } else {
      NULL
    }
  })

  combined_datasets <- combined_datasets[!sapply(combined_datasets, is.null)]

  if (length(combined_datasets) > 0) {
    results$final_dataset <- dplyr::bind_rows(combined_datasets)

    # Apply smart rounding to combined dataset
    results$final_dataset <- .round_mbg_output(results$final_dataset)

    # Save combined dataset
    if (length(years_to_process) > 1) {
      combined_basename <- glue::glue("{country_iso3}_dhs_mbg_indicators_combined")

      sntutils::write_snt_data(
        data = results$final_dataset,
        path = path_output,
        name = combined_basename,
        formats = c("csv", "xlsx")
      )
      cli::cli_alert_success("Saved combined dataset: {.file {combined_basename}}")
    }
  } else {
    results$final_dataset <- data.frame()
    cli::cli_alert_warning("No data could be processed")
  }

  cli::cli_alert_success(
    "Pipeline complete! Processed {length(years_to_process)} survey(s)"
  )

  results
}


#' Process Single Indicator Category
#'
#' Internal function to process one indicator category.
#'
#' @noRd
.process_indicator_category <- function(
  category,
  survey_data,
  gps_data,
  adm0_sf,
  adm1_sf,
  adm2_sf,
  pop_rast,
  path_output,
  country_iso3,
  survey_year,
  run_mbg,
  save_rasters,
  generate_maps,
  verbose
) {
  results <- list(
    cluster_data = list(),
    mbg_estimates = list(),
    raster_paths = list()
  )

  # Call appropriate MBG prep function based on category
  cluster_data <- switch(category,
    pfpr = {
      if (!"PR" %in% names(survey_data)) {
        cli::cli_alert_warning("PR data not available for PfPR")
        return(results)
      }
      tryCatch({
        calc_pfpr_mbg(
          dhs_pr = survey_data$PR,
          gps_data = gps_data,
          test_type = "both",
          age_groups = list(u5 = c(6, 59))
        )
      }, error = function(e) {
        cli::cli_alert_warning("PfPR calculation failed: {e$message}")
        list()
      })
    },

    itn = {
      if (!all(c("HR", "PR") %in% names(survey_data))) {
        cli::cli_alert_warning("HR and PR data required for ITN")
        return(results)
      }
      tryCatch({
        calc_itn_mbg(
          dhs_hr = survey_data$HR,
          dhs_pr = survey_data$PR,
          gps_data = gps_data,
          indicators = c("access", "use_u5")
        )
      }, error = function(e) {
        cli::cli_alert_warning("ITN calculation failed: {e$message}")
        list()
      })
    },

    irs = {
      if (!"HR" %in% names(survey_data)) {
        cli::cli_alert_warning("HR data not available for IRS")
        return(results)
      }
      tryCatch({
        irs_result <- calc_irs_mbg(
          dhs_hr = survey_data$HR,
          gps_data = gps_data
        )
        if (!is.null(irs_result) && nrow(irs_result) > 0) {
          list(irs_coverage = irs_result)
        } else {
          list()
        }
      }, error = function(e) {
        cli::cli_alert_warning("IRS variable not found: {e$message}")
        list()
      })
    },

    anc = {
      if (!"IR" %in% names(survey_data)) {
        cli::cli_alert_warning("IR data not available for ANC")
        return(results)
      }
      tryCatch({
        calc_anc_mbg(
          dhs_ir = survey_data$IR,
          gps_data = gps_data,
          indicators = c("anc1", "anc4")
        )
      }, error = function(e) {
        cli::cli_alert_warning("ANC calculation failed: {e$message}")
        list()
      })
    },

    csb = {
      if (!"KR" %in% names(survey_data)) {
        cli::cli_alert_warning("KR data not available for CSB")
        return(results)
      }
      tryCatch({
        calc_csb_mbg(
          dhs_kr = survey_data$KR,
          gps_data = gps_data,
          indicators = c("public", "private", "none")
        )
      }, error = function(e) {
        cli::cli_alert_warning("CSB calculation failed: {e$message}")
        list()
      })
    },

    anemia = {
      if (!"PR" %in% names(survey_data)) {
        cli::cli_alert_warning("PR data not available for anemia")
        return(results)
      }
      tryCatch({
        calc_anemia_mbg(
          dhs_pr = survey_data$PR,
          gps_data = gps_data,
          indicators = c("any", "moderate_plus", "severe")
        )
      }, error = function(e) {
        cli::cli_alert_warning("Anemia variable not found: {e$message}")
        list()
      })
    },

    iptp = {
      if (!"IR" %in% names(survey_data)) {
        cli::cli_alert_warning("IR data not available for IPTp")
        return(results)
      }
      tryCatch({
        calc_iptp_mbg(
          dhs_ir = survey_data$IR,
          gps_data = gps_data,
          indicators = c("1plus", "2plus", "3plus")
        )
      }, error = function(e) {
        cli::cli_alert_warning("IPTp variable not found: {e$message}")
        list()
      })
    },

    epi = {
      if (!"KR" %in% names(survey_data)) {
        cli::cli_alert_warning("KR data not available for EPI")
        return(results)
      }
      tryCatch({
        calc_epi_mbg(
          dhs_kr = survey_data$KR,
          gps_data = gps_data,
          indicators = c("bcg", "dpt3", "measles1")
        )
      }, error = function(e) {
        cli::cli_alert_warning("EPI calculation failed: {e$message}")
        list()
      })
    },

    u5mr = {
      if (!"BR" %in% names(survey_data)) {
        cli::cli_alert_warning("BR data not available for U5MR")
        return(results)
      }
      tryCatch({
        calc_u5mr_mbg(
          dhs_br = survey_data$BR,
          gps_data = gps_data
        )
      }, error = function(e) {
        cli::cli_alert_warning("U5MR calculation failed: {e$message}")
        list()
      })
    },

    smc = {
      if (!"KR" %in% names(survey_data)) {
        cli::cli_alert_warning("KR data not available for SMC")
        return(results)
      }
      tryCatch({
        smc_result <- calc_smc_mbg(
          dhs_kr = survey_data$KR,
          gps_data = gps_data
        )
        if (!is.null(smc_result) && nrow(smc_result) > 0) {
          list(smc_receipt = smc_result)
        } else {
          list()
        }
      }, error = function(e) {
        cli::cli_alert_warning("SMC variable not found: {e$message}")
        list()
      })
    },

    {
      cli::cli_alert_warning("Unknown indicator category: {category}")
      list()
    }
  )

  if (length(cluster_data) == 0) {
    return(results)
  }

  # Store cluster data
  for (name in names(cluster_data)) {
    results$cluster_data[[name]] <- cluster_data[[name]]
  }

  # Run MBG if enabled
  if (run_mbg && requireNamespace("mbg", quietly = TRUE)) {
    for (ind_name in names(cluster_data)) {
      cli::cli_alert_info("Running MBG for {ind_name}...")

      mbg_result <- tryCatch({
        .run_single_mbg(
          cluster_dt = cluster_data[[ind_name]],
          adm2_sf = adm2_sf,
          pop_rast = pop_rast,
          indicator_name = ind_name,
          path_output = path_output,
          country_iso3 = country_iso3,
          survey_year = survey_year,
          save_rasters = save_rasters
        )
      }, error = function(e) {
        cli::cli_alert_warning("MBG failed for {ind_name}: {e$message}")
        NULL
      })

      if (!is.null(mbg_result)) {
        results$mbg_estimates[[ind_name]] <- mbg_result$adm2_estimates
        if (save_rasters && !is.null(mbg_result$raster_path)) {
          results$raster_paths[[ind_name]] <- mbg_result$raster_path
        }
      }
    }
  }

  results
}


#' Run Single MBG Model
#'
#' @noRd
.run_single_mbg <- function(
  cluster_dt,
  adm2_sf,
  pop_rast,
  indicator_name,
  path_output,
  country_iso3,
  survey_year,
  save_rasters
) {
  # Build ID raster
  adm2_vect <- terra::vect(adm2_sf)

  id_raster <- mbg::build_id_raster(
    polygons = adm2_vect,
    template_raster = pop_rast
  )

  # Intercept-only model
  covariates <- list(
    intercept = terra::setValues(id_raster, 1)
  )

  # Build aggregation table
  aggregation_table <- mbg::build_aggregation_table(
    polygons = adm2_vect,
    id_raster = id_raster,
    polygon_id_field = "adm2",
    verbose = FALSE
  )

  # Run MBG
  model_runner <- mbg::MbgModelRunner$new(
    input_data = cluster_dt,
    id_raster = id_raster,
    covariate_rasters = covariates,
    aggregation_table = aggregation_table,
    aggregation_levels = list(adm2 = c("adm2", "adm1", "adm0")),
    population_raster = pop_rast
  )

  model_runner$run_mbg_pipeline()

  # Extract predictions
  cell_preds <- model_runner$grid_cell_predictions

  # Save rasters
  raster_path <- NULL
  if (save_rasters) {
    raster_path <- fs::path(
      path_output,
      glue::glue("{country_iso3}_{indicator_name}_mbg_{survey_year}_mean.tif")
    )

    terra::writeRaster(
      cell_preds$cell_pred_mean,
      raster_path,
      overwrite = TRUE
    )
  }

  # Extract ADM2 estimates
  adm2_estimates <- adm2_sf |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      !!paste0(indicator_name, "_mean") := terra::extract(
        cell_preds$cell_pred_mean, adm2_sf, fun = mean, na.rm = TRUE
      )[[2]],
      !!paste0(indicator_name, "_lower") := terra::extract(
        cell_preds$cell_pred_lower, adm2_sf, fun = mean, na.rm = TRUE
      )[[2]],
      !!paste0(indicator_name, "_upper") := terra::extract(
        cell_preds$cell_pred_upper, adm2_sf, fun = mean, na.rm = TRUE
      )[[2]]
    )

  list(
    adm2_estimates = adm2_estimates,
    raster_path = raster_path,
    cell_predictions = cell_preds
  )
}


#' Build Final Dataset
#'
#' @noRd
.build_final_dataset <- function(
  adm2_sf,
  mbg_estimates,
  cluster_data,
  survey_year,
  country_iso3 = NULL,
  country_iso2 = NULL
) {
  # Start with ADM2 base
  final <- adm2_sf |>
    sf::st_drop_geometry() |>
    tibble::as_tibble()

  # ---- Add standard identifier columns ----

  # Add country codes if provided
  if (!is.null(country_iso3)) {
    final$iso3 <- toupper(country_iso3)
  }

  if (!is.null(country_iso2)) {
    final$iso2 <- toupper(country_iso2)
  }

  # Ensure adm0, adm1, adm2 columns exist (try common names)
  if (!"adm0" %in% names(final)) {
    # Try to find adm0 from other column names
    adm0_candidates <- c("ADM0_NAME", "ADMIN0", "country", "Country", "NAME_0")
    for (col in adm0_candidates) {
      if (col %in% names(final)) {
        final$adm0 <- final[[col]]
        break
      }
    }
  }

  if (!"adm1" %in% names(final)) {
    # Try to find adm1 from other column names
    adm1_candidates <- c("ADM1_NAME", "ADMIN1", "province", "Province", "NAME_1", "region", "Region")
    for (col in adm1_candidates) {
      if (col %in% names(final)) {
        final$adm1 <- final[[col]]
        break
      }
    }
  }

  if (!"adm2" %in% names(final)) {
    # Try to find adm2 from other column names
    adm2_candidates <- c("ADM2_NAME", "ADMIN2", "district", "District", "NAME_2")
    for (col in adm2_candidates) {
      if (col %in% names(final)) {
        final$adm2 <- final[[col]]
        break
      }
    }
  }

  # Add survey year
  final$survey_year <- survey_year

  # ---- Merge MBG estimates ----

  # Warn if adm2 column not found in final dataset

  if (!"adm2" %in% names(final)) {
    cli::cli_alert_warning(
      "Column 'adm2' not found in ADM2 shapefile - MBG estimates cannot be merged"
    )
  }

  for (name in names(mbg_estimates)) {
    est <- mbg_estimates[[name]]

    if (!is.null(est) && nrow(est) > 0) {
      # Find columns to merge (exclude admin columns already present)
      merge_cols <- setdiff(names(est), names(final))

      if ("adm2" %in% names(est) && "adm2" %in% names(final)) {
        final <- final |>
          dplyr::left_join(
            est |> dplyr::select(adm2, dplyr::all_of(merge_cols)),
            by = "adm2"
          )
      } else if (!"adm2" %in% names(est)) {
        cli::cli_alert_warning(
          "MBG estimates for '{name}' missing 'adm2' column - skipping merge"
        )
      }
    }
  }

  # ---- Reorder columns ----

  # Put identifier columns first
  id_cols <- c("iso3", "iso2", "adm0", "adm1", "adm2", "survey_year")
  id_cols_present <- intersect(id_cols, names(final))
  other_cols <- setdiff(names(final), id_cols_present)

  final <- final |>
    dplyr::select(dplyr::all_of(id_cols_present), dplyr::all_of(other_cols))

  final
}


#' Load Population Raster
#'
#' Internal function to load population raster from various input formats.
#'
#' @param pop_rasters Input in one of several formats (see details).
#' @param target_year Target year to load.
#' @param country_iso3 Country code (used for searching directory).
#'
#' @return A SpatRaster object.
#'
#' @details
#' Supports three input formats:
#' 1. Named list of file paths (strings) - loads the TIF file for matching year
#' 2. Named list of SpatRaster objects - returns the raster for matching year
#' 3. Single directory path - searches for TIF files containing the year
#'
#' @noRd
.load_population_raster <- function(
  pop_rasters,
  target_year,
  country_iso3 = NULL
) {
  target_year_char <- as.character(target_year)

  # Case 1: Single directory path
  if (is.character(pop_rasters) && length(pop_rasters) == 1 && dir.exists(pop_rasters)) {
    cli::cli_alert_info("Searching for population raster in directory: {.path {pop_rasters}}")

    # Search for TIF files containing the year
    all_tifs <- list.files(
      pop_rasters,
      pattern = "\\.tif$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )

    if (length(all_tifs) == 0) {
      cli::cli_abort("No TIF files found in {.path {pop_rasters}}")
    }

    # First try exact year match
    year_pattern <- paste0("_", target_year, "[_.]|", target_year, "\\.tif$")
    matching_files <- all_tifs[grepl(year_pattern, all_tifs)]

    # If country code provided, filter further
    if (!is.null(country_iso3) && length(matching_files) > 1) {
      country_matches <- matching_files[grepl(country_iso3, matching_files, ignore.case = TRUE)]
      if (length(country_matches) > 0) {
        matching_files <- country_matches
      }
    }

    if (length(matching_files) == 0) {
      # Find closest year
      year_matches <- regmatches(all_tifs, regexpr("[0-9]{4}", all_tifs))
      available_years <- as.integer(unique(year_matches[nchar(year_matches) == 4]))
      available_years <- available_years[!is.na(available_years)]

      if (length(available_years) == 0) {
        cli::cli_abort("Could not identify years from TIF filenames in {.path {pop_rasters}}")
      }

      closest_year <- available_years[which.min(abs(available_years - target_year))]
      cli::cli_alert_warning(
        "No raster found for {target_year}, using closest year: {closest_year}"
      )

      year_pattern <- paste0("_", closest_year, "[_.]|", closest_year, "\\.tif$")
      matching_files <- all_tifs[grepl(year_pattern, all_tifs)]

      if (!is.null(country_iso3) && length(matching_files) > 1) {
        country_matches <- matching_files[grepl(country_iso3, matching_files, ignore.case = TRUE)]
        if (length(country_matches) > 0) {
          matching_files <- country_matches
        }
      }
    }

    if (length(matching_files) == 0) {
      cli::cli_abort("Could not find population raster for year {target_year}")
    }

    # Use first match if multiple
    raster_path <- matching_files[1]
    cli::cli_alert_success("Loading population raster: {.file {basename(raster_path)}}")

    return(terra::rast(raster_path))
  }

  # Case 2 & 3: Named list (either paths or rasters)
  if (is.list(pop_rasters)) {
    # Determine which year to use
    available_years <- as.integer(names(pop_rasters))

    if (target_year_char %in% names(pop_rasters)) {
      selected_year <- target_year_char
    } else {
      # Find closest year
      closest_year <- available_years[which.min(abs(available_years - target_year))]
      selected_year <- as.character(closest_year)
      cli::cli_alert_warning(
        "No raster for {target_year}, using closest year: {closest_year}"
      )
    }

    raster_input <- pop_rasters[[selected_year]]

    # Check if it's a path (string) or already a raster
    if (is.character(raster_input)) {
      # It's a file path - load it
      if (!file.exists(raster_input)) {
        cli::cli_abort("Population raster file not found: {.path {raster_input}}")
      }
      cli::cli_alert_success("Loading population raster: {.file {basename(raster_input)}}")
      return(terra::rast(raster_input))
    } else if (inherits(raster_input, "SpatRaster")) {
      # It's already a raster
      cli::cli_alert_success("Using pre-loaded population raster for {selected_year}")
      return(raster_input)
    } else {
      cli::cli_abort(
        "Invalid pop_rasters format. Expected file path or SpatRaster, got {class(raster_input)}"
      )
    }
  }

  # Case 4: Single file path
  if (is.character(pop_rasters) && length(pop_rasters) == 1 && file.exists(pop_rasters)) {
    cli::cli_alert_success("Loading population raster: {.file {basename(pop_rasters)}}")
    return(terra::rast(pop_rasters))
  }

  cli::cli_abort(
    c(
      "Invalid pop_rasters format",
      "i" = "Expected one of:",
      "*" = "Named list of file paths: list('2016' = 'path/to/pop_2016.tif')",
      "*" = "Named list of SpatRaster objects",
      "*" = "Directory path containing population TIFs",
      "*" = "Single TIF file path"
    )
  )
}


#' Get DHS Country Code from ISO3
#'
#' Converts an ISO3 country code to the DHS two-letter country code.
#'
#' @param iso3 Three-letter ISO country code (case-insensitive).
#'
#' @return Two-letter DHS country code.
#'
#' @details
#' Uses the `countrycode` package to convert ISO3 to DHS codes.
#' DHS codes are specific to the DHS program and don't always match
#' ISO2 codes (e.g., Burundi is "BU" in DHS but "BI" in ISO2).
#'
#' @noRd
.get_dhs_country_code <- function(iso3) {
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg countrycode} is required to derive DHS country code",
        "i" = "Install with: install.packages('countrycode')",
        "i" = "Or provide `country_iso2` directly"
      )
    )
  }

  # Normalize to uppercase for lookup
  iso3_upper <- toupper(iso3)

  # Try to get DHS code using countrycode package
  dhs_code <- countrycode::countrycode(
    sourcevar = iso3_upper,
    origin = "iso3c",
    destination = "dhs",
    warn = FALSE
  )

  # If DHS code not found, try custom mapping for common cases
  if (is.na(dhs_code)) {
    # Custom mapping for codes that might not be in countrycode
    custom_map <- c(
      "BDI" = "BU",  # Burundi
      "COD" = "CD",  # DRC
      "CIV" = "CI",  # Cote d'Ivoire
      "SWZ" = "SZ",  # Eswatini
      "ETH" = "ET",  # Ethiopia
      "GHA" = "GH",  # Ghana
      "GIN" = "GN",  # Guinea
      "KEN" = "KE",  # Kenya
      "LSO" = "LS",  # Lesotho
      "LBR" = "LB",  # Liberia
      "MWI" = "MW",  # Malawi
      "MLI" = "ML",  # Mali
      "MOZ" = "MZ",  # Mozambique
      "NAM" = "NM",  # Namibia
      "NER" = "NI",  # Niger
      "NGA" = "NG",  # Nigeria
      "RWA" = "RW",  # Rwanda
      "SEN" = "SN",  # Senegal
      "SLE" = "SL",  # Sierra Leone
      "ZAF" = "ZA",  # South Africa
      "TZA" = "TZ",  # Tanzania
      "TGO" = "TG",  # Togo
      "UGA" = "UG",  # Uganda
      "ZMB" = "ZM",  # Zambia
      "ZWE" = "ZW",  # Zimbabwe
      "BFA" = "BF",  # Burkina Faso
      "CMR" = "CM",  # Cameroon
      "TCD" = "TD",  # Chad
      "COG" = "CG",  # Congo
      "GAB" = "GA",  # Gabon
      "GMB" = "GM",  # Gambia
      "MRT" = "MR",  # Mauritania
      "AGO" = "AO",  # Angola
      "BEN" = "BJ",  # Benin
      "CAF" = "CF",  # Central African Republic
      "COM" = "KM",  # Comoros
      "GNQ" = "GQ",  # Equatorial Guinea
      "ERI" = "ER",  # Eritrea
      "GNB" = "GW",  # Guinea-Bissau
      "MDG" = "MD",  # Madagascar
      "SOM" = "SO",  # Somalia
      "SSD" = "SS",  # South Sudan
      "SDN" = "SD"   # Sudan
    )

    if (iso3_upper %in% names(custom_map)) {
      dhs_code <- custom_map[[iso3_upper]]
    }
  }

  if (is.na(dhs_code)) {
    cli::cli_abort(
      c(
        "Could not find DHS country code for ISO3: {.val {iso3}}",
        "i" = "Please provide `country_iso2` directly"
      )
    )
  }

  dhs_code
}


#' Round MBG Output Data Frame
#'
#' Applies smart rounding to MBG indicator output:
#' - Integer columns (counts, sample sizes) → whole numbers
#' - Proportion columns (0-1 indicators) → 2 decimal places
#'
#' @param df Data frame to round.
#'
#' @return Data frame with rounded values.
#'
#' @noRd
.round_mbg_output <- function(df) {

  if (is.null(df) || nrow(df) == 0) return(df)


  # Columns that should be integers (counts, sample sizes, IDs)
  integer_patterns <- c(
    "^n_", "^dhs_n_", "samplesize", "sample_size", "n_clusters",
    "n_tested", "n_pos", "n_households", "n_births", "n_deaths",
    "n_fever", "n_sought", "n_women", "n_recent", "n_iptp",
    "n_public", "n_private", "n_none", "n_trained", "n_individuals",
    "n_with_access", "positive", "pop", "cluster_id", "_id$"
  )

  # Columns that are proportions (0-1 scale) → 2 decimal places
  proportion_patterns <- c(
    "^pfpr_", "^dhs_pfpr", "^itn_", "^dhs_itn", "^irs_", "^dhs_irs",
    "^anc_", "^dhs_anc", "^csb_", "^dhs_csb", "^anemia", "^dhs_anemia",
    "^dhs_severe_anemia", "^iptp_", "^dhs_iptp", "^epi_", "^dhs_epi",
    "^u5mr", "^dhs_u5mr", "^smc_", "^dhs_smc", "^act_", "^dhs_act",
    "access", "use", "ownership", "coverage", "proportion", "prop_",
    "_low$", "_upp$", "_se$", "^ci_l", "^ci_u", "^mean$", "^lower$", "^upper$",
    "tpr", "reprate", "cs_public", "cs_private", "cs_none"
  )

  # Check each column
  for (col in names(df)) {
    if (!is.numeric(df[[col]])) next

    # Check if integer column
    is_integer_col <- any(sapply(integer_patterns, function(p) grepl(p, col, ignore.case = TRUE)))

    # Check if proportion column
    is_proportion_col <- any(sapply(proportion_patterns, function(p) grepl(p, col, ignore.case = TRUE)))

    if (is_integer_col) {
      df[[col]] <- round(df[[col]], 0)
    } else if (is_proportion_col) {
      df[[col]] <- round(df[[col]], 2)
    } else {
      # Default: 2 decimal places for other numerics
      df[[col]] <- round(df[[col]], 2)
    }
  }

  df
}
