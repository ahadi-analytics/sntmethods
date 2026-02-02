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
#' @param country_iso3 Three-letter ISO country code (e.g., "bdi").
#' @param country_iso2 Two-letter DHS country code (e.g., "BU"). If NULL
#'   (default), derived automatically from `country_iso3` using the
#'   `countrycode` package.
#' @param adm0_sf sf object with country boundary.
#' @param adm1_sf sf object with ADM1 boundaries.
#' @param adm2_sf sf object with ADM2 boundaries.
#' @param pop_raster Total population raster(s). Can be:
#'   \itemize{
#'     \item Named list with years as names and file paths as values:
#'       `list("2019" = "path/to/pop_2019.tif", "2020" = "path/to/pop_2020.tif")`
#'     \item Single file path (used for all years)
#'     \item Already-loaded SpatRaster object (used for all years)
#'   }
#' @param pop_raster_u5 Under-5 population raster(s) (optional). Same format as
#'   `pop_raster`. Used for u5-specific indicators (pfpr, itn). If NULL, uses
#'   `pop_raster` for all indicators.
#' @param path_dhs_parquet Path to DHS parquet archive.
#' @param table_out_path Output directory for tables (CSV, XLSX).
#' @param fig_out_path Output directory for figures and maps.
#' @param raster_out_path Output directory for prediction rasters.
#' @param intermediate_out_path Output directory for cached intermediate outputs
#'   (aggregation tables, ID rasters).
#' @param survey_year Survey year(s) to process. Can be:
#'   \itemize{
#'     \item NULL: Process ALL available surveys with GPS data
#'     \item Single integer: Process only that year (e.g., 2016)
#'     \item Integer vector: Process specific years (e.g., c(2012, 2016))
#'   }
#' @param min_year Minimum survey year to include. Surveys before this year
#'   will be excluded. Useful for filtering out older surveys that may lack
#'   key indicators. Default: NULL (no minimum).
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
#' @param cache Logical. If TRUE (default), reuses cached intermediate outputs
#'   (aggregation tables, ID rasters) when available. Set to FALSE to force
#'   regeneration of all intermediate outputs.
#' @param verbose Logical. If TRUE, prints detailed progress. Default: TRUE.
#' @param debug Logical. If TRUE, prints additional diagnostic messages for
#'   troubleshooting. Default: FALSE.
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
#'   pop_raster = list("2016" = "/path/to/bdi_ppp_2016.tif"),
#'   path_dhs_parquet = "path/to/parquet",
#'   table_out_path = "path/to/output/tables",
#'   fig_out_path = "path/to/output/plots",
#'   raster_out_path = "path/to/output/rasters",
#'   intermediate_out_path = "path/to/output/intermediate",
#'   survey_year = 2016,
#'   indicators = c("pfpr", "itn", "csb"),
#'   cache = TRUE
#' )
#' }
#'
#' @export
run_mbg_indicator_pipeline <- function(
  country_iso3,
  adm0_sf,
  adm1_sf,
  adm2_sf,
  pop_raster,
  path_dhs_parquet,
  table_out_path,
  fig_out_path,
  raster_out_path,
  intermediate_out_path,
  pop_raster_u5 = NULL,
  country_iso2 = NULL,
  survey_year = NULL,
  min_year = NULL,
  survey_type = "DHS",
  indicators = c(
    "pfpr", "itn", "irs", "anc", "csb", "anemia", "iptp"
  ),
  run_mbg = TRUE,
  save_rasters = TRUE,
  generate_maps = TRUE,
  cache = TRUE,
  verbose = TRUE,
  debug = FALSE
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
      "Derived DHS code: {.val {country_iso2}} from {.val {country_iso3}}"
    )
  }

  # Store output paths
  output_dirs <- list(
    tables = table_out_path,
    plots = fig_out_path,
    rasters = raster_out_path,
    intermediate = intermediate_out_path
  )

  # Debug: Show configured output paths
  if (isTRUE(debug)) {
    cli::cli_h3("Debug: Output Paths")
    .show_path <- function(name, path) {
      if (is.null(path) || path == "") {
        cli::cli_alert_warning("{name}: {.val NULL or empty}")
      } else {
        cli::cli_alert_info("{name}: {.file {.relative_path(path)}}")
      }
    }
    .show_path("Tables", output_dirs$tables)
    .show_path("Plots", output_dirs$plots)
    .show_path("Rasters", output_dirs$rasters)
    .show_path("Intermediate", output_dirs$intermediate)
  }

  # Validate required output paths
  if (is.null(intermediate_out_path) || intermediate_out_path == "") {
    cli::cli_abort(c(
      "{.arg intermediate_out_path} is required but not set",
      "i" = "Provide a path for intermediate outputs (ID rasters, aggregation tables)"
    ))
  }

  results <- list(
    final_dataset = NULL,
    adm1_estimates = list(),
    mbg_estimates = list(),
    cluster_data = list(),
    raster_paths = list(),
    survey_metadata = list(),
    skipped_indicators = list()  # Track skipped indicators per year
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

  years_str <- paste(available_years, collapse = ", ")
  cli::cli_alert_success(
    "Found GPS data for {length(available_years)} survey(s): {years_str}"
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
      missing_str <- paste(missing_years, collapse = ", ")
      avail_str <- paste(available_years, collapse = ", ")
      cli::cli_abort(c(
        "Requested survey year(s) not found with GPS data: {missing_str}",
        "i" = "Available years with GPS: {avail_str}"
      ))
    }

    process_str <- paste(years_to_process, collapse = ", ")
    cli::cli_alert_info(
      "Processing {length(years_to_process)} survey(s): {process_str}"
    )
  }

  # Apply min_year filter if specified
  if (!is.null(min_year)) {
    min_year <- as.integer(min_year)
    excluded_years <- years_to_process[years_to_process < min_year]

    if (length(excluded_years) > 0) {
      cli::cli_alert_info(
        "Excluding {length(excluded_years)} survey(s) before {min_year}: {paste(excluded_years, collapse = ', ')}"
      )
    }

    years_to_process <- years_to_process[years_to_process >= min_year]

    if (length(years_to_process) == 0) {
      cli::cli_abort(c(
        "No surveys remaining after applying min_year filter ({min_year})",
        "i" = "Available years: {paste(available_years, collapse = ', ')}"
      ))
    }

    cli::cli_alert_success(
      "Processing {length(years_to_process)} survey(s) >= {min_year}: {paste(years_to_process, collapse = ', ')}"
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

  types_str <- paste(file_types_needed, collapse = ", ")
  cli::cli_alert_info("Required file types: {types_str}")

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
    loaded_str <- paste(loaded_types, collapse = ", ")
    cli::cli_alert_success(
      "Loaded {length(loaded_types)} file types: {loaded_str}"
    )

    # ---- Load population rasters ----

    # Load total population raster for this year
    pop_rast <- tryCatch({
      .load_raster_for_year(pop_raster, current_year, "total population")
    }, error = function(e) {
      cli::cli_alert_warning(
        "Could not load total population raster: {e$message}"
      )
      NULL
    })

    # Load u5 population raster for this year (if provided)
    pop_rast_u5 <- NULL
    if (!is.null(pop_raster_u5)) {
      pop_rast_u5 <- tryCatch({
        .load_raster_for_year(pop_raster_u5, current_year, "u5 population")
      }, error = function(e) {
        cli::cli_alert_warning(
          "Could not load u5 population raster: {e$message}"
        )
        NULL
      })
    }

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

    # Track skipped indicators for this year
    skipped_indicators <- list()
    processed_indicators <- character()

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
          pop_rast_u5 = pop_rast_u5,
          output_dirs = output_dirs,
          country_iso3 = country_iso3,
          survey_year = current_year,
          run_mbg = run_mbg,
          save_rasters = save_rasters,
          generate_maps = generate_maps,
          cache = cache,
          verbose = verbose,
          debug = debug
        )

        # Check if indicator was skipped
        if (!is.null(ind_results$skipped)) {
          skipped_indicators[[ind_category]] <- ind_results$skipped
          cli::cli_alert_warning(
            "Skipped {.field {ind_category}}: {ind_results$skipped}"
          )
        } else if (length(ind_results$cluster_data) > 0) {
          processed_indicators <- c(processed_indicators, ind_category)
          cli::cli_alert_success("Processed {.field {ind_category}}")
        }

        # Store results for this indicator
        for (name in names(ind_results$cluster_data)) {
          year_results$cluster_data[[name]] <- ind_results$cluster_data[[name]]
        }

        for (name in names(ind_results$mbg_estimates)) {
          mbg_est <- ind_results$mbg_estimates[[name]]
          year_results$mbg_estimates[[name]] <- mbg_est
        }

        for (name in names(ind_results$raster_paths)) {
          year_results$raster_paths[[name]] <- ind_results$raster_paths[[name]]
        }

      }, error = function(e) {
        skipped_indicators[[ind_category]] <- glue::glue("Error: {e$message}")
        cli::cli_alert_danger(
          "Failed to process {ind_category} for {current_year}: {e$message}"
        )
      })
    }

    # ---- Summary for this year ----
    cli::cli_h3("Summary for {current_year}")

    if (length(processed_indicators) > 0) {
      cli::cli_alert_success(
        "Processed {length(processed_indicators)} indicator(s): {paste(processed_indicators, collapse = ', ')}"
      )
    }

    if (length(skipped_indicators) > 0) {
      cli::cli_alert_warning(
        "Skipped {length(skipped_indicators)} indicator(s):"
      )
      for (ind_name in names(skipped_indicators)) {
        cli::cli_bullets(c(
          "!" = "{.field {ind_name}}: {skipped_indicators[[ind_name]]}"
        ))
      }
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

    output_basename <- glue::glue(
      "{tolower(country_iso3)}_dhs_mbg_indicators_{current_year}"
    )

    if (isTRUE(debug)) {
      cli::cli_alert_info(
        "Debug: year_dataset nrow={.val {nrow(year_dataset)}}, ncol={.val {ncol(year_dataset)}}"
      )
      cli::cli_alert_info(
        "Debug: columns: {paste(names(year_dataset), collapse = ', ')}"
      )
    }

    # Ensure output_basename is a single character string
    if (length(output_basename) != 1) {
      cli::cli_alert_danger(
        "output_basename has unexpected length: {length(output_basename)}"
      )
      cli::cli_alert_info("Values: {paste(output_basename, collapse = ', ')}")
      output_basename <- as.character(output_basename)[1]
      cli::cli_alert_warning("Using first element: {.val {output_basename}}")
    }

    # Coerce to plain character to avoid glue class issues
    output_basename <- as.character(output_basename)

    # Build data dictionary
    year_data_dict <- sntutils::build_dictionary(
      data = year_dataset,
      language = "fr"
    )

    # Write with data dictionary as second tab
    sntutils::write_snt_data(
      obj = list(data = year_dataset, data_dict = year_data_dict),
      path = output_dirs$tables,
      data_name = output_basename,
      file_formats = c("xlsx", "qs2")
    )
    output_rel_path <- .relative_path(fs::path(output_dirs$tables, output_basename))
    cli::cli_alert_success("Saved: {.file {output_rel_path}}")

    # Store results for this year
    all_year_results[[year_key]] <- list(
      dataset = year_dataset,
      cluster_data = year_results$cluster_data,
      mbg_estimates = year_results$mbg_estimates,
      raster_paths = year_results$raster_paths,
      skipped_indicators = skipped_indicators,
      processed_indicators = processed_indicators
    )

    # Also add to main results structure
    results$cluster_data[[year_key]] <- year_results$cluster_data
    results$mbg_estimates[[year_key]] <- year_results$mbg_estimates
    results$raster_paths[[year_key]] <- year_results$raster_paths
    results$skipped_indicators[[year_key]] <- skipped_indicators

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
      combined_basename <- glue::glue(
        "{tolower(country_iso3)}_dhs_mbg_indicators_combined"
      )

      # Coerce to plain character to avoid glue class issues
      combined_basename <- as.character(combined_basename)

      # Build data dictionary for combined dataset
      combined_data_dict <- sntutils::build_dictionary(
        data = results$final_dataset,
        language = "fr"
      )

      # Write with data dictionary as second tab
      sntutils::write_snt_data(
        obj = list(data = results$final_dataset, data_dict = combined_data_dict),
        path = output_dirs$tables,
        data_name = combined_basename,
        file_formats = c("xlsx", "qs2")
      )
      combined_rel_path <- .relative_path(
        fs::path(output_dirs$tables, combined_basename)
      )
      cli::cli_alert_success(
        "Saved combined dataset: {.file {combined_rel_path}}"
      )
    }
  } else {
    results$final_dataset <- data.frame()
    cli::cli_alert_warning("No data could be processed")
  }

  # ---- Final Summary ----

  cli::cli_h2("Pipeline Summary")

  n_surveys <- length(years_to_process)
  cli::cli_alert_success("Processed {n_surveys} survey year(s)")


  # Summarize skipped indicators across all years
  all_skipped <- results$skipped_indicators
  if (length(all_skipped) > 0 && any(sapply(all_skipped, length) > 0)) {
    cli::cli_h3("Skipped Indicators Summary")

    for (yr in names(all_skipped)) {
      yr_skipped <- all_skipped[[yr]]
      if (length(yr_skipped) > 0) {
        cli::cli_alert_warning("Survey {.val {yr}}:")
        for (ind_name in names(yr_skipped)) {
          cli::cli_bullets(c(
            "!" = "{.field {ind_name}}: {yr_skipped[[ind_name]]}"
          ))
        }
      }
    }

    # Aggregate common skip reasons
    all_reasons <- unlist(lapply(all_skipped, function(x) {
      if (length(x) > 0) names(x) else character()
    }))

    if (length(all_reasons) > 0) {
      reason_counts <- table(all_reasons)
      cli::cli_alert_info(
        "Most commonly skipped: {paste(names(sort(reason_counts, decreasing = TRUE)[1:min(3, length(reason_counts))]), collapse = ', ')}"
      )
    }
  } else {
    cli::cli_alert_success("All requested indicators were processed successfully")
  }

  cli::cli_alert_success("Pipeline complete!")

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
  pop_rast_u5 = NULL,
  output_dirs,
  country_iso3,
  survey_year,
  run_mbg,
  save_rasters,
  generate_maps,
  cache,
  verbose,
  debug = FALSE
) {
  # Use u5 population raster for u5-specific indicators, fall back to total
  use_u5 <- category %in% c("pfpr", "itn") && !is.null(pop_rast_u5)
  pop_for_indicator <- if (use_u5) pop_rast_u5 else pop_rast

  # Log which population raster is being used (debug only)
  if (isTRUE(debug)) {
    if (use_u5) {
      pop_src <- terra::sources(pop_rast_u5)
      cli::cli_alert_info("Using u5 population raster: {.file {basename(pop_src)}}")
    } else if (!is.null(pop_rast)) {
      pop_src <- terra::sources(pop_rast)
      cli::cli_alert_info("Using total population raster: {.file {basename(pop_src)}}")
    }
  }

  results <- list(
    cluster_data = list(),
    mbg_estimates = list(),
    raster_paths = list(),
    skipped = NULL  # Will contain skip reason if indicator was skipped
  )

  # Helper to return skipped result
  skip_indicator <- function(reason) {
    results$skipped <- reason
    return(results)
  }

  # Call appropriate MBG prep function based on category
  cluster_data <- switch(category,
    pfpr = {
      if (!"PR" %in% names(survey_data)) {
        return(skip_indicator("Missing PR data (Person Recode)"))
      }
      tryCatch({
        calc_pfpr_mbg(
          dhs_pr = survey_data$PR,
          gps_data = gps_data,
          test_type = "both",
          age_groups = list(u5 = c(6, 59))
        )
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    itn = {
      missing_ft <- setdiff(c("HR", "PR"), names(survey_data))
      if (length(missing_ft) > 0) {
        return(skip_indicator(
          glue::glue("Missing {paste(missing_ft, collapse = ', ')} data")
        ))
      }
      tryCatch({
        calc_itn_mbg(
          dhs_hr = survey_data$HR,
          dhs_pr = survey_data$PR,
          gps_data = gps_data,
          indicators = c("access", "use_u5")
        )
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    irs = {
      if (!"HR" %in% names(survey_data)) {
        return(skip_indicator("Missing HR data (Household Recode)"))
      }
      tryCatch({
        irs_result <- calc_irs_mbg(
          dhs_hr = survey_data$HR,
          gps_data = gps_data
        )
        if (!is.null(irs_result) && nrow(irs_result) > 0) {
          list(irs_coverage = irs_result)
        } else {
          results$skipped <<- "IRS variable not found in survey"
          list()
        }
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    anc = {
      if (!"IR" %in% names(survey_data)) {
        return(skip_indicator("Missing IR data (Individual Recode)"))
      }
      tryCatch({
        calc_anc_mbg(
          dhs_ir = survey_data$IR,
          gps_data = gps_data,
          indicators = c("anc1", "anc4")
        )
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    csb = {
      if (!"KR" %in% names(survey_data)) {
        return(skip_indicator("Missing KR data (Children Recode)"))
      }
      tryCatch({
        calc_csb_mbg(
          dhs_kr = survey_data$KR,
          gps_data = gps_data,
          indicators = c("public", "private", "none")
        )
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    anemia = {
      if (!"PR" %in% names(survey_data)) {
        return(skip_indicator("Missing PR data (Person Recode)"))
      }
      tryCatch({
        calc_anemia_mbg(
          dhs_pr = survey_data$PR,
          gps_data = gps_data,
          indicators = c("any", "moderate_plus", "severe")
        )
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    iptp = {
      if (!"IR" %in% names(survey_data)) {
        return(skip_indicator("Missing IR data (Individual Recode)"))
      }
      tryCatch({
        calc_iptp_mbg(
          dhs_ir = survey_data$IR,
          gps_data = gps_data,
          indicators = c("1plus", "2plus", "3plus")
        )
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    epi = {
      if (!"KR" %in% names(survey_data)) {
        return(skip_indicator("Missing KR data (Children Recode)"))
      }
      tryCatch({
        calc_epi_mbg(
          dhs_kr = survey_data$KR,
          gps_data = gps_data,
          indicators = c("bcg", "dpt3", "measles1")
        )
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    u5mr = {
      if (!"BR" %in% names(survey_data)) {
        return(skip_indicator("Missing BR data (Births Recode)"))
      }
      tryCatch({
        calc_u5mr_mbg(
          dhs_br = survey_data$BR,
          gps_data = gps_data
        )
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    smc = {
      if (!"KR" %in% names(survey_data)) {
        return(skip_indicator("Missing KR data (Children Recode)"))
      }
      tryCatch({
        smc_result <- calc_smc_mbg(
          dhs_kr = survey_data$KR,
          gps_data = gps_data
        )
        if (!is.null(smc_result) && nrow(smc_result) > 0) {
          list(smc_receipt = smc_result)
        } else {
          results$skipped <<- "SMC variable not found in survey"
          list()
        }
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    {
      # Unknown indicator
      return(skip_indicator(glue::glue("Unknown indicator category")))
    }
  )

  # Check if indicator was skipped due to empty results
  if (length(cluster_data) == 0) {
    if (is.null(results$skipped)) {
      results$skipped <- "No data returned from calculation"
    }
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
          pop_rast = pop_for_indicator,
          indicator_name = ind_name,
          output_dirs = output_dirs,
          country_iso3 = country_iso3,
          survey_year = survey_year,
          save_rasters = save_rasters,
          cache = cache,
          debug = debug
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
  output_dirs,
  country_iso3,
  survey_year,
  save_rasters,
  cache,
  debug = FALSE
) {
  # Validate population raster

  if (is.null(pop_rast)) {
    cli::cli_abort("Population raster is NULL - cannot run MBG")
  }

  # Ensure pop_rast has a valid source file (terra rasters can lose source)
  pop_source <- terra::sources(pop_rast)
  if (is.null(pop_source) || pop_source == "" || !file.exists(pop_source)) {
    cli::cli_abort(c(
      "Population raster has no valid source file",
      "i" = "Source: {.val {pop_source}}",
      "i" = "This can happen if the raster was modified in-memory"
    ))
  }

  # Ensure intermediate output directory exists
  if (is.null(output_dirs$intermediate) || output_dirs$intermediate == "") {
    cli::cli_abort(c(
      "Intermediate output directory is not set",
      "i" = "Received: {.val {output_dirs$intermediate}}",
      "i" = "Pass {.arg intermediate_out_path} to the pipeline function"
    ))
  }
  fs::dir_create(output_dirs$intermediate)

  # Debug: Show input summary

  if (isTRUE(debug)) {
    cli::cli_h3("Debug: MBG Inputs for {indicator_name}")
    cli::cli_alert_info("Cluster data: {.val {nrow(cluster_dt)}} rows")
    cli::cli_alert_info("ADM2 polygons: {.val {nrow(adm2_sf)}} features")
    cli::cli_alert_info("Population raster: {.file {pop_source}}")
    cli::cli_alert_info("Intermediate dir: {.file {.relative_path(output_dirs$intermediate)}}")
  }

  adm2_vect <- terra::vect(adm2_sf)

  # ---- Cache ID raster ----
  id_raster_file <- fs::path(
    output_dirs$intermediate,
    glue::glue("{tolower(country_iso3)}_id_raster.tif")
  )

  if (isTRUE(cache) && fs::file_exists(id_raster_file)) {
    cli::cli_alert_info("Using cached ID raster")
    id_raster <- terra::rast(id_raster_file)
  } else {
    cli::cli_alert_info("Building ID raster...")
    id_raster <- mbg::build_id_raster(
      polygons = adm2_vect,
      template_raster = pop_rast
    )
    terra::writeRaster(id_raster, id_raster_file, overwrite = TRUE)
    cli::cli_alert_success("Saved ID raster: {.file {.relative_path(id_raster_file)}}")
  }

  if (isTRUE(debug)) {
    cli::cli_alert_info("ID raster dims: {.val {terra::nrow(id_raster)}} x {.val {terra::ncol(id_raster)}}")
  }

  # Intercept-only model
  covariates <- list(
    intercept = terra::setValues(id_raster, 1)
  )

  # ---- Cache aggregation table ----
  agg_file <- fs::path(
    output_dirs$intermediate,
    glue::glue("{tolower(country_iso3)}_aggregation_table_adm2.parquet")
  )

  if (isTRUE(cache) && fs::file_exists(agg_file)) {
    cli::cli_alert_info("Using cached aggregation table")
    aggregation_table <- arrow::read_parquet(agg_file)
  } else {
    cli::cli_alert_info("Building aggregation table...")
    aggregation_table <- mbg::build_aggregation_table(
      polygons = adm2_vect,
      id_raster = id_raster,
      polygon_id_field = "adm2",
      verbose = FALSE
    )
    arrow::write_parquet(aggregation_table, agg_file)
    cli::cli_alert_success("Saved aggregation table: {.file {.relative_path(agg_file)}}")
  }

  if (isTRUE(debug)) {
    cli::cli_alert_info("Aggregation table: {.val {nrow(aggregation_table)}} rows")
  }

  # Run MBG
  cli::cli_alert_info("Running MBG model...")
  model_runner <- mbg::MbgModelRunner$new(
    input_data = cluster_dt,
    id_raster = id_raster,
    covariate_rasters = covariates,
    aggregation_table = aggregation_table,
    aggregation_levels = list(adm2 = c("adm2", "adm1", "adm0")),
    population_raster = pop_rast
  )

  model_runner$run_mbg_pipeline()
  cli::cli_alert_success("MBG model complete")

  # Extract predictions
  cell_preds <- model_runner$grid_cell_predictions

  # Save rasters
  raster_path <- NULL
  if (save_rasters) {
    fs::dir_create(output_dirs$rasters)
    raster_file <- glue::glue(
      "{tolower(country_iso3)}_{indicator_name}_mbg_{survey_year}_mean.tif"
    )
    raster_path <- fs::path(output_dirs$rasters, raster_file)

    terra::writeRaster(
      cell_preds$cell_pred_mean,
      raster_path,
      overwrite = TRUE
    )
    cli::cli_alert_success("Saved raster: {.file {.relative_path(raster_path)}}")
  }

  # Extract ADM2 estimates
  mean_col <- paste0(indicator_name, "_mean")
  lower_col <- paste0(indicator_name, "_lower")
  upper_col <- paste0(indicator_name, "_upper")

  adm2_estimates <- adm2_sf |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      !!mean_col := terra::extract(
        cell_preds$cell_pred_mean, adm2_sf, fun = mean, na.rm = TRUE
      )[[2]],
      !!lower_col := terra::extract(
        cell_preds$cell_pred_lower, adm2_sf, fun = mean, na.rm = TRUE
      )[[2]],
      !!upper_col := terra::extract(
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

  # Add country codes if provided (lowercase)
  if (!is.null(country_iso3)) {
    final$iso3_code <- tolower(country_iso3)
  }

  if (!is.null(country_iso2)) {
    final$dhs_code <- tolower(country_iso2)
  }

  # Ensure adm0, adm1, adm2 columns exist (try common names)
  if (!"adm0" %in% names(final)) {
    # Try to find adm0 from other column names
    adm0_candidates <- c(
      "ADM0_NAME", "ADMIN0", "country", "Country", "NAME_0"
    )
    for (col in adm0_candidates) {
      if (col %in% names(final)) {
        final$adm0 <- final[[col]]
        break
      }
    }
  }

  if (!"adm1" %in% names(final)) {
    # Try to find adm1 from other column names
    adm1_candidates <- c(
      "ADM1_NAME", "ADMIN1", "province", "Province",
      "NAME_1", "region", "Region"
    )
    for (col in adm1_candidates) {
      if (col %in% names(final)) {
        final$adm1 <- final[[col]]
        break
      }
    }
  }

  if (!"adm2" %in% names(final)) {
    # Try to find adm2 from other column names
    adm2_candidates <- c(
      "ADM2_NAME", "ADMIN2", "district", "District", "NAME_2"
    )
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
      "Column 'adm2' not found in shapefile - MBG estimates cannot be merged"
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

  # ---- Select and reorder final columns ----

  # Define required identifier columns
  id_cols <- c("iso3_code", "dhs_code", "adm0", "adm1", "adm2", "survey_year")
  id_cols_present <- intersect(id_cols, names(final))

  # Identify indicator columns (end with _mean, _lower, _upper)
  indicator_cols <- names(final)[grepl("_(mean|lower|upper)$", names(final))]

  # Select only required columns (drop GUIDs, hashes, etc.)
  final <- final |>
    dplyr::select(dplyr::all_of(c(id_cols_present, indicator_cols)))

  final
}


#' Load Raster for Year
#'
#' Helper to load a raster for a specific year from various input formats.
#'
#' @param raster_input Can be:
#'   - Named list with years as names (e.g., list("2019" = "path.tif"))
#'   - Single file path (string)
#'   - Already-loaded SpatRaster object
#' @param target_year The survey year to load raster for.
#' @param label Label for CLI messages (e.g., "total population").
#'
#' @return A SpatRaster object.
#'
#' @noRd
.load_raster_for_year <- function(raster_input, target_year, label = "raster") {
  target_year_char <- as.character(target_year)

  # Case 1: Already a SpatRaster
 if (inherits(raster_input, "SpatRaster")) {
    cli::cli_alert_success("Using pre-loaded {label} raster")
    return(raster_input)
  }

  # Case 2: Named list with years
  if (is.list(raster_input) && !is.null(names(raster_input))) {
    available_years <- as.integer(names(raster_input))

    # Find exact match or closest year
    if (target_year_char %in% names(raster_input)) {
      selected_year <- target_year_char
    } else {
      # Find closest available year
      year_diff <- abs(available_years - target_year)
      closest_year <- available_years[which.min(year_diff)]
      selected_year <- as.character(closest_year)
      cli::cli_alert_warning(
        "No {label} raster for {target_year}, using closest: {closest_year}"
      )
    }

    raster_path <- raster_input[[selected_year]]

    # Handle if the list value is already a SpatRaster
    if (inherits(raster_path, "SpatRaster")) {
      cli::cli_alert_success(
        "Using pre-loaded {label} raster for {selected_year}"
      )
      return(raster_path)
    }

    # Otherwise load from path
    if (!file.exists(raster_path)) {
      cli::cli_abort(
        "{label} raster file not found: {.path {raster_path}}"
      )
    }
    cli::cli_alert_success(
      "Loading {label} raster ({selected_year}): {.file {basename(raster_path)}}"
    )
    return(terra::rast(raster_path))
  }

  # Case 3: Single file path
  if (is.character(raster_input) && length(raster_input) == 1) {
    if (!file.exists(raster_input)) {
      cli::cli_abort("{label} raster file not found: {.path {raster_input}}")
    }
    cli::cli_alert_success(
      "Loading {label} raster: {.file {basename(raster_input)}}"
    )
    return(terra::rast(raster_input))
  }

  cli::cli_abort(
    c(
      "Invalid {label} raster input",
      "i" = "Expected one of:",
      "*" = "Named list: list('2019' = 'path/pop_2019.tif', '2020' = ...)",
      "*" = "Single file path (string)",
      "*" = "SpatRaster object",
      "i" = "Received: {class(raster_input)[1]}"
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
    "_low$", "_upp$", "_se$", "^ci_l", "^ci_u",
    "^mean$", "^lower$", "^upper$",
    "tpr", "reprate", "cs_public", "cs_private", "cs_none"
  )

  # Check each column
  for (col in names(df)) {
    if (!is.numeric(df[[col]])) next

    # Check if integer column
    is_integer_col <- any(
      sapply(integer_patterns, function(p) grepl(p, col, ignore.case = TRUE))
    )

    # Check if proportion column
    is_proportion_col <- any(
      sapply(proportion_patterns, function(p) grepl(p, col, ignore.case = TRUE))
    )

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


#' Get Relative Path from Project Root
#'
#' Extracts the relative path portion from an absolute path by removing
#' the project root prefix. Uses working directory as reference.
#'
#' @param path Character. The absolute path.
#'
#' @return Character. The relative path from project root.
#'
#' @noRd
.relative_path <- function(path) {
  # Try to compute relative path from working directory
  rel <- tryCatch({
    fs::path_rel(path, start = getwd())
  }, error = function(e) {
    # Fallback: extract path after common project patterns
    # Matches patterns like: /project-name/01_data/... or /project-name/03_outputs/...
    match <- regmatches(
      path,
      regexpr("(01_data|02_scripts|03_outputs|04_reports|05_metadata_docs)/.*$", path)
    )
    if (length(match) > 0 && nchar(match) > 0) {
      return(match)
    }
    # Final fallback: just return basename
    basename(path)
  })
  as.character(rel)
}
