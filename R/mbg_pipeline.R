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
#' Orchestrates the full MBG processing pipeline for DHS indicators.
#' See indicator-specific methodology files at
#' \url{https://github.com/ahadi-analytics/sntmethods/tree/master/inst/methods}
#'
#' Pipeline steps:
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
#' @param adm3_sf sf object with ADM3 boundaries (optional). Required when
#'   `aggregation_level = "adm3"`. Should contain columns for adm3 names and
#'   parent adm2/adm1/adm0 linkages.
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
#' @param survey_type Survey type(s) to process. Can be:
#'   \itemize{
#'     \item NULL (default): Auto-detect all available survey types (DHS, MIS, etc.)
#'     \item Single string: Process only that type (e.g., "DHS")
#'     \item Character vector: Process specific types (e.g., c("DHS", "MIS"))
#'   }
#' @param indicators Character vector of indicator categories to process:
#'   \itemize{
#'     \item "pfpr": Parasite prevalence
#'     \item "itn": ITN ownership/access/use
#'     \item "irs": IRS coverage
#'     \item "anc": ANC attendance
#'     \item "csb": Care-seeking behavior
#'     \item "act": ACT treatment (case management)
#'     \item "anemia": Anemia prevalence
#'     \item "iptp": IPTp doses
#'     \item "epi": EPI vaccination (see `epi_indicators` for vaccine selection)
#'     \item "u5mr": Under-5 mortality
#'     \item "smc": SMC receipt
#'     \item "fever": Fever prevalence (U5)
#'     \item "antimalarial": Any antimalarial treatment (febrile U5)
#'     \item "eff_cm": Effective coverage of case management (derived;
#'       auto-adds "csb" and "act" as dependencies)
#'   }
#' @param epi_indicators Character vector of EPI vaccine indicators to process
#'   when "epi" is included in `indicators`. Passed to `calc_epi_mbg()`.
#'   Valid values: "bcg", "dpt1", "dpt2", "dpt3", "polio1", "polio2", "polio3",
#'   "measles1", "measles2", "vita1", "vita2", "malaria",
#'   "penta1", "penta2", "penta3", "pneumo1", "pneumo2", "pneumo3",
#'   "rota1", "rota2", "rota3", "ipv", "hepb0", "yellowfever",
#'   "fully_vaccinated".
#'   DPT indicators automatically fall back to pentavalent variables when the
#'   primary DHS variables are absent.
#'   Default: c("bcg", "dpt2", "dpt3", "measles1", "measles2").
#' @param aggregation_level Primary aggregation level for MBG outputs. One of:
#'   \itemize{
#'     \item "adm2": Aggregate to ADM2 level (default)
#'     \item "adm3": Aggregate to ADM3 level (requires `adm3_sf`)
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
#'     \item final_dataset: Combined dataset with all indicators at the specified
#'       aggregation level (ADM2 by default, or ADM3 if `aggregation_level = "adm3"`)
#'     \item adm1_estimates: Survey-weighted ADM1 estimates (if available)
#'     \item mbg_estimates: MBG predictions at the specified aggregation level
#'       (if MBG was run)
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
  adm3_sf = NULL,
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
  survey_type = NULL,
  indicators = c(
    "pfpr", "itn", "irs", "anc", "csb", "anemia", "iptp",
    "fever", "antimalarial"
  ),
  epi_indicators = c("bcg", "dpt2", "dpt3", "measles1", "measles2"),
  aggregation_level = c("adm2", "adm3"),
  run_mbg = TRUE,
  save_rasters = TRUE,
  generate_maps = TRUE,
  cache = TRUE,
  verbose = TRUE,
  debug = FALSE
) {

  # Check for required spatial packages
  .check_spatial_pkg("sf", "run_mbg_indicator_pipeline")
  .check_spatial_pkg("terra", "run_mbg_indicator_pipeline")
  .check_spatial_pkg("fs", "run_mbg_indicator_pipeline")
  .check_spatial_pkg("countrycode", "run_mbg_indicator_pipeline")

  # Warn about MBG dependencies (soft check - will abort later if run_mbg = TRUE)
  mbg_deps_missing <- character(0)

  if (!requireNamespace("mbg", quietly = TRUE)) {
    mbg_deps_missing <- c(mbg_deps_missing, "mbg")
  }
  if (!requireNamespace("INLA", quietly = TRUE)) {
    mbg_deps_missing <- c(mbg_deps_missing, "INLA")
  }
  if (!requireNamespace("fmesher", quietly = TRUE)) {
    mbg_deps_missing <- c(mbg_deps_missing, "fmesher")
  }
  if (!requireNamespace("MatrixModels", quietly = TRUE)) {
    mbg_deps_missing <- c(mbg_deps_missing, "MatrixModels")
  }
  if (!requireNamespace("sn", quietly = TRUE)) {
    mbg_deps_missing <- c(mbg_deps_missing, "sn")
  }

  if (length(mbg_deps_missing) > 0) {
    cli::cli_warn(c(
      "MBG dependencies not installed: {.pkg {mbg_deps_missing}}",
      "i" = "MBG modeling will not be available until these are installed",
      "i" = "Install INLA dependencies first:",
      " " = "{.code install.packages(c('fmesher', 'MatrixModels'))}",
      "i" = "Then install INLA:",
      " " = "{.code install.packages('INLA', repos = c(INLA = 'https://inla.r-inla-download.org/R/stable'), dep = TRUE)}",
      "i" = "Then install mbg:",
      " " = "{.code devtools::install_github('ihmeuw/mbg')}"
    ))
  }

  # Validate aggregation_level
  aggregation_level <- match.arg(aggregation_level)

  # Validate ADM3 is provided when aggregation_level is "adm3"
  if (aggregation_level == "adm3" && is.null(adm3_sf)) {
    cli::cli_abort(c(
      "{.arg adm3_sf} is required when {.arg aggregation_level} is 'adm3'",
      "i" = "Either provide adm3_sf or set aggregation_level = 'adm2'"
    ))
  }

  # Check mbg package and its dependencies are available when run_mbg = TRUE
  if (isTRUE(run_mbg)) {
    mbg_required_missing <- character(0)

    if (!requireNamespace("fmesher", quietly = TRUE)) {
      mbg_required_missing <- c(mbg_required_missing, "fmesher")
    }
    if (!requireNamespace("MatrixModels", quietly = TRUE)) {
      mbg_required_missing <- c(mbg_required_missing, "MatrixModels")
    }
    if (!requireNamespace("INLA", quietly = TRUE)) {
      mbg_required_missing <- c(mbg_required_missing, "INLA")
    }
    if (!requireNamespace("mbg", quietly = TRUE)) {
      mbg_required_missing <- c(mbg_required_missing, "mbg")
    }
    if (!requireNamespace("sn", quietly = TRUE)) {
      mbg_required_missing <- c(mbg_required_missing, "sn")
    }

    if (length(mbg_required_missing) > 0) {
      cli::cli_abort(c(
        "MBG dependencies required when {.arg run_mbg = TRUE}: {.pkg {mbg_required_missing}}",
        "i" = "Install in this order:",
        " " = "1. {.code install.packages(c('fmesher', 'MatrixModels', 'sn'))}",
        " " = "2. {.code install.packages('INLA', repos = c(INLA = 'https://inla.r-inla-download.org/R/stable'), dep = TRUE)}",
        " " = "3. {.code devtools::install_github('ihmeuw/mbg')}",
        "i" = "Or set {.arg run_mbg = FALSE} to skip MBG modeling"
      ))
    }
  }

  # ---- Input Validation ----

  # Validate indicator names
  # antimalarial/act already filter to febrile U5 via KR helper
  valid_indicators <- c(
    "pfpr", "itn", "irs", "anc", "csb", "act", "anemia", "iptp", "epi",
    "u5mr", "smc", "fever", "antimalarial",
    # ITN sub-indicators (selectable individually for faster pipelines)
    "itn_ownership", "itn_access", "itn_use_all", "itn_use_u5",
    "itn_use_pregnant", "itn_use_if_access",
    # Public care-seeking and WMR sub-indicators
    "act_public", "act_among_am", "antimalarial_public",
    # Derived indicators (auto-expand to required dependencies)
    "eff_cm"
  )
  invalid_indicators <- setdiff(indicators, valid_indicators)
  if (length(invalid_indicators) > 0) {
    cli::cli_abort(c(
      "Invalid indicator(s): {.val {invalid_indicators}}",
      "i" = "Valid indicators: {.val {valid_indicators}}"
    ))
  }

  # Collapse sub-indicators into parent categories (they are produced together)
  sub_to_parent <- c(act_public = "act", act_among_am = "act",
                     antimalarial_public = "antimalarial")
  for (sub in names(sub_to_parent)) {
    if (sub %in% indicators) {
      parent <- sub_to_parent[[sub]]
      if (!parent %in% indicators) {
        indicators <- c(indicators, parent)
      }
      indicators <- setdiff(indicators, sub)
    }
  }

  # Expand derived indicators to include their dependencies
  if ("eff_cm" %in% indicators) {
    deps <- c("csb", "act")
    new_deps <- setdiff(deps, indicators)
    if (length(new_deps) > 0) {
      cli::cli_alert_info(
        "Adding {.val {new_deps}} (required by {.val eff_cm})"
      )
      indicators <- unique(c(indicators, new_deps))
    }
  }

  # Validate epi_indicators if "epi" is requested
  if ("epi" %in% indicators) {
    valid_epi <- c(
      "bcg", "dpt1", "dpt2", "dpt3",
      "polio1", "polio2", "polio3",
      "measles1", "measles2",
      "vita1", "vita2",
      "malaria",
      "penta1", "penta2", "penta3",
      "pneumo1", "pneumo2", "pneumo3",
      "rota1", "rota2", "rota3",
      "ipv", "hepb0", "yellowfever",
      "fully_vaccinated"
    )
    invalid_epi <- setdiff(epi_indicators, valid_epi)
    if (length(invalid_epi) > 0) {
      cli::cli_abort(c(
        "Invalid epi_indicators: {.val {invalid_epi}}",
        "i" = "Valid values: {.val {valid_epi}}"
      ))
    }
  }

  # Validate DHS parquet path exists
  if (!fs::dir_exists(path_dhs_parquet)) {
    cli::cli_abort(c(
      "DHS parquet path does not exist",
      "x" = "Path not found: {.path {path_dhs_parquet}}",
      "i" = "Check that the path is correct and accessible"
    ))
  }

  # Validate shapefile inputs are sf objects
  if (!inherits(adm0_sf, "sf")) {
    cli::cli_abort("{.arg adm0_sf} must be an sf object, not {.cls {class(adm0_sf)}}")
  }
  if (!inherits(adm1_sf, "sf")) {
    cli::cli_abort("{.arg adm1_sf} must be an sf object, not {.cls {class(adm1_sf)}}")
  }
  if (!inherits(adm2_sf, "sf")) {
    cli::cli_abort("{.arg adm2_sf} must be an sf object, not {.cls {class(adm2_sf)}}")
  }
  if (!is.null(adm3_sf) && !inherits(adm3_sf, "sf")) {
    cli::cli_abort("{.arg adm3_sf} must be an sf object, not {.cls {class(adm3_sf)}}")
  }

  # Validate population raster paths exist (if run_mbg = TRUE)
  if (isTRUE(run_mbg)) {
    .validate_raster_paths(pop_raster, country_iso3, "pop_raster")
    if (!is.null(pop_raster_u5)) {
      .validate_raster_paths(pop_raster_u5, country_iso3, "pop_raster_u5")
    }
  }

  # Validate/create output directories
  output_paths <- c(table_out_path, fig_out_path, raster_out_path, intermediate_out_path)
  output_paths <- output_paths[!is.null(output_paths) & output_paths != ""]
  for (out_path in output_paths) {
    tryCatch({
      fs::dir_create(out_path, recurse = TRUE)
    }, error = function(e) {
      cli::cli_abort(c(
        "Cannot create output directory",
        "x" = "Path: {.path {out_path}}",
        "i" = "Error: {e$message}"
      ))
    })
  }

  # ---- Setup ----

  cli::cli_h1("MBG Indicator Pipeline: {toupper(country_iso3)}")
  cli::cli_alert_info("Aggregation level: {.val {aggregation_level}}")

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
    cli::cli_alert_info("Tables: {.file {output_dirs$tables}}")
    cli::cli_alert_info("Plots: {.file {output_dirs$plots}}")
    cli::cli_alert_info("Rasters: {.file {output_dirs$rasters}}")
    cli::cli_alert_info("Intermediate: {.file {output_dirs$intermediate}}")
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

  # Use dhs_read to find available surveys with GPS data (no survey_type filter
  # so we can discover all available types)
  gps_check <- tryCatch({
    dhs_read(
      path = path_dhs_parquet,
      file_type = "GE",
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

  # Build available surveys data frame with (year, type) pairs
  available_surveys <- gps_check |>
    dplyr::select(DHSYEAR, survey_type) |>
    dplyr::distinct() |>
    dplyr::arrange(DHSYEAR, survey_type)

  avail_str <- paste(
    apply(available_surveys, 1, function(r) paste0(r["survey_type"], " ", r["DHSYEAR"])),
    collapse = ", "
  )
  cli::cli_alert_success(
    "Found GPS data for {nrow(available_surveys)} survey(s): {avail_str}"
  )

  # Filter by survey_year if specified
  if (!is.null(survey_year)) {
    requested_years <- as.integer(survey_year)
    missing_years <- setdiff(requested_years, available_surveys$DHSYEAR)
    if (length(missing_years) > 0) {
      missing_str <- paste(missing_years, collapse = ", ")
      cli::cli_abort(c(
        "Requested survey year(s) not found with GPS data: {missing_str}",
        "i" = "Available surveys: {avail_str}"
      ))
    }
    available_surveys <- available_surveys |>
      dplyr::filter(DHSYEAR %in% requested_years)
  }

  # Filter by survey_type if specified (non-NULL)
  if (!is.null(survey_type)) {
    available_surveys <- available_surveys |>
      dplyr::filter(survey_type %in% .env$survey_type)
    if (nrow(available_surveys) == 0) {
      cli::cli_abort(c(
        "No surveys found matching survey_type: {paste(survey_type, collapse = ', ')}",
        "i" = "Available surveys: {avail_str}"
      ))
    }
  }

  surveys_to_process <- available_surveys
  years_to_process <- sort(unique(surveys_to_process$DHSYEAR))

  process_str <- paste(
    apply(surveys_to_process, 1, function(r) paste0(r["survey_type"], " ", r["DHSYEAR"])),
    collapse = ", "
  )
  cli::cli_alert_info(
    "Processing {nrow(surveys_to_process)} survey(s): {process_str}"
  )

  # Apply min_year filter if specified
  if (!is.null(min_year)) {
    min_year <- as.integer(min_year)
    excluded <- surveys_to_process |>
      dplyr::filter(DHSYEAR < min_year)

    if (nrow(excluded) > 0) {
      excl_str <- paste(
        apply(excluded, 1, function(r) paste0(r["survey_type"], " ", r["DHSYEAR"])),
        collapse = ", "
      )
      cli::cli_alert_info(
        "Excluding {nrow(excluded)} survey(s) before {min_year}: {excl_str}"
      )
    }

    surveys_to_process <- surveys_to_process |>
      dplyr::filter(DHSYEAR >= min_year)
    years_to_process <- sort(unique(surveys_to_process$DHSYEAR))

    if (nrow(surveys_to_process) == 0) {
      cli::cli_abort(c(
        "No surveys remaining after applying min_year filter ({min_year})",
        "i" = "Available surveys: {avail_str}"
      ))
    }

    remaining_str <- paste(
      apply(surveys_to_process, 1, function(r) paste0(r["survey_type"], " ", r["DHSYEAR"])),
      collapse = ", "
    )
    cli::cli_alert_success(
      "Processing {nrow(surveys_to_process)} survey(s) >= {min_year}: {remaining_str}"
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
    act = "KR",
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
  results$survey_metadata$surveys_processed <- surveys_to_process
  results$survey_metadata$years_processed <- years_to_process
  results$survey_metadata$available_surveys <- available_surveys

  # ---- Process each survey year ----

  all_survey_results <- list()

  for (i in seq_len(nrow(surveys_to_process))) {
    current_year <- surveys_to_process$DHSYEAR[i]
    current_survey_type <- surveys_to_process$survey_type[i]
    survey_key <- paste(tolower(current_survey_type), current_year, sep = "_")

    cli::cli_h2("Processing survey: {current_survey_type} {current_year}")

    # ---- Load survey data for this survey using dhs_read() ----

    cli::cli_alert_info("Loading survey data for {current_survey_type} {current_year}...")

    survey_data <- list()

    for (ft in file_types_needed) {
      tryCatch({
        ft_data <- dhs_read(
          path = path_dhs_parquet,
          file_type = ft,
          survey_type = current_survey_type,
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

    # Abort if run_mbg = TRUE but population raster is missing
    if (isTRUE(run_mbg) && is.null(pop_rast)) {
      cli::cli_abort(c(
        "Population raster is required when {.arg run_mbg = TRUE}",
        "x" = "Could not load population raster for year {current_year}",
        "i" = "Check that {.arg pop_raster} paths exist and use correct country code",
        "i" = "Or set {.arg run_mbg = FALSE} to skip MBG modeling"
      ))
    }

    # ---- Align CRS ----

    if (!is.null(pop_rast)) {
      crs_master <- terra::crs(pop_rast)
      adm0_aligned <- sf::st_transform(adm0_sf, crs_master)
      adm1_aligned <- sf::st_transform(adm1_sf, crs_master)
      adm2_aligned <- sf::st_transform(adm2_sf, crs_master)
      if (!is.null(adm3_sf)) {
        adm3_aligned <- sf::st_transform(adm3_sf, crs_master)
      } else {
        adm3_aligned <- NULL
      }
    } else {
      # Use a default CRS if no population raster
      adm0_aligned <- adm0_sf
      adm1_aligned <- adm1_sf
      adm2_aligned <- adm2_sf
      adm3_aligned <- adm3_sf
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

    # Extract interview month per cluster for survey timing metadata
    gps_clean_for_month <- .prepare_gps_data(gps_data)
    cluster_interview_months <- .extract_cluster_interview_month(
      survey_data = survey_data,
      gps_clean = gps_clean_for_month
    )

    year_results <- list(
      cluster_data = list(),
      mbg_estimates = list(),
      raster_paths = list()
    )

    # Track skipped indicators for this year
    skipped_indicators <- list()
    processed_indicators <- character()

    # Derived indicators are computed after primary processing
    derived_indicators <- c("eff_cm")
    primary_indicators <- setdiff(indicators, derived_indicators)

    for (ind_category in primary_indicators) {
      cli::cli_h3("Processing: {ind_category}")

      tryCatch({
        ind_results <- .process_indicator_category(
          category = ind_category,
          survey_data = survey_data,
          gps_data = gps_data,
          adm0_sf = adm0_aligned,
          adm1_sf = adm1_aligned,
          adm2_sf = adm2_aligned,
          adm3_sf = adm3_aligned,
          pop_rast = pop_rast,
          pop_rast_u5 = pop_rast_u5,
          output_dirs = output_dirs,
          country_iso3 = country_iso3,
          survey_year = current_year,
          survey_type = current_survey_type,
          aggregation_level = aggregation_level,
          run_mbg = run_mbg,
          save_rasters = save_rasters,
          generate_maps = generate_maps,
          cache = cache,
          verbose = verbose,
          debug = debug,
          epi_indicators = epi_indicators
        )

        # Check if indicator was skipped
        if (!is.null(ind_results$skipped)) {
          skipped_indicators[[ind_category]] <- ind_results$skipped
          cli::cli_alert_warning(
            "Skipped {.field {ind_category}}: {ind_results$skipped}"
          )
        } else if (length(ind_results$cluster_data) > 0) {
          processed_indicators <- c(processed_indicators, names(ind_results$cluster_data))
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

    # ---- Compute derived indicators (eff_cm = CSB x ACT) ----
    if (isTRUE(run_mbg) && length(year_results$raster_paths) > 0) {
      derived <- .compute_derived_rasters(
        raster_paths = year_results$raster_paths,
        primary_sf = if (aggregation_level == "adm3") adm3_aligned else adm2_aligned,
        output_dirs = output_dirs,
        country_iso3 = country_iso3,
        survey_year = current_year,
        survey_type = current_survey_type,
        save_rasters = save_rasters,
        cache = cache
      )
      for (name in names(derived$mbg_estimates)) {
        year_results$mbg_estimates[[name]] <- derived$mbg_estimates[[name]]
        processed_indicators <- c(processed_indicators, name)
      }
      for (name in names(derived$raster_paths)) {
        year_results$raster_paths[[name]] <- derived$raster_paths[[name]]
      }
    }

    # ---- Save cluster data for this year ----
    if (length(year_results$cluster_data) > 0) {
      .save_cluster_data(
        cluster_data = year_results$cluster_data,
        output_dir = output_dirs$tables,
        country_iso3 = country_iso3,
        survey_year = current_year,
        survey_type = current_survey_type
      )
    }

    # ---- Summary for this survey ----
    cli::cli_h3("Summary for {current_survey_type} {current_year}")

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
      survey_type = current_survey_type,
      country_iso3 = country_iso3,
      country_iso2 = country_iso2,
      adm3_sf = adm3_aligned,
      aggregation_level = aggregation_level,
      cluster_interview_months = cluster_interview_months
    )

    # Apply smart rounding
    year_dataset <- .round_mbg_output(year_dataset)

    # ---- Save outputs for this year ----

    output_basename <- glue::glue(
      "{tolower(country_iso3)}_{tolower(current_survey_type)}_mbg_indicators_{current_year}"
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

    # Build data dictionary with MBG-specific labels
    year_data_dict <- sntutils::build_dictionary(
      data = year_dataset,
      language = "fr"
    )
    mbg_labels <- dplyr::bind_rows(
      .mbg_id_labels(),
      .build_mbg_labels(processed_indicators)
    ) |>
      dplyr::filter(.data$variable %in% names(year_dataset))
    year_data_dict <- .enrich_dhs_dictionary(year_data_dict, mbg_labels)

    # Write with data dictionary as second tab (versioned with date)
    sntutils::write_snt_data(
      obj = list(data = year_dataset, data_dict = year_data_dict),
      path = output_dirs$tables,
      data_name = output_basename,
      file_formats = c("xlsx", "qs2"),
      include_date = TRUE,
      n_saved = 3
    )
    output_rel_path <- .relative_path(fs::path(output_dirs$tables, output_basename))
    cli::cli_alert_success("Saved: {.file {output_rel_path}}")

    # Store results for this survey
    all_survey_results[[survey_key]] <- list(
      dataset = year_dataset,
      cluster_data = year_results$cluster_data,
      mbg_estimates = year_results$mbg_estimates,
      raster_paths = year_results$raster_paths,
      skipped_indicators = skipped_indicators,
      processed_indicators = processed_indicators
    )

    # Also add to main results structure
    results$cluster_data[[survey_key]] <- year_results$cluster_data
    results$mbg_estimates[[survey_key]] <- year_results$mbg_estimates
    results$raster_paths[[survey_key]] <- year_results$raster_paths
    results$skipped_indicators[[survey_key]] <- skipped_indicators

  }  # End loop over surveys

  # ---- Build combined final dataset ----

  cli::cli_h2("Building combined dataset")

  # Combine all surveys into one dataset (survey_year and survey_type already set
  # by .build_final_dataset())
  combined_datasets <- lapply(names(all_survey_results), function(key) {
    df <- all_survey_results[[key]]$dataset
    if (!is.null(df) && nrow(df) > 0) {
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

    # Save combined dataset when processing multiple surveys
    if (nrow(surveys_to_process) > 1) {
      combined_basename <- glue::glue(
        "{tolower(country_iso3)}_mbg_indicators_combined"
      )

      # Coerce to plain character to avoid glue class issues
      combined_basename <- as.character(combined_basename)

      # Build data dictionary for combined dataset with MBG-specific labels
      combined_data_dict <- sntutils::build_dictionary(
        data = results$final_dataset,
        language = "fr"
      )
      all_indicators <- unique(unlist(lapply(
        all_survey_results, function(x) x$processed_indicators
      )))
      combined_mbg_labels <- dplyr::bind_rows(
        .mbg_id_labels(),
        .build_mbg_labels(all_indicators)
      ) |>
        dplyr::filter(.data$variable %in% names(results$final_dataset))
      combined_data_dict <- .enrich_dhs_dictionary(
        combined_data_dict, combined_mbg_labels
      )

      # Write with data dictionary as second tab (versioned with date)
      sntutils::write_snt_data(
        obj = list(data = results$final_dataset, data_dict = combined_data_dict),
        path = output_dirs$tables,
        data_name = combined_basename,
        file_formats = c("xlsx", "qs2"),
        include_date = TRUE,
        n_saved = 3
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

  n_surveys <- nrow(surveys_to_process)
  cli::cli_alert_success("Processed {n_surveys} survey(s)")


  # Summarize skipped indicators across all surveys
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
  adm3_sf = NULL,
  pop_rast,
  pop_rast_u5 = NULL,
  output_dirs,
  country_iso3,
  survey_year,
  survey_type = "DHS",
  aggregation_level = "adm2",
  run_mbg,
  save_rasters,
  generate_maps,
  cache,
  verbose,
  debug = FALSE,
  epi_indicators = c("bcg", "dpt2", "dpt3", "measles1", "measles2")
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
          age_groups = list(
            u5 = c(6, 59),
            `5_10` = c(60, 120),
            u10 = c(6, 119)
          )
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
          indicators = c(
            "itn_ownership",
            "itn_access",
            "itn_use_all",
            "itn_use_u5",
            "itn_use_pregnant",
            "itn_use_5_10",
            "itn_use_10_20",
            "itn_use_20plus",
            "itn_use_if_access"
          )
        )
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    itn_ownership = ,
    itn_access = ,
    itn_use_all = ,
    itn_use_u5 = ,
    itn_use_pregnant = ,
    itn_use_if_access = {
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
          indicators = category
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
          indicators = c("anc_1plus", "anc_2plus", "anc_3plus", "anc_4plus", "anc_8plus")
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
          indicators = c(
            "any", "public", "public_nochw", "chw",
            "private", "private_formal", "pharmacy",
            "private_informal", "private_formal_pha",
            "trained", "none"
          )
        )
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    act_care_seek = , act_antimal = , act_any_tx = ,
    act_trained = , act_pub = , act_pub_nochw = ,
    act_chw = , act_priv = , act_priv_formal = ,
    act_priv_pharm = , act_priv_informal = ,
    act_priv_form_pha = ,
    antimal = , antimal_any_tx = , antimal_trained = ,
    antimal_pub = , antimal_pub_nochw = , antimal_chw = ,
    antimal_priv = , antimal_formal = , antimal_pharm = ,
    antimal_priv_informal = , antimal_form_pharm = ,
    act_public = , act_among_am = ,
    act = {
      if (!"KR" %in% names(survey_data)) {
        return(skip_indicator("Missing KR data (Children Recode)"))
      }
      tryCatch({
        calc_act_mbg(
          dhs_kr = survey_data$KR,
          gps_data = gps_data,
          indicators = c(
            "act", "act_care_seek", "act_antimal",
            "act_any_tx", "act_trained", "act_pub",
            "act_pub_nochw", "act_chw", "act_priv",
            "act_priv_formal", "act_priv_pharm",
            "act_priv_informal", "act_priv_form_pha",
            "antimal", "antimal_any_tx",
            "antimal_trained", "antimal_pub",
            "antimal_pub_nochw", "antimal_chw",
            "antimal_priv", "antimal_formal",
            "antimal_pharm", "antimal_priv_informal",
            "antimal_form_pharm"
          )
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
          indicators = c("iptp_1plus", "iptp_2plus", "iptp_3plus", "iptp_4plus")
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
          indicators = epi_indicators
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

    fever = {
      if (!"KR" %in% names(survey_data)) {
        return(skip_indicator("Missing KR data (Children Recode)"))
      }
      tryCatch({
        calc_fever_mbg(
          dhs_kr = survey_data$KR,
          gps_data = gps_data
        )
      }, error = function(e) {
        results$skipped <<- glue::glue("Calculation error: {e$message}")
        list()
      })
    },

    antimalarial_public = ,
    antimalarial = {
      if (!"KR" %in% names(survey_data)) {
        return(skip_indicator("Missing KR data (Children Recode)"))
      }
      tryCatch({
        calc_antimalarial_mbg(
          dhs_kr = survey_data$KR,
          gps_data = gps_data,
          indicators = c("antimalarial", "antimalarial_public")
        )
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
      # Skip combined tables — they are reference data, not MBG inputs
      if (grepl("^pfpr_combined_", ind_name)) next

      cli::cli_alert_info("Running MBG for {ind_name}...")

      mbg_result <- tryCatch({
        .run_single_mbg(
          cluster_dt = cluster_data[[ind_name]],
          adm2_sf = adm2_sf,
          adm3_sf = adm3_sf,
          pop_rast = pop_for_indicator,
          indicator_name = ind_name,
          output_dirs = output_dirs,
          country_iso3 = country_iso3,
          survey_year = survey_year,
          survey_type = survey_type,
          aggregation_level = aggregation_level,
          save_rasters = save_rasters,
          cache = cache,
          debug = debug
        )
      }, error = function(e) {
        cli::cli_alert_warning("MBG failed for {ind_name}: {e$message}")
        NULL
      })

      if (!is.null(mbg_result)) {
        results$mbg_estimates[[ind_name]] <- mbg_result$adm_estimates
        if (save_rasters && !is.null(mbg_result$raster_path)) {
          results$raster_paths[[ind_name]] <- mbg_result$raster_path
        }
      }
    }
  }

  results
}


#' Check qs2 Package Availability
#'
#' @noRd
.check_qs2_pkg <- function() {
  if (!requireNamespace("qs2", quietly = TRUE)) {
    cli::cli_warn(c(
      "Package {.pkg qs2} is not installed - cluster data will not be saved",
      "i" = "Install with: install.packages('qs2')"
    ))
    return(FALSE)
  }
  TRUE
}


#' Get Indicator Labels
#'
#' Maps indicator prefixes to meaningful column labels for numerator/denominator.
#'
#' @param ind_name Character. The indicator name (e.g., "pfpr_rdt_u5").
#'
#' @return List with `numerator` and `denominator` label strings.
#'
#' @noRd
.get_indicator_labels <- function(ind_name) {
  # Map indicator prefixes to meaningful labels
  labels <- list(
    numerator = "numerator",
    denominator = "denominator"
  )

  if (grepl("^pfpr_", ind_name)) {
    labels <- list(numerator = "n_positive", denominator = "n_tested")
  } else if (grepl("^itn_ownership", ind_name)) {
    labels <- list(numerator = "n_hh_with_itn", denominator = "n_households")
  } else if (grepl("^itn_access", ind_name)) {
    labels <- list(numerator = "n_with_access", denominator = "n_individuals")
  } else if (grepl("^itn_use", ind_name)) {
    labels <- list(numerator = "n_used_itn", denominator = "n_eligible")
  } else if (grepl("^u5mr", ind_name)) {
    labels <- list(numerator = "n_deaths", denominator = "n_exposed")
  } else if (grepl("^anc", ind_name)) {
    labels <- list(numerator = "n_with_visits", denominator = "n_women")
  } else if (grepl("^act", ind_name)) {
    labels <- list(numerator = "n_received_act", denominator = "n_febrile")
  } else if (grepl("^csb_", ind_name)) {
    labels <- list(numerator = "n_sought_care", denominator = "n_febrile")
  } else if (grepl("^anemia", ind_name)) {
    labels <- list(numerator = "n_anemic", denominator = "n_tested_hb")
  } else if (grepl("^iptp", ind_name)) {
    labels <- list(numerator = "n_received_sp", denominator = "n_women")
  } else if (grepl("^irs", ind_name)) {
    labels <- list(numerator = "n_sprayed", denominator = "n_households")
  } else if (grepl("^epi_|^bcg|^dpt|^measles|^polio", ind_name)) {
    labels <- list(numerator = "n_vaccinated", denominator = "n_children")
  } else if (grepl("^smc", ind_name)) {
    labels <- list(numerator = "n_received_smc", denominator = "n_children")
  } else if (grepl("^fever", ind_name)) {
    labels <- list(numerator = "n_fever", denominator = "n_children")
  } else if (grepl("^antimalarial", ind_name)) {
    labels <- list(numerator = "n_antimalarial", denominator = "n_febrile")
  }

  labels
}


#' Save Cluster Data to qs2 Files
#'
#' Saves cluster-level data for each indicator as versioned qs2 files.
#'
#' Uses \code{sntutils::write_snt_data()} for date-stamped versioning
#' and automatic pruning of older versions (keeps 3 newest by default).
#'
#' @param cluster_data Named list of data.tables from calc_*_mbg functions.
#' @param output_dir Directory to save files.
#' @param country_iso3 Three-letter ISO country code.
#' @param survey_year Survey year.
#' @param survey_type Survey type (e.g., "DHS", "MIS").
#'
#' @return Invisible character vector of saved file paths.
#'
#' @noRd
.save_cluster_data <- function(
  cluster_data,
  output_dir,
  country_iso3,
  survey_year,
  survey_type = "DHS"
) {
  fs::dir_create(output_dir)
  saved_files <- character()

  for (ind_name in names(cluster_data)) {
    dt <- cluster_data[[ind_name]]
    if (is.null(dt) || nrow(dt) == 0) next

    # Convert to tibble to avoid data.table namespace issues
    df <- tibble::as_tibble(dt)

    # Combined tables already have final column names — save directly
    if (!grepl("^pfpr_combined_", ind_name)) {
      labels <- .get_indicator_labels(ind_name)

      df <- df |>
        dplyr::mutate(
          !!labels$numerator := .data$indicator,
          !!labels$denominator := .data$samplesize,
          prop_raw = .data$indicator / .data$samplesize
        ) |>
        dplyr::select(-"indicator", -"samplesize")
    }

    # Versioned save: {country}_{indicator}_cluster_points_{type}_{year}_v{date}.qs2
    data_name <- glue::glue(
      "{tolower(country_iso3)}_{ind_name}_cluster_points_{tolower(survey_type)}_{survey_year}"
    )
    data_name <- as.character(data_name)

    write_result <- sntutils::write_snt_data(
      obj = dt,
      path = output_dir,
      data_name = data_name,
      file_formats = "qs2",
      include_date = TRUE,
      n_saved = 3
    )
    saved_files <- c(saved_files, write_result$path)

    rel_path <- .relative_path(write_result$path)
    cli::cli_alert_success("Saved cluster data: {.file {rel_path}}")
  }

  invisible(saved_files)
}


#' Run Single MBG Model
#'
#' @noRd
.run_single_mbg <- function(
  cluster_dt,
  adm2_sf,
  adm3_sf = NULL,

  pop_rast,
  indicator_name,
  output_dirs,
  country_iso3,
  survey_year,
  survey_type = "DHS",
  aggregation_level = "adm2",
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

  # Pre-compute expected raster paths (used for cache check and saving)
  raster_base <- glue::glue(
    "{tolower(country_iso3)}_{indicator_name}_mbg_{tolower(survey_type)}_{survey_year}"
  )
  expected_raster_paths <- list(
    mean  = fs::path(output_dirs$rasters, paste0(raster_base, "_mean.tif")),
    lower = fs::path(output_dirs$rasters, paste0(raster_base, "_lower.tif")),
    upper = fs::path(output_dirs$rasters, paste0(raster_base, "_upper.tif"))
  )

  # Determine primary polygon based on aggregation level
  cli::cli_alert_info("MBG aggregation level: {.val {aggregation_level}}")
  if (aggregation_level == "adm3") {
    primary_sf <- adm3_sf
    polygon_id_field <- "adm3"
    agg_levels <- list(adm3 = c("adm3", "adm2", "adm1", "adm0"))
    cache_suffix <- "adm3"
  } else {
    primary_sf <- adm2_sf
    polygon_id_field <- "adm2"
    agg_levels <- list(adm2 = c("adm2", "adm1", "adm0"))
    cache_suffix <- "adm2"
  }

  # Debug: Show input summary
  if (isTRUE(debug)) {
    pop_rel_path <- .relative_path(pop_source)
    interm_rel_path <- .relative_path(output_dirs$intermediate)
    cli::cli_h3("Debug: MBG Inputs for {indicator_name}")
    cli::cli_alert_info("Cluster data: {.val {nrow(cluster_dt)}} rows")
    cli::cli_alert_info("Aggregation level: {.val {aggregation_level}}")
    cli::cli_alert_info("Primary polygons: {.val {nrow(primary_sf)}} features")
    cli::cli_alert_info("Population raster: {.file {pop_rel_path}}")
    cli::cli_alert_info("Intermediate dir: {.file {interm_rel_path}}")
  }

  # ---- Cache prediction rasters (early return) ----
  # If all three prediction rasters exist from a prior run, load them directly
  # and skip the entire MBG model fitting process
  if (isTRUE(cache) && isTRUE(save_rasters)) {
    all_rasters_exist <- all(vapply(
      expected_raster_paths, fs::file_exists, logical(1)
    ))

    if (all_rasters_exist) {
      cli::cli_alert_info("Using cached prediction rasters for {.val {indicator_name}}")

      cached_rasters <- lapply(expected_raster_paths, terra::rast)

      mean_col <- paste0(indicator_name, "_mean")
      lower_col <- paste0(indicator_name, "_lower")
      upper_col <- paste0(indicator_name, "_upper")
      multiplier <- if (grepl("^u5mr", indicator_name, ignore.case = TRUE)) {
        1000
      } else {
        100
      }

      adm_estimates <- primary_sf |>
        sf::st_drop_geometry() |>
        dplyr::mutate(
          !!mean_col := terra::extract(
            cached_rasters$mean, primary_sf, fun = mean, na.rm = TRUE
          )[[2]] * multiplier,
          !!lower_col := terra::extract(
            cached_rasters$lower, primary_sf, fun = mean, na.rm = TRUE
          )[[2]] * multiplier,
          !!upper_col := terra::extract(
            cached_rasters$upper, primary_sf, fun = mean, na.rm = TRUE
          )[[2]] * multiplier
        )

      return(list(
        adm_estimates = adm_estimates,
        raster_path = expected_raster_paths,
        cell_predictions = NULL
      ))
    }
  }

  primary_vect <- terra::vect(primary_sf)
  # Use a unique integer row index as the polygon ID to avoid failures when
  # admin names are duplicated across provinces (e.g. two "Central" districts).
  # adm estimates are extracted via terra::extract() directly on primary_sf so
  # this does not affect output column names or values.
  primary_vect$poly_id <- seq_len(nrow(primary_vect))
  polygon_id_field <- "poly_id"

  # ---- Cache ID raster ----
  id_raster_file <- fs::path(
    output_dirs$intermediate,
    glue::glue("{tolower(country_iso3)}_id_raster_{cache_suffix}.tif")
  )

  if (isTRUE(cache) && fs::file_exists(id_raster_file)) {
    cli::cli_alert_info("Using cached ID raster ({cache_suffix})")
    id_raster <- terra::rast(id_raster_file)
  } else {
    cli::cli_alert_info("Building ID raster ({cache_suffix})...")
    id_raster <- mbg::build_id_raster(
      polygons = primary_vect,
      template_raster = pop_rast
    )
    terra::writeRaster(id_raster, id_raster_file, overwrite = TRUE)
    rel_path <- .relative_path(id_raster_file)
    cli::cli_alert_success("Saved ID raster: {.file {rel_path}}")
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
    glue::glue("{tolower(country_iso3)}_aggregation_table_{cache_suffix}.parquet")
  )

  if (isTRUE(cache) && fs::file_exists(agg_file)) {
    cli::cli_alert_info("Using cached aggregation table ({cache_suffix})")
    aggregation_table <- arrow::read_parquet(agg_file)
  } else {
    cli::cli_alert_info("Building aggregation table ({cache_suffix})...")
    aggregation_table <- mbg::build_aggregation_table(
      polygons = primary_vect,
      id_raster = id_raster,
      polygon_id_field = polygon_id_field,
      verbose = FALSE
    )
    arrow::write_parquet(aggregation_table, agg_file)
    rel_path <- .relative_path(agg_file)
    cli::cli_alert_success("Saved aggregation table: {.file {rel_path}}")
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
    aggregation_levels = agg_levels,
    population_raster = pop_rast
  )

  model_runner$run_mbg_pipeline()
  cli::cli_alert_success("MBG model complete")

  # Extract predictions and ensure they are SpatRaster objects
  cell_preds <- model_runner$grid_cell_predictions
  for (.pred_name in c("cell_pred_mean", "cell_pred_lower", "cell_pred_upper")) {
    .pred <- cell_preds[[.pred_name]]
    if (!is.null(.pred) && !inherits(.pred, "SpatRaster")) {
      cell_preds[[.pred_name]] <- terra::setValues(id_raster, as.vector(.pred))
    }
  }

  # Save rasters (mean, lower, upper)
  raster_path <- NULL
  if (save_rasters) {
    fs::dir_create(output_dirs$rasters)
    raster_path <- expected_raster_paths

    rasters_to_save <- list(
      mean  = cell_preds$cell_pred_mean,
      lower = cell_preds$cell_pred_lower,
      upper = cell_preds$cell_pred_upper
    )

    for (rast_name in names(rasters_to_save)) {
      terra::writeRaster(
        rasters_to_save[[rast_name]],
        expected_raster_paths[[rast_name]],
        overwrite = TRUE
      )
      rel_path <- .relative_path(expected_raster_paths[[rast_name]])
      cli::cli_alert_success("Saved raster ({rast_name}): {.file {rel_path}}")
    }
  }

  # Extract ADM estimates at the primary aggregation level
  mean_col <- paste0(indicator_name, "_mean")
  lower_col <- paste0(indicator_name, "_lower")
  upper_col <- paste0(indicator_name, "_upper")

  # U5MR uses "per 1,000" units (epidemiological standard)
  # Other indicators use percentage units (0-100)
  multiplier <- if (grepl("^u5mr", indicator_name, ignore.case = TRUE)) 1000 else 100

  adm_estimates <- primary_sf |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      !!mean_col := terra::extract(
        cell_preds$cell_pred_mean, primary_sf, fun = mean, na.rm = TRUE
      )[[2]] * multiplier,
      !!lower_col := terra::extract(
        cell_preds$cell_pred_lower, primary_sf, fun = mean, na.rm = TRUE
      )[[2]] * multiplier,
      !!upper_col := terra::extract(
        cell_preds$cell_pred_upper, primary_sf, fun = mean, na.rm = TRUE
      )[[2]] * multiplier
    )

  list(
    adm_estimates = adm_estimates,
    raster_path = raster_path,
    cell_predictions = cell_preds
  )
}


#' Compute Derived Rasters (Effective Coverage of Case Management)
#'
#' Multiplies CSB and ACT raster surfaces to produce effective coverage
#' of case management indicators. Produces two derived indicators:
#' \itemize{
#'   \item \code{eff_cm_any}: CSB(any) x ACT
#'   \item \code{eff_cm_public}: CSB(public) x ACT(public care seekers)
#' }
#'
#' @param raster_paths Named list of raster paths from the indicator loop.
#'   Each element is a list with \code{mean}, \code{lower}, \code{upper} paths.
#' @param primary_sf sf object for admin-level extraction.
#' @param output_dirs List with output directory paths (must include \code{rasters}).
#' @param country_iso3 Three-letter ISO country code.
#' @param survey_year Survey year.
#' @param survey_type Survey type (e.g., "DHS").
#' @param save_rasters Logical. If TRUE, writes derived rasters to disk.
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{mbg_estimates}: Named list of admin-level estimate data frames
#'     \item \code{raster_paths}: Named list of raster path lists for derived indicators
#'   }
#'
#' @noRd
.compute_derived_rasters <- function(
  raster_paths,
  primary_sf,
  output_dirs,
  country_iso3,
  survey_year,

  survey_type,
  save_rasters = TRUE,
  cache = FALSE
) {
  result <- list(mbg_estimates = list(), raster_paths = list())

  # Define pairs: derived_name = list(csb_indicator, act_indicator)
  pairs <- list(
    eff_cm_any    = list(csb = "csb_any",    act = "act"),
    eff_cm_public = list(csb = "csb_public", act = "act_public")
  )

  for (derived_name in names(pairs)) {
    pair <- pairs[[derived_name]]
    csb_key <- pair$csb
    act_key <- pair$act

    # Build expected paths for the derived raster (used for cache check)
    raster_base <- glue::glue(
      "{tolower(country_iso3)}_{derived_name}_mbg_{tolower(survey_type)}_{survey_year}"
    )
    expected_paths <- list(
      mean  = fs::path(output_dirs$rasters, paste0(raster_base, "_mean.tif")),
      lower = fs::path(output_dirs$rasters, paste0(raster_base, "_lower.tif")),
      upper = fs::path(output_dirs$rasters, paste0(raster_base, "_upper.tif"))
    )

    # ---- Cache: use existing derived rasters if available ----
    all_cached <- isTRUE(cache) && all(vapply(
      expected_paths, fs::file_exists, logical(1)
    ))

    if (all_cached) {
      cli::cli_alert_info(
        "Using cached derived rasters for {.val {derived_name}}"
      )

      tryCatch({
        prod_mean  <- terra::rast(expected_paths$mean)
        prod_lower <- terra::rast(expected_paths$lower)
        prod_upper <- terra::rast(expected_paths$upper)

        mean_col  <- paste0(derived_name, "_mean")
        lower_col <- paste0(derived_name, "_lower")
        upper_col <- paste0(derived_name, "_upper")

        adm_estimates <- primary_sf |>
          sf::st_drop_geometry() |>
          dplyr::mutate(
            !!mean_col := terra::extract(
              prod_mean, primary_sf, fun = mean, na.rm = TRUE
            )[[2]] * 100,
            !!lower_col := terra::extract(
              prod_lower, primary_sf, fun = mean, na.rm = TRUE
            )[[2]] * 100,
            !!upper_col := terra::extract(
              prod_upper, primary_sf, fun = mean, na.rm = TRUE
            )[[2]] * 100
          )

        result$mbg_estimates[[derived_name]] <- adm_estimates
        result$raster_paths[[derived_name]] <- expected_paths
        cli::cli_alert_success(
          "Derived indicator {.val {derived_name}} loaded from cache"
        )
        next
      }, error = function(e) {
        cli::cli_alert_warning(
          "Cache load failed for {derived_name}, recomputing: {e$message}"
        )
      })
    }

    # ---- Compute from component rasters ----

    # Check both component indicators exist in current run
    if (is.null(raster_paths[[csb_key]]) || is.null(raster_paths[[act_key]])) {
      next
    }

    # Verify all raster files exist
    csb_paths <- raster_paths[[csb_key]]
    act_paths <- raster_paths[[act_key]]
    all_exist <- all(
      vapply(csb_paths, fs::file_exists, logical(1)),
      vapply(act_paths, fs::file_exists, logical(1))
    )
    if (!all_exist) next

    cli::cli_alert_info("Computing derived indicator: {.val {derived_name}}")

    tryCatch({
      # Load component rasters (on 0-1 scale)
      csb_mean  <- terra::rast(csb_paths$mean)
      csb_lower <- terra::rast(csb_paths$lower)
      csb_upper <- terra::rast(csb_paths$upper)
      act_mean  <- terra::rast(act_paths$mean)
      act_lower <- terra::rast(act_paths$lower)
      act_upper <- terra::rast(act_paths$upper)

      # Multiply surfaces (both on 0-1 scale)
      prod_mean  <- csb_mean  * act_mean
      prod_lower <- csb_lower * act_lower
      prod_upper <- csb_upper * act_upper

      # Save derived rasters
      derived_raster_paths <- NULL
      if (isTRUE(save_rasters)) {
        fs::dir_create(output_dirs$rasters)
        derived_raster_paths <- expected_paths

        terra::writeRaster(prod_mean, derived_raster_paths$mean, overwrite = TRUE)
        terra::writeRaster(prod_lower, derived_raster_paths$lower, overwrite = TRUE)
        terra::writeRaster(prod_upper, derived_raster_paths$upper, overwrite = TRUE)

        for (rast_type in names(derived_raster_paths)) {
          rel_path <- .relative_path(derived_raster_paths[[rast_type]])
          cli::cli_alert_success("Saved derived raster ({rast_type}): {.file {rel_path}}")
        }
      }

      # Extract admin-level estimates (multiply by 100 for percentage)
      mean_col  <- paste0(derived_name, "_mean")
      lower_col <- paste0(derived_name, "_lower")
      upper_col <- paste0(derived_name, "_upper")

      adm_estimates <- primary_sf |>
        sf::st_drop_geometry() |>
        dplyr::mutate(
          !!mean_col := terra::extract(
            prod_mean, primary_sf, fun = mean, na.rm = TRUE
          )[[2]] * 100,
          !!lower_col := terra::extract(
            prod_lower, primary_sf, fun = mean, na.rm = TRUE
          )[[2]] * 100,
          !!upper_col := terra::extract(
            prod_upper, primary_sf, fun = mean, na.rm = TRUE
          )[[2]] * 100
        )

      result$mbg_estimates[[derived_name]] <- adm_estimates
      if (!is.null(derived_raster_paths)) {
        result$raster_paths[[derived_name]] <- derived_raster_paths
      }

      cli::cli_alert_success("Derived indicator {.val {derived_name}} complete")

    }, error = function(e) {
      cli::cli_alert_warning(
        "Failed to compute {derived_name}: {e$message}"
      )
    })
  }

  result
}


#' Aggregate Cluster Data to Administrative Level
#'
#' Spatially joins cluster-level data to admin boundaries and aggregates
#' statistics (n_tested, n_positive, n_clusters, raw proportion) per admin unit.
#'
#' @param cluster_data Named list of data.tables from calc_*_mbg functions.
#'   Each data.table should have columns: cluster_id, indicator, samplesize, x, y.
#' @param admin_sf sf object with administrative boundaries.
#' @param admin_col Name of the admin identifier column in admin_sf.
#' @param join_nearest Logical. If TRUE, assigns clusters outside all polygons
#'   to the nearest administrative unit. Default: TRUE.
#'
#' @return A tibble with one row per admin unit and columns for each indicator:
#'   - n_tested_{indicator}: Total sample size
#'   - n_pos_{indicator}: Total positive count
#'   - n_clusters_{indicator}: Number of clusters
#'   - {indicator}_raw: Raw proportion (n_pos / n_tested)
#'
#' @noRd
.aggregate_cluster_to_admin <- function(
  cluster_data,
  admin_sf,
  admin_col = "adm2",
  join_nearest = TRUE
) {
  if (is.null(cluster_data) || length(cluster_data) == 0) {
    return(NULL)
  }

  # Ensure admin_sf is valid
  if (!inherits(admin_sf, "sf")) {
    cli::cli_alert_warning("admin_sf is not an sf object - skipping cluster aggregation")
    return(NULL)
  }

  # Check admin_col exists
 if (!admin_col %in% names(admin_sf)) {
    cli::cli_alert_warning(
      "Column '{admin_col}' not found in admin_sf - skipping cluster aggregation"
    )
    return(NULL)
  }

  # Initialize result with admin identifiers
  result <- admin_sf |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::all_of(admin_col)) |>
    dplyr::distinct() |>
    tibble::as_tibble()

  # Process each indicator
  for (ind_name in names(cluster_data)) {
    cluster_dt <- cluster_data[[ind_name]]

    if (is.null(cluster_dt) || nrow(cluster_dt) == 0) {
      next
    }

    # Check required columns
    required_cols <- c("x", "y", "indicator", "samplesize")
    if (!all(required_cols %in% names(cluster_dt))) {
      cli::cli_alert_warning(
        "Cluster data for '{ind_name}' missing required columns - skipping"
      )
      next
    }

    # Convert to sf
    cluster_sf <- cluster_dt |>
      tibble::as_tibble() |>
      dplyr::filter(!is.na(x), !is.na(y), x != 0, y != 0) |>
      sf::st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE)

    # Transform to match admin CRS
    admin_crs <- sf::st_crs(admin_sf)
    if (!is.na(admin_crs)) {
      cluster_sf <- sf::st_transform(cluster_sf, admin_crs)
    }

    # Spatial join
    joined <- sf::st_join(
      cluster_sf,
      admin_sf |> dplyr::select(dplyr::all_of(admin_col)),
      join = sf::st_within,
      left = TRUE
    )

    # Handle unmatched clusters
    if (join_nearest) {
      unmatched <- is.na(joined[[admin_col]])
      if (any(unmatched)) {
        nearest_idx <- sf::st_nearest_feature(
          joined[unmatched, ],
          admin_sf
        )
        joined[[admin_col]][unmatched] <- admin_sf[[admin_col]][nearest_idx]
      }
    }

    # Aggregate by admin unit
    joined_df <- sf::st_drop_geometry(joined)

    # U5MR uses "per 1,000" units (epidemiological standard)
    # Other indicators use percentage units (0-100)
    multiplier <- if (grepl("^u5mr", ind_name, ignore.case = TRUE)) 1000 else 100

    agg <- joined_df |>
      dplyr::filter(!is.na(.data[[admin_col]])) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(admin_col))) |>
      dplyr::summarise(
        n_tested = sum(samplesize, na.rm = TRUE),
        n_pos = sum(indicator, na.rm = TRUE),
        n_clusters = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        raw_prop = dplyr::if_else(
          n_tested > 0,
          round(n_pos / n_tested * multiplier, 2),
          NA_real_
        )
      )

    # Rename columns with indicator name
    full_name <- ind_name

    agg <- agg |>
      dplyr::rename(
        !!paste0("n_tested_", full_name) := n_tested,
        !!paste0("n_pos_", full_name) := n_pos,
        !!paste0("n_clusters_", full_name) := n_clusters,
        !!paste0(full_name, "_raw") := raw_prop
      )

    # Merge into result
    result <- result |>
      dplyr::left_join(agg, by = admin_col)
  }

  result
}


#' Build Final Dataset
#'
#' @noRd
.build_final_dataset <- function(
  adm2_sf,
  mbg_estimates,
  cluster_data,
  survey_year,
  survey_type = "DHS",
  country_iso3 = NULL,
  country_iso2 = NULL,
  adm3_sf = NULL,
  aggregation_level = "adm2",
  cluster_interview_months = NULL
) {
  # Determine base sf object based on aggregation level
  base_sf <- if (aggregation_level == "adm3") adm3_sf else adm2_sf

  # Start with base sf

  final <- base_sf |>
    sf::st_drop_geometry() |>
    tibble::as_tibble()

  # ---- Add standard identifier columns ----

  # Add country codes if provided (uppercase)
  if (!is.null(country_iso3)) {
    final$iso3_code <- toupper(country_iso3)
  }

  if (!is.null(country_iso2)) {
    final$dhs_code <- toupper(country_iso2)
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

  # If aggregation_level is adm3, ensure adm3 column exists
  if (aggregation_level == "adm3" && !"adm3" %in% names(final)) {
    adm3_candidates <- c(
      "ADM3_NAME", "ADMIN3", "commune", "Commune", "NAME_3",
      "subdistrict", "Subdistrict", "ward", "Ward"
    )
    for (col in adm3_candidates) {
      if (col %in% names(final)) {
        final$adm3 <- final[[col]]
        break
      }
    }
  }

  # Add survey year and type
  final$survey_year <- survey_year
  final$survey_type <- survey_type

  # Determine merge key based on aggregation level
  merge_key <- if (aggregation_level == "adm3") "adm3" else "adm2"

  # ---- Add median survey month per admin unit ----
  if (!is.null(cluster_interview_months) && nrow(cluster_interview_months) > 0) {
    month_agg <- .aggregate_interview_month_to_admin(
      cluster_months = cluster_interview_months,
      admin_sf = base_sf,
      admin_col = merge_key
    )
    if (!is.null(month_agg) && nrow(month_agg) > 0) {
      final <- final |>
        dplyr::left_join(month_agg, by = merge_key)
    }
  }

  # ---- Merge MBG estimates ----

  # Warn if merge key column not found in final dataset
  if (!merge_key %in% names(final)) {
    cli::cli_alert_warning(
      "Column '{merge_key}' not found in shapefile - MBG estimates cannot be merged"
    )
  }

  for (name in names(mbg_estimates)) {
    est <- mbg_estimates[[name]]

    if (!is.null(est) && nrow(est) > 0) {
      # Find columns to merge (exclude admin columns already present)
      merge_cols <- setdiff(names(est), names(final))

      if (merge_key %in% names(est) && merge_key %in% names(final)) {
        final <- final |>
          dplyr::left_join(
            est |> dplyr::select(dplyr::all_of(merge_key), dplyr::all_of(merge_cols)),
            by = merge_key
          )
      } else if (!merge_key %in% names(est)) {
        cli::cli_alert_warning(
          "MBG estimates for '{name}' missing '{merge_key}' column - skipping merge"
        )
      }
    }
  }

  # ---- Aggregate cluster data to admin level ----

  if (!is.null(cluster_data) && length(cluster_data) > 0) {
    # Filter out combined tables (different column structure)
    cluster_data_for_agg <- cluster_data[
      !grepl("^pfpr_combined_", names(cluster_data))
    ]

    cluster_stats <- .aggregate_cluster_to_admin(
      cluster_data = cluster_data_for_agg,
      admin_sf = base_sf,
      admin_col = merge_key,
      join_nearest = TRUE
    )

    if (!is.null(cluster_stats) && nrow(cluster_stats) > 0) {
      # Merge cluster stats into final
      cluster_cols <- setdiff(names(cluster_stats), names(final))
      if (length(cluster_cols) > 0 && merge_key %in% names(cluster_stats)) {
        final <- final |>
          dplyr::left_join(
            cluster_stats |> dplyr::select(
              dplyr::all_of(merge_key),
              dplyr::all_of(cluster_cols)
            ),
            by = merge_key
          )
      }
    }
  }

  # ---- Select and reorder final columns ----

  # Define required identifier columns (include adm3 if aggregation_level is adm3)
  if (aggregation_level == "adm3") {
    id_cols <- c("iso3_code", "dhs_code", "adm0", "adm1", "adm2", "adm3", "survey_year", "survey_type", "median_survey_month")
  } else {
    id_cols <- c("iso3_code", "dhs_code", "adm0", "adm1", "adm2", "survey_year", "survey_type", "median_survey_month")
  }
  id_cols_present <- intersect(id_cols, names(final))

  # Identify indicator columns (MBG estimates and cluster statistics)
  mbg_cols <- names(final)[grepl("_(mean|lower|upper)$", names(final))]
  cluster_stat_cols <- names(final)[grepl("^(n_tested_|n_pos_|n_clusters_|.*_raw$)", names(final))]
  indicator_cols <- c(mbg_cols, cluster_stat_cols)

  # Select only required columns (drop GUIDs, hashes, etc.)
  final <- final |>
    dplyr::select(dplyr::all_of(c(id_cols_present, indicator_cols)))

  final
}


#' Validate Raster Paths
#'
#' Validates that population raster paths exist and warns about country code mismatches.
#'
#' @param raster_input Raster input (list of paths, single path, or SpatRaster).
#' @param country_iso3 Expected country ISO3 code.
#' @param arg_name Name of the argument for error messages.
#'
#' @noRd
.validate_raster_paths <- function(raster_input, country_iso3, arg_name) {
  iso3_lower <- tolower(country_iso3)

  # Case 1: Already a SpatRaster - nothing to validate
  if (inherits(raster_input, "SpatRaster")) {
    return(invisible(TRUE))
  }

  # Case 2: Named list of paths
  if (is.list(raster_input) && !is.null(names(raster_input))) {
    for (year in names(raster_input)) {
      path <- raster_input[[year]]
      if (is.character(path) && length(path) == 1) {
        .validate_single_raster_path(path, iso3_lower, arg_name, year)
      }
    }
    return(invisible(TRUE))
  }

  # Case 3: Single file path
  if (is.character(raster_input) && length(raster_input) == 1) {
    .validate_single_raster_path(raster_input, iso3_lower, arg_name, NULL)
    return(invisible(TRUE))
  }

  # Unknown format - will be caught later by .load_raster_for_year
  invisible(TRUE)
}


#' Validate Single Raster Path
#'
#' @noRd
.validate_single_raster_path <- function(path, iso3_lower, arg_name, year) {
  # Check file exists
  if (!file.exists(path)) {
    year_msg <- if (!is.null(year)) glue::glue(" for year {year}") else ""
    cli::cli_abort(c(
      "Population raster file not found{year_msg}",
      "x" = "Path: {.path {path}}",
      "i" = "Check that {.arg {arg_name}} paths are correct"
    ))
  }

  # Check for country code mismatch in filename
  filename <- tolower(basename(path))

  # Common country code patterns in WorldPop filenames
  # e.g., "bfa_ppp_2010.tif", "bdi_total_00_04_2010.tif"
  country_pattern <- "^([a-z]{3})_"
  match <- regmatches(filename, regexpr(country_pattern, filename))

  if (length(match) > 0 && nchar(match) >= 3) {
    file_iso3 <- substr(match, 1, 3)
    if (file_iso3 != iso3_lower) {
      cli::cli_abort(c(
        "Country code mismatch in population raster filename",
        "x" = "Expected: {.val {toupper(iso3_lower)}} but file has: {.val {toupper(file_iso3)}}",
        "i" = "File: {.file {basename(path)}}",
        "i" = "Check that {.arg {arg_name}} uses the correct country's rasters"
      ))
    }
  }

  invisible(TRUE)
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
#' - Indicator columns (percentages, rates) → 2 decimal places
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
    "^n_", "samplesize", "sample_size", "n_clusters",
    "n_tested", "n_pos", "n_households", "n_births", "n_deaths",
    "n_fever", "n_sought", "n_women", "n_recent", "n_iptp",
    "n_public", "n_private", "n_none", "n_trained", "n_individuals",
    "n_with_access", "positive", "pop", "cluster_id", "_id$",
    "median_survey_month"
  )

  # Indicator columns (percentages 0-100, or rates like u5mr per 1,000) → 2 decimal places
  proportion_patterns <- c(
    "^pfpr_", "^itn_", "^irs_", "^anc_", "^csb_", "^anemia",
    "^severe_anemia", "^iptp_", "^epi_", "^u5mr", "^smc_", "^act_", "^eff_cm",
    "access", "use", "ownership", "coverage", "proportion", "prop_",
    "_low$", "_upp$", "_se$", "^ci_l", "^ci_u",
    "^mean$", "^lower$", "^upper$", "_raw$",
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
