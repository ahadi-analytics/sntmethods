#' MBG Output Generation
#'
#' Functions for generating outputs from MBG analysis: rasters, CSVs,
#' and various map types.
#'
#' @name mbg_outputs
#' @keywords internal
NULL


#' Add Proportion Columns to MBG Cluster Data
#'
#' Adds user-friendly columns (n_positive, n_tested, prop_raw) to MBG
#' cluster data.tables that have indicator/samplesize columns.
#'
#' @param dt data.table with indicator and samplesize columns
#'
#' @return data.table with additional columns
#'
#' @keywords internal
#' @noRd
.add_mbg_proportion_cols <- function(dt) {
  if (!data.table::is.data.table(dt)) {
    dt <- data.table::as.data.table(dt)
  }

  if (all(c("indicator", "samplesize") %in% names(dt))) {
    # Use set() to avoid data.table namespace issues in package context
    data.table::set(dt, j = "n_positive", value = dt$indicator)
    data.table::set(dt, j = "n_tested", value = dt$samplesize)
    data.table::set(dt, j = "prop_raw", value = dt$indicator / dt$samplesize)
  }

  dt
}


#' Save MBG Cluster Data
#'
#' Saves cluster-level MBG data to CSV files for later reuse. Works with
#' output from any calc_*_mbg() function. Each indicator is saved as a
#' separate file containing cluster coordinates, sample sizes, and values.
#'
#' @param mbg_results Named list of data.tables from any calc_*_mbg() function.
#' @param output_dir Directory to save CSV files.
#' @param file_prefix Prefix for output filenames. Default: "mbg_cluster_data".
#' @param country_iso3 Optional ISO3 country code to include in filename.
#' @param survey_year Optional survey year to include in filename.
#'
#' @return Invisibly returns a character vector of saved file paths.
#'
#' @details
#' Each saved CSV contains:
#' \itemize{
#'   \item cluster_id: DHS cluster identifier
#'   \item x: Longitude
#'   \item y: Latitude
#'   \item indicator: Numerator count (for MBG input)
#'   \item samplesize: Denominator count (for MBG input)
#'   \item n_positive: Numerator (alias)
#'   \item n_tested: Denominator (alias)
#'   \item prop_raw: Raw proportion (indicator / samplesize)
#' }
#'
#' @examples
#' \dontrun{
#' # Works with any MBG indicator
#' itn_results <- calc_itn_mbg(hr_data, pr_data, gps_data)
#' save_mbg_cluster_data(itn_results, "data/clusters", country_iso3 = "BDI")
#'
#' pfpr_results <- calc_pfpr_mbg(pr_data, gps_data)
#' save_mbg_cluster_data(pfpr_results, "data/clusters", country_iso3 = "BDI")
#'
#' anc_results <- calc_anc_mbg(ir_data, gps_data)
#' save_mbg_cluster_data(anc_results, "data/clusters", country_iso3 = "BDI")
#' }
#'
#' @export
save_mbg_cluster_data <- function(
  mbg_results,
  output_dir,
  file_prefix = "mbg_cluster_data",
  country_iso3 = NULL,
  survey_year = NULL
) {
  # Fail fast on missing suggested dependency (data.table for fwrite)
  .check_pkg(
    "data.table",
    reason = "to write MBG cluster CSVs in `save_mbg_cluster_data()`"
  )

  if (!is.list(mbg_results) || is.data.frame(mbg_results)) {
    cli::cli_abort(
      "`mbg_results` must be a named list from a calc_*_mbg() function"
    )
  }

  if (length(mbg_results) == 0) {
    cli::cli_abort("No indicators found in `mbg_results`")
  }

  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_alert_info("Created directory: {output_dir}")
  }

  saved_files <- character(0)

  for (indicator_name in names(mbg_results)) {
    mbg_data <- mbg_results[[indicator_name]]

    # Ensure proportion columns are present
    mbg_data <- .add_mbg_proportion_cols(mbg_data)

    # Build filename
    name_parts <- c(file_prefix, indicator_name)
    if (!is.null(country_iso3) && !is.na(country_iso3)) {
      name_parts <- c(country_iso3, name_parts)
    }
    if (!is.null(survey_year) && !is.na(survey_year)) {
      name_parts <- c(name_parts, survey_year)
    }

    filename <- paste0(paste(name_parts, collapse = "_"), ".csv")
    filepath <- file.path(output_dir, filename)

    # Save as CSV
    data.table::fwrite(mbg_data, filepath)
    saved_files <- c(saved_files, filepath)

    cli::cli_alert_success(
      "Saved {indicator_name}: {nrow(mbg_data)} clusters -> {filepath}"
    )
  }

  cli::cli_alert_info("Saved {length(saved_files)} cluster data files")

  invisible(saved_files)
}


#' Plot MBG Cluster Map
#'
#' Creates a map of DHS cluster-level estimates from any MBG indicator,
#' with points colored by proportion and sized by sample size.
#'
#' @param mbg_data Data.table or data.frame from any calc_*_mbg() function
#'   containing columns: cluster_id, x, y, indicator, samplesize.
#' @param adm0_sf sf object with country boundary (optional).
#' @param adm_sf sf object with admin boundaries to overlay (optional).
#' @param title Plot title. Default: "Indicator by DHS Cluster".
#' @param subtitle Plot subtitle. Default: NULL.
#' @param caption Plot caption. Default: NULL.
#' @param legend_label Legend label for proportion scale. Default: "Proportion".
#' @param point_alpha Point transparency. Default: 0.9.
#' @param point_size_range Size range for points. Default: c(0.8, 5).
#' @param palette Color palette - either "heat" (yellow-red) or "blue" (YlGnBu).
#'   Default: "heat".
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' itn_data <- calc_itn_mbg(hr_data, pr_data, gps_data)
#' plot_mbg_clusters(
#'   itn_data[["itn_access"]],
#'   adm0_sf = country_boundary,
#'   title = "ITN Access by DHS Cluster"
#' )
#' }
#'
#' @export
plot_mbg_clusters <- function(
  mbg_data,
  adm0_sf = NULL,
  adm_sf = NULL,
  title = "Indicator by DHS Cluster",
  subtitle = NULL,
  caption = NULL,
  legend_label = "Proportion",
  point_alpha = 0.9,
  point_size_range = c(0.8, 5),
  palette = "heat"
) {
  # Fail fast on missing suggested dependencies
  .check_pkg(
    "ggplot2",
    reason = "for plotting in `plot_mbg_clusters()`"
  )
  # sf is always needed because the function builds an sf object from x/y
  .check_spatial_pkg("sf", "plot_mbg_clusters")

  # Validate input data
  required_cols <- c("x", "y", "indicator", "samplesize")
  missing_cols <- setdiff(required_cols, names(mbg_data))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Missing required columns: {.var {missing_cols}}. ",
      "Use output from a calc_*_mbg() function."
    )
  }

  # Ensure proportion column exists
  mbg_data <- .add_mbg_proportion_cols(mbg_data)

  # Convert to sf for plotting
  cluster_sf <- sf::st_as_sf(
    mbg_data,
    coords = c("x", "y"),
    crs = 4326
  ) |>
    dplyr::mutate(prop_pct = .data$prop_raw * 100)

  # Select color palette
  if (palette == "heat") {
    colors <- c(
      "#ffffcc", "#ffeda0", "#fed976", "#feb24c",
      "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026"
    )
  } else {
    colors <- c(
      "#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb",
      "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58"
    )
  }

  # Build plot
  p <- ggplot2::ggplot()

  # Add country boundary if provided
  if (!is.null(adm0_sf)) {
    p <- p +
      ggplot2::geom_sf(
        data = adm0_sf,
        fill = ggplot2::alpha("grey98", 0.5),
        color = "grey70",
        linewidth = 0.4
      )
  }

  # Add admin boundaries if provided
  if (!is.null(adm_sf)) {
    p <- p +
      ggplot2::geom_sf(
        data = adm_sf,
        fill = ggplot2::alpha("grey99", 0.25),
        color = "grey85",
        linewidth = 0.2
      )
  }

  # Add cluster points
  p <- p +
    ggplot2::geom_sf(
      data = cluster_sf,
      ggplot2::aes(color = .data$prop_pct, size = .data$n_tested),
      alpha = point_alpha
    ) +
    ggplot2::scale_color_gradientn(
      colors = colors,
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%"),
      name = legend_label
    ) +
    ggplot2::scale_size_continuous(
      range = point_size_range,
      name = "Sample size"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title.position = "top",
      legend.key.width = grid::unit(1, "cm"),
      legend.spacing.x = ggplot2::unit(3.2, "cm")
    )

  p
}


#' Plot All MBG Cluster Maps
#'
#' Creates cluster-level maps for all indicators in the output from any
#' calc_*_mbg() function. Optionally saves each plot to disk.
#'
#' @param mbg_results Named list of data.tables from any calc_*_mbg() function.
#' @param adm0_sf sf object with country boundary (optional).
#' @param adm_sf sf object with admin boundaries to overlay (optional).
#' @param country_name Country name for plot titles. Default: NULL.
#' @param survey_year Survey year for plot titles. Default: NULL.
#' @param output_dir Directory to save plots. If NULL, plots are returned
#'   but not saved. Default: NULL.
#' @param file_prefix Prefix for output filenames. Default: "mbg_clusters".
#' @param width Plot width in inches. Default: 10.
#' @param height Plot height in inches. Default: 9.
#' @param dpi Plot resolution. Default: 320.
#' @param ... Additional arguments passed to `plot_mbg_clusters()`.
#'
#' @return A named list of ggplot2 objects (one per indicator).
#'
#' @examples
#' \dontrun{
#' # Works with any MBG indicator
#' itn_results <- calc_itn_mbg(hr_data, pr_data, gps_data)
#' plots <- plot_mbg_clusters_all(
#'   itn_results,
#'   adm0_sf = country_boundary,
#'   country_name = "Burundi",
#'   survey_year = 2021,
#'   output_dir = "outputs/"
#' )
#' }
#'
#' @export
plot_mbg_clusters_all <- function(
  mbg_results,
  adm0_sf = NULL,
  adm_sf = NULL,
  country_name = NULL,
  survey_year = NULL,
  output_dir = NULL,
  file_prefix = "mbg_clusters",
  width = 10,
  height = 9,
  dpi = 320,
  ...
) {
  # Fail fast on missing suggested dependencies (glue for title/filename,
  # ggplot2 for ggsave; sf is enforced inside plot_mbg_clusters())
  .check_pkg(
    c("glue", "ggplot2"),
    reason = "to build / save cluster maps in `plot_mbg_clusters_all()`"
  )

  if (!is.list(mbg_results) || is.data.frame(mbg_results)) {
    cli::cli_abort(
      "`mbg_results` must be a named list from a calc_*_mbg() function"
    )
  }

  if (length(mbg_results) == 0) {
    cli::cli_abort("No indicators found in `mbg_results`")
  }

  # Create output directory if saving
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
  }

  plots <- list()

  for (indicator_name in names(mbg_results)) {
    mbg_data <- mbg_results[[indicator_name]]

    # Build title
    indicator_label <- gsub("_", " ", indicator_name) |> toupper()
    title <- if (!is.null(country_name) && !is.null(survey_year)) {
      glue::glue("{country_name}: {indicator_label} by DHS cluster ({survey_year})")
    } else if (!is.null(country_name)) {
      glue::glue("{country_name}: {indicator_label} by DHS cluster")
    } else {
      glue::glue("{indicator_label} by DHS cluster")
    }

    caption <- if (!is.null(survey_year)) {
      glue::glue("Data source: DHS {survey_year}")
    } else {
      NULL
    }

    # Create plot
    p <- plot_mbg_clusters(
      mbg_data = mbg_data,
      adm0_sf = adm0_sf,
      adm_sf = adm_sf,
      title = title,
      caption = caption,
      legend_label = indicator_label,
      ...
    )

    plots[[indicator_name]] <- p

    # Save if output directory provided
    if (!is.null(output_dir)) {
      filename <- if (!is.null(survey_year)) {
        glue::glue("{file_prefix}_{indicator_name}_{survey_year}.png")
      } else {
        glue::glue("{file_prefix}_{indicator_name}.png")
      }

      filepath <- file.path(output_dir, filename)

      ggplot2::ggsave(
        filename = filepath,
        plot = p,
        width = width,
        height = height,
        dpi = dpi
      )

      cli::cli_alert_success("Saved: {filepath}")
    }
  }

  cli::cli_alert_info("Generated {length(plots)} cluster maps")

  invisible(plots)
}


#' Save MBG Rasters
#'
#' Saves MBG prediction rasters (mean, lower, upper) to files.
#'
#' @param cell_predictions List with cell_pred_mean, cell_pred_lower,
#'   cell_pred_upper rasters from MBG.
#' @param indicator_name Name of the indicator (e.g., "itn_access").
#' @param survey_year Survey year.
#' @param path Output directory path.
#' @param country_iso3 Three-letter country code.
#' @param format Raster format. Default: "tif".
#'
#' @return Named list of output file paths.
#'
#' @export
save_mbg_rasters <- function(
  cell_predictions,
  indicator_name,
  survey_year,
  path,
  country_iso3,
  format = "tif"
) {
  # Validate required parameters
  if (is.null(country_iso3) || is.na(country_iso3) || !nzchar(country_iso3)) {
    cli::cli_abort("country_iso3 must be a non-empty string")
  }
  if (is.null(indicator_name) || is.na(indicator_name) || !nzchar(indicator_name)) {
    cli::cli_abort("indicator_name must be a non-empty string")
  }
  if (is.null(survey_year) || is.na(survey_year)) {
    cli::cli_abort("survey_year must be non-NA")
  }

  # Check for required spatial packages + glue used for filename
  .check_spatial_pkg("terra", "save_mbg_rasters")
  .check_spatial_pkg("fs", "save_mbg_rasters")
  .check_pkg("glue", reason = "to build raster filenames in `save_mbg_rasters()`")
  fs::dir_create(path)

  # Build file paths
  base_name <- glue::glue("{country_iso3}_{indicator_name}_mbg_{survey_year}")

  paths <- list(
    mean = fs::path(path, paste0(base_name, "_mean.", format)),
    lower = fs::path(path, paste0(base_name, "_lower.", format)),
    upper = fs::path(path, paste0(base_name, "_upper.", format))
  )

  # Save rasters
  if (!is.null(cell_predictions$cell_pred_mean)) {
    terra::writeRaster(
      cell_predictions$cell_pred_mean,
      paths$mean,
      overwrite = TRUE
    )
    cli::cli_alert_success("Saved: {.file {basename(paths$mean)}}")
  }

  if (!is.null(cell_predictions$cell_pred_lower)) {
    terra::writeRaster(
      cell_predictions$cell_pred_lower,
      paths$lower,
      overwrite = TRUE
    )
    cli::cli_alert_success("Saved: {.file {basename(paths$lower)}}")
  }

  if (!is.null(cell_predictions$cell_pred_upper)) {
    terra::writeRaster(
      cell_predictions$cell_pred_upper,
      paths$upper,
      overwrite = TRUE
    )
    cli::cli_alert_success("Saved: {.file {basename(paths$upper)}}")
  }

  paths
}


#' Aggregate Raster to Administrative Level
#'
#' Calculates population-weighted or simple mean of raster values
#' within administrative unit polygons.
#'
#' @param raster SpatRaster to aggregate.
#' @param admin_sf sf object with admin boundaries.
#' @param pop_raster Optional population raster for weighting.
#' @param method Aggregation method: "mean", "weighted_mean", "sum".
#' @param fun Custom aggregation function (overrides method).
#'
#' @return Data frame with admin unit values.
#'
#' @export
aggregate_raster_to_admin <- function(
  raster,
  admin_sf,
  pop_raster = NULL,
  method = "weighted_mean",
  fun = NULL
) {
  # Fail fast on missing suggested dependencies. terra/sf required for any
  # path; exactextractr only required for population-weighted extraction.
  .check_spatial_pkg("terra", "aggregate_raster_to_admin")
  .check_spatial_pkg("sf", "aggregate_raster_to_admin")

  # Input validation
  if (!inherits(raster, "SpatRaster")) {
    cli::cli_abort("raster must be a SpatRaster object")
  }

  if (!inherits(admin_sf, "sf")) {
    cli::cli_abort("admin_sf must be an sf object")
  }

  if (!method %in% c("mean", "weighted_mean", "sum")) {
    cli::cli_abort("method must be one of: 'mean', 'weighted_mean', 'sum'")
  }

  if (is.null(fun)) {
    if (method == "weighted_mean" && !is.null(pop_raster)) {
      # Population-weighted extraction requires exactextractr
      .check_pkg(
        "exactextractr",
        reason = "for population-weighted raster extraction in `aggregate_raster_to_admin()`"
      )

      # Population-weighted mean
      result <- exactextractr::exact_extract(
        raster,
        admin_sf,
        fun = "weighted_mean",
        weights = pop_raster
      )
    } else if (method == "sum") {
      result <- terra::extract(
        raster,
        admin_sf,
        fun = sum,
        na.rm = TRUE
      )[[2]]
    } else {
      # Simple mean
      result <- terra::extract(
        raster,
        admin_sf,
        fun = mean,
        na.rm = TRUE
      )[[2]]
    }
  } else {
    result <- terra::extract(
      raster,
      admin_sf,
      fun = fun,
      na.rm = TRUE
    )[[2]]
  }

  result
}


#' Build Final ADM2 Dataset
#'
#' Combines multiple indicator estimates into a single ADM2-level dataset.
#'
#' @param adm2_sf sf object with ADM2 boundaries.
#' @param adm1_estimates Named list of ADM1 survey-weighted estimates.
#' @param mbg_estimates Named list of MBG ADM2 predictions.
#' @param survey_dates Data frame with survey date information.
#' @param survey_year Survey year.
#'
#' @return Data frame with one row per ADM2 and columns for each indicator.
#'
#' @export
build_final_dataset <- function(
  adm2_sf,
  adm1_estimates = NULL,
  mbg_estimates = NULL,
  survey_dates = NULL,
  survey_year = NULL
) {
  # Fail fast on missing suggested dependencies
  .check_spatial_pkg("sf", "build_final_dataset")
  .check_pkg(
    "tibble",
    reason = "to build the final dataset in `build_final_dataset()`"
  )

  # Start with ADM2 geometry info
  final <- adm2_sf |>
    sf::st_drop_geometry() |>
    tibble::as_tibble()

  # Add survey year
  if (!is.null(survey_year)) {
    final$survey_year <- survey_year
  }

  # Add survey dates
  if (!is.null(survey_dates) && "adm2" %in% names(survey_dates)) {
    final <- final |>
      dplyr::left_join(
        survey_dates |> dplyr::select(adm2, survey_median_date = median_date),
        by = "adm2"
      )
  }

  # Add ADM1 estimates (assigned to ADM2s)
  if (!is.null(adm1_estimates) && length(adm1_estimates) > 0) {
    for (name in names(adm1_estimates)) {
      est <- adm1_estimates[[name]]

      if (!is.null(est) && "adm1" %in% names(est)) {
        # Find indicator columns (not admin columns)
        ind_cols <- setdiff(names(est), c("adm1", "adm1_name"))

        if (length(ind_cols) > 0 && "adm1" %in% names(final)) {
          final <- final |>
            dplyr::left_join(
              est |> dplyr::select(adm1, dplyr::all_of(ind_cols)),
              by = "adm1",
              suffix = c("", "_adm1")
            )
        }
      }
    }
  }

  # Add MBG estimates (ADM2-level)
  if (!is.null(mbg_estimates) && length(mbg_estimates) > 0) {
    for (name in names(mbg_estimates)) {
      est <- mbg_estimates[[name]]

      if (!is.null(est) && "adm2" %in% names(est)) {
        # Find indicator columns
        ind_cols <- setdiff(names(est), names(final))

        if (length(ind_cols) > 0) {
          final <- final |>
            dplyr::left_join(
              est |> dplyr::select(adm2, dplyr::all_of(ind_cols)),
              by = "adm2"
            )
        }
      }
    }
  }

  # Round numeric indicator columns
  numeric_cols <- names(final)[sapply(final, is.numeric)]
  indicator_cols <- numeric_cols[grepl("_mean$|_lower$|_upper$|_pct$", numeric_cols)]

  final <- final |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(indicator_cols),
        ~ round(.x, 4)
      )
    )

  final
}


#' Generate Indicator Map
#'
#' Creates various types of maps for indicator visualization.
#'
#' @param estimate_data Data frame with estimates and geometry, or sf object.
#' @param boundaries sf object with admin boundaries.
#' @param indicator_col Name of indicator column to map.
#' @param map_type Type of map:
#'   \itemize{
#'     \item "adm1": ADM1 choropleth with ADM2 boundaries
#'     \item "adm1_clusters": ADM1 choropleth with cluster points
#'     \item "raster": Pixel-level raster map
#'     \item "adm2": ADM2 choropleth from raster aggregation
#'     \item "adm2_clusters": ADM2 choropleth with cluster points
#'     \item "raster_clusters": Raster with cluster points
#'   }
#' @param cluster_data Optional data frame with cluster locations and values.
#' @param raster Optional SpatRaster for raster maps.
#' @param title Map title.
#' @param palette Color palette. Default: "YlGnBu".
#' @param reverse_palette Logical. Reverse palette direction.
#' @param legend_title Legend title.
#' @param show_values Logical. Show values on admin units.
#'
#' @return A ggplot2 or tmap object.
#'
#' @export
generate_indicator_map <- function(
  estimate_data = NULL,
  boundaries,
  indicator_col,
  map_type = "adm2",
  cluster_data = NULL,
  raster = NULL,
  title = NULL,
  palette = "YlGnBu",
  reverse_palette = FALSE,
  legend_title = "Indicator",
  show_values = FALSE
) {
  # Fail fast on missing suggested dependencies
  .check_pkg(
    c("ggplot2", "RColorBrewer"),
    reason = "to render indicator maps in `generate_indicator_map()`"
  )
  .check_spatial_pkg("sf", "generate_indicator_map")

  # Validate indicator column exists in estimate_data
  if (!is.null(estimate_data) && !indicator_col %in% names(estimate_data)) {
    cli::cli_abort(
      "Column {.val {indicator_col}} not found in estimate_data"
    )
  }

  # Determine which admin level for boundaries
  adm2_boundaries <- boundaries
  adm1_boundaries <- NULL

  if ("adm1" %in% names(boundaries) && !"adm2" %in% names(boundaries)) {
    # boundaries is ADM1
    adm1_boundaries <- boundaries
    adm2_boundaries <- NULL
  }

  # Create base map based on type
  if (map_type %in% c("adm1", "adm1_clusters")) {
    # ADM1 choropleth
    if (is.null(estimate_data)) {
      cli::cli_abort("estimate_data required for ADM1 maps")
    }

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = estimate_data,
        ggplot2::aes(fill = .data[[indicator_col]]),
        color = "white",
        linewidth = 0.5
      )

    # Add ADM2 boundaries if available
    if (!is.null(adm2_boundaries)) {
      p <- p +
        ggplot2::geom_sf(
          data = adm2_boundaries,
          fill = NA,
          color = "grey60",
          linewidth = 0.2
        )
    }

  } else if (map_type == "raster") {
    # Raster map
    if (is.null(raster)) {
      cli::cli_abort("raster required for raster maps")
    }

    # Convert raster to data frame for ggplot
    rast_df <- as.data.frame(raster, xy = TRUE)
    names(rast_df)[3] <- "value"

    p <- ggplot2::ggplot() +
      ggplot2::geom_raster(
        data = rast_df,
        ggplot2::aes(x = x, y = y, fill = value)
      ) +
      ggplot2::geom_sf(
        data = boundaries,
        fill = NA,
        color = "black",
        linewidth = 0.3
      )

  } else if (map_type %in% c("adm2", "adm2_clusters")) {
    # ADM2 choropleth
    if (is.null(estimate_data)) {
      cli::cli_abort("estimate_data required for ADM2 maps")
    }

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = estimate_data,
        ggplot2::aes(fill = .data[[indicator_col]]),
        color = "white",
        linewidth = 0.3
      )

  } else if (map_type == "raster_clusters") {
    # Raster with clusters
    if (is.null(raster)) {
      cli::cli_abort("raster required for raster_clusters maps")
    }

    rast_df <- as.data.frame(raster, xy = TRUE)
    names(rast_df)[3] <- "value"

    p <- ggplot2::ggplot() +
      ggplot2::geom_raster(
        data = rast_df,
        ggplot2::aes(x = x, y = y, fill = value)
      ) +
      ggplot2::geom_sf(
        data = boundaries,
        fill = NA,
        color = "black",
        linewidth = 0.3
      )

  } else {
    cli::cli_abort("Unknown map_type: {map_type}")
  }

  # Add cluster points if requested
  if (grepl("clusters", map_type) && !is.null(cluster_data)) {
    # Ensure cluster_data is sf
    if (!inherits(cluster_data, "sf")) {
      if (all(c("x", "y") %in% names(cluster_data))) {
        cluster_sf <- cluster_data |>
          sf::st_as_sf(coords = c("x", "y"), crs = 4326)
      } else if (all(c("lon", "lat") %in% names(cluster_data))) {
        cluster_sf <- cluster_data |>
          sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
      } else {
        cli::cli_alert_warning("Cannot determine cluster coordinates")
        cluster_sf <- NULL
      }
    } else {
      cluster_sf <- cluster_data
    }

    if (!is.null(cluster_sf)) {
      # Transform to match boundaries CRS
      cluster_sf <- sf::st_transform(cluster_sf, sf::st_crs(boundaries))

      # Determine if we should color by indicator
      if (indicator_col %in% names(cluster_sf)) {
        p <- p +
          ggplot2::geom_sf(
            data = cluster_sf,
            ggplot2::aes(color = .data[[indicator_col]]),
            size = 1.5,
            alpha = 0.8
          ) +
          ggplot2::scale_color_gradientn(
            colors = if (reverse_palette) {
              rev(RColorBrewer::brewer.pal(9, palette))
            } else {
              RColorBrewer::brewer.pal(9, palette)
            },
            guide = "none"  # Don't show separate legend for points
          )
      } else {
        p <- p +
          ggplot2::geom_sf(
            data = cluster_sf,
            color = "black",
            size = 1,
            alpha = 0.6
          )
      }
    }
  }

  # Apply color scale
  fill_colors <- if (reverse_palette) {
    rev(RColorBrewer::brewer.pal(9, palette))
  } else {
    RColorBrewer::brewer.pal(9, palette)
  }

  p <- p +
    ggplot2::scale_fill_gradientn(
      colors = fill_colors,
      name = legend_title,
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1)
    )

  # Add labels if requested
  if (show_values && !is.null(estimate_data)) {
    centroids <- sf::st_centroid(estimate_data)

    p <- p +
      ggplot2::geom_sf_text(
        data = centroids,
        ggplot2::aes(label = scales::percent(.data[[indicator_col]], accuracy = 1)),
        size = 2.5,
        color = "grey30"
      )
  }

  # Add title and theme
  p <- p +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      legend.position = "right"
    )

  p
}


#' Save Indicator Map
#'
#' Saves a map to file.
#'
#' @param map ggplot2 or tmap object.
#' @param filename Output file path.
#' @param width Width in inches. Default: 10.
#' @param height Height in inches. Default: 8.
#' @param dpi Resolution. Default: 300.
#'
#' @export
save_indicator_map <- function(
  map,
  filename,
  width = 10,
  height = 8,
  dpi = 300
) {
  if (inherits(map, "ggplot")) {
    .check_pkg(
      "ggplot2",
      reason = "to save ggplot maps in `save_indicator_map()`"
    )
    ggplot2::ggsave(
      filename = filename,
      plot = map,
      width = width,
      height = height,
      dpi = dpi
    )
  } else if (inherits(map, "tmap")) {
    .check_pkg(
      "tmap",
      reason = "to save tmap objects in `save_indicator_map()`"
    )
    tmap::tmap_save(
      tm = map,
      filename = filename,
      width = width,
      height = height,
      dpi = dpi
    )
  } else {
    cli::cli_abort("Unknown map type")
  }

  cli::cli_alert_success("Saved map: {.file {basename(filename)}}")
}


#' Generate All Map Types for Indicator
#'
#' Generates multiple map types for a single indicator.
#'
#' @param indicator_name Indicator name.
#' @param mbg_raster MBG prediction raster (mean).
#' @param adm1_estimates ADM1-level survey estimates.
#' @param adm2_estimates ADM2-level MBG estimates (as sf).
#' @param cluster_data Cluster-level data.
#' @param adm1_sf ADM1 boundaries.
#' @param adm2_sf ADM2 boundaries.
#' @param path_output Output directory.
#' @param country_iso3 Country code.
#' @param survey_year Survey year.
#'
#' @return List of file paths to saved maps.
#'
#' @export
generate_all_maps <- function(
  indicator_name,
  mbg_raster = NULL,
  adm1_estimates = NULL,
  adm2_estimates = NULL,
  cluster_data = NULL,
  adm1_sf,
  adm2_sf,
  path_output,
  country_iso3,
  survey_year
) {
  # Fail fast on missing suggested dependencies
  .check_spatial_pkg("fs", "generate_all_maps")
  .check_spatial_pkg("sf", "generate_all_maps")
  .check_pkg(
    c("glue", "ggplot2", "RColorBrewer"),
    reason = "to build map filenames and render maps in `generate_all_maps()`"
  )

  map_paths <- list()

  # Ensure output directory exists
  map_dir <- fs::path(path_output, "maps")
  fs::dir_create(map_dir)

  base_name <- glue::glue("{country_iso3}_{indicator_name}_{survey_year}")

  indicator_col <- paste0(indicator_name, "_mean")

  # 1. ADM1 estimates with ADM2 boundaries
  if (!is.null(adm1_estimates) && indicator_col %in% names(adm1_estimates)) {
    adm1_sf_with_data <- adm1_sf |>
      dplyr::left_join(adm1_estimates, by = "adm1")

    map1 <- generate_indicator_map(
      estimate_data = adm1_sf_with_data,
      boundaries = adm2_sf,
      indicator_col = indicator_col,
      map_type = "adm1",
      title = glue::glue("{indicator_name} - ADM1 Survey Estimates")
    )

    map1_path <- fs::path(map_dir, paste0(base_name, "_adm1.png"))
    save_indicator_map(map1, map1_path)
    map_paths$adm1 <- map1_path
  }

  # 2. ADM2 MBG estimates
  if (!is.null(adm2_estimates) && indicator_col %in% names(adm2_estimates)) {
    adm2_sf_with_data <- adm2_sf |>
      dplyr::left_join(adm2_estimates, by = "adm2")

    map2 <- generate_indicator_map(
      estimate_data = adm2_sf_with_data,
      boundaries = adm2_sf,
      indicator_col = indicator_col,
      map_type = "adm2",
      title = glue::glue("{indicator_name} - MBG ADM2 Estimates")
    )

    map2_path <- fs::path(map_dir, paste0(base_name, "_mbg_adm2.png"))
    save_indicator_map(map2, map2_path)
    map_paths$adm2_mbg <- map2_path
  }

  # 3. Raster map
  if (!is.null(mbg_raster)) {
    map3 <- generate_indicator_map(
      boundaries = adm2_sf,
      indicator_col = "value",
      map_type = "raster",
      raster = mbg_raster,
      title = glue::glue("{indicator_name} - MBG Pixel Predictions")
    )

    map3_path <- fs::path(map_dir, paste0(base_name, "_mbg_raster.png"))
    save_indicator_map(map3, map3_path)
    map_paths$raster <- map3_path
  }

  # 4. ADM2 with clusters
  if (!is.null(adm2_estimates) && !is.null(cluster_data)) {
    adm2_sf_with_data <- adm2_sf |>
      dplyr::left_join(adm2_estimates, by = "adm2")

    map4 <- generate_indicator_map(
      estimate_data = adm2_sf_with_data,
      boundaries = adm2_sf,
      indicator_col = indicator_col,
      map_type = "adm2_clusters",
      cluster_data = cluster_data,
      title = glue::glue("{indicator_name} - MBG ADM2 with Clusters")
    )

    map4_path <- fs::path(map_dir, paste0(base_name, "_mbg_adm2_clusters.png"))
    save_indicator_map(map4, map4_path)
    map_paths$adm2_clusters <- map4_path
  }

  map_paths
}
