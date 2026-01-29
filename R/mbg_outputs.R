#' MBG Output Generation
#'
#' Functions for generating outputs from MBG analysis: rasters, CSVs,
#' and various map types.
#'
#' @name mbg_outputs
#' @keywords internal
NULL


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
  # Check for required spatial packages
  .check_spatial_pkg("terra", "save_mbg_rasters")
  .check_spatial_pkg("fs", "save_mbg_rasters")
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
      # Check for exactextractr package
      if (!requireNamespace("exactextractr", quietly = TRUE)) {
        cli::cli_abort(c(
          "Package {.pkg exactextractr} is required for weighted raster extraction",
          "i" = "Install with: install.packages('exactextractr')"
        ))
      }

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
  # Check for RColorBrewer package
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg RColorBrewer} is required for map color palettes",
      "i" = "Install with: install.packages('RColorBrewer')"
    ))
  }

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
    ggplot2::ggsave(
      filename = filename,
      plot = map,
      width = width,
      height = height,
      dpi = dpi
    )
  } else if (inherits(map, "tmap")) {
    if (!requireNamespace("tmap", quietly = TRUE)) {
      cli::cli_abort(c(
        "Package {.pkg tmap} is required to save tmap objects",
        "i" = "Install with: install.packages('tmap')"
      ))
    }
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
