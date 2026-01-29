#' Check if spatial packages are available
#'
#' @param pkg Package name to check
#' @param func_name Function name for error message
#' @return TRUE if available, otherwise throws informative error
#' @keywords internal
#' @noRd
.check_spatial_pkg <- function(pkg, func_name = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    func_msg <- if (!is.null(func_name)) {
      paste0("Function '", func_name, "' requires")
    } else {
      "This function requires"
    }

    stop(
      func_msg, " the '", pkg, "' package.\n",
      "Install it with: install.packages('", pkg, "')\n",
      "Note: On macOS/Linux, you may need system libraries (GDAL, GEOS, PROJ).\n",
      "  macOS: brew install gdal geos proj\n",
      "  Ubuntu: sudo apt-get install libgdal-dev libgeos-dev libproj-dev",
      call. = FALSE
    )
  }
  TRUE
}

#' Check if PROJ is properly configured
#'
#' @param silent If TRUE, suppress all warnings
#' @return TRUE if PROJ is working, FALSE otherwise
#' @keywords internal
#' @noRd
.check_proj <- function(silent = TRUE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(FALSE)
  }

  tryCatch({
    # Try to create a simple sf object to test PROJ
    # Suppress all output to avoid package check issues
    suppressMessages(suppressWarnings(sf::st_crs(4326)))
    TRUE
  }, error = function(e) {
    if (!silent && grepl("proj", tolower(e$message))) {
      warning(
        "PROJ library not properly configured. Spatial functions may fail.\n",
        "  macOS: brew reinstall proj && export PROJ_LIB=$(brew --prefix proj)/share/proj\n",
        "  Ubuntu: sudo apt-get install --reinstall proj-bin proj-data",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    FALSE
  })
}

# Package load hook - kept minimal to avoid check issues
.onLoad <- function(libname, pkgname) {
  # Nothing needed here - dependency checks happen when functions are called
  invisible()
}
