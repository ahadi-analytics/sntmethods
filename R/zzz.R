#' Check that one or more suggested packages are installed
#'
#' Thin wrapper around [rlang::check_installed()] used throughout the package
#' to fail fast (with a clear, actionable error message and an interactive
#' install prompt) when a suggested dependency is missing. Prefer this over
#' bare `requireNamespace()` checks so users learn about missing dependencies
#' at the start of a function rather than several minutes into a workflow.
#'
#' @param pkg Character vector of package names to check.
#' @param reason Short string explaining *why* the package is needed (used in
#'   the error message). Optional but strongly recommended.
#' @param call Calling environment, forwarded to [rlang::check_installed()] so
#'   the error is attributed to the user-facing function.
#'
#' @return `TRUE` invisibly if all packages are available. Errors otherwise.
#' @keywords internal
#' @noRd
.check_pkg <- function(pkg, reason = NULL, call = rlang::caller_env()) {
  rlang::check_installed(pkg, reason = reason, call = call)
  invisible(TRUE)
}

#' Check if spatial packages are available
#'
#' Backwards-compatible wrapper that now delegates to [.check_pkg()] so that
#' all dependency errors flow through the same code path. Retained for the
#' helpful PROJ/GDAL/GEOS system-library hint that is specific to spatial
#' stack failures.
#'
#' @param pkg Package name to check
#' @param func_name Function name for error message
#' @return TRUE if available, otherwise throws informative error
#' @keywords internal
#' @noRd
.check_spatial_pkg <- function(pkg, func_name = NULL) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(TRUE)
  }

  reason <- if (!is.null(func_name)) {
    paste0(
      "for `", func_name, "()` (spatial operations).\n",
      "Note: On macOS/Linux you may also need system libraries ",
      "(GDAL, GEOS, PROJ).\n",
      "  macOS:  brew install gdal geos proj\n",
      "  Ubuntu: sudo apt-get install libgdal-dev libgeos-dev libproj-dev"
    )
  } else {
    paste0(
      "for spatial operations.\n",
      "Note: On macOS/Linux you may also need system libraries ",
      "(GDAL, GEOS, PROJ)."
    )
  }

  rlang::check_installed(pkg, reason = reason, call = rlang::caller_env())
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
