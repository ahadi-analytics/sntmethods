# =============================================================================
# Generic helper for inspecting DHS variable haven labels
# =============================================================================

#' List DHS variables and their haven labels by name pattern
#'
#' Generic utility to inspect a (typically DHS) data frame and return a tidy
#' inventory of every column whose name matches a given prefix or regex,
#' together with its haven `label` attribute. This is useful when building
#' specs for `custom_csb_indicator` (or any indicator that routes columns by
#' label / variable name), since DHS recodes share many label conventions
#' across surveys but the exact set of populated columns varies by country
#' and round.
#'
#' Two matching modes are supported:
#'
#' \itemize{
#'   \item Prefix mode (default, `regex = FALSE`): the helper matches columns
#'     whose names start with `pattern` followed by one or more lowercase
#'     letters or digits. For example, `pattern = "h32"` will match `h32a`,
#'     `h32x`, `h32a1`, but not `h32_recoded` or `h32`.
#'   \item Regex mode (`regex = TRUE`): `pattern` is used directly as a
#'     regular expression. For example, `pattern = "^ml13[a-z]$"` will match
#'     ITN-source variables `ml13a`..`ml13h`.
#' }
#'
#' Optionally, the helper can:
#'
#' \itemize{
#'   \item Mark variables whose haven label is shared with another variable
#'     in the same inventory (`duplicate_label = TRUE`). This is the
#'     situation that motivates routing a `custom_csb_indicator` spec by
#'     variable **name** rather than by label (e.g. h32e and h32n in some
#'     surveys both carry the label "Fever/cough: comm.health wrkr").
#'   \item Drop variables that have no observed `1` (or no non-NA values
#'     at all for non-binary columns) via `only_observed = TRUE`. This
#'     keeps the inventory focused on slots actually populated by the
#'     survey.
#' }
#'
#' The function never modifies `data`; it only reads column names and
#' the `label` attribute (as set by `haven::read_dta()` or
#' `sntmethods::dhs_read()`).
#'
#' @param data A data frame (typically a DHS recode such as the KR file
#'   read by [`dhs_read()`]). Columns may be plain vectors or
#'   haven-labelled vectors; only the name and `label` attribute are
#'   inspected.
#' @param pattern Character scalar. Either a prefix (default) or a regex
#'   (when `regex = TRUE`). Must be non-empty.
#' @param regex Logical. If `FALSE` (default), `pattern` is treated as a
#'   prefix and the matched regex is `paste0("^", pattern, "[a-z0-9]+$")`.
#'   If `TRUE`, `pattern` is used as the full regex.
#' @param only_observed Logical. If `TRUE`, drop variables with no
#'   non-NA values, or - for numeric 0/1 columns - no observed `1`.
#'   Default `FALSE`.
#' @param duplicate_label Logical. If `TRUE` (default), add a
#'   `duplicate_label` logical column flagging variables whose label is
#'   shared with at least one other matched variable.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item `variable`: matched column name.
#'     \item `label`: haven `label` attribute (or `NA_character_` if
#'       absent).
#'     \item `n_nonmissing`: number of non-NA values in the column.
#'     \item `n_ones`: number of values equal to `1` (or `NA` for
#'       non-numeric columns).
#'     \item `duplicate_label` (only if `duplicate_label = TRUE`): logical,
#'       `TRUE` if another matched variable has the same non-NA label.
#'   }
#'   Variables are returned in the order they appear in `data`. If no
#'   variables match, an empty tibble with the same columns is returned
#'   and a `cli` info message is emitted.
#'
#' @examples
#' \dontrun{
#'   kr_data <- sntmethods::dhs_read(
#'     path        = path_dhs_parquet,
#'     file_type   = "KR",
#'     survey_type = "DHS"
#'   )
#'
#'   # All h32* treatment-seeking source columns
#'   list_dhs_var_labels(kr_data, "h32")
#'
#'   # Only h32 columns with at least one observed `1`, flagging duplicates
#'   list_dhs_var_labels(kr_data, "h32", only_observed = TRUE)
#'
#'   # ITN source columns ml13a..ml13h via regex
#'   list_dhs_var_labels(kr_data, "^ml13[a-z]$", regex = TRUE)
#' }
#'
#' @export
list_dhs_var_labels <- function(data,
                                pattern,
                                regex = FALSE,
                                only_observed = FALSE,
                                duplicate_label = TRUE) {

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (!is.character(pattern) || length(pattern) != 1L || !nzchar(pattern)) {
    cli::cli_abort("{.arg pattern} must be a single non-empty character string.")
  }
  if (!is.logical(regex) || length(regex) != 1L || is.na(regex)) {
    cli::cli_abort("{.arg regex} must be TRUE or FALSE.")
  }
  if (!is.logical(only_observed) || length(only_observed) != 1L ||
      is.na(only_observed)) {
    cli::cli_abort("{.arg only_observed} must be TRUE or FALSE.")
  }
  if (!is.logical(duplicate_label) || length(duplicate_label) != 1L ||
      is.na(duplicate_label)) {
    cli::cli_abort("{.arg duplicate_label} must be TRUE or FALSE.")
  }

  # Build the matching regex
  if (regex) {
    re <- pattern
  } else {
    re <- paste0("^", pattern, "[a-z0-9]+$")
  }

  matched <- grep(re, names(data), value = TRUE)

  empty_out <- tibble::tibble(
    variable     = character(),
    label        = character(),
    n_nonmissing = integer(),
    n_ones       = integer()
  )
  if (duplicate_label) {
    empty_out$duplicate_label <- logical()
  }

  if (length(matched) == 0L) {
    cli::cli_alert_info(
      "No variables in {.arg data} match {.val {re}}."
    )
    return(empty_out)
  }

  # Pull haven labels (NA when absent)
  labs <- unname(vapply(matched, function(v) {
    lab <- attr(data[[v]], "label", exact = TRUE)
    if (is.null(lab)) NA_character_ else as.character(lab)[[1L]]
  }, character(1L)))

  # Non-missing counts and observed-1 counts
  n_nm <- unname(vapply(matched, function(v) sum(!is.na(data[[v]])),
                        integer(1L)))
  n_ones <- unname(vapply(matched, function(v) {
    col <- data[[v]]
    if (is.numeric(col)) {
      sum(col == 1, na.rm = TRUE)
    } else {
      NA_integer_
    }
  }, integer(1L)))

  out <- tibble::tibble(
    variable     = matched,
    label        = labs,
    n_nonmissing = n_nm,
    n_ones       = n_ones
  )

  if (only_observed) {
    keep <- ifelse(
      is.na(out$n_ones),
      out$n_nonmissing > 0L,
      out$n_ones > 0L
    )
    out <- out[keep, , drop = FALSE]
    if (nrow(out) == 0L) {
      cli::cli_alert_info(
        "No matched variables have observed values in {.arg data}."
      )
    }
  }

  if (duplicate_label && nrow(out) > 0L) {
    lab_counts <- table(out$label[!is.na(out$label)])
    dup_labs <- names(lab_counts)[lab_counts > 1L]
    out$duplicate_label <- !is.na(out$label) & out$label %in% dup_labs
  } else if (duplicate_label) {
    out$duplicate_label <- logical(0)
  }

  out
}
