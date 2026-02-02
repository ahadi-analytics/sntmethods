#' Normalize a numeric vector using z-score standardization
#'
#' standardizes a numeric vector by subtracting the mean and dividing by the
#' standard deviation. this is intended for preparing time series prior to
#' trend analysis such as mann-kendall or sen's slope, especially when results
#' need to be comparable across indicators.
#'
#' the function fails fast on non-numeric input. for undefined normalization
#' cases (all missing values or zero variance), behavior is controlled via
#' `na_on_fail`.
#'
#' @param vec numeric vector to be normalized.
#' @param na_on_fail logical. if TRUE, returns a vector of NA_real_ when
#'   normalization is undefined. if FALSE, throws an error.
#'
#' @return a numeric vector of the same length as `vec`, containing z-scores or
#'   NA_real_ values if normalization is not possible.
#'
#' @examples
#' normalize_zscore(c(1, 2, 3, 4))
#'
#' normalize_zscore(c(5, 5, 5), na_on_fail = TRUE)
#'
#' try(
#'   normalize_zscore(c(5, 5, 5), na_on_fail = FALSE)
#' )
#'
#' normalize_zscore(c(NA_real_, NA_real_))
#' @export
normalize_zscore <- function(vec, na_on_fail = TRUE) {
  if (!is.numeric(vec)) {
    cli::cli_abort("`vec` must be numeric.")
  }

  if (all(is.na(vec))) {
    if (na_on_fail) {
      return(rep(NA_real_, length(vec)))
    }
    cli::cli_abort("`vec` contains only NA values.")
  }

  vec_mean <- mean(vec, na.rm = TRUE)
  vec_sd <- stats::sd(vec, na.rm = TRUE)

  if (is.na(vec_sd) || vec_sd == 0) {
    if (na_on_fail) {
      return(rep(NA_real_, length(vec)))
    }
    cli::cli_abort("standard deviation is zero or NA. cannot normalize.")
  }

  (vec - vec_mean) / vec_sd
}

#' Run STL decomposition and trend tests on grouped time series data
#'
#' Performs STL decomposition, Mann-Kendall trend testing, and Sen's slope
#' estimation for multiple indicators within grouped time series data.
#'
#' @param data A data.frame containing the time series data.
#' @param group_col Character vector of grouping column names.
#' @param date_col Name of the date column.
#' @param indicators A list of indicator specifications. Each element must
#'   contain:
#'   - col: column name of the indicator
#'   - type: indicator type label
#' @param freq Number of observations per year. Default is 12.
#' @param normalize_fun Function used to normalize indicator values.
#' @param stl_window STL seasonal window. Default is "periodic".
#'
#' @return A data.table containing STL components and trend statistics.
#'
#' @examples
#' \dontrun{
#' res <- run_grouped_stl_trend(
#'   data = monthly_adm2_incid,
#'   group_col = c("adm1", "adm2"),
#'   date_col = "date",
#'   indicators = indicators
#' )
#' }
#' @export
run_grouped_stl_trend <- function(
  data,
  group_col,
  date_col,
  indicators,
  freq = 12,
  normalize_fun = normalize_zscore,
  stl_window = "periodic"
) {
  if (!all(group_col %in% names(data))) {
    cli::cli_abort(
      paste0(
        "Grouping columns not found in data: ",
        paste(setdiff(group_col, names(data)), collapse = ", ")
      )
    )
  }

  if (!date_col %in% names(data)) {
    cli::cli_abort(
      paste0(
        "Date column '",
        date_col,
        "' not found in input data."
      )
    )
  }

  if (!is.list(indicators) || length(indicators) == 0) {
    cli::cli_abort(
      "'indicators' must be a non-empty list."
    )
  }

  group_keys <- data |>
    dplyr::distinct(dplyr::across(dplyr::all_of(group_col)))

  result_list <- list()

  for (i in seq_len(nrow(group_keys))) {
    group_vals <- group_keys[i, , drop = FALSE]

    group_data <- data |>
      dplyr::inner_join(group_vals, by = group_col) |>
      dplyr::arrange(.data[[date_col]])

    group_label <- paste(
      paste(group_col, group_vals[1, ], sep = "="),
      collapse = " | "
    )

    if (nrow(group_data) == 0) {
      cli::cli_warn(
        paste0(
          "No data for group ",
          group_label,
          ". Skipping."
        )
      )
      next
    }

    for (ind in indicators) {
      if (!ind$col %in% names(group_data)) {
        cli::cli_warn(
          paste0(
            "Column '",
            ind$col,
            "' not found for group ",
            group_label,
            ". Skipping."
          )
        )
        next
      }

      ind_values <- group_data[[ind$col]]

      if (
        !is.numeric(ind_values) ||
          length(ind_values) == 0 ||
          all(is.na(ind_values))
      ) {
        cli::cli_warn(
          paste0(
            "Invalid data for '",
            ind$col,
            "' in group ",
            group_label,
            ". Skipping."
          )
        )
        next
      }

      ind_norm <- normalize_fun(ind_values)

      valid_idx <- !is.na(ind_values)
      valid_values <- ind_norm[valid_idx]
      valid_dates <- group_data[[date_col]][valid_idx]

      if (length(valid_values) < 2) {
        cli::cli_warn(
          paste0(
            "Fewer than 2 valid observations for '",
            ind$col,
            "' in group ",
            group_label,
            ". Skipping."
          )
        )
        next
      }

      start_year <- as.numeric(format(valid_dates[1], "%Y"))
      start_period <- as.numeric(format(valid_dates[1], "%m"))

      ind_ts <- stats::ts(
        valid_values,
        start = c(start_year, start_period),
        deltat = 1 / freq
      )

      ind_stl <- stlplus::stlplus(
        ind_ts,
        s.window = stl_window
      )

      ind_stl_df <- as.data.frame(ind_stl$data[, 1:4])
      ind_stl_df[[date_col]] <- valid_dates

      if (nrow(ind_stl_df) != length(valid_dates)) {
        cli::cli_warn(
          paste0(
            "STL output length mismatch for '",
            ind$col,
            "' in group ",
            group_label,
            ". Skipping."
          )
        )
        next
      }

      mk_result <- tryCatch(
        trend::smk.test(ind_ts),
        error = function(e) {
          cli::cli_warn(
            paste0(
              "Mann-Kendall test failed for '",
              ind$col,
              "' in group ",
              group_label,
              ": ",
              e$message
            )
          )
          list(p.value = NA_real_)
        }
      )

      sens_result <- tryCatch(
        trend::sea.sens.slope(ind_ts),
        error = function(e) {
          cli::cli_warn(
            paste0(
              "Sen's slope failed for '",
              ind$col,
              "' in group ",
              group_label,
              ": ",
              e$message
            )
          )
          NA_real_
        }
      )

      ind_stl_df$type <- ind$type
      ind_stl_df$mk_p <- mk_result$p.value
      ind_stl_df$sens_slope <- sens_result

      for (gc in group_col) {
        ind_stl_df[[gc]] <- group_vals[[gc]]
      }

      result_list[[paste(group_label, ind$col, sep = " :: ")]] <-
        ind_stl_df
    }
  }

  out <- dplyr::bind_rows(result_list)

  # enforce column order: groups -> date -> rest
  ordered_cols <- c(
    group_col,
    date_col,
    setdiff(names(out), c(group_col, date_col))
  )

  out |>
    dplyr::select(dplyr::all_of(ordered_cols))
}
