# =============================================================================
# epi_recommend.R
# -----------------------------------------------------------------------------
# Profile-driven detector recommendation. The verb:
#
#   1. Builds a numeric profile of the input series (`profile_series()`):
#         n_obs, mean_count, low_count_fraction, dispersion (var/mean),
#         seasonality_strength, trend_strength, missingness, zero_inflation.
#
#   2. Applies a deterministic rule engine that maps the profile to a
#      ranked list of plausible detectors, each rule contributing a
#      `score` and a `reason` string.
#
#   3. Resolves ties using one of three strategies:
#        * "simplicity"      — prefer simpler methods.
#        * "evidence"        — break ties using `epi_evaluation$auc_ranking`
#                              (per-method AUC).
#        * "cost_minimising" — break ties using `epi_evaluation$cost` rows.
#
#   4. Returns an `epi_recommendation` S3 object with rationale tables and
#      autoplot showing the profile radar / bars and the ranked methods.
#
# All knobs (thresholds, score increments, baseline pools) live in
# `.recommend_rules()` and `.tie_break_*()` to keep the verb body small.
# =============================================================================


#' Profile a count time-series for detector selection.
#'
#' @description
#' Computes a small numeric profile of a univariate count series. Used by
#' [epi_recommend()] but exported for users who want to interrogate a series
#' before selecting detectors manually.
#'
#' @param data A data frame.
#' @param date_col Bare or string column name with dates.
#' @param count_col Bare or string column name with counts.
#' @param group_col Optional bare/string column name. If supplied, a profile
#'   is computed per group and returned in long format.
#'
#' @return A tibble with one row per group (or a single row if ungrouped)
#'   containing: `group_id`, `n_obs`, `mean_count`, `median_count`,
#'   `low_count_fraction`, `dispersion`, `zero_inflation`, `missingness`,
#'   `seasonality_strength`, `trend_strength`.
#'
#' @details
#' * `low_count_fraction` is the proportion of weeks with count < 5.
#' * `dispersion` is `var(x) / mean(x)` (1 = Poisson, > 1 = overdispersed).
#' * `seasonality_strength` is the maximum |ACF| over lags 4–12 weeks.
#' * `trend_strength` is the absolute Kendall tau of count ~ date.
#'
#' @export
profile_series <- function(data,
                           date_col,
                           count_col,
                           group_col = NULL) {

  date_col  <- rlang::ensym(date_col)
  count_col <- rlang::ensym(count_col)
  has_group <- !rlang::quo_is_null(rlang::enquo(group_col))
  group_sym <- if (has_group) rlang::ensym(group_col) else NULL

  if (has_group) {
    groups <- split(data, data[[rlang::as_string(group_sym)]])
  } else {
    groups <- list("(all)" = data)
  }

  purrr::map_dfr(names(groups), function(g_name) {
    df <- groups[[g_name]]
    df <- df[order(df[[rlang::as_string(date_col)]]), , drop = FALSE]
    x  <- df[[rlang::as_string(count_col)]]
    n  <- length(x)

    .one_profile(x, group_id = g_name)
  })
}


#' @keywords internal
#' @noRd
.one_profile <- function(x, group_id = NA_character_) {

  n_obs       <- length(x)
  missingness <- if (n_obs == 0) NA_real_ else mean(is.na(x))
  xv          <- stats::na.omit(x)

  if (length(xv) < 3L) {
    return(tibble::tibble(
      group_id              = group_id,
      n_obs                 = n_obs,
      mean_count            = if (length(xv)) mean(xv) else NA_real_,
      median_count          = if (length(xv)) stats::median(xv) else NA_real_,
      low_count_fraction    = NA_real_,
      dispersion            = NA_real_,
      zero_inflation        = NA_real_,
      missingness           = missingness,
      seasonality_strength  = NA_real_,
      trend_strength        = NA_real_
    ))
  }

  mn        <- mean(xv)
  vr        <- stats::var(xv)
  dispersion <- if (mn == 0) NA_real_ else vr / mn

  acf_max <- tryCatch({
    a <- stats::acf(xv, lag.max = min(12, length(xv) - 1),
                    plot = FALSE, na.action = stats::na.pass)
    lags <- which(a$lag[, 1, 1] >= 4)
    if (length(lags)) max(abs(a$acf[lags, 1, 1])) else 0
  }, error = function(e) NA_real_)

  trend_tau <- tryCatch({
    abs(stats::cor(seq_along(xv), xv, method = "kendall"))
  }, error = function(e) NA_real_)

  tibble::tibble(
    group_id              = group_id,
    n_obs                 = n_obs,
    mean_count            = mn,
    median_count          = stats::median(xv),
    low_count_fraction    = mean(xv < 5),
    dispersion            = dispersion,
    zero_inflation        = mean(xv == 0),
    missingness           = missingness,
    seasonality_strength  = acf_max,
    trend_strength        = trend_tau
  )
}


# =============================================================================
# Rule engine
# =============================================================================

# Returns a tibble: method, score, reason. The score is summed across rules;
# higher scores mean stronger fit to the profile.

#' @keywords internal
#' @noRd
.recommend_rules <- function(profile) {

  rules <- list()

  # always-eligible baselines
  rules[[length(rules) + 1]] <- tibble::tibble(
    method = c("threshold", "ears_c1"),
    score  = c(1, 1),
    reason = c("baseline (always considered)",
               "baseline (always considered)")
  )

  # short series: drop seasonal / change-point methods that need >= 1 year
  if (!is.na(profile$n_obs) && profile$n_obs >= 52) {
    rules[[length(rules) + 1]] <- tibble::tibble(
      method = c("farrington", "anomalize_stl", "stl_residual",
                 "bayesian_changepoint", "changepoint_pelt"),
      score  = c(1, 1, 1, 1, 1),
      reason = "sufficient history (>= 52 obs)"
    )
  }

  if (!is.na(profile$n_obs) && profile$n_obs >= 104) {
    rules[[length(rules) + 1]] <- tibble::tibble(
      method = c("farrington", "arima"),
      score  = c(2, 1),
      reason = "long history (>= 104 obs) supports model-based detectors"
    )
  }

  # seasonality
  if (!is.na(profile$seasonality_strength) &&
      profile$seasonality_strength > 0.3) {
    rules[[length(rules) + 1]] <- tibble::tibble(
      method = c("farrington", "anomalize_stl", "stl_residual"),
      score  = c(3, 2, 2),
      reason = sprintf("strong seasonality (|ACF| = %.2f)",
                       profile$seasonality_strength)
    )
  }

  # dispersion -> NB methods
  if (!is.na(profile$dispersion) && profile$dispersion > 1.5) {
    rules[[length(rules) + 1]] <- tibble::tibble(
      method = c("trending", "glrnb", "farrington"),
      score  = c(3, 2, 2),
      reason = sprintf("overdispersion (var/mean = %.2f)",
                       profile$dispersion)
    )
  }

  # low counts
  if (!is.na(profile$low_count_fraction) &&
      profile$low_count_fraction > 0.5) {
    rules[[length(rules) + 1]] <- tibble::tibble(
      method = c("threshold", "endemic_channel", "ears_c1"),
      score  = c(2, 2, 1),
      reason = sprintf("low-count weeks dominate (%.0f%%)",
                       100 * profile$low_count_fraction)
    )
    # demote methods that struggle with low counts
    rules[[length(rules) + 1]] <- tibble::tibble(
      method = c("arima", "anomalize_stl", "stl_residual"),
      score  = c(-2, -1, -1),
      reason = "demoted: low-count series"
    )
  }

  # zero-inflation
  if (!is.na(profile$zero_inflation) && profile$zero_inflation > 0.3) {
    rules[[length(rules) + 1]] <- tibble::tibble(
      method = c("threshold", "endemic_channel"),
      score  = c(2, 1),
      reason = sprintf("zero-inflated (%.0f%% zeros)",
                       100 * profile$zero_inflation)
    )
  }

  # trend
  if (!is.na(profile$trend_strength) && profile$trend_strength > 0.3) {
    rules[[length(rules) + 1]] <- tibble::tibble(
      method = c("cusum_classical", "ears_c2", "ears_c3", "trending"),
      score  = c(2, 1, 1, 1),
      reason = sprintf("trend present (|tau| = %.2f)",
                       profile$trend_strength)
    )
  }

  # high missingness — favour simple methods that tolerate NAs
  if (!is.na(profile$missingness) && profile$missingness > 0.1) {
    rules[[length(rules) + 1]] <- tibble::tibble(
      method = c("threshold", "ears_c1"),
      score  = c(1, 1),
      reason = sprintf("missingness elevated (%.0f%%)",
                       100 * profile$missingness)
    )
    rules[[length(rules) + 1]] <- tibble::tibble(
      method = c("arima", "bayesian_changepoint"),
      score  = c(-1, -1),
      reason = "demoted: missingness elevated"
    )
  }

  dplyr::bind_rows(rules) |>
    dplyr::group_by(.data$method) |>
    dplyr::summarise(
      score  = sum(.data$score),
      reason = paste(unique(.data$reason), collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$score))
}


# =============================================================================
# Tie-break strategies
# =============================================================================

# Simplicity ordering (lower index = simpler).
.simplicity_order <- c(
  "threshold", "endemic_channel", "ears_c1", "ears_c2", "ears_c3",
  "cusum_classical", "trending", "farrington", "glrnb",
  "stl_residual", "anomalize_stl", "arima",
  "changepoint_pelt", "bayesian_changepoint"
)


#' @keywords internal
#' @noRd
.tie_break_simplicity <- function(ranked) {
  ranked |>
    dplyr::mutate(
      simplicity_rank = match(.data$method, .simplicity_order),
      simplicity_rank = dplyr::if_else(
        is.na(.data$simplicity_rank),
        Inf,
        as.numeric(.data$simplicity_rank)
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$score), .data$simplicity_rank)
}


#' @keywords internal
#' @noRd
.tie_break_evidence <- function(ranked, evaluation) {
  if (is.null(evaluation) || !inherits(evaluation, "epi_evaluation")) {
    cli::cli_warn(c(
      "`tie_break = 'evidence'` requires an {.cls epi_evaluation} object.",
      "i" = "Falling back to {.val simplicity}."
    ))
    return(.tie_break_simplicity(ranked))
  }

  per_method <- evaluation$per_method
  if (is.null(per_method) || !"auc" %in% names(per_method)) {
    cli::cli_warn(c(
      "`evaluation$per_method$auc` missing; falling back to simplicity."
    ))
    return(.tie_break_simplicity(ranked))
  }

  evidence <- per_method |>
    dplyr::select("method", "auc") |>
    dplyr::distinct()

  ranked |>
    dplyr::left_join(evidence, by = "method") |>
    dplyr::mutate(auc = dplyr::if_else(is.na(.data$auc), 0.5, .data$auc)) |>
    dplyr::arrange(dplyr::desc(.data$score), dplyr::desc(.data$auc))
}


#' @keywords internal
#' @noRd
.tie_break_cost <- function(ranked, evaluation) {
  if (is.null(evaluation) || !inherits(evaluation, "epi_evaluation")) {
    cli::cli_warn(c(
      "`tie_break = 'cost_minimising'` requires an {.cls epi_evaluation} object.",
      "i" = "Falling back to {.val simplicity}."
    ))
    return(.tie_break_simplicity(ranked))
  }

  per_method <- evaluation$per_method
  if (is.null(per_method) || !"cost" %in% names(per_method)) {
    cli::cli_warn(c(
      "`evaluation$per_method$cost` missing; falling back to simplicity.",
      "i" = "Re-run {.fn epi_evaluate} with a `cost` argument."
    ))
    return(.tie_break_simplicity(ranked))
  }

  cost_tbl <- per_method |>
    dplyr::select("method", "cost") |>
    dplyr::distinct()

  ranked |>
    dplyr::left_join(cost_tbl, by = "method") |>
    dplyr::mutate(cost = dplyr::if_else(is.na(.data$cost), Inf, .data$cost)) |>
    dplyr::arrange(dplyr::desc(.data$score), .data$cost)
}


# =============================================================================
# Verb
# =============================================================================

#' Recommend outbreak detectors for a count time-series.
#'
#' @description
#' Profiles the series, applies a rule engine to score candidate detectors,
#' resolves ties via the requested strategy, and returns the top-`k`
#' recommendations with rationale.
#'
#' @param data A data frame.
#' @param date_col Bare or string column name with dates.
#' @param count_col Bare or string column name with counts.
#' @param group_col Optional bare/string column name for panel data.
#' @param evaluation Optional `epi_evaluation` object. Required when
#'   `tie_break` is `"evidence"` or `"cost_minimising"`.
#' @param tie_break One of `"simplicity"` (default), `"evidence"`,
#'   `"cost_minimising"`.
#' @param max_recommendations Integer. How many detectors to return per
#'   group. Default 3.
#'
#' @return An `epi_recommendation` object with elements
#'   \itemize{
#'     \item `$profiles` — tibble from [profile_series()].
#'     \item `$recommendations` — long tibble: `group_id`, `rank`, `method`,
#'       `score`, `reason`.
#'     \item `$tie_break`, `$evaluation`, `$call`.
#'   }
#'
#' @seealso [epi_detect()], [epi_evaluate()], [epi_ensemble()],
#'   [profile_series()].
#'
#' @export
epi_recommend <- function(data,
                          date_col,
                          count_col,
                          group_col = NULL,
                          evaluation = NULL,
                          tie_break = c("simplicity",
                                        "evidence",
                                        "cost_minimising"),
                          max_recommendations = 3L) {

  tie_break <- match.arg(tie_break)

  if (!is.numeric(max_recommendations) || length(max_recommendations) != 1L ||
      max_recommendations < 1L) {
    cli::cli_abort("{.arg max_recommendations} must be a positive integer.")
  }
  max_recommendations <- as.integer(max_recommendations)

  date_col  <- rlang::ensym(date_col)
  count_col <- rlang::ensym(count_col)
  has_group <- !rlang::quo_is_null(rlang::enquo(group_col))
  group_sym <- if (has_group) rlang::ensym(group_col) else NULL

  profiles <- if (has_group) {
    profile_series(data, !!date_col, !!count_col, !!group_sym)
  } else {
    profile_series(data, !!date_col, !!count_col)
  }

  recs <- purrr::map_dfr(seq_len(nrow(profiles)), function(i) {
    p <- profiles[i, ]
    ranked <- .recommend_rules(p)

    ranked <- switch(tie_break,
      "simplicity"      = .tie_break_simplicity(ranked),
      "evidence"        = .tie_break_evidence(ranked, evaluation),
      "cost_minimising" = .tie_break_cost(ranked, evaluation)
    )

    ranked |>
      dplyr::mutate(
        group_id = p$group_id,
        rank     = dplyr::row_number()
      ) |>
      dplyr::filter(.data$rank <= max_recommendations) |>
      dplyr::select("group_id", "rank", "method", "score", "reason",
                    dplyr::any_of(c("simplicity_rank", "auc", "cost")))
  })

  out <- list(
    profiles        = profiles,
    recommendations = recs,
    tie_break       = tie_break,
    evaluation      = evaluation,
    max_recommendations = max_recommendations,
    call            = match.call()
  )

  class(out) <- c("epi_recommendation", "list")
  out
}


# =============================================================================
# S3 methods for `epi_recommendation`
# =============================================================================

#' @export
print.epi_recommendation <- function(x, ...) {
  cli::cli_h2("epi_recommendation")
  cli::cli_text("Tie-break strategy: {.val {x$tie_break}}.")
  cli::cli_text("Top {.val {x$max_recommendations}} per group.")
  print(x$recommendations)
  invisible(x)
}


#' @export
summary.epi_recommendation <- function(object, ...) {
  cli::cli_h2("epi_recommendation summary")
  cli::cli_text("Tie-break: {.val {object$tie_break}}")
  cli::cli_text("Groups profiled: {.val {nrow(object$profiles)}}")

  consensus <- object$recommendations |>
    dplyr::filter(.data$rank == 1L) |>
    dplyr::count(.data$method, name = "n_groups_top1") |>
    dplyr::arrange(dplyr::desc(.data$n_groups_top1))

  cli::cli_text("Top-1 method counts across groups:")
  print(consensus)
  invisible(list(consensus = consensus, profiles = object$profiles))
}


#' Convert an `epi_recommendation` to a tibble.
#'
#' @param x An `epi_recommendation`.
#' @param ... Unused.
#' @return The `$recommendations` tibble.
#' @method as_tibble epi_recommendation
#' @export
as_tibble.epi_recommendation <- function(x, ...) {
  x$recommendations
}


#' @export
plot.epi_recommendation <- function(x, ...) {
  print(autoplot.epi_recommendation(x, ...))
}


#' Autoplot for `epi_recommendation`.
#'
#' Two-panel: top shows the numeric profile as horizontal bars (one facet per
#' group); bottom shows the ranked recommendations as a coloured tile grid.
#'
#' @param object An `epi_recommendation`.
#' @param ... Unused.
#' @return A ggplot.
#' @method autoplot epi_recommendation
#' @export
autoplot.epi_recommendation <- function(object, ...) {
  .check_pkg("ggplot2",
             reason = "for plotting `epi_recommendation` objects.")
  .check_pkg("patchwork",
             reason = "for combining recommendation plot panels.")
  .check_pkg("tidyr",
             reason = "for reshaping the recommendation profile.")

  prof_long <- object$profiles |>
    tidyr::pivot_longer(
      cols = -"group_id",
      names_to = "metric", values_to = "value"
    ) |>
    dplyr::filter(!is.na(.data$value))

  p_prof <- ggplot2::ggplot(
    prof_long,
    ggplot2::aes(x = .data$value, y = .data$metric)
  ) +
    ggplot2::geom_col(fill = "#4a6fa5") +
    ggplot2::facet_wrap(~ .data$group_id, scales = "free_x") +
    ggplot2::labs(title = "Series profile", x = NULL, y = NULL) +
    ggplot2::theme_minimal()

  recs <- object$recommendations
  p_rec <- ggplot2::ggplot(
    recs,
    ggplot2::aes(x = factor(.data$rank),
                 y = .data$group_id,
                 fill = .data$method)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$method),
      size = 3, colour = "black"
    ) +
    ggplot2::labs(
      title = paste0(
        "Top-",
        object$max_recommendations,
        " recommendations (tie-break: ",
        object$tie_break, ")"
      ),
      x = "Rank", y = NULL
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme_minimal()

  patchwork::wrap_plots(p_prof, p_rec, ncol = 1, heights = c(1, 1))
}
