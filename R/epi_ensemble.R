# =============================================================================
# epi_ensemble.R
# -----------------------------------------------------------------------------
# Combine per-method alarms from an `epi_detection_run` into a single
# ensemble alarm series. Three strategies are supported:
#
#   * "majority_vote"  — alarm if >= k of M methods agree (default k = ceil(M/2))
#   * "weighted_vote"  — alarm if weighted sum of alarms exceeds half of weights
#   * "score_average"  — min-max normalise scores per (group, date) and alarm
#                        if the mean normalised score exceeds 0.5
#
# Weights may be supplied as (a) `NULL` (equal), (b) a named numeric vector,
# or (c) an `epi_evaluation` object — in which case weights are derived from
# `loo_method$delta_auc` (negative deltas floored to zero, then normalised).
#
# Failed cells (alarm = NA, failed = TRUE) are handled by `na_handling`:
#   * "drop"     — exclude failed cell from that (group, date)'s vote
#   * "as_false" — treat failed cell as a negative (no alarm)
#
# Returns an `epi_ensemble` S3 object with print/summary/as_tibble/
# plot/autoplot methods. The object is accepted as input by `epi_evaluate()`
# (treated as a single-method run for grading purposes).
# =============================================================================


#' Combine per-method alarms from an `epi_detection_run` into an ensemble.
#'
#' @description
#' Three ensemble strategies are supported (`majority_vote`, `weighted_vote`,
#' `score_average`). Failed detector cells are governed by `na_handling`.
#' Weights may be derived from an `epi_evaluation` object so that methods
#' which hurt the LOO ensemble are down-weighted to zero.
#'
#' @param detection_run An `epi_detection_run` object (the output of
#'   [epi_detect()]).
#' @param strategy One of `"majority_vote"`, `"weighted_vote"`,
#'   `"score_average"`.
#' @param weights Optional. Either `NULL` (equal weighting), a named numeric
#'   vector keyed by detector method, or an `epi_evaluation` object.
#' @param k Optional integer. Minimum vote count to raise an alarm under
#'   `majority_vote`. Defaults to `ceiling(M/2)` where `M` is the number of
#'   non-failed methods at that (group, date).
#' @param na_handling One of `"drop"` (default) or `"as_false"`. Controls how
#'   failed detector cells (alarm = NA) are treated when combining.
#' @param methods_subset Optional character vector restricting the ensemble
#'   to a subset of the detectors present in `detection_run`.
#'
#' @return An `epi_ensemble` object: a list with elements
#'   \itemize{
#'     \item `$predictions` — tibble with `method = "ensemble"`, `group_id`,
#'       `date`, `alarm`, `score`, `upper_threshold = NA`, `failed`,
#'       `error_message`.
#'     \item `$strategy`, `$k`, `$na_handling`, `$weights` — configuration.
#'     \item `$source_run` — the source `epi_detection_run` (for plotting).
#'     \item `$call` — matched call.
#'   }
#'
#' @seealso [epi_detect()], [epi_evaluate()], [epi_recommend()].
#'
#' @export
epi_ensemble <- function(detection_run,
                         strategy = c("majority_vote",
                                      "weighted_vote",
                                      "score_average"),
                         weights = NULL,
                         k = NULL,
                         na_handling = c("drop", "as_false"),
                         methods_subset = NULL) {

  if (!inherits(detection_run, "epi_detection_run")) {
    cli::cli_abort(c(
      "{.arg detection_run} must be an {.cls epi_detection_run} object.",
      "i" = "Got: {.cls {class(detection_run)[1]}}."
    ))
  }

  strategy <- match.arg(strategy)
  na_handling <- match.arg(na_handling)

  preds <- detection_run$predictions
  methods_in_run <- unique(preds$method)

  if (!is.null(methods_subset)) {
    missing_m <- setdiff(methods_subset, methods_in_run)
    if (length(missing_m)) {
      cli::cli_abort(c(
        "`methods_subset` references unknown method{?s}.",
        "x" = "Not in detection run: {.val {missing_m}}.",
        "i" = "Available: {.val {methods_in_run}}."
      ))
    }
    methods_in_run <- methods_subset
  }

  if (length(methods_in_run) < 2L) {
    cli::cli_warn(c(
      "Ensembling a single detector is degenerate.",
      "i" = "Consider passing >= 2 methods to {.fn epi_detect}."
    ))
  }

  # `.combine_methods()` lives in epi_evaluate.R and is reused here.
  # Map user-facing strategy names onto the internal vocabulary.
  internal_strategy <- switch(strategy,
    "majority_vote"  = "majority",
    "weighted_vote"  = "weighted",
    "score_average"  = "probability"
  )

  # `.resolve_weights()` (epi_evaluate.R) supports epi_evaluation objects.
  resolved_weights <- if (strategy == "weighted_vote") {
    .resolve_weights(weights, methods_in_run)
  } else {
    NULL
  }

  if (strategy == "weighted_vote" && !is.null(weights) &&
      is.numeric(weights) && !is.null(names(weights))) {
    # caller supplied a named numeric vector directly; pass through
    resolved_weights <- weights
  }

  combined <- .combine_methods(
    predictions   = preds,
    strategy      = internal_strategy,
    k             = k,
    weights       = resolved_weights,
    methods_subset = methods_subset,
    na_handling   = na_handling
  )

  # propagate failure metadata: count failures per (group, date) across methods
  failure_summary <- preds |>
    dplyr::filter(.data$method %in% methods_in_run) |>
    dplyr::group_by(.data$group_id, .data$date) |>
    dplyr::summarise(
      n_failed = sum(.data$failed, na.rm = TRUE),
      n_total  = dplyr::n(),
      first_error = {
        msgs <- stats::na.omit(.data$error_message)
        if (length(msgs)) msgs[1] else NA_character_
      },
      .groups = "drop"
    )

  predictions <- combined |>
    dplyr::rename(alarm = "ensemble_alarm", score = "ensemble_score") |>
    dplyr::left_join(failure_summary, by = c("group_id", "date")) |>
    dplyr::mutate(
      method = "ensemble",
      upper_threshold = NA_real_,
      failed = .data$n_failed == .data$n_total,
      alarm = dplyr::if_else(.data$failed, NA, .data$alarm),
      error_message = dplyr::if_else(
        .data$failed,
        .data$first_error %||% "all member detectors failed",
        NA_character_
      )
    ) |>
    dplyr::select(
      "method", "group_id", "date", "alarm", "score",
      "upper_threshold", "failed", "error_message"
    )

  out <- list(
    predictions   = tibble::as_tibble(predictions),
    strategy      = strategy,
    k             = k,
    na_handling   = na_handling,
    weights       = resolved_weights,
    methods_used  = methods_in_run,
    source_run    = detection_run,
    call          = match.call()
  )

  class(out) <- c("epi_ensemble", "list")
  out
}


# =============================================================================
# S3 methods for `epi_ensemble`
# =============================================================================

#' @export
print.epi_ensemble <- function(x, ...) {
  cli::cli_h2("epi_ensemble")
  cli::cli_text("Strategy: {.val {x$strategy}}")
  if (x$strategy == "majority_vote") {
    k_disp <- if (is.null(x$k)) "ceil(M/2)" else as.character(x$k)
    cli::cli_text("Vote threshold (k): {.val {k_disp}}")
  }
  cli::cli_text("Members: {.val {x$methods_used}}")
  if (!is.null(x$weights)) {
    w_summary <- paste0(
      names(x$weights), "=",
      sprintf("%.3f", x$weights),
      collapse = ", "
    )
    cli::cli_text("Weights: {w_summary}")
  }
  cli::cli_text("NA handling: {.val {x$na_handling}}")
  n_alarm <- sum(x$predictions$alarm, na.rm = TRUE)
  n_fail  <- sum(x$predictions$failed, na.rm = TRUE)
  n_total <- nrow(x$predictions)
  cli::cli_text(
    "{n_alarm} alarm{?s} across {n_total} (group, date) cell{?s}; ",
    "{n_fail} fully-failed cell{?s}."
  )
  invisible(x)
}


#' @export
summary.epi_ensemble <- function(object, ...) {
  per_group <- object$predictions |>
    dplyr::group_by(.data$group_id) |>
    dplyr::summarise(
      n_obs     = dplyr::n(),
      n_alarms  = sum(.data$alarm, na.rm = TRUE),
      n_failed  = sum(.data$failed, na.rm = TRUE),
      .groups   = "drop"
    )

  cli::cli_h2("epi_ensemble summary")
  cli::cli_text("Strategy: {.val {object$strategy}} ({length(object$methods_used)} member{?s}).")
  print(per_group)
  invisible(list(per_group = per_group))
}


#' Convert an `epi_ensemble` to a tibble.
#'
#' @param x An `epi_ensemble`.
#' @param ... Unused.
#' @return The `$predictions` tibble.
#' @method as_tibble epi_ensemble
#' @export
as_tibble.epi_ensemble <- function(x, ...) {
  x$predictions
}


#' @export
plot.epi_ensemble <- function(x, ...) {
  print(autoplot.epi_ensemble(x, ...))
}


#' Autoplot for `epi_ensemble`.
#'
#' Two-panel ridge: top shows the per-detector alarm raster (from the source
#' detection run); bottom shows the ensemble alarm time-series, with ensemble
#' alarms highlighted.
#'
#' @param object An `epi_ensemble` object.
#' @param ... Unused.
#' @return A ggplot object.
#' @method autoplot epi_ensemble
#' @export
autoplot.epi_ensemble <- function(object, ...) {
  .check_pkg("ggplot2",
             reason = "for plotting `epi_ensemble` objects.")
  .check_pkg("patchwork",
             reason = "for combining ensemble plot panels.")

  # member raster
  member_preds <- object$source_run$predictions |>
    dplyr::filter(.data$method %in% object$methods_used)

  p_top <- ggplot2::ggplot(
    member_preds,
    ggplot2::aes(x = .data$date, y = .data$method,
                 fill = .data$alarm)
  ) +
    ggplot2::geom_tile(colour = "grey90", linewidth = 0.1) +
    ggplot2::facet_wrap(~ .data$group_id, scales = "free_x") +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#d7301f", "FALSE" = "#fdcc8a"),
      na.value = "grey60",
      name = "alarm"
    ) +
    ggplot2::labs(title = "Member detector alarms",
                  x = NULL, y = NULL) +
    ggplot2::theme_minimal()

  # ensemble time-series
  ens <- object$predictions
  p_bot <- ggplot2::ggplot(
    ens,
    ggplot2::aes(x = .data$date, y = .data$score)
  ) +
    ggplot2::geom_line(colour = "grey40") +
    ggplot2::geom_point(
      data = dplyr::filter(ens, .data$alarm %in% TRUE),
      ggplot2::aes(x = .data$date, y = .data$score),
      colour = "#d7301f", size = 2
    ) +
    ggplot2::facet_wrap(~ .data$group_id, scales = "free") +
    ggplot2::labs(
      title = paste0("Ensemble alarm (strategy = ", object$strategy, ")"),
      x = NULL, y = "Ensemble score"
    ) +
    ggplot2::theme_minimal()

  patchwork::wrap_plots(p_top, p_bot, ncol = 1, heights = c(1, 1))
}
