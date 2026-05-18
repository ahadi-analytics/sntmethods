# ---------------------------------------------------------------------------
# epi_evaluate.R
#
# Evaluation of an epi_detection_run against ground truth (labels or
# simulation-injected outbreaks), or unlabelled qualitative diagnostics.
# Computes standard, surveillance-specific, and cost-aware metrics; runs the
# leave-one-method-out engine; produces operating-point and k-sweep tables;
# and exposes a nine-plot diagnostic suite via autoplot.
#
# Per the v1.1 spec, helpers specific to this verb live in this script.
# `.combine_methods()` is the one shared helper that `epi_ensemble.R` also
# calls; it is defined here because it is needed first by the LOO engine.
# ---------------------------------------------------------------------------


# ---- metric computers -----------------------------------------------------

#' Compute the standard binary-classification confusion-matrix metrics
#'
#' Returns sensitivity, specificity, ppv, npv, accuracy, f1, and a
#' user-specified F-beta. Missing alarms (failed detector cells) are
#' dropped from the count.
#'
#' @keywords internal
#' @noRd
.compute_standard_metrics <- function(alarm, truth, f_beta = 1) {
  keep <- !is.na(alarm) & !is.na(truth)
  alarm <- alarm[keep]
  truth <- truth[keep]
  if (length(alarm) == 0L) {
    return(list(
      sensitivity = NA_real_, specificity = NA_real_,
      ppv = NA_real_, npv = NA_real_, accuracy = NA_real_,
      f1 = NA_real_, f_beta = NA_real_,
      tp = 0L, fp = 0L, tn = 0L, fn = 0L
    ))
  }

  tp <- sum(alarm & truth)
  fp <- sum(alarm & !truth)
  tn <- sum(!alarm & !truth)
  fn <- sum(!alarm & truth)

  sens <- if (tp + fn > 0L) tp / (tp + fn) else NA_real_
  spec <- if (tn + fp > 0L) tn / (tn + fp) else NA_real_
  ppv <- if (tp + fp > 0L) tp / (tp + fp) else NA_real_
  npv <- if (tn + fn > 0L) tn / (tn + fn) else NA_real_
  acc <- (tp + tn) / length(alarm)

  f1 <- if (!is.na(ppv) && !is.na(sens) && (ppv + sens) > 0) {
    2 * ppv * sens / (ppv + sens)
  } else {
    NA_real_
  }
  fb <- if (!is.na(ppv) && !is.na(sens) &&
              (f_beta^2 * ppv + sens) > 0) {
    (1 + f_beta^2) * ppv * sens / (f_beta^2 * ppv + sens)
  } else {
    NA_real_
  }

  list(
    sensitivity = sens, specificity = spec,
    ppv = ppv, npv = npv, accuracy = acc,
    f1 = f1, f_beta = fb,
    tp = tp, fp = fp, tn = tn, fn = fn
  )
}

#' Compute AUC (Mann-Whitney U) for binary truth and continuous score
#'
#' @keywords internal
#' @noRd
.compute_auc <- function(score, truth) {
  keep <- !is.na(score) & !is.na(truth)
  score <- score[keep]
  truth <- truth[keep]
  n_pos <- sum(truth)
  n_neg <- length(truth) - n_pos
  if (n_pos == 0L || n_neg == 0L) return(NA_real_)
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[truth]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

#' Compute area under the precision-recall curve via trapezoidal rule on
#' score-sorted rankings.
#'
#' @keywords internal
#' @noRd
.compute_auprc <- function(score, truth) {
  keep <- !is.na(score) & !is.na(truth)
  score <- score[keep]
  truth <- truth[keep]
  if (sum(truth) == 0L) return(NA_real_)
  ord <- order(score, decreasing = TRUE)
  truth <- truth[ord]
  tp <- cumsum(truth)
  fp <- cumsum(!truth)
  precision <- tp / (tp + fp)
  recall <- tp / sum(truth)
  # trapezoidal AUC under PR curve
  sum(diff(c(0, recall)) * precision)
}

#' Compute surveillance-specific metrics
#'
#' - **time_to_detect**: weeks between each outbreak run's start and the
#'   first true alarm in that run; reported as mean across runs.
#' - **false_alarm_rate**: proportion of non-outbreak weeks that fired.
#' - **alarm_persistence**: mean length of contiguous alarm runs.
#' - **alarm_shoulder_fraction**: share of FP alarms that fall within
#'   `shoulder_window` weeks of any outbreak run (treated as defensible).
#'
#' @keywords internal
#' @noRd
.compute_surveillance_metrics <- function(alarm, truth,
                                           shoulder_window = 4L) {
  alarm[is.na(alarm)] <- FALSE
  truth[is.na(truth)] <- FALSE

  # contiguous outbreak runs in truth
  truth_runs <- .runs(truth)
  ttd <- if (nrow(truth_runs) > 0L) {
    lags <- purrr::map_dbl(seq_len(nrow(truth_runs)), function(i) {
      r <- truth_runs[i, ]
      span <- r$start:r$end
      first_hit <- which(alarm[span])[1]
      if (is.na(first_hit)) NA_real_ else as.numeric(first_hit - 1L)
    })
    mean(lags, na.rm = TRUE)
  } else {
    NA_real_
  }

  # false-alarm rate over non-outbreak weeks
  far <- if (sum(!truth) > 0L) sum(alarm & !truth) / sum(!truth) else NA_real_

  # alarm persistence
  alarm_runs <- .runs(alarm)
  persistence <- if (nrow(alarm_runs) > 0L) {
    mean(alarm_runs$end - alarm_runs$start + 1L)
  } else {
    NA_real_
  }

  # shoulder fraction: FP alarms within shoulder_window of any outbreak
  if (sum(alarm & !truth) > 0L && nrow(truth_runs) > 0L) {
    fp_idx <- which(alarm & !truth)
    is_shoulder <- purrr::map_lgl(fp_idx, function(i) {
      any(abs(i - c(truth_runs$start, truth_runs$end)) <= shoulder_window)
    })
    shoulder_fraction <- mean(is_shoulder)
  } else {
    shoulder_fraction <- NA_real_
  }

  list(
    time_to_detect = ttd,
    false_alarm_rate = far,
    alarm_persistence = persistence,
    alarm_shoulder_fraction = shoulder_fraction
  )
}

#' Identify start/end indices of contiguous TRUE runs in a logical vector.
#'
#' @keywords internal
#' @noRd
.runs <- function(x) {
  if (length(x) == 0L || !any(x)) {
    return(tibble::tibble(start = integer(0), end = integer(0)))
  }
  rle_x <- rle(x)
  ends <- cumsum(rle_x$lengths)
  starts <- ends - rle_x$lengths + 1L
  keep <- rle_x$values
  tibble::tibble(start = starts[keep], end = ends[keep])
}

#' Expected cost per method given cost-per-FP, cost-per-FN, and the
#' empirically observed outbreak prevalence.
#'
#' @keywords internal
#' @noRd
.compute_cost <- function(sensitivity, specificity, p_outbreak,
                           cost_per_fp = 1, cost_per_fn = 1) {
  if (is.na(sensitivity) || is.na(specificity)) return(NA_real_)
  cost_per_fp * (1 - specificity) * (1 - p_outbreak) +
    cost_per_fn * (1 - sensitivity) * p_outbreak
}


# ---- ground-truth resolution ----------------------------------------------

#' Pull a logical ground-truth vector aligned with `predictions$date` from
#' either a label column or vector or a simulation specification.
#'
#' Returns `NULL` for unlabelled qualitative mode.
#'
#' @keywords internal
#' @noRd
.resolve_truth <- function(detection_run, labels, simulation) {
  if (!is.null(labels) && !is.null(simulation)) {
    cli::cli_abort(
      "Supply at most one of {.arg labels} or {.arg simulation}, not both."
    )
  }

  preds <- detection_run$predictions
  prep <- detection_run$preprocessing

  if (!is.null(labels)) {
    label_vec <- if (is.character(labels) && length(labels) == 1L) {
      # column name in original data
      col <- labels
      if (!col %in% names(prep$original_data)) {
        cli::cli_abort(
          "Label column {.val {col}} not found in detection_run input data."
        )
      }
      prep$original_data[[col]]
    } else if (is.logical(labels)) {
      labels
    } else {
      cli::cli_abort(
        "{.arg labels} must be a column name or a logical vector."
      )
    }

    # restrict label vec to target rows
    target_dates <- prep$original_data[[prep$date_col]][prep$target_range]
    label_by_date <- tibble::tibble(
      date = prep$original_data[[prep$date_col]],
      .truth = as.logical(label_vec)
    ) |>
      dplyr::filter(.data$date %in% target_dates)

    return(list(
      mode = "labelled",
      truth_table = label_by_date,
      simulation = NULL
    ))
  }

  if (!is.null(simulation)) {
    return(list(
      mode = "simulation",
      truth_table = NULL,
      simulation = simulation
    ))
  }

  list(mode = "unlabelled", truth_table = NULL, simulation = NULL)
}

#' Align ground-truth labels to a per-method predictions tibble (joins by
#' date within group).
#'
#' @keywords internal
#' @noRd
.attach_truth <- function(predictions, truth_table) {
  if (is.null(truth_table)) return(predictions)
  dplyr::left_join(
    predictions,
    truth_table,
    by = "date"
  )
}


# ---- quiet baseline + simulation injector ---------------------------------

#' Build a "quiet" version of a series by replacing known outbreak weeks
#' with the per-iso-week median computed over the non-outbreak weeks.
#'
#' @param data A data frame with a date column and a count column.
#' @param date_col,count_col Column names.
#' @param outbreak_col Optional logical column flagging outbreak weeks. If
#'   `NULL`, the whole series is treated as quiet.
#'
#' @keywords internal
#' @noRd
.build_quiet_baseline <- function(data,
                                   date_col = "date",
                                   count_col = "cases",
                                   outbreak_col = NULL) {
  if (is.null(outbreak_col)) {
    return(data)
  }

  out <- data
  iso_week <- as.integer(format(out[[date_col]], "%V"))
  out[[".iso_week"]] <- iso_week
  is_outbreak <- as.logical(out[[outbreak_col]])
  is_outbreak[is.na(is_outbreak)] <- FALSE

  per_week_median <- tapply(
    out[[count_col]][!is_outbreak],
    out[[".iso_week"]][!is_outbreak],
    stats::median,
    na.rm = TRUE
  )

  for (i in which(is_outbreak)) {
    wk <- as.character(iso_week[i])
    if (wk %in% names(per_week_median)) {
      out[[count_col]][i] <- per_week_median[[wk]]
    }
  }

  out[[".iso_week"]] <- NULL
  out
}

#' Inject a synthetic outbreak into a quiet series.
#'
#' @param series A data frame with `date` and `cases` columns.
#' @param magnitude Multiple of baseline SD for the spike peak.
#' @param duration Outbreak length in weeks.
#' @param shape One of `"constant"`, `"ramp"`, `"peaked"`.
#' @param start_idx 1-based row index where the outbreak starts.
#'
#' @return A list with `$series` (modified data) and `$outbreak_idx`
#'   (integer vector of injected row indices).
#'
#' @keywords internal
#' @noRd
.inject_outbreak <- function(series,
                              magnitude,
                              duration,
                              shape = c("ramp", "constant", "peaked"),
                              start_idx) {
  shape <- match.arg(shape)
  duration <- as.integer(duration)
  baseline_sd <- stats::sd(series$cases, na.rm = TRUE)
  if (is.na(baseline_sd) || baseline_sd == 0) baseline_sd <- 1

  spike <- switch(shape,
    "constant" = rep(magnitude * baseline_sd, duration),
    "ramp" = seq(0, magnitude * baseline_sd, length.out = duration),
    "peaked" = {
      half <- ceiling(duration / 2)
      up <- seq(0, magnitude * baseline_sd, length.out = half)
      down <- seq(
        magnitude * baseline_sd, 0,
        length.out = duration - half + 1L
      )[-1]
      c(up, down)
    }
  )

  end_idx <- min(start_idx + duration - 1L, nrow(series))
  used <- seq.int(start_idx, end_idx)
  series$cases[used] <- series$cases[used] + spike[seq_along(used)]
  list(series = series, outbreak_idx = used)
}


# ---- combine methods (shared with epi_ensemble) ---------------------------

#' Combine per-method alarms into an ensemble alarm series.
#'
#' Shared engine between [epi_ensemble()] and the leave-one-method-out and
#' k-sweep machinery inside [epi_evaluate()]. Implements three strategies:
#'
#' - **majority**: alarm if `sum(per_method_alarm) >= k`. Default `k` is
#'   `ceiling(n_methods / 2)`.
#' - **weighted**: alarm if the weighted sum of per-method alarms exceeds
#'   half the total weight. `weights` is a named numeric vector indexed
#'   by method id, or an `epi_evaluation` object (in which case weights
#'   are derived from `loo_method$delta_auc` by default).
#' - **probability**: alarm if the mean of per-method min-max-normalised
#'   scores exceeds `0.5`.
#'
#' @param predictions Long tibble (must contain `method`, `group_id`,
#'   `date`, `alarm`, `score`, `failed`).
#' @param strategy Combination strategy.
#' @param k For majority strategy.
#' @param weights For weighted strategy.
#' @param methods_subset Optional character vector of method ids to keep.
#' @param na_handling `"drop"` (default) or `"as_false"`. Controls how
#'   failed-method rows participate in the ensemble.
#'
#' @return Tibble with `date`, `group_id`, `ensemble_alarm`,
#'   `ensemble_score`.
#'
#' @keywords internal
#' @noRd
.combine_methods <- function(predictions,
                              strategy = c("majority", "weighted",
                                           "probability"),
                              k = NULL,
                              weights = NULL,
                              methods_subset = NULL,
                              na_handling = c("drop", "as_false")) {
  strategy <- match.arg(strategy)
  na_handling <- match.arg(na_handling)

  if (!is.null(methods_subset)) {
    predictions <- predictions |>
      dplyr::filter(.data$method %in% methods_subset)
  }

  # extract numeric weights from epi_evaluation if needed
  if (strategy == "weighted") {
    weights <- .resolve_weights(weights, unique(predictions$method))
  }

  # handle failed cells
  predictions <- predictions |>
    dplyr::mutate(
      alarm = dplyr::case_when(
        is.na(.data$alarm) & na_handling == "as_false" ~ FALSE,
        TRUE ~ .data$alarm
      )
    )

  if (na_handling == "drop") {
    predictions <- predictions |>
      dplyr::filter(!is.na(.data$alarm))
  }

  predictions <- predictions |>
    dplyr::mutate(
      alarm_int = as.integer(.data$alarm),
      score = dplyr::if_else(is.na(.data$score), 0, .data$score)
    )

  # per (group_id, date) combine
  combined <- predictions |>
    dplyr::group_by(.data$group_id, .data$date) |>
    dplyr::summarise(
      ensemble_alarm = switch(strategy,
        "majority" = {
          n_methods <- dplyr::n()
          this_k <- if (is.null(k)) ceiling(n_methods / 2) else k
          sum(.data$alarm_int) >= this_k
        },
        "weighted" = {
          w_vec <- weights[as.character(.data$method)]
          w_vec[is.na(w_vec)] <- 0
          sum(.data$alarm_int * w_vec) > (sum(w_vec) / 2)
        },
        "probability" = {
          # min-max normalise per call
          s <- .data$score
          rng <- diff(range(s, na.rm = TRUE))
          s_norm <- if (rng == 0) rep(0, length(s)) else
            (s - min(s, na.rm = TRUE)) / rng
          mean(s_norm, na.rm = TRUE) > 0.5
        }
      ),
      ensemble_score = mean(.data$score, na.rm = TRUE),
      .groups = "drop"
    )

  combined
}

#' Resolve `weights` argument for `.combine_methods()` weighted strategy.
#'
#' @keywords internal
#' @noRd
.resolve_weights <- function(weights, methods_in_run, metric = "delta_auc") {
  if (is.null(weights)) {
    # equal weighting
    out <- rep(1, length(methods_in_run))
    names(out) <- methods_in_run
    return(out)
  }

  if (inherits(weights, "epi_evaluation")) {
    if (is.null(weights$loo_method)) {
      cli::cli_abort(c(
        "Cannot derive weights from {.cls epi_evaluation} object.",
        "i" = "It does not contain a `loo_method` table.",
        "i" = "Re-run {.fn epi_evaluate} with {.code loo_method = TRUE}."
      ))
    }
    loo <- weights$loo_method
    if (!metric %in% names(loo)) {
      cli::cli_abort(c(
        "Metric {.val {metric}} not present in `loo_method` table.",
        "i" = "Available: {.val {setdiff(names(loo), 'method')}}."
      ))
    }
    raw <- loo[[metric]]
    # negative deltas mean the method hurts the ensemble; floor at 0
    raw <- pmax(raw, 0)
    names(raw) <- loo$method
    if (sum(raw) == 0) {
      # fallback to equal weighting if all deltas are non-positive
      out <- rep(1, length(methods_in_run))
      names(out) <- methods_in_run
      return(out)
    }
    return(raw)
  }

  if (!is.numeric(weights) || is.null(names(weights))) {
    cli::cli_abort(
      "{.arg weights} must be a named numeric vector or an {.cls epi_evaluation} object."
    )
  }

  weights
}


# ---- LOO engine -----------------------------------------------------------

#' Compute leave-one-method-out deltas for every method in the run.
#'
#' Builds the full ensemble across all methods, computes ensemble metrics,
#' then for each method m rebuilds the ensemble excluding m and reports
#' `delta_<metric> = full - leave_m_out`. Positive deltas mean the method
#' is essential to the ensemble; negative deltas mean it is degrading the
#' ensemble.
#'
#' @keywords internal
#' @noRd
.run_loo <- function(predictions,
                      truth_table,
                      strategy,
                      k,
                      f_beta) {
  methods <- unique(predictions$method)
  if (length(methods) < 2L) {
    cli::cli_warn(
      "Leave-one-method-out requires >= 2 methods; skipping LOO table."
    )
    return(NULL)
  }

  truth_by_date <- truth_table |>
    dplyr::rename(.truth = ".truth")

  metric_full <- .ensemble_metrics(
    predictions, truth_by_date, strategy, k, f_beta,
    methods_subset = methods
  )

  per_method <- purrr::map_dfr(methods, function(m) {
    subset_methods <- setdiff(methods, m)
    metric_reduced <- .ensemble_metrics(
      predictions, truth_by_date, strategy, k, f_beta,
      methods_subset = subset_methods
    )
    tibble::tibble(
      method = m,
      delta_auc = metric_full$auc - metric_reduced$auc,
      delta_sens = metric_full$sensitivity - metric_reduced$sensitivity,
      delta_spec = metric_full$specificity - metric_reduced$specificity,
      delta_f1 = metric_full$f1 - metric_reduced$f1,
      delta_f_beta = metric_full$f_beta - metric_reduced$f_beta
    )
  })

  per_method |>
    dplyr::arrange(dplyr::desc(.data$delta_auc))
}

#' Compute ensemble metrics against a truth table (one call site for LOO
#' and k-sweep so behaviour stays consistent).
#'
#' @keywords internal
#' @noRd
.ensemble_metrics <- function(predictions,
                               truth_table,
                               strategy,
                               k,
                               f_beta,
                               methods_subset = NULL) {
  combined <- .combine_methods(
    predictions = predictions,
    strategy = strategy,
    k = k,
    methods_subset = methods_subset,
    na_handling = "drop"
  )
  joined <- dplyr::left_join(combined, truth_table, by = "date")
  std <- .compute_standard_metrics(
    joined$ensemble_alarm, joined$.truth, f_beta = f_beta
  )
  auc <- .compute_auc(as.numeric(joined$ensemble_alarm), joined$.truth)
  c(std, list(auc = auc))
}


# ---- k-sweep diagnostic ---------------------------------------------------

#' Sweep majority-ensemble `k` from 1..n_methods and report precision /
#' recall / F1 at each k.
#'
#' @keywords internal
#' @noRd
.run_k_sweep <- function(predictions,
                          truth_table,
                          f_beta = 1) {
  methods <- unique(predictions$method)
  n <- length(methods)
  if (n < 2L || is.null(truth_table)) return(NULL)

  purrr::map_dfr(seq_len(n), function(this_k) {
    m <- .ensemble_metrics(
      predictions,
      truth_table |> dplyr::rename(.truth = ".truth"),
      strategy = "majority",
      k = this_k,
      f_beta = f_beta,
      methods_subset = methods
    )
    tibble::tibble(
      k = this_k,
      sensitivity = m$sensitivity,
      specificity = m$specificity,
      ppv = m$ppv,
      f1 = m$f1,
      f_beta = m$f_beta
    )
  })
}


# ---- operating-point analysis ---------------------------------------------

#' Build the operating-point table by injecting synthetic outbreaks across
#' a magnitude x duration grid and reporting the smallest reliably
#' detected outbreak per method.
#'
#' Returns `NULL` if no `detection_run`-equivalent re-detection is
#' possible (e.g. the original data is unavailable or contains no
#' non-outbreak quiet baseline).
#'
#' @keywords internal
#' @noRd
.build_operating_points <- function(detection_run,
                                     simulation,
                                     grace_period = 2L) {
  prep <- detection_run$preprocessing
  methods <- detection_run$methods

  n_iterations <- simulation$n_iterations %||% 100L
  magnitudes <- simulation$magnitudes %||% c(1.5, 3, 5)
  durations <- as.integer(simulation$durations %||% c(2L, 4L, 8L))
  shape <- simulation$shape %||% "ramp"

  # build a quiet baseline (no known outbreaks given => assume whole series
  # is quiet)
  outbreak_col <- if (!is.null(prep$label_col)) prep$label_col else NULL
  quiet <- .build_quiet_baseline(
    prep$original_data,
    date_col = prep$date_col,
    count_col = prep$count_col,
    outbreak_col = outbreak_col
  )

  # candidate start indices: within target_range, far enough from the end
  cand_idx <- prep$target_range
  cand_idx <- cand_idx[cand_idx + max(durations) - 1L <= nrow(quiet)]
  if (length(cand_idx) == 0L) {
    return(NULL)
  }

  set.seed(simulation$seed %||% 1L)

  grid <- expand.grid(
    magnitude = magnitudes,
    duration = durations,
    iter = seq_len(n_iterations),
    stringsAsFactors = FALSE
  )

  results <- purrr::pmap_dfr(grid, function(magnitude, duration, iter) {
    start_idx <- sample(cand_idx, 1L)
    injection <- .inject_outbreak(
      series = quiet |>
        dplyr::rename(date = !!prep$date_col, cases = !!prep$count_col),
      magnitude = magnitude,
      duration = duration,
      shape = shape,
      start_idx = match(quiet[[prep$date_col]][start_idx],
                        quiet[[prep$date_col]])
    )

    injected_data <- injection$series
    names(injected_data)[names(injected_data) == "date"] <- prep$date_col
    names(injected_data)[names(injected_data) == "cases"] <- prep$count_col

    run <- tryCatch(
      epi_detect(
        data = injected_data,
        methods = methods,
        date_col = prep$date_col,
        count_col = prep$count_col,
        target_range = prep$target_range,
        baseline_range = prep$baseline_range,
        transform = prep$transform,
        detrend = prep$detrend,
        use_rates = prep$use_rates,
        aggregation = prep$aggregation
      ),
      error = function(e) NULL
    )
    if (is.null(run)) return(NULL)

    outbreak_dates <- injected_data[[prep$date_col]][injection$outbreak_idx]
    grace_window <- c(
      min(outbreak_dates) - 7L * grace_period,
      max(outbreak_dates) + 7L * grace_period
    )

    run$predictions |>
      dplyr::group_by(.data$method) |>
      dplyr::summarise(
        caught = any(.data$alarm[
          .data$date >= grace_window[1] & .data$date <= grace_window[2]
        ], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        magnitude = magnitude,
        duration = duration,
        iter = iter
      )
  })

  if (is.null(results) || nrow(results) == 0L) return(NULL)

  # per (method, magnitude, duration): empirical sensitivity
  per_cell <- results |>
    dplyr::group_by(.data$method, .data$magnitude, .data$duration) |>
    dplyr::summarise(
      sensitivity = mean(.data$caught, na.rm = TRUE),
      .groups = "drop"
    )

  # smallest reliably detected (sens >= 0.8) cell per method
  smallest <- per_cell |>
    dplyr::filter(.data$sensitivity >= 0.8) |>
    dplyr::group_by(.data$method) |>
    dplyr::arrange(.data$magnitude, .data$duration) |>
    dplyr::slice(1L) |>
    dplyr::ungroup() |>
    dplyr::rename(
      smallest_caught_magnitude = "magnitude",
      shortest_caught_duration = "duration"
    )

  attr(per_cell, "smallest") <- smallest
  per_cell
}

`%||%` <- function(x, y) if (is.null(x)) y else x


# ---- unlabelled qualitative diagnostics -----------------------------------

#' Compute pairwise Cohen's kappa, alarm density, and persistence for
#' unlabelled mode.
#'
#' @keywords internal
#' @noRd
.unlabelled_diagnostics <- function(predictions) {
  per_method <- predictions |>
    dplyr::group_by(.data$method) |>
    dplyr::summarise(
      alarm_density = mean(.data$alarm, na.rm = TRUE),
      persistence = {
        runs <- .runs(.data$alarm)
        if (nrow(runs) == 0L) NA_real_
        else mean(runs$end - runs$start + 1L)
      },
      .groups = "drop"
    )

  methods <- unique(predictions$method)
  pairs <- utils::combn(methods, 2L, simplify = FALSE)
  pairwise <- purrr::map_dfr(pairs, function(pair) {
    a <- predictions |>
      dplyr::filter(.data$method == pair[1]) |>
      dplyr::arrange(.data$date) |>
      dplyr::pull(.data$alarm)
    b <- predictions |>
      dplyr::filter(.data$method == pair[2]) |>
      dplyr::arrange(.data$date) |>
      dplyr::pull(.data$alarm)
    if (length(a) != length(b)) return(NULL)
    a[is.na(a)] <- FALSE
    b[is.na(b)] <- FALSE
    po <- mean(a == b)
    pe <- mean(a) * mean(b) + (1 - mean(a)) * (1 - mean(b))
    kappa <- if (pe < 1) (po - pe) / (1 - pe) else NA_real_
    tibble::tibble(method_a = pair[1], method_b = pair[2], kappa = kappa)
  })

  list(per_method = per_method, pairwise_kappa = pairwise)
}


# ---- epi_evaluate() public verb -------------------------------------------

#' Evaluate an outbreak detection run
#'
#' Compute per-method metrics, leave-one-method-out (LOO) deltas,
#' operating-point tables, k-sweep, cost-aware metrics, and false-positive
#' shoulder timing for an [epi_detection_run] returned by [epi_detect()].
#'
#' Three operating modes are selected automatically by what the caller
#' supplies:
#' \itemize{
#'   \item **labelled** — pass `labels` (a column name in the original
#'     data, or a logical vector aligned with `target_range`).
#'   \item **simulation-injected** — pass `simulation` (a list with
#'     `n_iterations`, `magnitudes`, `durations`, `shape`,
#'     `grace_period`, optional `seed`). Synthetic outbreaks are added
#'     to a quiet baseline and detection is re-run.
#'   \item **unlabelled qualitative** — pass neither. Limited diagnostics
#'     (pairwise method agreement, alarm density, persistence) only.
#' }
#'
#' @param detection_run An object of class `epi_detection_run`.
#' @param labels Optional column name or logical vector.
#' @param simulation Optional simulation specification list. See details.
#' @param metrics Either `"all"` or a character vector of metric names.
#' @param cost Optional list with `cost_per_fp` and `cost_per_fn` numeric
#'   scalars; enables expected-cost computation.
#' @param loo_method If `TRUE`, run the leave-one-method-out engine.
#'   Requires ground truth (`labels` or `simulation`).
#' @param ensemble_strategy Strategy used internally by LOO and k-sweep:
#'   `"majority"`, `"weighted"`, or `"probability"`.
#' @param ensemble_k For `strategy = "majority"`; defaults to
#'   `ceiling(n_methods / 2)`.
#' @param k_sweep If `TRUE`, sweep `k` across `1..n_methods` and report
#'   precision-recall at each k.
#' @param operating_points If `TRUE` and `simulation` is supplied, build
#'   the operating-point table.
#' @param f_beta Numeric scalar for the F-beta metric.
#'
#' @return An object of class `epi_evaluation` carrying `$per_method`,
#'   `$loo_method`, `$operating_points`, `$ensemble_sweep`, `$cost`,
#'   `$fp_shoulder`, `$unlabelled`, `$mode`, `$metadata`.
#'
#' @seealso [epi_detect()], [epi_ensemble()], [epi_recommend()].
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' weekly <- tibble::tibble(
#'   date = seq.Date(as.Date("2018-01-07"), by = "week",
#'                   length.out = 260L),
#'   cases = rpois(260L, 8),
#'   outbreak_label = FALSE
#' )
#' weekly$outbreak_label[200:210] <- TRUE
#' weekly$cases[200:210] <- weekly$cases[200:210] + 20
#'
#' run <- epi_detect(
#'   weekly,
#'   methods = c("threshold", "ears_c1"),
#'   target_range = 209:260,
#'   baseline_range = 1:208
#' )
#' eval_res <- epi_evaluate(
#'   run,
#'   labels = "outbreak_label",
#'   loo_method = TRUE,
#'   k_sweep = TRUE
#' )
#' eval_res$per_method
#' }
#'
#' @export
epi_evaluate <- function(detection_run,
                          labels = NULL,
                          simulation = NULL,
                          metrics = "all",
                          cost = NULL,
                          loo_method = TRUE,
                          ensemble_strategy = c("majority", "weighted",
                                                 "probability"),
                          ensemble_k = NULL,
                          k_sweep = TRUE,
                          operating_points = TRUE,
                          f_beta = 1) {
  if (!inherits(detection_run, "epi_detection_run")) {
    cli::cli_abort(
      "{.arg detection_run} must be of class {.cls epi_detection_run}."
    )
  }
  ensemble_strategy <- match.arg(ensemble_strategy)

  truth_info <- .resolve_truth(detection_run, labels, simulation)
  mode <- truth_info$mode
  preds <- detection_run$predictions

  # per-method metrics ----------------------------------------------------
  per_method <- NULL
  fp_shoulder <- NULL
  cost_tbl <- NULL

  if (mode == "labelled") {
    truth_table <- truth_info$truth_table
    preds_truth <- .attach_truth(preds, truth_table)
    p_outbreak <- mean(truth_table$.truth, na.rm = TRUE)

    per_method <- preds_truth |>
      dplyr::group_by(.data$method) |>
      dplyr::group_modify(~ {
        std <- .compute_standard_metrics(.x$alarm, .x$.truth, f_beta = f_beta)
        surv <- .compute_surveillance_metrics(.x$alarm, .x$.truth)
        auc <- .compute_auc(.x$score, .x$.truth)
        auprc <- .compute_auprc(.x$score, .x$.truth)
        tibble::tibble(
          sensitivity = std$sensitivity,
          specificity = std$specificity,
          ppv = std$ppv, npv = std$npv,
          accuracy = std$accuracy,
          f1 = std$f1, f_beta = std$f_beta,
          auc = auc, auprc = auprc,
          time_to_detect = surv$time_to_detect,
          false_alarm_rate = surv$false_alarm_rate,
          alarm_persistence = surv$alarm_persistence,
          alarm_shoulder_fraction = surv$alarm_shoulder_fraction
        )
      }) |>
      dplyr::ungroup()

    if (!is.null(cost)) {
      cp_fp <- cost$cost_per_fp %||% 1
      cp_fn <- cost$cost_per_fn %||% 1
      cost_tbl <- per_method |>
        dplyr::transmute(
          method = .data$method,
          expected_cost = purrr::pmap_dbl(
            list(.data$sensitivity, .data$specificity),
            function(s, sp) .compute_cost(s, sp, p_outbreak, cp_fp, cp_fn)
          )
        )
    }

    fp_shoulder <- per_method |>
      dplyr::select("method", "alarm_shoulder_fraction")
  }

  # LOO + k-sweep --------------------------------------------------------
  loo_tbl <- NULL
  k_sweep_tbl <- NULL
  if (mode == "labelled" && isTRUE(loo_method)) {
    loo_tbl <- .run_loo(
      predictions = preds,
      truth_table = truth_info$truth_table,
      strategy = ensemble_strategy,
      k = ensemble_k,
      f_beta = f_beta
    )
  }
  if (mode == "labelled" && isTRUE(k_sweep)) {
    k_sweep_tbl <- .run_k_sweep(
      predictions = preds,
      truth_table = truth_info$truth_table,
      f_beta = f_beta
    )
  }

  # operating points -----------------------------------------------------
  op_tbl <- NULL
  if (!is.null(simulation) && isTRUE(operating_points)) {
    op_tbl <- .build_operating_points(
      detection_run = detection_run,
      simulation = simulation,
      grace_period = simulation$grace_period %||% 2L
    )
  }

  # unlabelled diagnostics ----------------------------------------------
  unlabelled_diag <- NULL
  if (mode == "unlabelled") {
    unlabelled_diag <- .unlabelled_diagnostics(preds)
  }

  out <- list(
    per_method = per_method,
    loo_method = loo_tbl,
    operating_points = op_tbl,
    ensemble_sweep = k_sweep_tbl,
    cost = cost_tbl,
    fp_shoulder = fp_shoulder,
    unlabelled = unlabelled_diag,
    mode = mode,
    metadata = list(
      cost = cost,
      f_beta = f_beta,
      ensemble_strategy = ensemble_strategy,
      ensemble_k = ensemble_k,
      detection_run = detection_run
    )
  )
  class(out) <- c("epi_evaluation", "list")
  out
}


# ---- epi_evaluation S3 methods --------------------------------------------

#' @export
print.epi_evaluation <- function(x, ...) {
  cli::cli_h2("epi_evaluation ({x$mode} mode)")

  if (!is.null(x$per_method)) {
    cli::cli_h3("per-method metrics")
    print(x$per_method)
  }
  if (!is.null(x$loo_method)) {
    cli::cli_h3("leave-one-method-out")
    print(x$loo_method)
  }
  if (!is.null(x$operating_points)) {
    cli::cli_h3("operating points (smallest detected)")
    print(attr(x$operating_points, "smallest"))
  }
  if (!is.null(x$ensemble_sweep)) {
    cli::cli_h3("k-sweep")
    print(x$ensemble_sweep)
  }
  if (!is.null(x$cost)) {
    cli::cli_h3("expected cost")
    print(x$cost)
  }
  if (!is.null(x$unlabelled)) {
    cli::cli_h3("unlabelled diagnostics")
    print(x$unlabelled$per_method)
  }
  invisible(x)
}

#' @export
summary.epi_evaluation <- function(object, ...) {
  print(object)
  invisible(object)
}

#' @export
as_tibble.epi_evaluation <- function(x, ...) {
  if (is.null(x$per_method)) {
    cli::cli_warn(
      "No per-method table available (likely unlabelled mode)."
    )
    return(tibble::tibble())
  }
  x$per_method
}

#' Plot an `epi_evaluation` object
#'
#' @param x An `epi_evaluation` object.
#' @param which Character scalar selecting one of nine diagnostic panels:
#'   `"roc"`, `"pr"`, `"calibration"`, `"agreement"`, `"time_to_detect"`,
#'   `"loo_delta"` (default), `"fp_shoulder"`, `"operating_grid"`,
#'   `"cost_curve"`.
#' @param ... Reserved.
#'
#' @return A ggplot object (invisibly).
#'
#' @export
plot.epi_evaluation <- function(x, which = "loo_delta", ...) {
  p <- autoplot.epi_evaluation(x, which = which, ...)
  print(p)
  invisible(p)
}

#' @export
autoplot.epi_evaluation <- function(object,
                                     which = c(
                                       "loo_delta", "roc", "pr",
                                       "calibration", "agreement",
                                       "time_to_detect", "fp_shoulder",
                                       "operating_grid", "cost_curve"
                                     ),
                                     ...) {
  which <- match.arg(which)

  switch(which,
    "loo_delta" = .plot_loo_delta(object),
    "roc" = .plot_roc(object),
    "pr" = .plot_pr(object),
    "calibration" = .plot_calibration(object),
    "agreement" = .plot_agreement(object),
    "time_to_detect" = .plot_ttd(object),
    "fp_shoulder" = .plot_fp_shoulder(object),
    "operating_grid" = .plot_operating_grid(object),
    "cost_curve" = .plot_cost(object)
  )
}


# ---- 9-plot diagnostic suite ----------------------------------------------

#' @keywords internal
#' @noRd
.plot_loo_delta <- function(x) {
  if (is.null(x$loo_method)) {
    cli::cli_abort("No leave-one-method-out table available.")
  }
  x$loo_method |>
    dplyr::mutate(method = stats::reorder(.data$method, .data$delta_auc)) |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$method, y = .data$delta_auc,
      fill = .data$delta_auc > 0
    )) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
      guide = "none"
    ) +
    ggplot2::labs(
      title = "Leave-one-method-out: delta AUC",
      subtitle = "positive: method earns its keep; negative: ensemble better without it",
      x = NULL, y = "delta AUC"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' @keywords internal
#' @noRd
.plot_roc <- function(x) {
  preds <- x$metadata$detection_run$predictions
  truth_info <- .resolve_truth(
    x$metadata$detection_run,
    labels = x$metadata$detection_run$preprocessing$label_col,
    simulation = NULL
  )
  if (truth_info$mode != "labelled") {
    cli::cli_abort("ROC requires labelled mode.")
  }
  preds_truth <- .attach_truth(preds, truth_info$truth_table)
  roc_df <- preds_truth |>
    dplyr::group_by(.data$method) |>
    dplyr::group_modify(~ {
      ord <- order(.x$score, decreasing = TRUE)
      truth <- .x$.truth[ord]
      tp <- cumsum(truth)
      fp <- cumsum(!truth)
      tibble::tibble(
        tpr = tp / max(1L, sum(truth)),
        fpr = fp / max(1L, sum(!truth))
      )
    }) |>
    dplyr::ungroup()

  ggplot2::ggplot(roc_df, ggplot2::aes(
    x = .data$fpr, y = .data$tpr, colour = .data$method
  )) +
    ggplot2::geom_line() +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                          linetype = "dashed", colour = "grey60") +
    ggplot2::labs(
      title = "ROC curves",
      x = "false-positive rate", y = "true-positive rate",
      colour = "method"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' @keywords internal
#' @noRd
.plot_pr <- function(x) {
  preds <- x$metadata$detection_run$predictions
  truth_info <- .resolve_truth(
    x$metadata$detection_run,
    labels = x$metadata$detection_run$preprocessing$label_col,
    simulation = NULL
  )
  if (truth_info$mode != "labelled") {
    cli::cli_abort("PR requires labelled mode.")
  }
  preds_truth <- .attach_truth(preds, truth_info$truth_table)
  pr_df <- preds_truth |>
    dplyr::group_by(.data$method) |>
    dplyr::group_modify(~ {
      ord <- order(.x$score, decreasing = TRUE)
      truth <- .x$.truth[ord]
      tp <- cumsum(truth)
      fp <- cumsum(!truth)
      tibble::tibble(
        precision = tp / pmax(1L, tp + fp),
        recall = tp / max(1L, sum(truth))
      )
    }) |>
    dplyr::ungroup()

  ggplot2::ggplot(pr_df, ggplot2::aes(
    x = .data$recall, y = .data$precision, colour = .data$method
  )) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "Precision-recall curves",
      x = "recall", y = "precision",
      colour = "method"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' @keywords internal
#' @noRd
.plot_calibration <- function(x) {
  preds <- x$metadata$detection_run$predictions
  truth_info <- .resolve_truth(
    x$metadata$detection_run,
    labels = x$metadata$detection_run$preprocessing$label_col,
    simulation = NULL
  )
  if (truth_info$mode != "labelled") {
    cli::cli_abort("Calibration requires labelled mode.")
  }
  preds_truth <- .attach_truth(preds, truth_info$truth_table)
  cal_df <- preds_truth |>
    dplyr::group_by(.data$method) |>
    dplyr::group_modify(~ {
      .x |>
        dplyr::mutate(bin = cut(
          .data$score,
          breaks = stats::quantile(.data$score,
                                    probs = seq(0, 1, 0.1),
                                    na.rm = TRUE),
          include.lowest = TRUE
        )) |>
        dplyr::group_by(.data$bin) |>
        dplyr::summarise(
          mean_score = mean(.data$score, na.rm = TRUE),
          empirical = mean(.data$.truth, na.rm = TRUE),
          .groups = "drop"
        )
    }) |>
    dplyr::ungroup()

  ggplot2::ggplot(cal_df, ggplot2::aes(
    x = .data$mean_score, y = .data$empirical, colour = .data$method
  )) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                          linetype = "dashed", colour = "grey60") +
    ggplot2::labs(
      title = "Reliability diagram",
      x = "mean predicted score (decile)", y = "empirical outbreak rate",
      colour = "method"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' @keywords internal
#' @noRd
.plot_agreement <- function(x) {
  preds <- x$metadata$detection_run$predictions
  diag <- .unlabelled_diagnostics(preds)
  if (nrow(diag$pairwise_kappa) == 0L) {
    cli::cli_abort("Need at least 2 methods to plot agreement matrix.")
  }

  ggplot2::ggplot(diag$pairwise_kappa, ggplot2::aes(
    x = .data$method_a, y = .data$method_b, fill = .data$kappa
  )) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$kappa)),
                        colour = "white") +
    ggplot2::scale_fill_gradient2(
      low = "firebrick", mid = "white", high = "steelblue",
      midpoint = 0, limits = c(-1, 1)
    ) +
    ggplot2::labs(
      title = "Pairwise method agreement (Cohen's kappa)",
      x = NULL, y = NULL, fill = "kappa"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust = 1))
}

#' @keywords internal
#' @noRd
.plot_ttd <- function(x) {
  if (is.null(x$per_method) || !"time_to_detect" %in% names(x$per_method)) {
    cli::cli_abort("Time-to-detect requires labelled mode.")
  }
  x$per_method |>
    dplyr::filter(!is.na(.data$time_to_detect)) |>
    ggplot2::ggplot(ggplot2::aes(
      x = stats::reorder(.data$method, .data$time_to_detect),
      y = .data$time_to_detect
    )) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Time-to-detect (weeks)",
      subtitle = "mean weeks between outbreak start and first true alarm",
      x = NULL, y = "weeks"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' @keywords internal
#' @noRd
.plot_fp_shoulder <- function(x) {
  if (is.null(x$fp_shoulder)) {
    cli::cli_abort("FP shoulder analysis requires labelled mode.")
  }
  x$fp_shoulder |>
    dplyr::filter(!is.na(.data$alarm_shoulder_fraction)) |>
    ggplot2::ggplot(ggplot2::aes(
      x = stats::reorder(.data$method, .data$alarm_shoulder_fraction),
      y = .data$alarm_shoulder_fraction
    )) +
    ggplot2::geom_col(fill = "darkorange") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "False-positive shoulder fraction",
      subtitle = "share of FP alarms within +/- 4 weeks of an outbreak",
      x = NULL, y = "fraction"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' @keywords internal
#' @noRd
.plot_operating_grid <- function(x) {
  if (is.null(x$operating_points)) {
    cli::cli_abort("Operating-point grid requires simulation mode.")
  }
  ggplot2::ggplot(x$operating_points, ggplot2::aes(
    x = factor(.data$duration), y = factor(.data$magnitude),
    fill = .data$sensitivity
  )) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$sensitivity)),
                        colour = "white", size = 3) +
    ggplot2::facet_wrap(~ method) +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1)) +
    ggplot2::labs(
      title = "Operating-point sensitivity grid",
      x = "duration (weeks)", y = "magnitude (x baseline SD)",
      fill = "sensitivity"
    ) +
    ggplot2::theme_minimal(base_size = 10)
}

#' @keywords internal
#' @noRd
.plot_cost <- function(x) {
  if (is.null(x$cost)) {
    cli::cli_abort("Cost curve requires a cost matrix supplied to epi_evaluate().")
  }
  x$cost |>
    dplyr::filter(!is.na(.data$expected_cost)) |>
    ggplot2::ggplot(ggplot2::aes(
      x = stats::reorder(.data$method, .data$expected_cost),
      y = .data$expected_cost
    )) +
    ggplot2::geom_col(fill = "purple") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Expected cost per method",
      subtitle = "lower is better",
      x = NULL, y = "expected cost"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}
