# ---------------------------------------------------------------------------
# epi_detect.R
#
# Detector base class, fit / predict generics, registry environment, contract
# validator, per-detector failure-isolation shell, and the user-facing
# extensibility helpers `register_detector()` and `available_detectors()`.
#
# Built-in detector subclasses live below this preamble (added in subsequent
# build steps). `epi_detect()` and the `epi_detection_run` S3 class are added
# in steps 3 and 4.
# ---------------------------------------------------------------------------


# ---- detector registry environment ----------------------------------------

# internal package-level registry; populated by .register_builtin_detectors()
# inside .onLoad() and extended at runtime by register_detector()
.epi_registry <- new.env(parent = emptyenv())


# ---- base class constructor -----------------------------------------------

#' Construct an `epi_detector` object
#'
#' Internal constructor used by built-in detector factories and by
#' [register_detector()] to assemble S3 detector objects. The resulting
#' object carries the detector's identity (`method`), its tuning parameters
#' (`params`), and a flag (`fitted`) that gets flipped by [fit_detector()].
#'
#' @param method Character scalar. Detector identifier (e.g. `"farrington"`).
#'   Becomes part of the object's S3 class as `epi_detector_<method>`.
#' @param params Named list of tuning parameters consumed by
#'   [predict_detector()] for this detector.
#'
#' @return An object inheriting from `epi_detector` with subclass
#'   `epi_detector_<method>`.
#'
#' @keywords internal
#' @noRd
new_epi_detector <- function(method, params = list()) {
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    cli::cli_abort(
      "{.arg method} must be a single non-NA character string."
    )
  }
  if (!is.list(params)) {
    cli::cli_abort("{.arg params} must be a named list.")
  }

  structure(
    list(method = method, params = params, fitted = FALSE),
    class = c(paste0("epi_detector_", method), "epi_detector")
  )
}


# ---- S3 generics ----------------------------------------------------------

#' Fit a detector on baseline data
#'
#' Generic for fitting a detector against a baseline window. Detectors that
#' fit lazily (e.g. Farrington, where fitting happens inside `predict`)
#' should store the baseline frame and flip `detector$fitted <- TRUE`.
#'
#' @param detector An `epi_detector` object returned by a detector
#'   constructor or by [register_detector()].
#' @param baseline_data A tibble with at minimum `date` (Date) and `cases`
#'   (numeric) columns covering the historical / baseline window.
#' @param ... Reserved for method-specific arguments.
#'
#' @return The fitted `epi_detector` object with any cached state attached.
#'
#' @export
fit_detector <- function(detector, baseline_data, ...) {
  UseMethod("fit_detector")
}

#' Generate alarms for target data
#'
#' Generic for producing the per-week alarm series from a fitted detector.
#'
#' **Invariant contract** — every `predict_detector` method must return a
#' tibble with **exactly** these four columns, in order:
#'
#' | Column | Type | Meaning |
#' | --- | --- | --- |
#' | `date` | Date | matches `target_data$date` |
#' | `alarm` | logical | binary alarm decision |
#' | `score` | numeric | continuous signal (e.g. observed/expected ratio) |
#' | `upper_threshold` | numeric | cutoff value used to produce `alarm` |
#'
#' The framework's failure-isolation shell adds `method`, `group_id`,
#' `failed`, and `error_message` columns at the [epi_detect()] level on top
#' of this four-column contract, so third-party detector authors registered
#' via [register_detector()] only need to honour the four columns above.
#'
#' @param detector A fitted `epi_detector` object.
#' @param target_data A tibble with `date` and `cases` columns covering the
#'   detection window.
#' @param ... Reserved for method-specific arguments.
#'
#' @return A tibble with the four-column contract described above.
#'
#' @export
predict_detector <- function(detector, target_data, ...) {
  UseMethod("predict_detector")
}


# ---- contract validator ---------------------------------------------------

#' Validate a detector's `predict_detector` output against the four-column
#' contract
#'
#' Used internally by [.run_one_detector_safely()] to guarantee that every
#' detector — built-in or user-registered — returns a tibble that downstream
#' verbs can rely on. Mismatches abort with a clear message naming the
#' offending detector.
#'
#' @param output The tibble returned by a `predict_detector` method.
#' @param expected_dates The Date vector the output must align with (taken
#'   from `target_data$date`).
#' @param method Character scalar; the detector method id, used in error
#'   messages.
#'
#' @return The input `output` (unchanged) on success; aborts on contract
#'   violation.
#'
#' @keywords internal
#' @noRd
.validate_predict_output <- function(output, expected_dates, method) {
  required_cols <- c("date", "alarm", "score", "upper_threshold")

  if (!is.data.frame(output)) {
    cli::cli_abort(c(
      "Detector {.val {method}} returned a non-data-frame object.",
      "i" = "Expected a tibble with columns {.val {required_cols}}.",
      "x" = "Got an object of class {.cls {class(output)[1]}}."
    ))
  }

  missing_cols <- setdiff(required_cols, names(output))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "Detector {.val {method}} violated the predict contract.",
      "i" = "Required columns: {.val {required_cols}}.",
      "x" = "Missing columns: {.val {missing_cols}}."
    ))
  }

  if (nrow(output) != length(expected_dates)) {
    cli::cli_abort(c(
      "Detector {.val {method}} returned the wrong number of rows.",
      "i" = "Expected {length(expected_dates)} row{?s} (one per target date).",
      "x" = "Got {nrow(output)} row{?s}."
    ))
  }

  if (!inherits(output$date, "Date")) {
    cli::cli_abort(
      "Detector {.val {method}}: column {.field date} must be {.cls Date}."
    )
  }
  if (!is.logical(output$alarm)) {
    cli::cli_abort(
      "Detector {.val {method}}: column {.field alarm} must be {.cls logical}."
    )
  }
  if (!is.numeric(output$score)) {
    cli::cli_abort(
      "Detector {.val {method}}: column {.field score} must be {.cls numeric}."
    )
  }
  if (!is.numeric(output$upper_threshold)) {
    cli::cli_abort(
      "Detector {.val {method}}: column {.field upper_threshold} must be ",
      "{.cls numeric}."
    )
  }

  # return only the four contract columns, in canonical order
  output[, required_cols, drop = FALSE]
}


# ---- per-detector failure-isolation shell ---------------------------------

#' Run a single detector with `tryCatch`-based failure isolation
#'
#' Wraps `fit_detector()` and `predict_detector()` in a `tryCatch` so that a
#' single failing method does not crash the whole `epi_detect()` run. On
#' success, returns the contract-validated four-column tibble augmented with
#' `failed = FALSE` and `error_message = NA_character_`. On failure, returns
#' a tibble of the same shape with `alarm = NA`, `score = NA_real_`,
#' `upper_threshold = NA_real_`, `failed = TRUE`, and the captured
#' `error_message`, while emitting a [cli::cli_warn()] naming the method and
#' group.
#'
#' @param detector An `epi_detector` object.
#' @param baseline_data Tibble for the baseline window.
#' @param target_data Tibble for the target detection window.
#' @param group_id Character scalar group identifier (used in warnings).
#'
#' @return Tibble with six columns: `date`, `alarm`, `score`,
#'   `upper_threshold`, `failed`, `error_message`.
#'
#' @keywords internal
#' @noRd
.run_one_detector_safely <- function(detector,
                                     baseline_data,
                                     target_data,
                                     group_id = NA_character_) {
  method <- detector$method
  expected_dates <- target_data$date

  result <- tryCatch(
    {
      fitted <- fit_detector(detector, baseline_data)
      raw <- predict_detector(fitted, target_data)
      validated <- .validate_predict_output(raw, expected_dates, method)
      validated$failed <- FALSE
      validated$error_message <- NA_character_
      validated
    },
    error = function(e) {
      group_label <- if (is.na(group_id)) "(ungrouped)" else group_id
      cli::cli_warn(c(
        "Detector {.val {method}} failed for group {.val {group_label}}.",
        "i" = "Row will be marked as failed; downstream verbs will skip it.",
        "x" = "Error: {conditionMessage(e)}"
      ))
      tibble::tibble(
        date = expected_dates,
        alarm = NA,
        score = NA_real_,
        upper_threshold = NA_real_,
        failed = TRUE,
        error_message = conditionMessage(e)
      )
    }
  )

  result
}


# ---- registry helpers -----------------------------------------------------

#' Register a custom outbreak detector
#'
#' Adds a user-defined detector to the package-internal registry so it can
#' be referenced by name inside [epi_detect()]. The framework supplies the
#' failure-isolation shell, contract validation, and downstream integration;
#' authors only need to honour the four-column predict contract.
#'
#' @section Detector contract:
#' Every registered detector consists of three callables:
#'
#' - `constructor(...)` — returns an `epi_detector` object (typically via
#'   [new_epi_detector()]).
#' - `fit_method(detector, baseline_data, ...)` — an S3 method on
#'   `epi_detector_<name>` that returns the fitted detector.
#' - `predict_method(detector, target_data, ...)` — an S3 method on
#'   `epi_detector_<name>` that returns a tibble with **exactly** the four
#'   columns `date`, `alarm`, `score`, `upper_threshold`. Anything else
#'   (e.g. `failed`, `error_message`, `method`, `group_id`) is added by the
#'   framework — do not return those columns yourself.
#'
#' @param name Character scalar method id used in
#'   `epi_detect(methods = ...)`. Must be unique within the active R
#'   session.
#' @param constructor Function that builds an `epi_detector` object.
#' @param fit_method Function implementing `fit_detector` for this
#'   detector's subclass.
#' @param predict_method Function implementing `predict_detector` for this
#'   detector's subclass; must return the four-column contract.
#'
#' @return Invisibly returns `name`.
#'
#' @seealso [available_detectors()], [new_epi_detector()],
#'   [fit_detector()], [predict_detector()].
#'
#' @examples
#' \dontrun{
#' my_constructor <- function(k = 2) {
#'   new_epi_detector(method = "my_z", params = list(k = k))
#' }
#'
#' my_fit <- function(detector, baseline_data, ...) {
#'   detector$baseline_mean <- mean(baseline_data$cases, na.rm = TRUE)
#'   detector$baseline_sd <- stats::sd(baseline_data$cases, na.rm = TRUE)
#'   detector$fitted <- TRUE
#'   detector
#' }
#'
#' my_predict <- function(detector, target_data, ...) {
#'   thresh <- detector$baseline_mean +
#'     detector$params$k * detector$baseline_sd
#'   tibble::tibble(
#'     date = target_data$date,
#'     alarm = target_data$cases > thresh,
#'     score = (target_data$cases - detector$baseline_mean) /
#'       detector$baseline_sd,
#'     upper_threshold = thresh
#'   )
#' }
#'
#' register_detector("my_z", my_constructor, my_fit, my_predict)
#' "my_z" %in% available_detectors()
#' }
#'
#' @export
register_detector <- function(name,
                              constructor,
                              fit_method,
                              predict_method) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
        !nzchar(name)) {
    cli::cli_abort(
      "{.arg name} must be a single non-empty character string."
    )
  }
  if (!is.function(constructor)) {
    cli::cli_abort("{.arg constructor} must be a function.")
  }
  if (!is.function(fit_method)) {
    cli::cli_abort("{.arg fit_method} must be a function.")
  }
  if (!is.function(predict_method)) {
    cli::cli_abort("{.arg predict_method} must be a function.")
  }

  subclass <- paste0("epi_detector_", name)

  # install S3 methods into the sntmethods namespace under epi_detector_<name>
  # so UseMethod() dispatches correctly inside fit_detector / predict_detector
  registerS3method(
    "fit_detector", subclass, fit_method,
    envir = asNamespace("sntmethods")
  )
  registerS3method(
    "predict_detector", subclass, predict_method,
    envir = asNamespace("sntmethods")
  )

  # cache the constructor so resolve-by-name can build the detector object
  assign(name, constructor, envir = .epi_registry)

  invisible(name)
}


#' List all registered outbreak detectors
#'
#' Returns the names of detectors currently in the package registry — both
#' the v1.1 built-ins and any added via [register_detector()] during the
#' active R session.
#'
#' @return Character vector of detector ids, sorted alphabetically.
#'
#' @seealso [register_detector()], [epi_detect()].
#'
#' @examples
#' available_detectors()
#'
#' @export
available_detectors <- function() {
  sort(ls(envir = .epi_registry))
}


# ---- name resolution ------------------------------------------------------

#' Resolve a method id (and optional param overrides) to a detector object
#'
#' Looks up `method` in [.epi_registry] and calls its constructor, applying
#' any user-supplied overrides from `method_params[[method]]`.
#'
#' @param method Character scalar method id.
#' @param method_params Named list keyed by method id; each entry is a
#'   named list of parameter overrides forwarded to the constructor via
#'   [do.call()].
#'
#' @return An `epi_detector` object.
#'
#' @keywords internal
#' @noRd
.resolve_detector <- function(method, method_params = list()) {
  if (!exists(method, envir = .epi_registry, inherits = FALSE)) {
    available <- available_detectors()
    cli::cli_abort(c(
      "Unknown detector {.val {method}}.",
      "i" = "Registered detectors: {.val {available}}.",
      "i" = "Custom detectors can be added with {.fn register_detector}."
    ))
  }

  constructor <- get(method, envir = .epi_registry)
  overrides <- method_params[[method]]
  if (is.null(overrides)) overrides <- list()
  if (!is.list(overrides)) {
    cli::cli_abort(c(
      "{.code method_params[[{.val {method}}]]} must be a named list.",
      "x" = "Got an object of class {.cls {class(overrides)[1]}}."
    ))
  }

  do.call(constructor, overrides)
}


# ---- built-in detectors (step 2: threshold, ears_c1) ----------------------

# ---------------------------------------------------------------------------
# built-in detector: threshold
#
# Hand-rolled per-iso-week baseline with three aggregator flavours:
#   - "mean":   mean(baseline) + k * sd(baseline)
#   - "median": median(baseline) + k * mad(baseline)
#   - "q3":     3rd quartile of baseline (k is ignored)
# Per-iso-week aggregation captures seasonality without fitting a model.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_threshold <- function(k = 2,
                                   aggregator = c("mean", "median", "q3")) {
  aggregator <- match.arg(aggregator)
  new_epi_detector(
    method = "threshold",
    params = list(k = k, aggregator = aggregator)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_threshold <- function(detector,
                                                 baseline_data,
                                                 ...) {
  baseline_data <- baseline_data |>
    dplyr::mutate(
      iso_week = as.integer(format(.data$date, "%V"))
    )

  aggregator <- detector$params$aggregator
  k <- detector$params$k

  centre_fun <- switch(aggregator,
    "mean"   = function(x) mean(x, na.rm = TRUE),
    "median" = function(x) stats::median(x, na.rm = TRUE),
    "q3"     = function(x) stats::quantile(
      x, probs = 0.75, na.rm = TRUE, names = FALSE
    )
  )
  spread_fun <- switch(aggregator,
    "mean"   = function(x) stats::sd(x, na.rm = TRUE),
    "median" = function(x) stats::mad(x, na.rm = TRUE),
    "q3"     = function(x) 0
  )

  summaries <- baseline_data |>
    dplyr::group_by(.data$iso_week) |>
    dplyr::summarise(
      centre = centre_fun(.data$cases),
      spread = spread_fun(.data$cases),
      .groups = "drop"
    )

  summaries$upper <- if (aggregator == "q3") {
    summaries$centre
  } else {
    summaries$centre + k * summaries$spread
  }

  detector$lookup <- summaries
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_threshold <- function(detector,
                                                     target_data,
                                                     ...) {
  target_data <- target_data |>
    dplyr::mutate(
      iso_week = as.integer(format(.data$date, "%V"))
    ) |>
    dplyr::left_join(detector$lookup, by = "iso_week")

  # if a week never appeared in baseline, fall back to overall baseline upper
  fallback_upper <- mean(detector$lookup$upper, na.rm = TRUE)
  upper_thresh <- dplyr::if_else(
    is.na(target_data$upper), fallback_upper, target_data$upper
  )

  # standardised z-style score; for q3 (spread = 0) report raw exceedance
  score <- dplyr::if_else(
    is.na(target_data$spread) | target_data$spread == 0,
    target_data$cases - upper_thresh,
    (target_data$cases - target_data$centre) / target_data$spread
  )

  tibble::tibble(
    date = target_data$date,
    alarm = target_data$cases > upper_thresh,
    score = as.numeric(score),
    upper_threshold = as.numeric(upper_thresh)
  )
}


# ---------------------------------------------------------------------------
# built-in detector: ears_c1
#
# Wraps surveillance::earsC() with method = "C1". The C1 statistic compares
# the current week against the mean and SD of a short rolling baseline
# (default 7 weeks), making no seasonal adjustment.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_ears_c1 <- function(baseline = 7L,
                                 alpha = 0.001) {
  new_epi_detector(
    method = "ears_c1",
    params = list(method = "C1", baseline = baseline, alpha = alpha)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_ears_c1 <- function(detector,
                                               baseline_data,
                                               ...) {
  # EARS fits during predict; cache the baseline for combination with target
  detector$baseline <- baseline_data
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_ears_c1 <- function(detector,
                                                   target_data,
                                                   ...) {
  .check_pkg("surveillance", reason = "for the EARS family of detectors.")

  combined <- dplyr::bind_rows(detector$baseline, target_data) |>
    dplyr::arrange(.data$date)
  target_idx <- which(combined$date %in% target_data$date)

  sts_object <- surveillance::sts(
    observed = combined$cases,
    frequency = 52L
  )

  fit <- surveillance::earsC(
    sts_object,
    control = list(
      range = target_idx,
      method = detector$params$method,
      baseline = detector$params$baseline,
      alpha = detector$params$alpha
    )
  )

  tibble::tibble(
    date = target_data$date,
    alarm = as.logical(surveillance::alarms(fit)),
    score = as.numeric(surveillance::upperbound(fit)),
    upper_threshold = as.numeric(surveillance::upperbound(fit))
  )
}


# ---- built-in detectors (step 17: remaining 12) ---------------------------

# ---------------------------------------------------------------------------
# built-in detector: ears_c2
#
# surveillance::earsC method = "C2": as C1 but with a 2-week guard band
# between the current observation and the rolling baseline window.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_ears_c2 <- function(baseline = 7L, alpha = 0.001) {
  new_epi_detector(
    method = "ears_c2",
    params = list(method = "C2", baseline = baseline, alpha = alpha)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_ears_c2 <- function(detector, baseline_data, ...) {
  detector$baseline <- baseline_data
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_ears_c2 <- function(detector, target_data, ...) {
  .check_pkg("surveillance", reason = "for the EARS family of detectors.")
  combined <- dplyr::bind_rows(detector$baseline, target_data) |>
    dplyr::arrange(.data$date)
  target_idx <- which(combined$date %in% target_data$date)
  sts_object <- surveillance::sts(observed = combined$cases, frequency = 52L)
  fit <- surveillance::earsC(
    sts_object,
    control = list(
      range = target_idx,
      method = "C2",
      baseline = detector$params$baseline,
      alpha = detector$params$alpha
    )
  )
  tibble::tibble(
    date = target_data$date,
    alarm = as.logical(surveillance::alarms(fit)),
    score = as.numeric(surveillance::upperbound(fit)),
    upper_threshold = as.numeric(surveillance::upperbound(fit))
  )
}


# ---------------------------------------------------------------------------
# built-in detector: ears_c3
#
# surveillance::earsC method = "C3": cumulative score over the last three
# weeks; sensitive to short, sustained signals.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_ears_c3 <- function(baseline = 7L, alpha = 0.001) {
  new_epi_detector(
    method = "ears_c3",
    params = list(method = "C3", baseline = baseline, alpha = alpha)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_ears_c3 <- function(detector, baseline_data, ...) {
  detector$baseline <- baseline_data
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_ears_c3 <- function(detector, target_data, ...) {
  .check_pkg("surveillance", reason = "for the EARS family of detectors.")
  combined <- dplyr::bind_rows(detector$baseline, target_data) |>
    dplyr::arrange(.data$date)
  target_idx <- which(combined$date %in% target_data$date)
  sts_object <- surveillance::sts(observed = combined$cases, frequency = 52L)
  fit <- surveillance::earsC(
    sts_object,
    control = list(
      range = target_idx,
      method = "C3",
      baseline = detector$params$baseline,
      alpha = detector$params$alpha
    )
  )
  tibble::tibble(
    date = target_data$date,
    alarm = as.logical(surveillance::alarms(fit)),
    score = as.numeric(surveillance::upperbound(fit)),
    upper_threshold = as.numeric(surveillance::upperbound(fit))
  )
}


# ---------------------------------------------------------------------------
# built-in detector: cusum_classical
#
# surveillance::cusum on a Poisson reference model fitted to the baseline.
# Designed for detecting persistent step shifts of size `k_shift`.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_cusum_classical <- function(k_shift = 1.5, h = 5) {
  new_epi_detector(
    method = "cusum_classical",
    params = list(k_shift = k_shift, h = h)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_cusum_classical <- function(detector,
                                                       baseline_data, ...) {
  detector$lambda0 <- mean(baseline_data$cases, na.rm = TRUE)
  detector$baseline <- baseline_data
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_cusum_classical <- function(detector,
                                                           target_data, ...) {
  .check_pkg("surveillance", reason = "for cusum_classical.")
  combined <- dplyr::bind_rows(detector$baseline, target_data) |>
    dplyr::arrange(.data$date)
  target_idx <- which(combined$date %in% target_data$date)

  sts_object <- surveillance::sts(observed = combined$cases, frequency = 52L)
  fit <- surveillance::cusum(
    sts_object,
    control = list(
      range = target_idx,
      k = detector$params$k_shift,
      h = detector$params$h,
      m = detector$lambda0,
      trans = "anscombe"
    )
  )
  tibble::tibble(
    date = target_data$date,
    alarm = as.logical(surveillance::alarms(fit)),
    score = as.numeric(surveillance::upperbound(fit)),
    upper_threshold = as.numeric(surveillance::upperbound(fit))
  )
}


# ---------------------------------------------------------------------------
# built-in detector: farrington
#
# surveillance::farringtonFlexible: NB-GLM with iterative re-weighting of
# past outbreaks, plus seasonality via lagged windows. The classical method
# for endemic seasonal series with prior outbreak history.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_farrington <- function(b = 3L,
                                     w = 3L,
                                     alpha = 0.001,
                                     reweight = TRUE) {
  new_epi_detector(
    method = "farrington",
    params = list(b = b, w = w, alpha = alpha, reweight = reweight)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_farrington <- function(detector,
                                                  baseline_data, ...) {
  detector$baseline <- baseline_data
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_farrington <- function(detector,
                                                     target_data, ...) {
  .check_pkg("surveillance", reason = "for farringtonFlexible.")
  combined <- dplyr::bind_rows(detector$baseline, target_data) |>
    dplyr::arrange(.data$date)
  target_idx <- which(combined$date %in% target_data$date)

  sts_object <- surveillance::sts(observed = combined$cases, frequency = 52L)
  fit <- surveillance::farringtonFlexible(
    sts_object,
    control = list(
      range = target_idx,
      b = detector$params$b,
      w = detector$params$w,
      alpha = detector$params$alpha,
      reweight = detector$params$reweight
    )
  )
  tibble::tibble(
    date = target_data$date,
    alarm = as.logical(surveillance::alarms(fit)),
    score = as.numeric(surveillance::upperbound(fit)),
    upper_threshold = as.numeric(surveillance::upperbound(fit))
  )
}


# ---------------------------------------------------------------------------
# built-in detector: glrnb
#
# surveillance::glrnb: generalised likelihood-ratio detector with a
# Negative Binomial reference model. Performs well on overdispersed counts.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_glrnb <- function(c_ARL = 5, theta = NULL,
                                ret = c("cases", "value")) {
  ret <- match.arg(ret)
  new_epi_detector(
    method = "glrnb",
    params = list(c_ARL = c_ARL, theta = theta, ret = ret)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_glrnb <- function(detector, baseline_data, ...) {
  detector$baseline <- baseline_data
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_glrnb <- function(detector, target_data, ...) {
  .check_pkg("surveillance", reason = "for glrnb.")
  combined <- dplyr::bind_rows(detector$baseline, target_data) |>
    dplyr::arrange(.data$date)
  target_idx <- which(combined$date %in% target_data$date)

  sts_object <- surveillance::sts(observed = combined$cases, frequency = 52L)
  ctrl <- list(
    range = target_idx,
    c.ARL = detector$params$c_ARL,
    ret   = detector$params$ret
  )
  if (!is.null(detector$params$theta)) ctrl$theta <- detector$params$theta

  fit <- surveillance::glrnb(sts_object, control = ctrl)

  tibble::tibble(
    date = target_data$date,
    alarm = as.logical(surveillance::alarms(fit)),
    score = as.numeric(surveillance::upperbound(fit)),
    upper_threshold = as.numeric(surveillance::upperbound(fit))
  )
}


# ---------------------------------------------------------------------------
# built-in detector: trending
#
# trending::glm_model() fitting a negative-binomial GLM with seasonal sin/cos
# harmonics and a time trend. Prediction interval upper bound at level
# `1 - alpha` is the alarm threshold.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_trending <- function(alpha = 0.001,
                                   harmonics = 1L,
                                   include_trend = TRUE) {
  new_epi_detector(
    method = "trending",
    params = list(alpha = alpha,
                  harmonics = harmonics,
                  include_trend = include_trend)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_trending <- function(detector,
                                                baseline_data, ...) {
  .check_pkg("trending", reason = "for the trending NB-GLM detector.")
  .check_pkg("MASS", reason = "for trending::glm_nb_model().")

  d <- baseline_data |>
    dplyr::mutate(
      t = as.numeric(.data$date - min(.data$date)) / 7,
      doy = as.numeric(format(.data$date, "%j")),
      sin1 = sin(2 * pi * .data$doy / 365.25),
      cos1 = cos(2 * pi * .data$doy / 365.25)
    )

  rhs <- c(if (detector$params$include_trend) "t",
           if (detector$params$harmonics >= 1L) c("sin1", "cos1"))
  if (!length(rhs)) rhs <- "1"
  form <- stats::as.formula(paste("cases ~", paste(rhs, collapse = " + ")))

  model <- tryCatch(
    trending::glm_nb_model(form),
    error = function(e) trending::glm_model(form, family = "poisson")
  )
  fit <- trending::fit(model, d)

  detector$model <- fit
  detector$origin_date <- min(baseline_data$date)
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_trending <- function(detector,
                                                    target_data, ...) {
  .check_pkg("trending", reason = "for the trending NB-GLM detector.")

  td <- target_data |>
    dplyr::mutate(
      t = as.numeric(.data$date - detector$origin_date) / 7,
      doy = as.numeric(format(.data$date, "%j")),
      sin1 = sin(2 * pi * .data$doy / 365.25),
      cos1 = cos(2 * pi * .data$doy / 365.25)
    )

  preds <- trending::predict(
    detector$model, td,
    alpha = detector$params$alpha, simulate_pi = FALSE
  )

  pred_df <- as.data.frame(preds)
  # trending returns columns "estimate", "lower_pi", "upper_pi"
  upper_col <- intersect(c("upper_pi", "upper", "upper_ci"), names(pred_df))[1]
  est_col   <- intersect(c("estimate", "pred", "fitted"), names(pred_df))[1]
  upper_v <- pred_df[[upper_col]]
  est_v   <- pred_df[[est_col]]

  tibble::tibble(
    date = target_data$date,
    alarm = target_data$cases > upper_v,
    score = as.numeric((target_data$cases - est_v) /
                         pmax(est_v, 1)),
    upper_threshold = as.numeric(upper_v)
  )
}


# ---------------------------------------------------------------------------
# built-in detector: endemic_channel
#
# Hand-rolled WHO-style endemic channel: per-iso-week median + Q3 of
# baseline. Alarm raised when target exceeds Q3. Self-contained (no
# external dependency).
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_endemic_channel <- function(upper_quantile = 0.75) {
  new_epi_detector(
    method = "endemic_channel",
    params = list(upper_quantile = upper_quantile)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_endemic_channel <- function(detector,
                                                       baseline_data, ...) {
  q <- detector$params$upper_quantile
  detector$lookup <- baseline_data |>
    dplyr::mutate(iso_week = as.integer(format(.data$date, "%V"))) |>
    dplyr::group_by(.data$iso_week) |>
    dplyr::summarise(
      centre = stats::median(.data$cases, na.rm = TRUE),
      upper  = stats::quantile(.data$cases, probs = q,
                               na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_endemic_channel <- function(detector,
                                                           target_data, ...) {
  td <- target_data |>
    dplyr::mutate(iso_week = as.integer(format(.data$date, "%V"))) |>
    dplyr::left_join(detector$lookup, by = "iso_week")

  fallback_upper <- mean(detector$lookup$upper, na.rm = TRUE)
  upper_v <- dplyr::if_else(is.na(td$upper), fallback_upper, td$upper)

  tibble::tibble(
    date = target_data$date,
    alarm = target_data$cases > upper_v,
    score = as.numeric(target_data$cases - upper_v),
    upper_threshold = as.numeric(upper_v)
  )
}


# ---------------------------------------------------------------------------
# built-in detector: stl_residual
#
# stats::stl() seasonal-trend decomposition of the combined series; alarm if
# the target-period residual exceeds `k` SDs of the baseline residuals.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_stl_residual <- function(k = 3, s_window = "periodic") {
  new_epi_detector(
    method = "stl_residual",
    params = list(k = k, s_window = s_window)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_stl_residual <- function(detector,
                                                    baseline_data, ...) {
  detector$baseline <- baseline_data
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_stl_residual <- function(detector,
                                                       target_data, ...) {
  combined <- dplyr::bind_rows(detector$baseline, target_data) |>
    dplyr::arrange(.data$date)

  if (nrow(combined) < 104L) {
    cli::cli_abort("stl_residual needs >= 104 obs after combining baseline + target.")
  }

  series <- stats::ts(combined$cases, frequency = 52L)
  fit <- stats::stl(series, s.window = detector$params$s_window,
                    na.action = stats::na.exclude, robust = TRUE)

  resid_vec <- as.numeric(fit$time.series[, "remainder"])
  baseline_n <- nrow(detector$baseline)
  baseline_resid <- resid_vec[seq_len(baseline_n)]
  sd_b <- stats::sd(baseline_resid, na.rm = TRUE)

  target_resid <- resid_vec[(baseline_n + 1):length(resid_vec)]
  z <- target_resid / pmax(sd_b, 1e-9)
  upper_v <- detector$params$k * sd_b

  tibble::tibble(
    date = target_data$date,
    alarm = z > detector$params$k,
    score = as.numeric(z),
    upper_threshold = as.numeric(upper_v)
  )
}


# ---------------------------------------------------------------------------
# built-in detector: anomalize_stl
#
# Wraps anomalize::anomalize() pipeline (frequency-aware STL decomposition +
# IQR/GESD anomaly classification). Anomaly direction "up" is reported as an
# alarm; "down" or "none" are not.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_anomalize_stl <- function(alpha = 0.05,
                                        method = c("iqr", "gesd"),
                                        max_anomalies = 0.2) {
  method <- match.arg(method)
  new_epi_detector(
    method = "anomalize_stl",
    params = list(alpha = alpha, method = method,
                  max_anomalies = max_anomalies)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_anomalize_stl <- function(detector,
                                                     baseline_data, ...) {
  detector$baseline <- baseline_data
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_anomalize_stl <- function(detector,
                                                        target_data, ...) {
  .check_pkg("anomalize", reason = "for the anomalize_stl detector.")
  .check_pkg("tibbletime", reason = "for anomalize's time tibbles.")

  combined <- dplyr::bind_rows(detector$baseline, target_data) |>
    dplyr::arrange(.data$date) |>
    dplyr::select("date", "cases") |>
    tibble::as_tibble()

  decomp <- anomalize::time_decompose(combined, .data$cases,
                                       method = "stl",
                                       frequency = "auto",
                                       trend = "auto")
  anom <- anomalize::anomalize(decomp, .data$remainder,
                                method = detector$params$method,
                                alpha = detector$params$alpha,
                                max_anoms = detector$params$max_anomalies)
  recomp <- anomalize::time_recompose(anom)

  target_rows <- recomp |>
    dplyr::filter(.data$date %in% target_data$date)

  alarm_v <- target_rows$anomaly == "Yes" &
    target_rows$observed > target_rows$recomposed_l2
  upper_v <- target_rows$recomposed_l2

  tibble::tibble(
    date = target_data$date,
    alarm = as.logical(alarm_v),
    score = as.numeric(target_rows$observed - target_rows$recomposed_l2),
    upper_threshold = as.numeric(upper_v)
  )
}


# ---------------------------------------------------------------------------
# built-in detector: arima
#
# forecast::auto.arima on the baseline, then one-step-ahead forecasts over
# the target window. Alarm if observed exceeds the upper prediction interval.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_arima <- function(alpha = 0.001,
                                seasonal = TRUE,
                                stepwise = TRUE) {
  new_epi_detector(
    method = "arima",
    params = list(alpha = alpha, seasonal = seasonal, stepwise = stepwise)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_arima <- function(detector, baseline_data, ...) {
  .check_pkg("forecast", reason = "for the arima detector.")

  series <- stats::ts(baseline_data$cases, frequency = 52L)
  detector$model <- forecast::auto.arima(
    series,
    seasonal = detector$params$seasonal,
    stepwise = detector$params$stepwise,
    approximation = TRUE
  )
  detector$baseline <- baseline_data
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_arima <- function(detector, target_data, ...) {
  .check_pkg("forecast", reason = "for the arima detector.")

  n_target <- nrow(target_data)
  level <- (1 - detector$params$alpha) * 100
  fc <- forecast::forecast(detector$model, h = n_target, level = level)

  upper_v <- as.numeric(fc$upper[, 1])
  mean_v  <- as.numeric(fc$mean)

  tibble::tibble(
    date = target_data$date,
    alarm = target_data$cases > upper_v,
    score = as.numeric(target_data$cases - mean_v),
    upper_threshold = upper_v
  )
}


# ---------------------------------------------------------------------------
# built-in detector: changepoint_pelt
#
# changepoint::cpt.meanvar with PELT. Alarm at every target observation
# that falls *after* the last detected changepoint in the combined series.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_changepoint_pelt <- function(penalty = "BIC",
                                           minseglen = 4L) {
  new_epi_detector(
    method = "changepoint_pelt",
    params = list(penalty = penalty, minseglen = minseglen)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_changepoint_pelt <- function(detector,
                                                       baseline_data, ...) {
  detector$baseline <- baseline_data
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_changepoint_pelt <- function(detector,
                                                            target_data, ...) {
  .check_pkg("changepoint", reason = "for changepoint_pelt.")

  combined <- dplyr::bind_rows(detector$baseline, target_data) |>
    dplyr::arrange(.data$date)
  fit <- changepoint::cpt.meanvar(
    combined$cases,
    penalty = detector$params$penalty,
    method = "PELT",
    minseglen = detector$params$minseglen
  )
  cps <- changepoint::cpts(fit)
  baseline_n <- nrow(detector$baseline)

  # target rows
  idx <- seq(baseline_n + 1, nrow(combined))
  # alarm if a changepoint lies at or before this row in the target range
  # OR if any changepoint was detected after the baseline window.
  cps_in_target <- cps[cps > baseline_n]
  alarm_v <- vapply(idx, function(i) any(cps_in_target <= i), logical(1))
  score_v <- vapply(idx, function(i) sum(cps_in_target <= i),
                    numeric(1))

  tibble::tibble(
    date = target_data$date,
    alarm = alarm_v,
    score = as.numeric(score_v),
    upper_threshold = rep(NA_real_, length(idx))
  )
}


# ---------------------------------------------------------------------------
# built-in detector: bayesian_changepoint
#
# Rbeast::beast on the combined series; alarm if posterior probability of
# a changepoint at the target date exceeds `prob_threshold`.
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_detector_bayesian_changepoint <- function(prob_threshold = 0.5) {
  new_epi_detector(
    method = "bayesian_changepoint",
    params = list(prob_threshold = prob_threshold)
  )
}

#' @keywords internal
#' @noRd
fit_detector.epi_detector_bayesian_changepoint <- function(detector,
                                                            baseline_data,
                                                            ...) {
  detector$baseline <- baseline_data
  detector$fitted <- TRUE
  detector
}

#' @keywords internal
#' @noRd
predict_detector.epi_detector_bayesian_changepoint <- function(detector,
                                                                target_data,
                                                                ...) {
  .check_pkg("Rbeast", reason = "for bayesian_changepoint.")

  combined <- dplyr::bind_rows(detector$baseline, target_data) |>
    dplyr::arrange(.data$date)
  baseline_n <- nrow(detector$baseline)

  fit <- Rbeast::beast(combined$cases, season = "none",
                       print.progress = FALSE,
                       print.options = FALSE, quiet = TRUE)

  # cpOccPr is the posterior probability vector of a changepoint at each t
  prob_vec <- as.numeric(fit$trend$cpOccPr)
  if (length(prob_vec) != nrow(combined)) {
    # Rbeast occasionally returns a shorter vector; pad with zeros
    prob_vec <- c(prob_vec, rep(0, nrow(combined) - length(prob_vec)))
  }

  target_idx <- seq(baseline_n + 1, nrow(combined))
  prob_target <- prob_vec[target_idx]
  thr <- detector$params$prob_threshold

  tibble::tibble(
    date = target_data$date,
    alarm = prob_target >= thr,
    score = as.numeric(prob_target),
    upper_threshold = rep(thr, length(target_idx))
  )
}


# ---------------------------------------------------------------------------
# built-in registration hook
# ---------------------------------------------------------------------------

#' Register all v1.1 built-in detectors in the package registry
#'
#' Called from [.onLoad()] in `R/zzz.R`. Each built-in is registered with
#' its constructor and S3 methods (defined above for v1.1 step-2 detectors
#' and below in subsequent build steps). Re-running this function is safe:
#' existing registry entries are overwritten.
#'
#' @return Invisibly returns the character vector of registered names.
#'
#' @keywords internal
#' @noRd
.register_builtin_detectors <- function() {
  ns <- asNamespace("sntmethods")

  # step 2 detectors
  assign("threshold", new_detector_threshold, envir = .epi_registry)
  registerS3method(
    "fit_detector", "epi_detector_threshold",
    fit_detector.epi_detector_threshold,
    envir = ns
  )
  registerS3method(
    "predict_detector", "epi_detector_threshold",
    predict_detector.epi_detector_threshold,
    envir = ns
  )

  assign("ears_c1", new_detector_ears_c1, envir = .epi_registry)
  registerS3method(
    "fit_detector", "epi_detector_ears_c1",
    fit_detector.epi_detector_ears_c1,
    envir = ns
  )
  registerS3method(
    "predict_detector", "epi_detector_ears_c1",
    predict_detector.epi_detector_ears_c1,
    envir = ns
  )

  # step 17 detectors
  step17 <- c(
    "ears_c2", "ears_c3", "cusum_classical", "farrington", "glrnb",
    "trending", "endemic_channel", "stl_residual", "anomalize_stl",
    "arima", "changepoint_pelt", "bayesian_changepoint"
  )
  for (nm in step17) {
    ctor <- get(paste0("new_detector_", nm), envir = ns,
                inherits = FALSE)
    assign(nm, ctor, envir = .epi_registry)
    cls <- paste0("epi_detector_", nm)
    fit_fn <- get(paste0("fit_detector.", cls),
                  envir = ns, inherits = FALSE)
    pred_fn <- get(paste0("predict_detector.", cls),
                   envir = ns, inherits = FALSE)
    registerS3method("fit_detector", cls, fit_fn, envir = ns)
    registerS3method("predict_detector", cls, pred_fn, envir = ns)
  }

  invisible(available_detectors())
}


# ---- input validation, range resolution, preprocessing --------------------

#' Validate the `epi_detect()` input contract
#'
#' Checks that the supplied data frame and column names honour the input
#' contract documented on [epi_detect()]. Returns the data frame
#' (unchanged) on success; aborts via [cli::cli_abort()] on contract
#' violation. Soft issues (e.g. gappy series) are surfaced via
#' [cli::cli_warn()] elsewhere.
#'
#' @keywords internal
#' @noRd
.validate_epi_input <- function(data,
                                 date_col,
                                 count_col,
                                 group_col = NULL,
                                 denominator_col = NULL,
                                 delay_col = NULL,
                                 label_col = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (nrow(data) == 0L) {
    cli::cli_abort("{.arg data} is empty (0 rows).")
  }

  required_cols <- c(date_col, count_col)
  optional_cols <- c(group_col, denominator_col, delay_col, label_col)
  missing_cols <- setdiff(c(required_cols, optional_cols), names(data))
  missing_cols <- intersect(missing_cols,
                            c(required_cols, optional_cols[!is.null(optional_cols)]))

  hard_missing <- setdiff(required_cols, names(data))
  if (length(hard_missing) > 0L) {
    cli::cli_abort(c(
      "Required column{?s} not found in {.arg data}: {.field {hard_missing}}.",
      "i" = "Available columns: {.field {names(data)}}."
    ))
  }

  if (!inherits(data[[date_col]], "Date")) {
    cli::cli_abort(c(
      "Column {.field {date_col}} must be of class {.cls Date}.",
      "x" = "Got {.cls {class(data[[date_col]])[1]}}."
    ))
  }

  if (!is.numeric(data[[count_col]])) {
    cli::cli_abort(c(
      "Column {.field {count_col}} must be numeric.",
      "x" = "Got {.cls {class(data[[count_col]])[1]}}."
    ))
  }
  if (any(data[[count_col]] < 0, na.rm = TRUE)) {
    cli::cli_abort(
      "Column {.field {count_col}} contains negative values."
    )
  }

  if (!is.null(group_col) && !group_col %in% names(data)) {
    cli::cli_abort("Group column {.field {group_col}} not found in data.")
  }

  if (!is.null(denominator_col)) {
    if (!denominator_col %in% names(data)) {
      cli::cli_abort(
        "Denominator column {.field {denominator_col}} not found in data."
      )
    }
    if (!is.numeric(data[[denominator_col]])) {
      cli::cli_abort(
        "Denominator column {.field {denominator_col}} must be numeric."
      )
    }
    if (any(data[[denominator_col]] <= 0, na.rm = TRUE)) {
      cli::cli_abort(
        "Denominator column {.field {denominator_col}} must be strictly positive."
      )
    }
  }

  if (!is.null(delay_col)) {
    if (!delay_col %in% names(data)) {
      cli::cli_abort(
        "Delay column {.field {delay_col}} not found in data."
      )
    }
    delays <- data[[delay_col]]
    if (!is.numeric(delays)) {
      cli::cli_abort("Delay column {.field {delay_col}} must be numeric.")
    }
    if (any(delays < 0, na.rm = TRUE)) {
      cli::cli_abort(
        "Delay column {.field {delay_col}} must be non-negative."
      )
    }
    cli::cli_warn(c(
      "{.arg delay_col} is recognised but delay-corrected detection is",
      "deferred to v2 (via {.fn surveillance::bodaDelay}).",
      "i" = "Detection will proceed on raw counts; reported lags may be ",
      "biased toward late detection."
    ))
  }

  if (!is.null(label_col)) {
    if (!label_col %in% names(data)) {
      cli::cli_abort(
        "Label column {.field {label_col}} not found in data."
      )
    }
    if (!is.logical(data[[label_col]])) {
      cli::cli_abort(
        "Label column {.field {label_col}} must be {.cls logical}."
      )
    }
  }

  invisible(data)
}


#' Resolve a row range argument to integer row indices
#'
#' Accepts integer row indices, a logical vector of length `nrow(data)`,
#' or a `Date` vector of values to match against `data[[date_col]]`.
#'
#' @keywords internal
#' @noRd
.resolve_range <- function(range, data, date_col, arg_name) {
  if (is.null(range)) return(NULL)

  if (is.logical(range)) {
    if (length(range) != nrow(data)) {
      cli::cli_abort(c(
        "Logical {.arg {arg_name}} must have length {nrow(data)}.",
        "x" = "Got length {length(range)}."
      ))
    }
    return(which(range))
  }

  if (inherits(range, "Date")) {
    # length-2 range is treated as (start, end) inclusive interval; longer
    # vectors fall back to exact-match semantics.
    if (length(range) == 2L) {
      lo <- min(range)
      hi <- max(range)
      idx <- which(data[[date_col]] >= lo & data[[date_col]] <= hi)
    } else {
      idx <- which(data[[date_col]] %in% range)
    }
    if (length(idx) == 0L) {
      cli::cli_abort(
        "No rows in {.arg data} match the dates supplied in {.arg {arg_name}}."
      )
    }
    return(idx)
  }

  if (is.numeric(range)) {
    rng <- as.integer(range)
    if (any(rng < 1L | rng > nrow(data))) {
      cli::cli_abort(c(
        "{.arg {arg_name}} contains row indices outside 1..{nrow(data)}."
      ))
    }
    return(rng)
  }

  cli::cli_abort(c(
    "{.arg {arg_name}} must be an integer / logical / Date vector.",
    "x" = "Got an object of class {.cls {class(range)[1]}}."
  ))
}


#' Default range resolution: target = last full calendar year of data,
#' baseline = everything before it.
#'
#' @keywords internal
#' @noRd
.default_ranges <- function(data, date_col) {
  years <- as.integer(format(data[[date_col]], "%Y"))
  last_full_year <- max(years[years < max(years)], na.rm = TRUE)
  if (!is.finite(last_full_year)) {
    last_full_year <- max(years, na.rm = TRUE)
  }
  target_idx <- which(years == last_full_year)
  baseline_idx <- which(years < last_full_year)
  list(target = target_idx, baseline = baseline_idx)
}


#' Warn (do not abort) when a per-group series has gappy weekly spacing
#'
#' @keywords internal
#' @noRd
.warn_gaps <- function(data, date_col, group_col) {
  groups <- if (is.null(group_col)) {
    list(`(ungrouped)` = data)
  } else {
    split(data, data[[group_col]])
  }

  gappy_groups <- character(0)
  for (g in names(groups)) {
    dates <- sort(unique(groups[[g]][[date_col]]))
    if (length(dates) < 2L) next
    diffs <- as.integer(diff(dates))
    if (any(diffs != 7L)) {
      gappy_groups <- c(gappy_groups, g)
    }
  }

  if (length(gappy_groups) > 0L) {
    cli::cli_warn(c(
      "Irregular weekly spacing detected in {length(gappy_groups)} group{?s}:",
      "i" = "Affected: {.val {gappy_groups}}.",
      "i" = "Detectors that assume fixed weekly cadence may misbehave.",
      ">" = "Consider running {.fn tidyr::complete} upstream to fill gaps."
    ))
  }

  invisible(gappy_groups)
}


#' Apply pre-processing (aggregation, transform, detrend, rate) to a per-
#' group data frame and return the modified `cases` column ready for
#' detection.
#'
#' The aggregator collapses to coarser temporal grain; the rate option
#' divides cases by denominator; the variance-stabilising transform is
#' applied last so detectors see comparable scales.
#'
#' @keywords internal
#' @noRd
.apply_preprocessing <- function(data,
                                  date_col,
                                  count_col,
                                  denominator_col,
                                  transform,
                                  detrend,
                                  use_rates,
                                  aggregation) {
  out <- data |>
    dplyr::rename(date = !!date_col, cases = !!count_col)

  # rate conversion
  if (use_rates) {
    if (is.null(denominator_col)) {
      cli::cli_abort(
        "{.code use_rates = TRUE} requires {.arg denominator_col}."
      )
    }
    out <- out |>
      dplyr::mutate(cases = .data$cases / .data[[denominator_col]])
  }

  # aggregation
  out <- switch(aggregation,
    "week" = out,
    "epi_week_4" = out |>
      dplyr::arrange(.data$date) |>
      dplyr::mutate(
        cases = zoo::rollsum(
          .data$cases, k = 4L, fill = NA_real_, align = "right"
        )
      ) |>
      dplyr::filter(!is.na(.data$cases)),
    "month" = {
      out |>
        dplyr::mutate(
          .month = lubridate::floor_date(.data$date, unit = "month")
        ) |>
        dplyr::group_by(.data$.month) |>
        dplyr::summarise(
          cases = sum(.data$cases, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::rename(date = ".month")
    }
  )

  # detrend
  if (isTRUE(detrend)) {
    out <- out |>
      dplyr::arrange(.data$date) |>
      dplyr::mutate(
        .time_index = as.numeric(.data$date),
        cases = .data$cases - stats::predict(
          stats::lm(.data$cases ~ .data$.time_index)
        )
      ) |>
      dplyr::select(-".time_index")
  }

  # variance-stabilising transform
  out$cases <- switch(transform,
    "none" = out$cases,
    "log1p" = log1p(pmax(out$cases, 0)),
    "anscombe" = 2 * sqrt(pmax(out$cases, 0) + 3 / 8),
    "freeman_tukey" = sqrt(pmax(out$cases, 0)) +
      sqrt(pmax(out$cases, 0) + 1)
  )

  out
}


# ---- epi_detect() verb ----------------------------------------------------

#' Run outbreak detectors on a weekly count series
#'
#' Apply one or more registered outbreak detectors to a tidy weekly count
#' series and return a per-method, per-week tibble of alarms and scores
#' wrapped in an `epi_detection_run` S3 object that downstream
#' [epi_evaluate()], [epi_ensemble()], and [epi_recommend()] verbs consume.
#'
#' @param data Tidy data frame with at minimum a date column and a non-
#'   negative count column. See **Input contract** below.
#' @param methods Character vector of detector ids to apply. Run
#'   [available_detectors()] to see what is registered.
#' @param date_col,count_col Column names; defaults `"date"` and
#'   `"cases"`.
#' @param group_col Optional column name identifying panel groups
#'   (district, facility, stratum). Each group is detected independently
#'   in v1.1; hierarchical pooling is deferred to v2.
#' @param denominator_col Optional column name with catchment population
#'   or expected counts. Required when `use_rates = TRUE`.
#' @param delay_col Optional column name with reporting delay in weeks.
#'   Currently triggers a warning (delay correction is deferred to v2 via
#'   `surveillance::bodaDelay`).
#' @param label_col Optional column name with known outbreak labels
#'   (logical). Carried through to the run object so [epi_evaluate()] can
#'   use it without re-joining data.
#' @param target_range,baseline_range Each accepts integer row indices, a
#'   logical vector of length `nrow(data)`, or a `Date` vector. Defaults:
#'   `target_range = last full calendar year`,
#'   `baseline_range = everything earlier`.
#' @param transform Variance-stabilising transformation: one of
#'   `"none"`, `"log1p"`, `"anscombe"`, `"freeman_tukey"`.
#' @param detrend If `TRUE`, remove a linear time trend from cases before
#'   detection. Methods with their own trend handling (Farrington,
#'   trending, arima) should not be double-detrended; a warning is
#'   emitted if they are.
#' @param use_rates If `TRUE`, divide cases by `denominator_col` before
#'   detection.
#' @param aggregation Temporal grain: `"week"`, `"epi_week_4"` (four-week
#'   rolling sum), or `"month"`.
#' @param method_params Named list of per-method parameter overrides; e.g.
#'   `list(threshold = list(k = 3), ears_c1 = list(baseline = 12))`.
#'
#' @section Input contract:
#' \describe{
#'   \item{`date`}{Date, regular weekly spacing within each group.}
#'   \item{`cases`}{Non-negative numeric count.}
#'   \item{`group_id`}{Optional character/factor panel id.}
#'   \item{`outbreak_label`}{Optional logical; used by [epi_evaluate()].}
#'   \item{`denominator`}{Optional positive numeric population/expected.}
#'   \item{`report_delay_weeks`}{Optional non-negative integer; warned
#'   about (delay correction deferred to v2).}
#' }
#'
#' @section Failure isolation:
#' If a detector errors on a given group, the framework returns rows with
#' `alarm = NA`, `score = NA_real_`, `upper_threshold = NA_real_`,
#' `failed = TRUE`, and the captured `error_message` rather than crashing
#' the run. Downstream verbs ([epi_evaluate()], [epi_ensemble()],
#' [epi_recommend()]) skip or downweight failed methods consistently.
#'
#' @return An `epi_detection_run` S3 object (printable; coerce to tibble
#'   with `tibble::as_tibble()`) carrying:
#'   \itemize{
#'     \item `$predictions` — long tibble with `method`, `group_id`,
#'       `date`, `alarm`, `score`, `upper_threshold`, `failed`,
#'       `error_message`.
#'     \item `$preprocessing` — list capturing transform, detrend,
#'       use_rates, aggregation, target/baseline ranges, and the raw
#'       input data, so LOO and operating-point analyses can re-derive
#'       detection deterministically.
#'     \item `$methods` — the requested method ids.
#'     \item `$call` — captured call for printing.
#'   }
#'
#' @seealso [available_detectors()], [register_detector()],
#'   [epi_evaluate()], [epi_ensemble()], [epi_recommend()].
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' weekly <- tibble::tibble(
#'   date = seq.Date(as.Date("2018-01-07"), by = "week",
#'                   length.out = 260L),
#'   cases = rpois(260L, 8)
#' )
#' run <- epi_detect(
#'   weekly,
#'   methods = c("threshold", "ears_c1"),
#'   target_range = which(format(weekly$date, "%Y") == "2022"),
#'   baseline_range = which(format(weekly$date, "%Y") < "2022")
#' )
#' run$predictions
#' }
#'
#' @export
epi_detect <- function(data,
                       methods,
                       date_col = "date",
                       count_col = "cases",
                       group_col = NULL,
                       denominator_col = NULL,
                       delay_col = NULL,
                       label_col = NULL,
                       target_range = NULL,
                       baseline_range = NULL,
                       transform = c("none", "log1p", "anscombe",
                                     "freeman_tukey"),
                       detrend = FALSE,
                       use_rates = FALSE,
                       aggregation = c("week", "epi_week_4", "month"),
                       method_params = list()) {
  transform <- match.arg(transform)
  aggregation <- match.arg(aggregation)

  # 1. validate input contract
  .validate_epi_input(
    data = data,
    date_col = date_col,
    count_col = count_col,
    group_col = group_col,
    denominator_col = denominator_col,
    delay_col = delay_col,
    label_col = label_col
  )

  if (!is.character(methods) || length(methods) == 0L) {
    cli::cli_abort(
      "{.arg methods} must be a non-empty character vector."
    )
  }
  unknown <- setdiff(methods, available_detectors())
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "Unknown method{?s}: {.val {unknown}}.",
      "i" = "Available: {.val {available_detectors()}}.",
      "i" = "Register custom detectors with {.fn register_detector}."
    ))
  }

  # warn if detrend requested for methods that handle trend internally
  trend_aware <- intersect(methods, c("farrington", "trending", "arima"))
  if (isTRUE(detrend) && length(trend_aware) > 0L) {
    cli::cli_warn(c(
      "{.code detrend = TRUE} requested with trend-aware methods.",
      "i" = "These will see detrended input which may degrade their fit:",
      "x" = "Affected method{?s}: {.val {trend_aware}}."
    ))
  }

  # 2. gap warning per group
  .warn_gaps(data, date_col, group_col)

  # 3. range resolution
  if (is.null(target_range) && is.null(baseline_range)) {
    defaults <- .default_ranges(data, date_col)
    target_range <- defaults$target
    baseline_range <- defaults$baseline
  }
  target_idx <- .resolve_range(target_range, data, date_col, "target_range")
  baseline_idx <- .resolve_range(baseline_range, data, date_col,
                                  "baseline_range")
  if (length(intersect(target_idx, baseline_idx)) > 0L) {
    cli::cli_abort(
      "{.arg target_range} and {.arg baseline_range} overlap."
    )
  }

  # 4. preprocessing per group, then detector loop
  groups <- if (is.null(group_col)) {
    list(`(ungrouped)` = list(data = data, target_idx = target_idx,
                              baseline_idx = baseline_idx))
  } else {
    split_data <- split(data, data[[group_col]])
    lapply(split_data, function(g) {
      g_rows <- which(data[[group_col]] %in% unique(g[[group_col]]))
      list(
        data = g,
        target_idx = match(intersect(g_rows, target_idx), g_rows) |>
          stats::na.omit() |> as.integer(),
        baseline_idx = match(intersect(g_rows, baseline_idx), g_rows) |>
          stats::na.omit() |> as.integer()
      )
    })
  }

  predictions <- purrr::map_dfr(names(groups), function(g_name) {
    bundle <- groups[[g_name]]
    g_data <- bundle$data
    preprocessed <- .apply_preprocessing(
      data = g_data,
      date_col = date_col,
      count_col = count_col,
      denominator_col = denominator_col,
      transform = transform,
      detrend = detrend,
      use_rates = use_rates,
      aggregation = aggregation
    )

    # split into baseline/target after preprocessing
    target_dates <- g_data[[date_col]][bundle$target_idx]
    baseline_dates <- g_data[[date_col]][bundle$baseline_idx]
    baseline_data <- preprocessed |>
      dplyr::filter(.data$date %in% baseline_dates)
    target_data <- preprocessed |>
      dplyr::filter(.data$date %in% target_dates)

    purrr::map_dfr(methods, function(m) {
      detector <- .resolve_detector(m, method_params)
      result <- .run_one_detector_safely(
        detector = detector,
        baseline_data = baseline_data,
        target_data = target_data,
        group_id = g_name
      )
      result$method <- m
      result$group_id <- g_name
      result
    })
  })

  # canonical column order
  predictions <- predictions[, c(
    "method", "group_id", "date", "alarm", "score",
    "upper_threshold", "failed", "error_message"
  ), drop = FALSE]

  # 5. preprocessing trace (consumed by epi_evaluate LOO + ops)
  preprocessing <- list(
    transform = transform,
    detrend = detrend,
    use_rates = use_rates,
    aggregation = aggregation,
    target_range = target_idx,
    baseline_range = baseline_idx,
    date_col = date_col,
    count_col = count_col,
    group_col = group_col,
    denominator_col = denominator_col,
    delay_col = delay_col,
    label_col = label_col,
    original_data = data
  )

  out <- list(
    predictions = tibble::as_tibble(predictions),
    preprocessing = preprocessing,
    methods = methods,
    call = match.call()
  )
  class(out) <- c("epi_detection_run", "list")
  out
}


# ---- epi_detection_run S3 methods -----------------------------------------

#' @export
print.epi_detection_run <- function(x, ...) {
  preds <- x$predictions
  n_methods <- length(x$methods)
  n_groups <- length(unique(preds$group_id))
  n_target <- length(unique(preds$date))
  n_failed <- sum(preds$failed, na.rm = TRUE)
  alarm_rate <- mean(preds$alarm, na.rm = TRUE)

  cat("<epi_detection_run>\n")
  cli::cli_h2("epi_detection_run")
  cli::cli_bullets(c(
    "*" = "methods: {.val {x$methods}}",
    "*" = "groups: {n_groups} | target weeks: {n_target}",
    "*" = "alarms fired: {sum(preds$alarm, na.rm = TRUE)} ",
    "  ({round(100 * alarm_rate, 1)}% of method-week cells)",
    "*" = "preprocessing: transform = {.val {x$preprocessing$transform}}, ",
    "  aggregation = {.val {x$preprocessing$aggregation}}, ",
    "  detrend = {.val {x$preprocessing$detrend}}, ",
    "  use_rates = {.val {x$preprocessing$use_rates}}"
  ))
  if (n_failed > 0L) {
    failed_methods <- unique(preds$method[preds$failed])
    cli::cli_alert_warning(
      "{n_failed} method-week cell{?s} failed in method{?s} {.val {failed_methods}}."
    )
  }
  invisible(x)
}

#' @export
summary.epi_detection_run <- function(object, ...) {
  preds <- object$predictions

  per_method <- preds |>
    dplyr::group_by(.data$method) |>
    dplyr::summarise(
      n_alarms = sum(.data$alarm, na.rm = TRUE),
      alarm_rate = mean(.data$alarm, na.rm = TRUE),
      n_failed = sum(.data$failed, na.rm = TRUE),
      mean_score = mean(.data$score, na.rm = TRUE),
      .groups = "drop"
    )

  invisible(structure(
    list(per_method = per_method,
         preprocessing = object$preprocessing,
         methods = object$methods),
    class = "summary.epi_detection_run"
  ))
}

#' @export
print.summary.epi_detection_run <- function(x, ...) {
  cli::cli_h2("epi_detection_run summary")
  print(x$per_method)
  invisible(x)
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.epi_detection_run <- function(x, ...) {
  x$predictions
}

#' Plot per-method alarm series from an `epi_detection_run`
#'
#' @param x An `epi_detection_run` object returned by [epi_detect()].
#' @param ... Forwarded to [autoplot.epi_detection_run()].
#'
#' @return A ggplot object (invisibly).
#'
#' @export
plot.epi_detection_run <- function(x, ...) {
  p <- autoplot.epi_detection_run(x, ...)
  print(p)
  invisible(p)
}

#' @export
#' @importFrom ggplot2 autoplot
autoplot.epi_detection_run <- function(object, ...) {
  preds <- object$predictions |>
    dplyr::mutate(
      alarm = dplyr::if_else(is.na(.data$alarm), FALSE, .data$alarm)
    )

  ggplot2::ggplot(
    preds,
    ggplot2::aes(x = .data$date, y = .data$score)
  ) +
    ggplot2::geom_line(colour = "grey50") +
    ggplot2::geom_point(
      data = preds[preds$alarm, , drop = FALSE],
      ggplot2::aes(x = .data$date, y = .data$score),
      colour = "firebrick", size = 1.5
    ) +
    ggplot2::facet_grid(method ~ group_id, scales = "free_y") +
    ggplot2::labs(
      title = "epi_detect alarms by method and group",
      x = NULL,
      y = "score"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      strip.text.y = ggplot2::element_text(angle = 0)
    )
}
