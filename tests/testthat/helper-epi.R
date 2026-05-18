# Shared helpers for the epi_* test files.
# Builds a small reproducible weekly series with an injected outbreak.

.epi_sim <- function(seed = 1L,
                     n_baseline_weeks = 156L,
                     n_target_weeks = 52L,
                     n_groups = 1L) {
  set.seed(seed)
  total_weeks <- n_baseline_weeks + n_target_weeks
  dates <- seq.Date(as.Date("2020-01-06"),
                    by = "week",
                    length.out = total_weeks)
  doy <- as.numeric(format(dates, "%j"))
  seasonal <- 50 + 30 * sin(2 * pi * doy / 365.25)

  groups <- if (n_groups == 1L) NULL else paste0("g", seq_len(n_groups))

  if (is.null(groups)) {
    counts <- stats::rpois(total_weeks, lambda = seasonal)
    spike <- ((n_baseline_weeks + 20):(n_baseline_weeks + 28))
    counts[spike] <- counts[spike] + stats::rpois(length(spike), lambda = 50)
    truth <- rep(FALSE, total_weeks)
    truth[spike] <- TRUE
    return(tibble::tibble(date = dates, cases = counts, truth = truth))
  }

  purrr::map_dfr(groups, function(g) {
    counts <- stats::rpois(total_weeks, lambda = seasonal)
    spike <- ((n_baseline_weeks + 20):(n_baseline_weeks + 28))
    counts[spike] <- counts[spike] + stats::rpois(length(spike), lambda = 50)
    truth <- rep(FALSE, total_weeks)
    truth[spike] <- TRUE
    tibble::tibble(group = g, date = dates, cases = counts, truth = truth)
  })
}

.epi_ranges <- function(df) {
  list(
    target_range = c(df$date[157], df$date[208]),
    baseline_range = c(df$date[1], df$date[156])
  )
}
