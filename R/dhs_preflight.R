# =============================================================================
# PROPOSED CHANGE C11  ->  R/dhs_preflight.R  (new file)
#
# Answers, before any modelling: which surveys are in the archive, which
# variables each one carries and how populated they are, what its value
# codes mean, and which indicators it can actually support.
#
# Every failure this is built to catch is one that currently surfaces
# mid-run as a skip message, or does not surface at all:
#
#   - a variable that is absent          (hml12 in the 2024-25 RDHS)
#   - a variable that is present but empty (hv042 in the 2023 MIS)
#   - a codebook that differs by survey  (hml35 = 6 is "other" in the
#     2023 MIS, "test undetermined" in the standard recode)
#   - an age variable the indicator's window cannot use (hc1 above 59
#     months, or absent entirely)
#
#   pf <- dhs_preflight(path_dhs_parquet, "RW")
#   pf$feasibility      # indicator x survey: can this be computed?
#   pf$coverage         # variable x survey: present, and how populated
#   pf$codebook         # value labels, for drift detection
#   dhs_preflight_write(pf, path = here::here(paths$dhs, "processed"),
#                       country_iso3 = "rwa")
# =============================================================================


# ---------------------------------------------------------------------------
# what each indicator needs -------------------------------------------------
# ---------------------------------------------------------------------------

#' Variable requirements per indicator
#'
#' @description
#' Mirrors the `survey_vars` defaults and the `setdiff(needed, names(x))`
#' checks inside the `calc_*` functions, read forward so a survey can be
#' assessed before it is processed rather than after it fails.
#'
#' `vars_any` holds alternatives: at least one must be usable. That is how
#' age is expressed, because DHS records it in `hc1` (months, under-fives
#' only), `hml16a` (months, malaria roster) or `hml16` (years) depending
#' on the survey, and an MIS that tested all ages may carry none of the
#' first.
#'
#' @return A tibble of requirements, one row per indicator.
#' @export
dhs_indicator_requirements <- function() {
  tibble::tribble(
    ~indicator, ~recode, ~vars_all, ~vars_any,
    "pfpr_mic", "PR",
    c("hv001", "hv103", "hml32"), c("hc1", "hml16a", "hml16"),
    "pfpr_rdt", "PR",
    c("hv001", "hv103", "hml35"), c("hc1", "hml16a", "hml16"),
    "use_itn", "PR",
    c("hv001", "hv005", "hml12"), c("hv105", "hml16"),
    "access_itn", "HR",
    c("hv001", "hv005", "hml10_1"), NA,
    "own_itn", "HR",
    c("hv001", "hv005", "hml1"), NA,
    "irs", "HR",
    c("hv001", "hv005", "hv253"), NA,
    "anemia", "PR",
    c("hv001", "hc57"), c("hc1", "hml16a"),
    "wealth", "HR",
    c("hv001", "hv005", "hv270"), NA,
    "fever", "KR",
    c("v001", "v005", "h22"), c("b19", "hw1", "hc1"),
    "csb", "KR",
    c("v001", "v005", "h22"), c("b19", "hw1", "hc1"),
    "act", "KR",
    c("v001", "v005", "ml13a"), c("b19", "hw1", "hc1"),
    "malaria_dx", "KR",
    c("v001", "v005", "h47"), c("b19", "hw1", "hc1"),
    "epi", "KR",
    c("v001", "v005", "h2"), c("b19", "hw1", "hc1"),
    "u5mr", "KR",
    c("v001", "v005", "b3", "b7"), NA,
    "iptp", "IR",
    c("v001", "v005", "m49a"), NA,
    "gps", "GE",
    c("DHSCLUST", "LATNUM", "LONGNUM"), NA
  )
}


# ---------------------------------------------------------------------------
# variable coverage ----------------------------------------------------------
# ---------------------------------------------------------------------------

#' Coverage of every variable in one recode
#'
#' @description
#' Presence alone is not enough. A column can exist and hold nothing: the
#' 2023 Rwanda MIS carries `hv042` labelled "na - household selected for
#' hemoglobin" with zero non-missing values, and any guard that tests
#' `%in% names(x)` treats it as available, then filters every row away.
#' `pct_populated` is what separates the two cases.
#'
#' @param x A recode data frame.
#' @return A tibble: variable, label, pct_populated, n_distinct, labels.
#' @keywords internal
#' @noRd
.dhs_var_coverage <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(
    variable = names(x),
    label = vapply(
      x,
      function(v) {
        lbl <- attr(v, "label")
        if (is.null(lbl)) NA_character_ else as.character(lbl)[1]
      },
      character(1)
    ),
    pct_populated = vapply(
      x, function(v) round(100 * mean(!is.na(v)), 2), numeric(1)
    ),
    n_distinct = vapply(
      x, function(v) length(unique(stats::na.omit(as.vector(v)))), integer(1)
    ),
    value_labels = vapply(
      x,
      function(v) {
        labs <- attr(v, "labels")
        if (is.null(labs)) {
          return(NA_character_)
        }
        paste(unname(labs), names(labs), sep = "=", collapse = "; ")
      },
      character(1)
    )
  )
}


# ---------------------------------------------------------------------------
# feasibility ----------------------------------------------------------------
# ---------------------------------------------------------------------------

#' Can this survey support this indicator?
#'
#' @param coverage Coverage tibble for one survey and recode.
#' @param req One row of `dhs_indicator_requirements()`.
#' @param min_pct Minimum percent populated for a variable to count as
#'   usable. Defaults to `0.1` rather than `0`: a column that is entirely
#'   missing has not been collected, whatever its label says.
#' @keywords internal
#' @noRd
.dhs_check_requirement <- function(coverage, req, min_pct = 0.1) {
  usable <- coverage$variable[coverage$pct_populated >= min_pct]
  present <- coverage$variable

  need_all <- unlist(req$vars_all)
  need_any <- unlist(req$vars_any)
  need_any <- need_any[!is.na(need_any)]

  missing_all <- setdiff(need_all, present)
  empty_all <- setdiff(intersect(need_all, present), usable)

  any_ok <- length(need_any) == 0 || any(need_any %in% usable)

  status <- dplyr::case_when(
    length(missing_all) > 0 ~ "not collected",
    length(empty_all) > 0 ~ "present but empty",
    !any_ok ~ "no usable age variable",
    TRUE ~ "feasible"
  )

  reason <- dplyr::case_when(
    status == "not collected" ~ paste(
      "absent:", paste(missing_all, collapse = ", ")
    ),
    status == "present but empty" ~ paste(
      "0% populated:", paste(empty_all, collapse = ", ")
    ),
    status == "no usable age variable" ~ paste(
      "none of:", paste(need_any, collapse = ", ")
    ),
    TRUE ~ NA_character_
  )

  tibble::tibble(
    indicator = req$indicator,
    recode = req$recode,
    feasible = status == "feasible",
    status = status,
    reason = reason,
    age_var = if (length(need_any) == 0) {
      NA_character_
    } else {
      c(need_any[need_any %in% usable], NA_character_)[1]
    }
  )
}


# ---------------------------------------------------------------------------
# the preflight --------------------------------------------------------------
# ---------------------------------------------------------------------------

#' Assess an archive before modelling
#'
#' @param path Root parquet directory.
#' @param country_code DHS two-letter country code.
#' @param recodes Recodes to inspect.
#' @param requirements Requirements table; defaults to
#'   `dhs_indicator_requirements()`.
#' @param min_pct Minimum percent populated for a variable to count.
#' @param surveys Optional subset of `dhs_archive_surveys()` output.
#'
#' @return A list with `surveys`, `coverage`, `codebook`, `feasibility`.
#' @export
dhs_preflight <- function(
  path,
  country_code,
  recodes = c("GE", "PR", "HR", "KR", "IR"),
  requirements = dhs_indicator_requirements(),
  min_pct = 0.1,
  surveys = NULL
) {
  if (is.null(surveys)) {
    surveys <- dhs_archive_surveys(path, country_code)
  }

  surveys <- dplyr::filter(surveys, !is.na(.data$survey_type))

  if (nrow(surveys) == 0) {
    cli::cli_abort("No surveys with a resolvable type for {country_code}")
  }

  cli::cli_h2("Preflight: {country_code}, {nrow(surveys)} survey{?s}")

  coverage <- list()
  feasibility <- list()

  for (i in seq_len(nrow(surveys))) {
    s_type <- surveys$survey_type[i]
    s_year <- surveys$DHSYEAR[i]
    s_key <- paste0(s_type, " ", s_year)

    cli::cli_h3(s_key)

    for (ft in recodes) {
      d <- tryCatch(
        dhs_read(
          path = path,
          file_type = ft,
          country_code = country_code,
          survey_year = s_year,
          survey_type = s_type,
          verbose = FALSE
        ),
        error = function(e) NULL
      )

      reqs <- dplyr::filter(requirements, .data$recode == ft)

      if (is.null(d) || nrow(d) == 0) {
        cli::cli_alert_warning("{ft}: not available")

        if (nrow(reqs) > 0) {
          feasibility[[paste(s_key, ft)]] <- tibble::tibble(
            survey_type = s_type,
            survey_year = s_year,
            indicator = reqs$indicator,
            recode = ft,
            feasible = FALSE,
            status = "recode missing",
            reason = paste(ft, "not in archive"),
            age_var = NA_character_
          )
        }
        next
      }

      cov <- .dhs_var_coverage(d) |>
        dplyr::mutate(
          survey_type = s_type,
          survey_year = s_year,
          recode = ft,
          n_rows = nrow(d),
          .before = 1
        )

      coverage[[paste(s_key, ft)]] <- cov

      if (nrow(reqs) > 0) {
        checks <- purrr::map(
          seq_len(nrow(reqs)),
          function(j) .dhs_check_requirement(cov, reqs[j, ], min_pct)
        ) |>
          purrr::list_rbind() |>
          dplyr::mutate(
            survey_type = s_type,
            survey_year = s_year,
            .before = 1
          )

        feasibility[[paste(s_key, ft)]] <- checks

        n_ok <- sum(checks$feasible)
        cli::cli_alert_info(
          "{ft}: {nrow(d)} rows, {n_ok}/{nrow(checks)} indicator{?s} feasible"
        )
      }
    }
  }

  coverage <- purrr::list_rbind(coverage)
  feasibility <- purrr::list_rbind(feasibility)

  # codebook: one row per variable, code and label, for drift detection
  codebook <- coverage |>
    dplyr::filter(!is.na(.data$value_labels)) |>
    dplyr::select(
      "survey_type", "survey_year", "recode", "variable", "value_labels"
    ) |>
    tidyr::separate_longer_delim("value_labels", delim = "; ") |>
    tidyr::separate_wider_delim(
      "value_labels",
      delim = "=",
      names = c("code", "code_label"),
      too_many = "merge",
      too_few = "align_start"
    )

  .dhs_report_infeasible(feasibility)

  list(
    surveys = surveys,
    coverage = coverage,
    codebook = codebook,
    feasibility = feasibility
  )
}


#' Print the indicators that cannot be computed, and why
#'
#' @keywords internal
#' @noRd
.dhs_report_infeasible <- function(feasibility) {
  if (nrow(feasibility) == 0) {
    return(invisible(NULL))
  }

  blocked <- dplyr::filter(feasibility, !.data$feasible)

  if (nrow(blocked) == 0) {
    cli::cli_alert_success("Every indicator is feasible for every survey")
    return(invisible(NULL))
  }

  cli::cli_h3("Indicators that cannot be computed")

  cli::cli_ul(paste0(
    blocked$survey_type, " ", blocked$survey_year, " - ",
    blocked$indicator, ": ", blocked$status,
    ifelse(is.na(blocked$reason), "", paste0(" (", blocked$reason, ")"))
  ))

  invisible(blocked)
}


#' Compare codebooks across surveys
#'
#' @description
#' Returns the codes whose meaning changed between surveys. Value codes
#' are not universal: `hml35 = 6` is "test undetermined" in the standard
#' recode but "other" in the 2023 Rwanda MIS, and a definition written
#' against one survey can quietly mean something else in the next.
#'
#' @param codebook The `codebook` element of `dhs_preflight()`.
#' @return A tibble of variables and codes with more than one label.
#' @export
dhs_codebook_drift <- function(codebook) {
  codebook |>
    dplyr::group_by(.data$variable, .data$code) |>
    dplyr::filter(dplyr::n_distinct(.data$code_label) > 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$variable, .data$code, .data$survey_year)
}


#' Write the preflight to disk
#'
#' @param preflight Output of `dhs_preflight()`.
#' @param path Output directory.
#' @param country_iso3 Country code used in the file name.
#' @export
dhs_preflight_write <- function(preflight, path, country_iso3) {
  fs::dir_create(path)

  for (nm in c("surveys", "coverage", "codebook", "feasibility")) {
    obj <- preflight[[nm]]

    if (is.null(obj) || nrow(obj) == 0) {
      next
    }

    sntutils::write_snt_data(
      obj = list(
        data = obj,
        data_dict = sntutils::build_dictionary(data = obj)
      ),
      data_name = glue::glue("{country_iso3}_preflight_{nm}"),
      path = path,
      file_formats = c("xlsx", "qs2")
    )
  }

  invisible(preflight)
}
