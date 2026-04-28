#' Prepare a Custom CSB Partition for MBG Analysis
#'
#' Internal MBG-prep function that computes one user-defined, mutually
#' exclusive care-seeking partition from a DHS Children's Recode (KR)
#' dataset. The partition is defined at runtime by the user via the
#' `custom_csb_indicator` argument and produces three derived cluster-level
#' indicators:
#'
#' \itemize{
#'   \item `<name>_dhis`     - sought care at any user-listed DHIS source
#'   \item `<name>_nondhis`  - sought care at any non-DHIS source (and
#'     never at a DHIS source)
#'   \item `<name>_untreat`  - did not seek care at any positive `h32*`
#'     source, or only at user-listed untreat sources
#' }
#'
#' The triple is mutually exclusive at the child level: each febrile U5
#' child is assigned to exactly one bucket via the priority rule
#' `dhis > nondhis > untreat`.
#'
#' @param dhs_kr DHS Children's Recode (KR) dataset (haven labels intact).
#' @param gps_data DHS GPS dataset with cluster coordinates.
#' @param custom_csb_indicator A validated spec list with fields `name`,
#'   `dhis_locs`, `nondhis_locs`, `untreat_locs`. See
#'   \code{run_mbg_pipeline()} for the public interface.
#' @param survey_vars Named list mapping DHS variable names. Must include
#'   `cluster`, `age`, `fever`.
#' @param gps_vars Named list for GPS variable mapping with keys `cluster`,
#'   `lat`, `lon`.
#'
#' @return A named list of data.tables (one per derived indicator), each
#'   with columns `cluster_id`, `indicator`, `samplesize`, `x`, `y`. The
#'   list is keyed by the derived indicator codes
#'   (`<name>_dhis`, `<name>_nondhis`, `<name>_untreat`).
#'
#' @details
#' Step 1 builds a per-survey label-to-bucket lookup from the original
#' KR file (where haven labels are still intact). Step 2 reuses
#' \code{.prepare_csb_data()} with `csb_priority_method = "all"` to obtain
#' the febrile-U5 cleaned dataset (the built-in `csb_priority_method`
#' setting does not apply to the custom partition because the custom
#' triple is always mutually exclusive by construction). Step 3 classifies
#' each febrile child into exactly one custom bucket. Step 4 aggregates
#' each bucket to cluster level via the shared
#' \code{.aggregate_to_mbg_clusters()} helper.
#'
#' @noRd
calc_csb_custom_mbg <- function(
  dhs_kr,
  gps_data,
  custom_csb_indicator,
  survey_vars = list(
    cluster = "v001",
    age = "hw1",
    fever = "h22"
  ),
  gps_vars = list(
    cluster = "DHSCLUST",
    lat = "LATNUM",
    lon = "LONGNUM"
  )
) {
  if (!is.data.frame(dhs_kr)) {
    cli::cli_abort("`dhs_kr` must be a data.frame or tibble")
  }
  if (!is.data.frame(gps_data)) {
    cli::cli_abort("`gps_data` must be a data.frame or tibble")
  }

  spec <- .validate_custom_csb_indicator_spec(custom_csb_indicator)
  prefix <- spec$name
  out_names <- .custom_csb_indicator_names(spec)

  # ---- Build the per-survey custom slot-to-bucket classification ----
  # Labels must be read from the ORIGINAL `dhs_kr` (before any zap_labels()
  # downstream) so haven labels are still intact.
  classification <- .build_custom_csb_classification(dhs_kr, spec)

  # ---- Prepare GPS clusters ----
  gps_clean <- .prepare_gps_data(gps_data, gps_vars)

  # ---- Prepare febrile U5 dataset ----
  # We use csb_priority_method = "all" because the built-in priority
  # resolution doesn't matter for the custom triple: we re-derive
  # mutually exclusive buckets directly from raw h32 columns below.
  kr_fever <- .prepare_csb_data(
    dhs_kr = dhs_kr,
    survey_vars = survey_vars,
    include_survey_vars = FALSE,
    csb_priority_method = "all"
  )

  if (is.null(kr_fever) || nrow(kr_fever) == 0) {
    cli::cli_alert_warning(
      "Custom CSB ({.val {prefix}}): no febrile U5 records; returning empty result."
    )
    return(stats::setNames(vector("list", length(out_names)), out_names))
  }

  # h32 columns present in the cleaned febrile dataset
  h32_cols <- grep("^h32[a-z0-9]+$", names(kr_fever), value = TRUE)
  h32_cols <- setdiff(h32_cols, c("h32y", "h32z"))

  # ---- Classify into <prefix>_dhis / _nondhis / _untreat ----
  kr_fever <- .classify_custom_csb_from_h32(
    data = kr_fever,
    h32_cols = h32_cols,
    classification = classification,
    prefix = prefix
  )

  # ---- Aggregate each derived column to cluster level ----
  results <- list()
  for (out_name in out_names) {
    if (!out_name %in% names(kr_fever)) next
    dt <- .aggregate_to_mbg_clusters(
      individual_data = kr_fever,
      indicator_col = out_name,
      gps_clean = gps_clean,
      result_name = out_name
    )
    if (!is.null(dt)) {
      results[[out_name]] <- dt
    }
  }

  if (length(results) == 0) {
    cli::cli_alert_warning(
      "Custom CSB ({.val {prefix}}): no valid clusters produced."
    )
  }

  results
}
