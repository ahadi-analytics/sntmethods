###########################  Process DHS data  #################################

cli::cli_h1("Process DHS Data")

## ---------------------------------------------------------------------------##
# 1) Set-up paths and parameters -----------------------------------------------
## ---------------------------------------------------------------------------##

country_iso2 <- "TG"
country_iso3 <- "tgo"

admin_levels <- c("adm1", "adm2")

path_dhs_parquet <- here::here(sntmethods::ahadi_path(), "01_data/parquet")

paths <-
  sntutils::setup_project_paths(
base_path = "/Users/mohamedyusuf/Library/CloudStorage/OneDrive-SharedLibraries-AppliedHealthAnalyticsforDeliveryandInnovationInc/Togo SNT 2025 - 2025_SNT/tgo-snt-2025",
    # base_path = Sys.getenv("AHADI_ONEDRIVE_PROJECT"),
    quiet = TRUE
  )

shp_admin <- sntutils::read_snt_data(
  path = here::here(paths$admin_shp, "processed"),
  data_name = glue::glue("{country_iso3}_shp_list"),
  file_formats = c("qs2")
)$final_spat_vec$adm2

# get the survey years
survey_years_dhs <- sntmethods::dhs_read(
  path = path_dhs_parquet,
  file_type = "GE",
  survey_type = "DHS",
  country_code = country_iso2
) |>
  dplyr::pull(DHSYEAR) |>
  unique()

# survey_years_dhs <- survey_years_dhs[1]
# survey_years_dhs <- NULL
survey_years_mis <- sntmethods::dhs_read(
  path = path_dhs_parquet,
  file_type = "GE",
  survey_type = "MIS",
  country_code = country_iso2
) |>
  dplyr::filter(DHSYEAR == 2017) |>
  dplyr::pull(DHSYEAR) |>
  unique()

devtools::load_all()

## ---------------------------------------------------------------------------##
# 2) Read DHS + MIS data for all years -----------------------------------------
## ---------------------------------------------------------------------------##

# Read a single DHS file_type defensively: missing directories, empty
# pulls or any read error degrade to NULL with a warning instead of
# aborting. This lets the loop continue across surveys when one or
# more recodes (or one specific year) is unavailable.
.try_read <- function(file_type, survey_year, survey_type) {
  tryCatch(
    sntmethods::dhs_read(
      path         = path_dhs_parquet,
      file_type    = file_type,
      survey_type  = survey_type,
      country_code = country_iso2,
      survey_year  = survey_year,
      verbose      = FALSE
    ),
    error = function(e) {
      cli::cli_alert_warning(
        "Skipping {.val {file_type}} ({survey_type} {survey_year}): {conditionMessage(e)}"
      )
      NULL
    }
  )
}

read_dhs_bundle <- function(survey_year, survey_type) {
  cli::cli_h2(
    glue::glue("Reading {survey_type} data for {survey_year}")
  )

  list(
    ge = .try_read("GE", survey_year, survey_type),
    pr = .try_read("PR", survey_year, survey_type),
    hr = .try_read("HR", survey_year, survey_type),
    kr = .try_read("KR", survey_year, survey_type),
    ir = .try_read("IR", survey_year, survey_type)
  )
}

dhs_bundles <-
  purrr::map(
    survey_years_dhs,
    ~ read_dhs_bundle(.x, "DHS")
  ) |>
  rlang::set_names(survey_years_dhs)

mis_bundles <-
  purrr::map(
    survey_years_mis,
    ~ read_dhs_bundle(.x, "MIS")
  ) |>
  rlang::set_names(survey_years_mis)

# Build dictionary defensively: skip NULL / zero-row recodes (e.g. when a
# given file_type was unavailable for that survey year) so the script
# never crashes on a missing slice.
.build_dict <- function(bundles, label) {
  purrr::imap_dfr(bundles, \(year_bundle, year_name) {
    year_value <- readr::parse_integer(year_name)
    purrr::imap_dfr(year_bundle, \(dhs_dataset, dataset_name) {
      if (is.null(dhs_dataset) || nrow(dhs_dataset) == 0) return(NULL)
      tryCatch(
        sntmethods::make_dhs_raw_dictionary(
          dhs_dataset |> dplyr::slice(1:2)
        ) |>
          dplyr::mutate(
            surevey_type = label,
            survey_year  = year_value,
            dataset      = toupper(dataset_name),
            .before = 1
          ),
        error = function(e) {
          cli::cli_alert_warning(
            "Dictionary skip ({label} {year_value} {toupper(dataset_name)}): {conditionMessage(e)}"
          )
          NULL
        }
      )
    })
  })
}

mis_raw_dictionary <- .build_dict(mis_bundles, "MIS")
dhs_raw_dictionary <- .build_dict(dhs_bundles, "DHS")

# save dictionary
dplyr::bind_rows(
  dhs_raw_dictionary,
  mis_raw_dictionary
) |>
  sntutils::write(
    here::here(
      paths$dhs,
      "processed",
      glue::glue("{country_iso3}_dhs_mis_dictionary.csv")
    )
  )

## ---------------------------------------------------------------------------##
# 3) Calculate all DHS indicators per survey -----------------------------------
## ---------------------------------------------------------------------------##

# All calc_*_dhs() functions return list(adm0, adm1, adm2, ...) in long format
# with standardised columns: survey_id, iso3, iso2, survey_type, survey_year,
# adm0, [adm1], [adm2], type, geo_source, point, ci_l, ci_u, numerator,
# denominator, indicator, indicator_code, numerator_description,
# denominator_description, denominator_code.

admin_levels <- c("adm0", "adm1", "adm2")

# Defensive wrapper: skip the calc when any required recode is missing
# / empty, and trap any error from the calc function itself so a single
# failure (e.g. early-DHS surveys missing v021 / h22 / b7) does not
# break the loop across surveys.
.try_calc <- function(label, required_recodes, fn) {
  cli::cli_h3(label)
  missing <- vapply(
    required_recodes, function(r) is.null(r) || !inherits(r, "data.frame") ||
      nrow(r) == 0, logical(1)
  )
  if (any(missing)) {
    cli::cli_alert_warning(
      "Skipping {.val {label}}: required recode(s) unavailable."
    )
    return(NULL)
  }
  tryCatch(
    fn(),
    error = function(e) {
      cli::cli_alert_warning(
        "Skipping {.val {label}}: {conditionMessage(e)}"
      )
      NULL
    }
  )
}

calc_dhs_indicators <- function(dhs, shp_admin, admin_levels) {
  results <- list()

  results$fever <- .try_calc(
    "Fever prevalence", list(dhs$kr),
    function() sntmethods::calc_fever_dhs(
      dhs_kr = dhs$kr, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  results$csb <- .try_calc(
    "Care-seeking behaviour (CSB)", list(dhs$kr),
    function() sntmethods::calc_csb_dhs(
      dhs_kr = dhs$kr, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  results$malaria_dx <- .try_calc(
    "Malaria diagnosis", list(dhs$kr),
    function() sntmethods::calc_malaria_dx_dhs(
      dhs_kr = dhs$kr, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  results$antimalarial <- .try_calc(
    "Antimalarial treatment", list(dhs$kr),
    function() sntmethods::calc_antimalarial_dhs(
      dhs_kr = dhs$kr, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  results$act <- .try_calc(
    "ACT treatment", list(dhs$kr),
    function() sntmethods::calc_act_dhs(
      dhs_kr = dhs$kr, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  results$pfpr <- .try_calc(
    "PfPR", list(dhs$pr),
    function() sntmethods::calc_pfpr_dhs(
      dhs_pr = dhs$pr, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  results$itn <- .try_calc(
    "ITN ownership and use", list(dhs$hr, dhs$pr),
    function() sntmethods::calc_itn_dhs(
      dhs_hr = dhs$hr, dhs_pr = dhs$pr, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  results$wealth <- .try_calc(
    "Household wealth", list(dhs$hr),
    function() sntmethods::calc_wealth_dhs(
      dhs_hr = dhs$hr, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  results$iptp <- .try_calc(
    "IPTp coverage", list(dhs$ir),
    function() sntmethods::calc_iptp_dhs(
      dhs_ir = dhs$ir, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  results$anemia <- .try_calc(
    "Severe anemia prevalence", list(dhs$pr),
    function() sntmethods::calc_severe_anemia_dhs(
      dhs_pr = dhs$pr, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels,
      altitude_adjusted = FALSE
    )
  )

  results$u5mr <- .try_calc(
    "Under-five mortality rate", list(dhs$kr),
    function() sntmethods::calc_u5mr_dhs(
      dhs_kr = dhs$kr, period_years = 5, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  results$epi <- .try_calc(
    "EPI vaccination coverage", list(dhs$kr),
    function() sntmethods::calc_epi_dhs(
      dhs_kr = dhs$kr, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  results$irs <- .try_calc(
    "IRS coverage", list(dhs$hr),
    function() sntmethods::calc_irs_dhs(
      dhs_hr = dhs$hr, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  results$smc <- .try_calc(
    "SMC receipt", list(dhs$kr),
    function() sntmethods::calc_smc_dhs(
      dhs_kr = dhs$kr, gps_data = dhs$ge,
      shapefile = shp_admin, admin_level = admin_levels
    )
  )

  # Drop indicators that were skipped entirely (NULL).
  results <- purrr::compact(results)

  # Stack all indicators per admin level. Within an indicator's result,
  # individual admin levels may also be missing (e.g. when a calc only
  # returns adm0/adm1) -- fall back to NULL in that case.
  purrr::map(
    purrr::set_names(admin_levels),
    function(lvl) {
      purrr::map_df(results, function(res) {
        if (!is.null(res) && lvl %in% names(res)) res[[lvl]] else NULL
      })
    }
  )
}

## ---------------------------------------------------------------------------##
# 4) Run across all DHS + MIS surveys and combine -----------------------------
## ---------------------------------------------------------------------------##

all_bundles <- c(dhs_bundles, mis_bundles)

all_survey_results <- purrr::imap(all_bundles, function(dhs, year_label) {
  cli::cli_h2(glue::glue("Calculating indicators for {year_label}"))
  calc_dhs_indicators(dhs, shp_admin, admin_levels)
})

# Combine across surveys: bind_rows within each admin level
dhs_output <- purrr::map(
  purrr::set_names(admin_levels),
  function(lvl) {
    purrr::map_df(all_survey_results, function(survey_res) {
      if (lvl %in% names(survey_res)) survey_res[[lvl]] else NULL
    })
  }
)

cli::cli_alert_success("Combined all DHS/MIS indicators into long-format output")

## ---------------------------------------------------------------------------##
# 5) Save final output ---------------------------------------------------------
## ---------------------------------------------------------------------------##

sntutils::write_snt_data(
  obj = dhs_output,
  data_name = glue::glue("{country_iso3}_dhs_indicators"),
  path = here::here(paths$dhs, "processed"),
  file_formats = c("qs2", "xlsx")
)
