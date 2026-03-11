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
    base_path = Sys.getenv("AHADI_ONEDRIVE_PROJECT"),
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

survey_years_dhs <- survey_years_dhs[1]
survey_years_dhs <- NULL
survey_years_mis <- sntmethods::dhs_read(
  path = path_dhs_parquet,
  file_type = "GE",
  survey_type = "MIS",
  country_code = country_iso2
) |>
  dplyr::pull(DHSYEAR) |>
  unique()

## ---------------------------------------------------------------------------##
# 2) Read DHS + MIS data for all years -----------------------------------------
## ---------------------------------------------------------------------------##

read_dhs_bundle <- function(survey_year, survey_type) {
  cli::cli_h2(
    glue::glue("Reading {survey_type} data for {survey_year}")
  )

  list(
    ge = sntmethods::dhs_read(
      path = path_dhs_parquet,
      file_type = "GE",
      survey_type = survey_type,
      country_code = country_iso2,
      survey_year = survey_year
    ),
    pr = sntmethods::dhs_read(
      path = path_dhs_parquet,
      file_type = "PR",
      survey_type = survey_type,
      country_code = country_iso2,
      survey_year = survey_year
    ),
    hr = sntmethods::dhs_read(
      path = path_dhs_parquet,
      file_type = "HR",
      survey_type = survey_type,
      country_code = country_iso2,
      survey_year = survey_year
    ),
    kr = sntmethods::dhs_read(
      path = path_dhs_parquet,
      file_type = "KR",
      survey_type = survey_type,
      country_code = country_iso2,
      survey_year = survey_year
    ),
    ir = sntmethods::dhs_read(
      path = path_dhs_parquet,
      file_type = "IR",
      survey_type = survey_type,
      country_code = country_iso2,
      survey_year = survey_year
    )
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

mis_raw_dictionary <-
  mis_bundles |>
  purrr::imap_dfr(\(year_bundle, year_name) {
    year_value <- readr::parse_integer(year_name)

    year_bundle |>
      purrr::imap_dfr(\(dhs_dataset, dataset_name) {
        sntmethods::make_dhs_raw_dictionary(
          dhs_dataset |> dplyr::slice(1:2)
        ) |>
          dplyr::mutate(
            surevey_type = "MIS",
            survey_year = year_value,
            dataset = toupper(dataset_name),
            .before = 1
          )
      })
  })

dhs_raw_dictionary <-
  dhs_bundles |>
  purrr::imap_dfr(\(year_bundle, year_name) {
    year_value <- readr::parse_integer(year_name)

    year_bundle |>
      purrr::imap_dfr(\(dhs_dataset, dataset_name) {
        sntmethods::make_dhs_raw_dictionary(
          dhs_dataset |> dplyr::slice(1:2)
        ) |>
          dplyr::mutate(
            surevey_type = "DHS",
            survey_year = year_value,
            dataset = toupper(dataset_name),
            .before = 1
          )
      })
  })

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

calc_dhs_indicators <- function(dhs, shp_admin, admin_levels) {
  results <- list()

  cli::cli_h3("Fever prevalence")
  results$fever <- sntmethods::calc_fever_dhs(
    dhs_kr = dhs$kr,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  cli::cli_h3("Care-seeking behaviour (CSB)")
  results$csb <- sntmethods::calc_csb_dhs(
    dhs_kr = dhs$kr,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  cli::cli_h3("Malaria diagnosis")
  results$malaria_dx <- sntmethods::calc_malaria_dx_dhs(
    dhs_kr = dhs$kr,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  cli::cli_h3("Antimalarial treatment")
  results$antimalarial <- sntmethods::calc_antimalarial_dhs(
    dhs_kr = dhs$kr,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  cli::cli_h3("ACT treatment")
  results$act <- sntmethods::calc_act_dhs(
    dhs_kr = dhs$kr,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  cli::cli_h3("PfPR")
  results$pfpr <- sntmethods::calc_pfpr_dhs(
    dhs_pr = dhs$pr,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  cli::cli_h3("ITN ownership and use")
  results$itn <- sntmethods::calc_itn_dhs(
    dhs_hr = dhs$hr,
    dhs_pr = dhs$pr,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  cli::cli_h3("Household wealth")
  results$wealth <- sntmethods::calc_wealth_dhs(
    dhs_hr = dhs$hr,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  cli::cli_h3("IPTp coverage")
  results$iptp <- sntmethods::calc_iptp_dhs(
    dhs_ir = dhs$ir,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  cli::cli_h3("Severe anemia prevalence")
  results$anemia <- sntmethods::calc_severe_anemia_dhs(
    dhs_pr = dhs$pr,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels,
    altitude_adjusted = FALSE
  )

  cli::cli_h3("Under-five mortality rate")
  results$u5mr <- sntmethods::calc_u5mr_dhs(
    dhs_kr = dhs$kr,
    period_years = 5,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  cli::cli_h3("EPI vaccination coverage")
  results$epi <- sntmethods::calc_epi_dhs(
    dhs_kr = dhs$kr,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  cli::cli_h3("IRS coverage")
  results$irs <- sntmethods::calc_irs_dhs(
    dhs_hr = dhs$hr,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  cli::cli_h3("SMC receipt")
  results$smc <- sntmethods::calc_smc_dhs(
    dhs_kr = dhs$kr,
    gps_data = dhs$ge,
    shapefile = shp_admin,
    admin_level = admin_levels
  )

  # Stack all indicators per admin level
  purrr::map(
    purrr::set_names(admin_levels),
    function(lvl) {
      purrr::map_df(results, function(res) {
        if (lvl %in% names(res)) res[[lvl]] else NULL
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
