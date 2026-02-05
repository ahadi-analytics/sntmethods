###########################  Process DHS data  #################################

cli::cli_h1("Process DHS Data")

## ---------------------------------------------------------------------------##
# 1) Set-up paths and parameters -----------------------------------------------
## ---------------------------------------------------------------------------##

devtools::load_all()

# country metadata
country_iso2 <- "BF"
country_iso3 <- "BFA"
survey_year_dhs <- 2021
survey_year_mis <- 2017

# admin levels to process
admin_levels <- c("adm1", "adm2")

# paths
path_dhs_parquet <- here::here(ahadi_path(), "01_data/parquet")

# set paths
paths <- here::here(
  "/Users/mohamedyusuf/Downloads/burkina_MAP"
)

# shapefile
shp_admin <- sntutils::read(
  here::here(paths,
  "70ds_from_nmcp_2019_numbered.shp")
) |>
  dplyr::mutate(
    adm0 = "BURKINA FASO",
    adm1 = NOMREGION,
    adm2 = NOMPROVINC,
    adm3 = NOMDEP
  ) |>
  dplyr::select(adm0, adm1, adm2, adm3)

## ---------------------------------------------------------------------------##
# 2) Get DHS data --------------------------------------------------------------
## ---------------------------------------------------------------------------##

ge_mis <- dhs_read(
  path = path_dhs_parquet,
  file_type = "GE",
  survey_type = "MIS",
  country_code = country_iso2,
  survey_year = survey_year_mis
)

ge_dhs <- dhs_read(
  path = path_dhs_parquet,
  file_type = "GE",
  survey_type = "DHS",
  country_code = country_iso2,
  survey_year = survey_year_dhs
)

pr_mis <- dhs_read(
  path = path_dhs_parquet,
  file_type = "PR",
  survey_type = "MIS",
  country_code = country_iso2,
  survey_year = survey_year_mis
)

pr_dhs <- dhs_read(
  path = path_dhs_parquet,
  file_type = "PR",
  survey_type = "DHS",
  country_code = country_iso2,
  survey_year = survey_year_dhs
)

hr_dhs <- dhs_read(
  path = path_dhs_parquet,
  file_type = "HR",
  survey_type = "DHS",
  country_code = country_iso2,
  survey_year = survey_year_dhs
)

kr_dhs <- dhs_read(
  path = path_dhs_parquet,
  file_type = "KR",
  survey_type = "DHS",
  country_code = country_iso2,
  survey_year = survey_year_dhs
)

ir_dhs <- dhs_read(
  path = path_dhs_parquet,
  file_type = "IR",
  survey_type = "DHS",
  country_code = country_iso2,
  survey_year = survey_year_dhs
)

## ---------------------------------------------------------------------------##
# 3–4) Calculate metrics at adm1 and adm2 --------------------------------------
## ---------------------------------------------------------------------------##

admin_levels <- c("adm1", "adm2")

dhs_data_by_level <- list()
dhs_dict_by_level <- list()

for (admin_level in admin_levels) {
  cli::cli_h2(glue::glue("Processing DHS indicators at {admin_level}"))

  pfpr_results <- calc_pfpr_dhs(
    dhs_pr = pr_mis,
    gps_data = ge_mis,
    shapefile = shp_admin,
    admin_level = admin_level
  )

  u5mr_results <- calc_u5mr_dhs(
    dhs_kr = kr_dhs,
    period_years = 5,
    gps_data = ge_dhs,
    shapefile = shp_admin,
    admin_level = admin_level
  )

  csb_results <- calc_csb_dhs(
    dhs_kr = kr_dhs,
    gps_data = ge_dhs,
    shapefile = shp_admin,
    admin_level = admin_level
  )

  itn_results <- calc_itn_dhs(
    dhs_hr = hr_dhs,
    dhs_pr = pr_dhs,
    gps_data = ge_dhs,
    shapefile = shp_admin,
    admin_level = admin_level,
    # Enable age stratification to get use_if_access indicator
    age_breaks = c(0, 5, 15, Inf),
    age_labels = c("u5", "5_14", "15plus")
  )

  wealth_results <- calc_wealth_dhs(
    dhs_hr = hr_dhs,
    gps_data = ge_dhs,
    shapefile = shp_admin,
    admin_level = admin_level
  )

  iptp_results <- calc_iptp_dhs(
    dhs_ir = ir_dhs,
    gps_data = ge_dhs,
    shapefile = shp_admin,
    admin_level = admin_level
  )

  anemia_results <- calc_severe_anemia_dhs(
    dhs_pr = pr_dhs,
    gps_data = ge_dhs,
    shapefile = shp_admin,
    admin_level = admin_level,
    altitude_adjusted = FALSE
  )

  join_key <- admin_level

  dhs_indicators <-
    pfpr_results$data |>
    dplyr::left_join(u5mr_results$data, by = join_key) |>
    dplyr::left_join(csb_results$data, by = join_key) |>
    dplyr::left_join(itn_results$data, by = join_key) |>
    dplyr::left_join(wealth_results$data, by = join_key) |>
    dplyr::left_join(iptp_results$data, by = join_key) |>
    dplyr::left_join(anemia_results$data, by = join_key)

  dhs_data_by_level[[admin_level]] <- dhs_indicators
}

## ---------------------------------------------------------------------------##
# 5) Build single final object and save once ----------------------------------
## ---------------------------------------------------------------------------##

dhs_dict <- sntutils::build_dictionary(
  dhs_data_by_level[["adm2"]],
  language = "fr"
) |>
  dplyr::select(variable, type, label_en, label_fr)

final_dhs_output <- list(
  data_adm1 = dhs_data_by_level[["adm1"]],
  data_adm2 = dhs_data_by_level[["adm2"]],
  dict = dhs_dict
)

sntutils::write_snt_data(
  obj = final_dhs_output,
  data_name = glue::glue("{country_iso3}_dhs_indicators"),
  path = here::here(paths),
  file_formats = c("qs2", "xlsx")
)
