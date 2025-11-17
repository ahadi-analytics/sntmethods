###########################  Process DHS data  #################################

cli::cli_h1("Process DHS Data")

## ---------------------------------------------------------------------------##
# 1) Set-up paths and parameters -----------------------------------------------
## ---------------------------------------------------------------------------##

devtools::load_all()

# country metadata
country_iso2 <- "SL"
country_iso3 <- "SLE"
survey_year_dhs <- 2019
survey_year_mis <- 2016
# aggregation level
# (rememebr survey is doen at adm1 level)
admin_level <- c("adm1")

# paths
path_dhs_parquet <- here::here(ahadi_path(), "01_data/parquet")

# shapefile
shp_admin <- sntutils::download_shapefile(country_iso3)

## ---------------------------------------------------------------------------##
# 2) Get DHS data --------------------------------------------------------------
## ---------------------------------------------------------------------------##

# survey geolocation data -----------------------------------------------------
ge_mis_2016 <- dhs_read(
  path = path_dhs_parquet,
  file_type = "GE",
  survey_type = "MIS",
  country_code = country_iso2,
  survey_year = survey_year_mis
)

ge_dhs_2019 <- dhs_read(
  path = path_dhs_parquet,
  file_type = "GE",
  survey_type = "DHS",
  country_code = country_iso2,
  survey_year = survey_year_dhs
)

# survey household & individual data -----------------------------------------
pr_mis_2016 <- dhs_read(
  path = path_dhs_parquet,
  file_type = "PR",
  survey_type = "MIS",
  country_code = country_iso2,
  survey_year = survey_year_mis
)

pr_dhs_2019 <- dhs_read(
  path = path_dhs_parquet,
  file_type = "PR",
  survey_type = "DHS",
  country_code = country_iso2,
  survey_year = survey_year_dhs
)

hr_dhs_2019 <- dhs_read(
  path = path_dhs_parquet,
  file_type = "HR",
  survey_type = "DHS",
  country_code = country_iso2,
  survey_year = survey_year_dhs
)

kr_dhs_2019 <- dhs_read(
  path = path_dhs_parquet,
  file_type = "KR",
  survey_type = "DHS",
  country_code = country_iso2,
  survey_year = survey_year_dhs
)

## ---------------------------------------------------------------------------##
# 3) Calculate metrics from DHS data -------------------------------------------
## ---------------------------------------------------------------------------##

# pfpr -------------------------------------------------------------------------
pfpr_results <- calc_pfpr_dhs(
  dhs_pr = pr_mis_2016,
  gps_data = ge_mis_2016,
  shapefile = shp_admin,
  admin_level = admin_level
)

# u5mr ------------------------------------------------------------------------
u5mr_results <- calc_u5mr_dhs(
  dhs_kr = kr_dhs_2019,
  period_years = 5,
  gps_data = ge_dhs_2019,
  shapefile = shp_admin,
  admin_level = admin_level
)

# csb --------------------------------------------------------------------------
csb_results <- calc_csb_dhs(
  dhs_kr = kr_dhs_2019,
  gps_data = ge_dhs_2019,
  shapefile = shp_admin,
  admin_level = admin_level
)

devtools::load_all()

# itn --------------------------------------------------------------------------
itn_results <- calc_itn_dhs(
  dhs_hr = hr_dhs_2019,
  dhs_pr = pr_dhs_2019,
  gps_data = ge_dhs_2019,
  shapefile = shp_admin,
  admin_level = admin_level
)

# wealth quantile --------------------------------------------------------------

wealth_results <- calc_wealth_dhs(
  dhs_hr = hr_dhs_2019,
  gps_data = ge_dhs_2019,
  shapefile = shp_admin,
  admin_level = admin_level
)
