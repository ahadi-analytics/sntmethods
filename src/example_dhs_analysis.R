devtools::load_all()

# Set country and survey parameters
iso2 <- "SL" # Sierra Leone
iso3 <- "SLE"
survey_year <- 2019

# set path fr dhs data
dhs_path <- here::here(ahadi_path(), "01_data/parquet")


# Download shapefile with admin boundaries
shapefile <- sntutils::download_shapefile(iso3)

# get survey coords data
gps_data_mis <- dhs_read(
  path = dhs_path,
  file_type = 'GE',
  survey_type = "MIS",
  country_code = iso2,
  survey_year = 2016
)

gps_data_dhs <- dhs_read(
  path = dhs_path,
  file_type = 'GE',
  survey_type = "DHS",
  country_code = iso2,
  survey_year = 2019
)

# get survey personal records data
pr_data <- dhs_read(
  path = dhs_path,
  file_type = 'PR',
  survey_type = "MIS",
  country_code = iso2,
  survey_year = 2016
)

# get survey children records data
kr_data <- dhs_read(
  path = dhs_path,
  file_type = 'KR',
  survey_type = "DHS",
  country_code = iso2,
  survey_year = 2019
)

pfpr_results <- calc_pfpr_dhs(
  dhs_pr = pr_data,
  gps_data = gps_data_mis,
  shapefile = shapefile,
  admin_level = c("adm1", "adm2"),
  join_nearest = TRUE
)

u5mr_results <- calc_u5mr_dhs(
  dhs_kr = kr_data,
  period_years = 5,
  gps_data = gps_data_dhs,
  shapefile = shapefile,
  admin_level = c("adm1", "adm2"),
  join_nearest = TRUE
)

csb_results <- calc_csb_dhs(
  dhs_kr = kr_data,
  gps_data = gps_data_dhs,
  shapefile = shapefile,
  admin_level = c("adm1", "adm2"),
  join_nearest = TRUE
)
