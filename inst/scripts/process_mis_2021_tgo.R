###########################  Process MIS 2021 data  ############################

cli::cli_h1("Process MIS 2021 Data")

## ---------------------------------------------------------------------------##
# 1) Set-up paths and parameters -----------------------------------------------
## ---------------------------------------------------------------------------##

devtools::load_all()

country_iso2 <- "TG"
country_iso3 <- "tgo"

path_dhs_parquet <- here::here(sntmethods::ahadi_path(), "01_data/parquet")

paths <-
  sntutils::setup_project_paths(
    base_path = Sys.getenv("AHADI_ONEDRIVE_PROJECT"),
    quiet = TRUE
  )

## ---------------------------------------------------------------------------##
# 2) Load data -----------------------------------------------------------------
## ---------------------------------------------------------------------------##

kr <- sntutils::read(
  here::here(paths$dhs, "raw", "2020 MIS/enfant.dta")
)

## ---------------------------------------------------------------------------##
# 3) Calculate CSB -------------------------------------------------------------
## ---------------------------------------------------------------------------##

# Required variables in dhs_kr:
#   v021  - cluster ID (primary sampling unit)
#   v005  - survey sample weight
#   v022  - sample stratum
#   hw1   - child's age in months
#   h22   - had fever in last 2 weeks (0/1)
#   b5    - child is alive (optional)
#   h32*  - treatment source variables (h32a, h32b, h32j, etc.)
#   v024  - region (used as grouping when no GPS data)

# No GPS data available — use v024 (region) as grouping variable
csb_results <- calc_csb_dhs(
  dhs_kr = kr,
  region_var = "v024"
)

csb_results$data
