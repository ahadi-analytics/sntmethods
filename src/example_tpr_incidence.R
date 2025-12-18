
# set paths for projects
paths <- sntutils::setup_project_paths(
  "/Users/mohamedyusuf/Library/CloudStorage/OneDrive-SharedLibraries-AppliedHealthAnalyticsforDeliveryandInnovationInc/Burundi SNT 2025 - Documents/2025_SNT/bdi_snt_2025"
)

# set up iso3 code for cntry
iso3 = "bdi"

# set paths for save
val_plot_path <- here::here(paths$validation_plots, "routine")
cache_path <- here::here(paths$cache)

# import pop data
pop_df <- sntutils::read_snt_data(
  path = here::here(paths$pop_national, "processed"),
  data_name = glue::glue("{iso3}_pop_processed_2023-2025_adm2"),
  file_formats = c("qs2")
)$pop_data_adm2 |>
  dplyr::mutate(year = as.integer(as.character(year))) |>
  sntutils::extrapolate_pop(
    year_col = "year",
    pop_col = "pop",
    group_cols = c("adm0", "adm1", "adm2"),
    years_to_extrap = c(2020:2025)
  )


# import routine data
dhis2_hf <- sntutils::read_snt_data(
  path = here::here(paths$dhis2, "processed"),
  data_name = glue::glue(
    "{iso3}_dhis2_processed_2020-2025"
  ),
  file_formats = c("qs2")
)$data |>
dplyr::distinct(record_id, .keep_all = TRUE) |>
# dplyr::mutate(year = as.factor(year)) |>
dplyr::left_join(
  pop_df,
  by = c("adm0", "adm1", "adm2", "year")
)

shp_admin <- sntutils::read_snt_data(
  path = here::here(paths$admin_shp, "processed"),
  data_name = glue::glue("{iso3}_adm0_adm2_pre2023"),
  file_formats = c("qs2")
)$final_spat_vec$adm2

# select the key vars we want
vars_of_interest <- c("susp", "conf", "test", "pres")

# Get and prep CSB data --------------------------------------------------------

csb_dhs <- sntutils::read_snt_data(
  path = here::here(paths$dhs, "raw"),
  data_name = glue::glue("CSB_BDI_2010-12-16"),
  file_formats = c("xlsx")
) |>
  dplyr::filter(
    INDICATOR %in% c("CSB_NOTREATMENT", "CSB_PRIVATE", "CSB_PUBLIC"),
    surveyyear == 2016
  ) |>
  dplyr::mutate(
    adm1 = region |>
      tolower() |>
      dplyr::recode(
        !!!c(
          "bujumbura" = "BUJUMBURA-MAIRIE",
          "bujumbura maire" = "BUJUMBURA-MAIRIE",
          "bujumbura mairie" = "BUJUMBURA-MAIRIE",
          "bujumbura rural" = "BUJUMBURA-RURAL",
          "cankuzo" = "CANKUZO",
          "karusi" = "KARUZI",
          "ruyigi" = "RUYIGI",
          "rutana" = "RUTANA",
          "gitega" = "GITEGA",
          "ngozi" = "NGOZI",
          "cibitoke" = "CIBITOKE",
          "muramvya" = "MURAMVYA",
          "mwaro" = "MWARO",
          "kirundo" = "KIRUNDO",
          "kayanza" = "KAYANZA",
          "bubanza" = "BUBANZA",
          "muyinga" = "MUYINGA",
          "makamba" = "MAKAMBA",
          "bururi" = "BURURI",
          "rumonge" = "RUMONGE"
        )
      )
  ) |>
  dplyr::select(adm1, point, var1) |>
  tidyr::pivot_wider(
    id_cols = "adm1",
    names_from = var1,
    values_from = point
  )



# calcualte tpr ----------------------------------------------------------------
  devtools::load_all()

  result <- calc_tpr(
    dhis2_hf,
    hf_var = "hf_uid",
    adm1_var = "adm1",
    adm2_var = "adm2",
    date_var = "date",
    conf_var = "conf",
    test_var = "test",
    pres_var = "pres",
    reporting_threshold = .80,
    extreme_threshold = c(0.01, 0.99),
    activity_indicators = vars_of_interest,
    include_flags = TRUE
  )

  devtools::load_all()
  # vadiate tpr
validate_tpr_proxies(
    result
  )

  # calcualte incidence ----------------------------------------------------------

  incidence_input <- result |>
    dplyr::left_join(
      csb_dhs,
      by = "adm1"
    ) |>
    dplyr::select(
      hf_uid,
      adm0,
      adm1,
      adm2,
      date,
      year,
      month,
      conf,
      test,
      pres,
      tpr,
      reprate,
      pop,
      csb_no_treatment,
      csb_private,
      csb_public
    ) |>
    dplyr::filter(
      !is.na(conf) & !is.na(test) & !is.na(pop) & !is.na(pres)
    )

  incid <- calc_incidence(
    incidence_input, # Must contain TPR + reprate columns!
    levels = c("N0", "N1", "N2", "N3"),
    hf_var = "hf_uid",
    adm0_var = "adm0",
    adm1_var = "adm1",
    adm2_var = "adm2",
    date_var = "date",
    pop_var = "pop",
    conf_var = "conf",
    test_var = "test",
    pres_var = "pres",
    tpr_var = "tpr", # Must exist in data for N1+
    reprate_var = "reprate", # Must exist in data for N2+
    cs_public_var = "csb_public",
    cs_private_var = "csb_private",
    cs_none_var = "csb_no_treatment",
    rho = 0,
    scale_factor = 1000,
    include_flags = T
  )


  # Step 2: Create S3 object
  incid_obj <- create_incidence(incid, scale = 1000)

  # Step 3: Explore
  print(incid_obj)        # Overview
  summary(incid_obj)      # Stats
  plot(incid_obj)         # Visualize

  # Step 4: Back to tibble if needed
  data <- tibble::as_tibble(incid_obj)
