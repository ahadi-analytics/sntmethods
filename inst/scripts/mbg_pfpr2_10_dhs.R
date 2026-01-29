####################  MBG model for PfPR2-10 (DHS data)  ########################

cli::cli_h1("MBG model for PfPR2-10 (DHS data)")

## ---------------------------------------------------------------------------##
# 1) Setup and parameters -----------------------------------------------------
## ---------------------------------------------------------------------------##

devtools::load_all(
  "/Users/mohamedyusuf/ahadi-analytics/code/GitHub/sntmethods"
)

country_iso2 <- "BU"
country_iso3 <- "bdi"
survey_year <- 2016
survey_type <- "DHS"

# test type: "mic" (microscopy) or "rdt" (rapid diagnostic test)
test_type <- "mic"

# age range for PfPR2-10 (months)
age_min_months <- 24
age_max_months <- 119

paths <- sntutils::setup_project_paths()
path_dhs_parquet <- here::here(ahadi_path(), "01_data/parquet")
path_output <- here::here(paths$pfpr_est, "processed")

## ---------------------------------------------------------------------------##
# 2) Load inputs --------------------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Loading inputs")

# load DHS data
ge <- dhs_read(
  path = path_dhs_parquet,
  file_type = "GE",
  survey_type = survey_type,
  country_code = country_iso2,
  survey_year = survey_year
)

pr <- dhs_read(
  path = path_dhs_parquet,
  file_type = "PR",
  survey_type = survey_type,
  country_code = country_iso2,
  survey_year = survey_year
)

# load shapefiles
shp_list <- sntutils::read_snt_data(
  data_name = glue::glue("{country_iso3}_adm0_adm2_post2023"),
  path = here::here(paths$admin_shp, "processed"),
  file_formats = c("qs2")
)$final_spat_vec

adm0_sf <- shp_list$adm0
adm1_sf <- shp_list$adm1
adm2_sf <- shp_list$adm2

# load population raster
pop_rast <- terra::rast(here::here(
  paths$pop_worldpop, "raw",
  glue::glue("{country_iso3}_ppp_{survey_year}_1km_Aggregated_UNadj.tif")
))

## ---------------------------------------------------------------------------##
# 3) Prepare DHS malaria data -------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Preparing PfPR2-10 cluster data")

# clean PR data
pr_clean <- pr |>
  dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
  dplyr::transmute(
    cluster_id = hv001,
    weight = hv005 / 1e6,
    age_months = hc1,
    present = hv103,
    selected = hv042,
    mic_result = hml32,
    rdt_result = hml35
  ) |>
  dplyr::filter(
    age_months >= age_min_months,
    age_months <= age_max_months,
    present == 1,
    selected == 1
  )

# create test result based on preference
if (test_type == "mic") {
  pr_clean <- pr_clean |>
    dplyr::mutate(
      tested = dplyr::if_else(mic_result %in% c(0, 1, 6), 1L, 0L),
      positive = dplyr::if_else(mic_result %in% c(1, 6), 1L, 0L)
    )
} else {
  pr_clean <- pr_clean |>
    dplyr::mutate(
      tested = dplyr::if_else(rdt_result %in% c(0, 1), 1L, 0L),
      positive = dplyr::if_else(rdt_result == 1, 1L, 0L)
    )
}

# aggregate to cluster level
pfpr_cluster <- pr_clean |>
  dplyr::filter(tested == 1) |>
  dplyr::group_by(cluster_id) |>
  dplyr::summarise(
    n_positive = sum(positive, na.rm = TRUE),
    n_tested = dplyr::n(),
    pfpr_raw = n_positive / n_tested,
    .groups = "drop"
  )

cli::cli_alert_info("{nrow(pfpr_cluster)} clusters prepared")

## ---------------------------------------------------------------------------##
# 4) Merge GPS coordinates ----------------------------------------------------
## ---------------------------------------------------------------------------##

gps_coords <- ge |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    cluster_id = DHSCLUST,
    lat = LATNUM,
    lon = LONGNUM
  ) |>
  dplyr::filter(!is.na(lat), !is.na(lon), lat != 0, lon != 0)

pfpr_cluster <- pfpr_cluster |>
  dplyr::inner_join(gps_coords, by = "cluster_id")

pfpr_sf <- pfpr_cluster |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

## ---------------------------------------------------------------------------##
# 5) Align CRS ----------------------------------------------------------------
## ---------------------------------------------------------------------------##

crs_master <- terra::crs(pop_rast)
adm0_sf <- sf::st_transform(adm0_sf, crs_master)
adm1_sf <- sf::st_transform(adm1_sf, crs_master)
adm2_sf <- sf::st_transform(adm2_sf, crs_master)
pfpr_sf <- sf::st_transform(pfpr_sf, crs_master)

adm2_vect <- terra::vect(adm2_sf)

## ---------------------------------------------------------------------------##
# 6) Prepare MBG inputs -------------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Building MBG inputs")

coords <- sf::st_coordinates(pfpr_sf)

pfpr_dt <- data.table::data.table(
  cluster_id = pfpr_sf$cluster_id,
  indicator = pfpr_sf$n_positive,
  samplesize = pfpr_sf$n_tested,
  x = coords[, 1],
  y = coords[, 2]
) |>
  _[samplesize > 0]

# build ID raster
id_raster <- mbg::build_id_raster(
  polygons = adm2_vect,
  template_raster = pop_rast
)

# intercept-only model (pure spatial smoothing)
covariates <- list(
  intercept = terra::setValues(id_raster, 1)
)

# build aggregation table
agg_file <- fs::path(
  path_output,
  glue::glue("{country_iso3}_aggregation_table_adm2.parquet")
)

if (fs::file_exists(agg_file)) {
  aggregation_table <- arrow::read_parquet(agg_file)
} else {
  aggregation_table <- mbg::build_aggregation_table(
    polygons = adm2_vect,
    id_raster = id_raster,
    polygon_id_field = "adm2",
    verbose = TRUE
  )
  arrow::write_parquet(aggregation_table, sink = agg_file)
}

## ---------------------------------------------------------------------------##
# 7) Run MBG model ------------------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Running MBG model")

model_runner <- mbg::MbgModelRunner$new(
  input_data = pfpr_dt,
  id_raster = id_raster,
  covariate_rasters = covariates,
  aggregation_table = aggregation_table,
  aggregation_levels = list(adm2 = c("adm2", "adm1", "adm0")),
  population_raster = pop_rast
)

model_runner$run_mbg_pipeline()

## ---------------------------------------------------------------------------##
# 8) Save outputs -------------------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Saving outputs")

cell_preds <- model_runner$grid_cell_predictions

# save prediction rasters
terra::writeRaster(
  cell_preds$cell_pred_mean,
  fs::path(path_output, glue::glue("{country_iso3}_pfpr2_10_mbg_mean.tif")),
  overwrite = TRUE
)

terra::writeRaster(
  cell_preds$cell_pred_lower,
  fs::path(path_output, glue::glue("{country_iso3}_pfpr2_10_mbg_lower.tif")),
  overwrite = TRUE
)

terra::writeRaster(
  cell_preds$cell_pred_upper,
  fs::path(path_output, glue::glue("{country_iso3}_pfpr2_10_mbg_upper.tif")),
  overwrite = TRUE
)

# ADM2 summaries
adm2_summary <- adm2_sf |>
  dplyr::mutate(
    pfpr_mean = terra::extract(
      cell_preds$cell_pred_mean, adm2_sf, fun = mean, na.rm = TRUE
    )[[2]],
    pfpr_lower = terra::extract(
      cell_preds$cell_pred_lower, adm2_sf, fun = mean, na.rm = TRUE
    )[[2]],
    pfpr_upper = terra::extract(
      cell_preds$cell_pred_upper, adm2_sf, fun = mean, na.rm = TRUE
    )[[2]],
    pfpr_width = pfpr_upper - pfpr_lower
  )

sntutils::write_snt_data(
  obj = list(
    adm2_summary = adm2_summary |> sf::st_drop_geometry(),
    cluster_data = pfpr_cluster
  ),
  data_name = glue::glue("{country_iso3}_pfpr2_10_mbg_dhs_{survey_year}"),
  path = path_output,
  file_formats = c("qs2", "xlsx")
)

## ---------------------------------------------------------------------------##
# 9) Visualizations -----------------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Creating visualizations")

# mean PfPR raster map
r_mean <- cell_preds$cell_pred_mean * 100

map_mean <- tmap::tm_shape(r_mean) +
  tmap::tm_raster(
    palette = "YlOrRd",
    title = "PfPR2-10 (%)"
  ) +
  tmap::tm_shape(adm2_sf) +
  tmap::tm_borders(col = "black", lwd = 0.3) +
  tmap::tm_compass() +
  tmap::tm_scale_bar() +
  tmap::tm_layout(
    main.title = glue::glue(
      "Burundi: PfPR2-10 (DHS {survey_year})\nSpatially smoothed MBG model"
    ),
    main.title.position = "center",
    main.title.size = 1.5,
    legend.outside = TRUE
  )

# uncertainty width (percentage points)
r_unc <- (cell_preds$cell_pred_upper - cell_preds$cell_pred_lower) * 100

map_unc <- tmap::tm_shape(r_unc) +
  tmap::tm_raster(
    palette = sf::sf.colors(n = 100),
    title = "Uncertainty width (pp)"
  ) +
  tmap::tm_shape(adm2_sf) +
  tmap::tm_borders(col = "black", lwd = 0.3) +
  tmap::tm_compass() +
  tmap::tm_scale_bar() +
  tmap::tm_layout(
    main.title = glue::glue(
      "Burundi: PfPR2-10 Uncertainty (DHS {survey_year})\n95% credible interval width"
    ),
    main.title.position = "center",
    main.title.size = 1.5,
    legend.outside = TRUE
  )

tmap::tmap_save(
  tm = map_mean,
  filename = fs::path(
    path_output,
    glue::glue("{country_iso3}_pfpr2_10_mean_dhs_{survey_year}.png")
  ),
  dpi = 500,
  width = 10,
  height = 7.5
)

tmap::tmap_save(
  tm = map_unc,
  filename = fs::path(
    path_output,
    glue::glue("{country_iso3}_pfpr2_10_uncertainty_dhs_{survey_year}.png")
  ),
  dpi = 500,
  width = 10,
  height = 7.5
)

# cluster-level point map
pfpr_sf <- pfpr_sf |>
  dplyr::mutate(pfpr_pct = pfpr_raw * 100)

p_clusters <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = adm0_sf,
    fill = ggplot2::alpha("grey98", 0.5),
    color = "grey70",
    linewidth = 0.4
  ) +
  ggplot2::geom_sf(
    data = adm2_sf,
    fill = ggplot2::alpha("grey99", 0.25),
    color = "grey85",
    linewidth = 0.2
  ) +
  ggplot2::geom_sf(
    data = pfpr_sf,
    ggplot2::aes(color = pfpr_pct, size = n_tested),
    alpha = 0.9
  ) +
  ggplot2::scale_color_gradientn(
    colors = c(
      "#ffffcc", "#ffeda0", "#fed976", "#feb24c",
      "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026"
    ),
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    name = "PfPR2-10"
  ) +
  ggplot2::scale_size_continuous(
    range = c(0.8, 5),
    name = "Sample size"
  ) +
  ggplot2::labs(
    title = glue::glue("Burundi: PfPR2-10 by DHS cluster ({survey_year})"),
    subtitle = "Children aged 2-10 years, microscopy results",
    caption = glue::glue("Data source: DHS {survey_year}")
  ) +
  ggplot2::theme_void(base_size = 13) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.key.width = grid::unit(1, "cm"),
    legend.spacing.x = ggplot2::unit(3.2, "cm")
  )

ggplot2::ggsave(
  filename = fs::path(
    path_output,
    glue::glue("{country_iso3}_pfpr2_10_clusters_dhs_{survey_year}.png")
  ),
  plot = p_clusters,
  width = 10,
  height = 9,
  dpi = 320
)

cli::cli_alert_success("MBG PfPR2-10 analysis complete")
