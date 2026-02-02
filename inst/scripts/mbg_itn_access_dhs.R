####################  MBG model for ITN Access (DHS data)  ########################

cli::cli_h1("MBG model for ITN Access (DHS data)")

## ---------------------------------------------------------------------------##
# 1) Setup and parameters -----------------------------------------------------
## ---------------------------------------------------------------------------##

country_iso2 <- "BU"
country_iso3 <- "bdi"
survey_year <- 2016
survey_type <- "DHS"

paths <- sntutils::setup_project_paths()
path_dhs_parquet <- here::here(ahadi_path(), "01_data/parquet")
path_output <- here::here(paths$itn, "processed")

fs::dir_create(path_output)

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

hr <- dhs_read(
 path = path_dhs_parquet,
 file_type = "HR",
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
# 3) Calculate household-level ITN metrics ------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Calculating household ITN metrics")

# identify ITN variables (hml10_1, hml10_2, etc.)
itn_vars <- names(hr)[grepl("^hml10_", names(hr))]
cli::cli_alert_info("Found {length(itn_vars)} ITN variables")

# clean HR data and count ITNs per household
hr_clean <- hr |>
 dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
 dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
 dplyr::transmute(
   cluster_id = hv001,
   hhid = hv002,
   hh_size = hv013,
   weight = hv005 / 1e6,
   # count ITNs: hml10_* = 1 means ITN present
   n_itns = rowSums(
     dplyr::across(dplyr::all_of(itn_vars), ~ dplyr::if_else(. == 1, 1L, 0L)),
     na.rm = TRUE
   )
 ) |>
 dplyr::mutate(
   # potential ITN users = min(ITNs * 2, household size)
   potential_users = pmin(n_itns * 2, hh_size),
   # household has at least one ITN
   has_itn = dplyr::if_else(n_itns >= 1, 1L, 0L)
 )

cli::cli_alert_info(
 "{format(nrow(hr_clean), big.mark = ',')} households processed"
)

## ---------------------------------------------------------------------------##
# 4) Calculate individual-level ITN access ------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Calculating individual ITN access")
# clean PR data
pr_clean <- pr |>
 dplyr::mutate(dplyr::across(dplyr::everything(), haven::zap_labels)) |>
 dplyr::mutate(dplyr::across(dplyr::everything(), as.vector)) |>
 dplyr::transmute(
   cluster_id = hv001,
   hhid = hv002,
   line_num = hvidx,
   weight = hv005 / 1e6,
   age = hv105,
   sex = hv104,
   # de facto household member (slept in HH last night)
   de_facto = hv103
 ) |>
 # keep only de facto household members
 dplyr::filter(de_facto == 1)

# merge household ITN info to individuals
pr_with_itn <- pr_clean |>
 dplyr::left_join(
   hr_clean |> dplyr::select(cluster_id, hhid, hh_size, n_itns, potential_users),
   by = c("cluster_id", "hhid")
 )

# calculate individual access probability
# access ratio = potential_users / hh_size (capped at 1)
pr_with_itn <- pr_with_itn |>
 dplyr::mutate(
   access_ratio = dplyr::if_else(
     hh_size > 0,
     pmin(potential_users / hh_size, 1),
     0
   ),
   # for binomial MBG, we need 0/1 outcome
   # assign access probabilistically based on access_ratio
   # simpler approach: use access_ratio as the proportion with access
   has_access = dplyr::if_else(access_ratio >= stats::runif(dplyr::n()), 1L, 0L)
 )

cli::cli_alert_info(
 "{format(nrow(pr_with_itn), big.mark = ',')} individuals with ITN access data"
)

## ---------------------------------------------------------------------------##
# 5) Aggregate to cluster level -----------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Aggregating to cluster level")

# aggregate to cluster: sum of individuals with access / total individuals
itn_cluster <- pr_with_itn |>
 dplyr::group_by(cluster_id) |>
 dplyr::summarise(
   n_with_access = sum(has_access, na.rm = TRUE),
   n_individuals = dplyr::n(),
   access_raw = n_with_access / n_individuals,
   # also calculate mean access ratio (continuous measure)
   mean_access_ratio = mean(access_ratio, na.rm = TRUE),
   .groups = "drop"
 )

cli::cli_alert_info("{nrow(itn_cluster)} clusters prepared")

## ---------------------------------------------------------------------------##
# 6) Merge GPS coordinates ----------------------------------------------------
## ---------------------------------------------------------------------------##

gps_coords <- ge |>
 sf::st_drop_geometry() |>
 dplyr::transmute(
   cluster_id = DHSCLUST,
   lat = LATNUM,
   lon = LONGNUM
 ) |>
 dplyr::filter(!is.na(lat), !is.na(lon), lat != 0, lon != 0)

itn_cluster <- itn_cluster |>
 dplyr::inner_join(gps_coords, by = "cluster_id")

itn_sf <- itn_cluster |>
 sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

cli::cli_alert_info(
 "{nrow(itn_cluster)} clusters with GPS coordinates"
)

## ---------------------------------------------------------------------------##
# 7) Align CRS ----------------------------------------------------------------
## ---------------------------------------------------------------------------##

crs_master <- terra::crs(pop_rast)
adm0_sf <- sf::st_transform(adm0_sf, crs_master)
adm1_sf <- sf::st_transform(adm1_sf, crs_master)
adm2_sf <- sf::st_transform(adm2_sf, crs_master)
itn_sf <- sf::st_transform(itn_sf, crs_master)

adm2_vect <- terra::vect(adm2_sf)

## ---------------------------------------------------------------------------##
# 8) Prepare MBG inputs -------------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Building MBG inputs")

coords <- sf::st_coordinates(itn_sf)

itn_dt <- data.table::data.table(
 cluster_id = itn_sf$cluster_id,
 indicator = itn_sf$n_with_access,
 samplesize = itn_sf$n_individuals,
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
# 9) Run MBG model ------------------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Running MBG model")

model_runner <- mbg::MbgModelRunner$new(
 input_data = itn_dt,
 id_raster = id_raster,
 covariate_rasters = covariates,
 aggregation_table = aggregation_table,
 aggregation_levels = list(adm2 = c("adm2", "adm1", "adm0")),
 population_raster = pop_rast
)

model_runner$run_mbg_pipeline()

## ---------------------------------------------------------------------------##
# 10) Save outputs ------------------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Saving outputs")

cell_preds <- model_runner$grid_cell_predictions

# save prediction rasters
terra::writeRaster(
 cell_preds$cell_pred_mean,
 fs::path(path_output, glue::glue("{country_iso3}_itn_access_mbg_mean.tif")),
 overwrite = TRUE
)

terra::writeRaster(
 cell_preds$cell_pred_lower,
 fs::path(path_output, glue::glue("{country_iso3}_itn_access_mbg_lower.tif")),
 overwrite = TRUE
)

terra::writeRaster(
 cell_preds$cell_pred_upper,
 fs::path(path_output, glue::glue("{country_iso3}_itn_access_mbg_upper.tif")),
 overwrite = TRUE
)

# ADM2 summaries
adm2_summary <- adm2_sf |>
 dplyr::mutate(
   access_mean = terra::extract(
     cell_preds$cell_pred_mean, adm2_sf, fun = mean, na.rm = TRUE
   )[[2]],
   access_lower = terra::extract(
     cell_preds$cell_pred_lower, adm2_sf, fun = mean, na.rm = TRUE
   )[[2]],
   access_upper = terra::extract(
     cell_preds$cell_pred_upper, adm2_sf, fun = mean, na.rm = TRUE
   )[[2]],
   access_width = access_upper - access_lower
 )

sntutils::write_snt_data(
 obj = list(
   adm2_summary = adm2_summary |> sf::st_drop_geometry(),
   cluster_data = itn_cluster
 ),
 data_name = glue::glue("{country_iso3}_itn_access_mbg_dhs_{survey_year}"),
 path = path_output,
 file_formats = c("qs2", "xlsx")
)

## ---------------------------------------------------------------------------##
# 11) Visualizations ----------------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Creating visualizations")

# mean ITN access raster map
r_mean <- cell_preds$cell_pred_mean * 100

map_mean <- tmap::tm_shape(r_mean) +
 tmap::tm_raster(
   palette = "YlGnBu",
   title = "ITN Access (%)"
 ) +
 tmap::tm_shape(adm2_sf) +
 tmap::tm_borders(col = "black", lwd = 0.3) +
 tmap::tm_compass() +
 tmap::tm_scale_bar() +
 tmap::tm_layout(
   main.title = glue::glue(
     "Burundi: ITN Access (DHS {survey_year})\nSpatially smoothed MBG model"
   ),
   main.title.position = "center",
   main.title.size = 1.5,
   legend.outside = TRUE
 )

# uncertainty width (percentage points)
r_unc <- (cell_preds$cell_pred_upper - cell_preds$cell_pred_lower) * 100

map_unc <- tmap::tm_shape(r_unc) +
 tmap::tm_raster(
   palette = "Purples",
   title = "Uncertainty width (pp)"
 ) +
 tmap::tm_shape(adm2_sf) +
 tmap::tm_borders(col = "black", lwd = 0.3) +
 tmap::tm_compass() +
 tmap::tm_scale_bar() +
 tmap::tm_layout(
   main.title = glue::glue(
     "Burundi: ITN Access Uncertainty (DHS {survey_year})\n95% credible interval width"
   ),
   main.title.position = "center",
   main.title.size = 1.5,
   legend.outside = TRUE
 )

tmap::tmap_save(
 tm = map_mean,
 filename = fs::path(
   path_output,
   glue::glue("{country_iso3}_itn_access_mean_dhs_{survey_year}.png")
 ),
 dpi = 500,
 width = 10,
 height = 7.5
)

tmap::tmap_save(
 tm = map_unc,
 filename = fs::path(
   path_output,
   glue::glue("{country_iso3}_itn_access_uncertainty_dhs_{survey_year}.png")
 ),
 dpi = 500,
 width = 10,
 height = 7.5
)

# cluster-level point map
itn_sf <- itn_sf |>
 dplyr::mutate(access_pct = access_raw * 100)

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
   data = itn_sf,
   ggplot2::aes(color = access_pct, size = n_individuals),
   alpha = 0.9
 ) +
 ggplot2::scale_color_gradientn(
   colors = c(
     "#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb",
     "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58"
   ),
   limits = c(0, 100),
   breaks = seq(0, 100, 20),
   labels = function(x) paste0(x, "%"),
   name = "ITN Access"
 ) +
 ggplot2::scale_size_continuous(
   range = c(0.8, 5),
   name = "Sample size"
 ) +
 ggplot2::labs(
   title = glue::glue("Burundi: ITN Access by DHS cluster ({survey_year})"),
   subtitle = "Proportion of de facto population with access to an ITN",
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
   glue::glue("{country_iso3}_itn_access_clusters_dhs_{survey_year}.png")
 ),
 plot = p_clusters,
 width = 10,
 height = 9,
 dpi = 320
)

cli::cli_alert_success("MBG ITN Access analysis complete")
