# ---- Tests for .aggregate_interview_month_to_admin gap-filling ----

test_that("gap-fill uses sibling adm2 median within same adm1", {
  skip_if_not_installed("sf")

  # Create admin shapefile with 4 adm2 units in 2 adm1 regions
  polys <- sf::st_sf(
    adm1 = c("North", "North", "South", "South"),
    adm2 = c("N1", "N2", "S1", "S2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
      sf::st_polygon(list(rbind(c(0, -1), c(1, -1), c(1, 0), c(0, 0), c(0, -1)))),
      sf::st_polygon(list(rbind(c(1, -1), c(2, -1), c(2, 0), c(1, 0), c(1, -1))))
    ),
    crs = 4326
  )

  # Clusters only in N1 and S1 — N2 and S2 have no data
  cluster_months <- tibble::tibble(
    x = c(0.5, 0.5, 0.5, 0.5, 0.5),
    y = c(0.5, 0.5, 0.5, -0.5, -0.5),
    interview_month = c(3L, 5L, 4L, 8L, 10L)
  )

  result <- .aggregate_interview_month_to_admin(
    cluster_months = cluster_months,
    admin_sf = polys,
    admin_col = "adm2",
    parent_col = "adm1"
  )

  expect_equal(nrow(result), 4)
  expect_true(all(c("N1", "N2", "S1", "S2") %in% result$adm2))

  # N1 has clusters with months 3,5,4 → median = 4
  expect_equal(result$median_survey_month[result$adm2 == "N1"], 4L)

  # N2 has no clusters → filled from North sibling (N1) median = 4
  expect_equal(result$median_survey_month[result$adm2 == "N2"], 4L)

  # S1 has clusters with months 8,10 → median = 9
  expect_equal(result$median_survey_month[result$adm2 == "S1"], 9L)

  # S2 has no clusters → filled from South sibling (S1) median = 9
  expect_equal(result$median_survey_month[result$adm2 == "S2"], 9L)
})

test_that("gap-fill is skipped when parent_col is NULL", {
  skip_if_not_installed("sf")

  polys <- sf::st_sf(
    adm1 = c("North", "North"),
    adm2 = c("N1", "N2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
    ),
    crs = 4326
  )

  # Only cluster in N1
  cluster_months <- tibble::tibble(
    x = 0.5, y = 0.5, interview_month = 6L
  )

  result <- .aggregate_interview_month_to_admin(
    cluster_months = cluster_months,
    admin_sf = polys,
    admin_col = "adm2",
    parent_col = NULL
  )

  # Without parent_col, only N1 should appear (no gap-filling)
  expect_equal(nrow(result), 1)
  expect_equal(result$adm2, "N1")
  expect_equal(result$median_survey_month, 6L)
})

test_that("gap-fill returns NULL for empty input", {
  result <- .aggregate_interview_month_to_admin(
    cluster_months = NULL,
    admin_sf = data.frame(),
    admin_col = "adm2"
  )
  expect_null(result)

  result2 <- .aggregate_interview_month_to_admin(
    cluster_months = tibble::tibble(x = numeric(), y = numeric(),
                                     interview_month = integer()),
    admin_sf = data.frame(),
    admin_col = "adm2"
  )
  expect_null(result2)
})
