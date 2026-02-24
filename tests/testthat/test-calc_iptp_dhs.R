# ---- Helper: create minimal mock IR data for IPTp tests ----
.mock_ir_iptp <- function(
  sp_doses = c(0, 1, 2, 3, 4, 5, 1, 2, 3, 4),
  n_clusters = 10,
  seed = 42
) {
  set.seed(seed)
  n <- length(sp_doses)
  data.frame(
    v001 = seq_len(n),          # cluster (one per woman for unique PSUs)
    v005 = rep(1000000, n),     # weight
    v022 = rep(1, n),           # stratum
    v024 = rep("REGION1", n),   # adm1
    v008 = rep(1440, n),        # interview CMC
    b3_01 = rep(1425, n),       # birth CMC (15 months ago → within 24-month window)
    m49a_1 = as.integer(sp_doses >= 1), # sp_taken (binary)
    ml1_1 = sp_doses,           # sp_doses (dose count)
    stringsAsFactors = FALSE
  )
}


# ---- Input validation ----

test_that("calc_iptp_dhs_core rejects non-dataframe input", {
  expect_error(
    calc_iptp_dhs_core("not a dataframe"),
    "data.frame"
  )
})

test_that("calc_iptp_dhs_core rejects empty dataframe", {
  expect_error(
    calc_iptp_dhs_core(data.frame()),
    "empty"
  )
})

test_that("calc_iptp_dhs_core errors when SP variable is missing", {
  ir_data <- .mock_ir_iptp()
  ir_data$ml1_1 <- NULL

  expect_error(
    calc_iptp_dhs_core(ir_data),
    "ml1_1"
  )
})


# ---- Basic IPTp calculation ----

test_that("calc_iptp_dhs_core returns tibble with all 8 IPTp columns", {
  skip_if_not_installed("survey")

  ir_data <- .mock_ir_iptp()
  result <- calc_iptp_dhs_core(ir_data)

  expect_s3_class(result, "tbl_df")
  expect_true("dhs_iptp_1" %in% names(result))
  expect_true("dhs_iptp_2" %in% names(result))
  expect_true("dhs_iptp_3" %in% names(result))
  expect_true("dhs_iptp_4" %in% names(result))
  expect_true("dhs_n_iptp_1plus" %in% names(result))
  expect_true("dhs_n_iptp_2plus" %in% names(result))
  expect_true("dhs_n_iptp_3plus" %in% names(result))
  expect_true("dhs_n_iptp_4plus" %in% names(result))
})

test_that("calc_iptp_dhs_core computes correct point estimates with known data", {
  skip_if_not_installed("survey")

  # sp_doses = c(0, 1, 2, 3, 4, 5, 1, 2, 3, 4) → 10 women, equal weights
  # iptp_1 (>= 1): 9/10 = 0.90
  # iptp_2 (>= 2): 7/10 = 0.70
  # iptp_3 (>= 3): 5/10 = 0.50
  # iptp_4 (>= 4): 3/10 = 0.30
  ir_data <- .mock_ir_iptp()
  result <- calc_iptp_dhs_core(ir_data)

  expect_equal(result$dhs_iptp_1, 0.90)
  expect_equal(result$dhs_iptp_2, 0.70)
  expect_equal(result$dhs_iptp_3, 0.50)
  expect_equal(result$dhs_iptp_4, 0.30)
})

test_that("calc_iptp_dhs_core reports correct unweighted sample sizes", {
  skip_if_not_installed("survey")

  # sp_doses = c(0, 1, 2, 3, 4, 5, 1, 2, 3, 4)
  # n_iptp_1plus: 9 (sp_doses >= 1)
  # n_iptp_2plus: 7 (sp_doses >= 2)
  # n_iptp_3plus: 5 (sp_doses >= 3)
  # n_iptp_4plus: 3 (sp_doses >= 4)
  ir_data <- .mock_ir_iptp()
  result <- calc_iptp_dhs_core(ir_data)

  expect_equal(result$dhs_n_women, 10L)
  expect_equal(result$dhs_n_iptp_1plus, 9L)
  expect_equal(result$dhs_n_iptp_2plus, 7L)
  expect_equal(result$dhs_n_iptp_3plus, 5L)
  expect_equal(result$dhs_n_iptp_4plus, 3L)
})


# ---- Monotonicity ----

test_that("IPTp estimates satisfy monotonicity: iptp_1 >= iptp_2 >= iptp_3 >= iptp_4", {
  skip_if_not_installed("survey")

  ir_data <- .mock_ir_iptp()
  result <- calc_iptp_dhs_core(ir_data)

  expect_gte(result$dhs_iptp_1, result$dhs_iptp_2)
  expect_gte(result$dhs_iptp_2, result$dhs_iptp_3)
  expect_gte(result$dhs_iptp_3, result$dhs_iptp_4)
})


# ---- CI ordering and clamping ----

test_that("IPTp CIs satisfy low <= estimate <= upp and are clamped to [0, 1]", {
  skip_if_not_installed("survey")

  ir_data <- .mock_ir_iptp()
  result <- calc_iptp_dhs_core(ir_data)

  for (suffix in c("1", "2", "3", "4")) {
    est <- result[[paste0("dhs_iptp_", suffix)]]
    low <- result[[paste0("dhs_iptp_", suffix, "_low")]]
    upp <- result[[paste0("dhs_iptp_", suffix, "_upp")]]

    expect_lte(low, est)
    expect_gte(upp, est)
    expect_gte(low, 0)
    expect_lte(upp, 1)
  }
})


# ---- iptp_4plus requires ml1_1 (binary variable produces zero) ----

test_that("iptp_4plus is zero when sp_doses is binary (m49a_1 style)", {
  skip_if_not_installed("survey")

  # Use a binary SP variable (max value 1) — same effect as mapping m49a_1 to sp_doses
  ir_data <- .mock_ir_iptp()
  ir_data$sp_binary <- as.integer(ir_data$ml1_1 >= 1)  # 0/1 only

  result <- calc_iptp_dhs_core(
    ir_data,
    survey_vars = list(
      cluster = "v001",
      weight = "v005",
      stratum = "v022",
      adm1 = "v024",
      adm2 = NULL,
      interview_cmc = "v008",
      birth_cmc = "b3_01",
      birth_age_months = "b19_01",
      sp_taken = "m49a_1",
      sp_doses = "sp_binary"
    )
  )

  # binary variable (max = 1) means sp_doses >= 4 is always 0
  expect_equal(result$dhs_iptp_4, 0)
})


# ---- calc_iptp_dhs wrapper (metadata + dict) ----

test_that("calc_iptp_dhs returns list with data, dict, and metadata", {
  skip_if_not_installed("survey")
  skip_if_not_installed("sntutils")

  ir_data <- .mock_ir_iptp()
  result <- calc_iptp_dhs(ir_data)

  expect_type(result, "list")
  expect_named(result, c("data", "dict", "metadata"))
  expect_equal(result$metadata$analysis_type, "IPTp Coverage")
  expect_equal(result$metadata$file_type, "IR")
  expect_true("dhs_iptp_1" %in% names(result$data))
  expect_true("dhs_iptp_4" %in% names(result$data))
})


# ---- Birth window filter ----

test_that("calc_iptp_dhs_core excludes births outside the window", {
  skip_if_not_installed("survey")

  sp_doses_all <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
  ir_data <- .mock_ir_iptp(sp_doses = sp_doses_all)

  # Set half the births to be outside the 24-month window
  ir_data$b3_01[6:10] <- 1390  # 50 months ago

  result <- calc_iptp_dhs_core(ir_data)

  # Only 5 women within window
  expect_equal(result$dhs_n_women, 5L)
})
