# ============================================================================
# Tests for dhs_dictionary() — master DHS indicator dictionary
# ============================================================================

test_that("dhs_dictionary returns a tibble with correct columns", {
  dict <- dhs_dictionary()

  expect_s3_class(dict, "tbl_df")

  expected_cols <- c(
    "domain", "observation_unit", "dhs_recode", "calc_function",
    "indicator_code", "indicator", "indicator_title",
    "numerator_code", "numerator_description",
    "denominator_code", "denominator_description",
    "eligibility", "dhs_variables", "notes"
  )
  expect_true(all(expected_cols %in% names(dict)))
  expect_equal(names(dict), expected_cols)
})


test_that("dhs_dictionary has correct total row count", {
  dict <- dhs_dictionary()

  # Sum of all individual dictionaries: 171
  # (includes `irs`/`irs_coverage` and `smc`/`smc_coverage` alias pairs,
  # plus 6 Demographics rows: prop_u5/ov5 and their urban/rural strata)
  expect_equal(nrow(dict), 171L)
})


test_that("all 17 domains are present", {
  dict <- dhs_dictionary()

  expected_domains <- c(
    "ACT", "Antimalarial", "ANC", "Case Management", "CSB", "Demographics",
    "EPI", "Fever", "IPTp", "IRS", "ITN", "Malaria Dx", "PfPR",
    "Severe Anemia", "SMC", "U5MR", "Wealth"
  )
  actual_domains <- sort(unique(dict$domain))
  expect_equal(actual_domains, sort(expected_domains))
})


test_that("no NA in required columns", {
  dict <- dhs_dictionary()

  expect_false(any(is.na(dict$domain)))
  expect_false(any(is.na(dict$indicator_code)))
  expect_false(any(is.na(dict$dhs_recode)))
  expect_false(any(is.na(dict$calc_function)))
  expect_false(any(is.na(dict$numerator_code)))
  expect_false(any(is.na(dict$eligibility)))
  expect_false(any(is.na(dict$dhs_variables)))
  expect_false(any(is.na(dict$observation_unit)))
})


test_that("numerator_code follows n_ convention", {
  dict <- dhs_dictionary()

  expect_true(all(grepl("^n_", dict$numerator_code)))
  expect_equal(dict$numerator_code, paste0("n_", dict$indicator_code))
})


test_that("dhs_recode values are valid", {
  dict <- dhs_dictionary()

  valid_recodes <- c("KR", "IR", "HR", "PR", "HR+PR")
  expect_true(all(dict$dhs_recode %in% valid_recodes))
})


test_that("observation_unit values are valid", {
  dict <- dhs_dictionary()

  valid_units <- c("Country", "Household", "Person")
  expect_true(all(dict$observation_unit %in% valid_units))

  # ITN should have mix of Household and Person
  itn <- dict[dict$domain == "ITN", ]
  expect_true("Household" %in% itn$observation_unit)
  expect_true("Person" %in% itn$observation_unit)

  # Demographics is person-level (de jure household members)
  demog <- dict[dict$domain == "Demographics", ]
  expect_true(all(demog$observation_unit == "Person"))

  # Remaining domains should be Country
  other <- dict[!dict$domain %in% c("ITN", "Demographics"), ]
  expect_true(all(other$observation_unit == "Country"))
})


test_that("individual domain row counts match their dictionaries", {
  dict <- dhs_dictionary()

  expect_equal(sum(dict$domain == "ACT"), nrow(act_dictionary()))
  expect_equal(sum(dict$domain == "ITN"), nrow(itn_dictionary()))
  expect_equal(sum(dict$domain == "EPI"), nrow(epi_dictionary()))
  expect_equal(sum(dict$domain == "PfPR"), nrow(pfpr_dictionary()))
  expect_equal(sum(dict$domain == "ANC"), nrow(anc_dictionary()))
  expect_equal(sum(dict$domain == "Fever"), nrow(fever_dictionary()))
  expect_equal(sum(dict$domain == "U5MR"), nrow(u5mr_dictionary()))
})


test_that("key indicator_codes are present", {
  dict <- dhs_dictionary()
  codes <- dict$indicator_code

  # Spot-check across domains
  expect_true("act" %in% codes)
  expect_true("fever" %in% codes)
  expect_true("with_itn" %in% codes)
  expect_true("use_itn" %in% codes)
  expect_true("irs" %in% codes)
  expect_true("anc_4plus" %in% codes)
  expect_true("iptp_3plus" %in% codes)
  expect_true("u5mr" %in% codes)
  expect_true("severe_anemia" %in% codes)
  expect_true("smc" %in% codes)
  expect_true("wealth_q1" %in% codes)
  expect_true("gini" %in% codes)
  expect_true("epi_dpt3" %in% codes)
  expect_true("malaria_dx" %in% codes)
  expect_true("csb_any" %in% codes)
  expect_true("eff_cm_any" %in% codes)
})
