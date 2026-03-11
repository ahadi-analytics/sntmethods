##############################################################################
##  Diagnostic: ACT variable detection for Togo MIS 2017                   ##
##  Tests the fix step by step to verify labels are preserved               ##
##############################################################################

library(sntmethods)

path_dhs_parquet <- here::here(sntmethods::ahadi_path(), "01_data/parquet")

# ---- Step 1: Verify dhs_read() uses direct parquet read -------------------
cli::cli_h1("Step 1: dhs_read() direct parquet read")

kr <- sntmethods::dhs_read(
  path = path_dhs_parquet,
  file_type = "KR",
  country_code = "TG",
  survey_year = 2017,
  survey_type = "MIS"
)
# Should print: "Direct parquet read for single survey (preserving labels + all variables)"
# If it prints "Opening Arrow dataset..." instead, the fix didn't trigger

# ---- Step 2: Check ml13 labels are REAL (not standardized) ----------------
cli::cli_h1("Step 2: Check ml13 variable labels")

ml13_vars <- grep("^ml13[a-z]", names(kr), value = TRUE)
cli::cli_alert_info("Found {length(ml13_vars)} ml13 variables")

for (v in ml13_vars) {
  lbl <- attr(kr[[v]], "label")
  raw <- as.vector(haven::zap_labels(kr[[v]]))
  n_pos <- sum(raw == 1, na.rm = TRUE)
  cli::cli_alert_info("  {v}: label = {.val {lbl %||% '(none)'}}, n_positive = {n_pos}")
}

# Key check: ml13f should say "Artemether-lumefantrine" NOT "NA - CS antimalarial"
ml13f_label <- attr(kr$ml13f, "label")
if (!is.null(ml13f_label) && grepl("artemether|lumefantrine", ml13f_label, ignore.case = TRUE)) {
  cli::cli_alert_success("ml13f has REAL label: {.val {ml13f_label}}")
} else {
  cli::cli_alert_danger("ml13f has STANDARDIZED label: {.val {ml13f_label %||% '(none)'}}")
  cli::cli_alert_danger("dhs_read() direct parquet read did NOT work!")
}

# ---- Step 3: Test .detect_act_vars() with real labels ---------------------
cli::cli_h1("Step 3: ACT variable detection")

act_vars <- sntmethods:::.detect_act_vars(kr)
cli::cli_alert_info("Detected ACT variables: {paste(act_vars, collapse = ', ')}")

# Should find ml13e, ml13f, ml13g (all ACT formulations)
# Should NOT find ml13aa (artesunate rectal) or ml13ab (artesunate injection)

# ---- Step 4: Test .prepare_act_data() composite --------------------------
cli::cli_h1("Step 4: ACT data preparation")

kr_fever <- sntmethods:::.prepare_act_data(
  dhs_kr = kr,
  survey_vars = list(
    cluster = "v021", weight = "v005", stratum = "v022",
    age = "hw1", fever = "h22", alive = "b5",
    act = "ml13e", test = "ml13a"
  ),
  include_survey_vars = TRUE
)

act_vars_used <- attr(kr_fever, "act_vars_used")
cli::cli_alert_info("ACT vars used: {paste(act_vars_used, collapse = ', ')}")
cli::cli_alert_info("N febrile: {nrow(kr_fever)}")
cli::cli_alert_info("N received ACT: {sum(kr_fever$received_act == 1, na.rm = TRUE)}")
cli::cli_alert_info("ACT rate: {round(mean(kr_fever$has_act, na.rm = TRUE) * 100, 1)}%")

# ---- Step 5: Full calc_act_dhs() -----------------------------------------
cli::cli_h1("Step 5: Full calc_act_dhs()")

result <- calc_act_dhs(kr)
cli::cli_alert_info("National ACT rate: {round(result$dhs_act * 100, 1)}%")
cli::cli_alert_info("N fever: {result$dhs_n_fever}")
cli::cli_alert_info("N ACT: {result$dhs_n_act}")

if (result$dhs_act > 0.5) {
  cli::cli_alert_success("ACT rate looks correct (~76% expected)")
} else if (result$dhs_act > 0) {
  cli::cli_alert_warning("ACT rate is low — check if all ACT variables were detected")
} else {
  cli::cli_alert_danger("ACT rate is 0% — composite detection failed!")
}
