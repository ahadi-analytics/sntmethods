# =============================================================================
# Wealth-Stratified Care-Seeking Behavior Analysis
# =============================================================================
#
# This example demonstrates how to use the new wealth-stratified functions to
# calculate care-seeking behavior indicators separately for each wealth quintile.
#
# IMPORTANT: Wealth stratification filters data FIRST by wealth quintile,
# THEN calculates the indicator. This gives you the correct interpretation:
# "care-seeking rate AMONG the poorest quintile" not "proportion who are both
# poor AND seeking care".

library(sntmethods)

# =============================================================================
# Example 1: MBG Analysis - Poorest Quintile Only
# =============================================================================

# Calculate care-seeking for the poorest quintile at cluster level
# This is for spatial modeling / MBG analysis

csb_poorest_mbg <- calc_csb_by_wealth_mbg(
  dhs_kr = your_kr_data,
  gps_data = your_gps_data,
  indicators = c("public", "private", "any", "none"),
  quintiles = 1,  # Poorest quintile only
  wealth_var = "v190"  # Default wealth quintile variable in KR
)

# Output structure:
# List with elements:
#   - csb_public_q1: data.table with cluster_id, indicator, samplesize, x, y
#   - csb_private_q1: data.table with cluster_id, indicator, samplesize, x, y
#   - csb_any_q1: data.table with cluster_id, indicator, samplesize, x, y
#   - csb_none_q1: data.table with cluster_id, indicator, samplesize, x, y

# Each data.table can be used directly in MBG spatial modeling


# =============================================================================
# Example 2: MBG Analysis - Compare Poorest vs Richest
# =============================================================================

csb_inequality_mbg <- calc_csb_by_wealth_mbg(
  dhs_kr = your_kr_data,
  gps_data = your_gps_data,
  indicators = c("public", "private"),
  quintiles = c(1, 5)  # Poorest and richest only
)

# Output has both Q1 and Q5 for each indicator:
#   - csb_public_q1, csb_public_q5
#   - csb_private_q1, csb_private_q5

# You can now map wealth inequality in care-seeking access


# =============================================================================
# Example 3: Survey-Weighted Analysis - National Estimates
# =============================================================================

# Calculate survey-weighted estimates with confidence intervals
# This is for reporting national/subnational statistics

csb_all_quintiles <- calc_csb_by_wealth_dhs(
  dhs_kr = your_kr_data,
  quintiles = 1:5  # All quintiles
)

# Output structure:
# List with two elements:
#   - adm0: National-level estimates (tibble)
#   - adm1: Regional estimates (tibble, if region_var provided)

# The adm0 tibble has columns:
#   - survey_id, iso3, iso2, survey_type, survey_year, adm0
#   - wealth_quintile (1-5)
#   - indicator, indicator_code
#   - point (proportion estimate)
#   - ci_l, ci_u (confidence intervals)
#   - numerator, denominator (sample sizes)

# Example: Extract public care-seeking for poorest quintile
poorest_public <- csb_all_quintiles$adm0 |>
  filter(
    wealth_quintile == 1,
    indicator_code == "csb_public"
  )

# point column gives proportion (multiply by 100 for percentage)
cat(sprintf(
  "Among poorest quintile: %.1f%% sought care at public facilities\n",
  poorest_public$point * 100
))


# =============================================================================
# Example 4: Regional Analysis by Wealth
# =============================================================================

csb_regional <- calc_csb_by_wealth_dhs(
  dhs_kr = your_kr_data,
  quintiles = c(1, 5),  # Compare poorest vs richest
  region_var = "v024"   # Use DHS region variable
)

# Now csb_regional$adm1 has estimates by region AND wealth quintile
# You can analyze:
# - Which regions have biggest wealth gaps
# - Where poorest quintile has best/worst access

regional_gaps <- csb_regional$adm1 |>
  filter(indicator_code == "csb_any") |>
  select(adm1, wealth_quintile, point) |>
  tidyr::pivot_wider(
    names_from = wealth_quintile,
    values_from = point,
    names_prefix = "q"
  ) |>
  mutate(
    gap = q5 - q1,  # Difference: richest minus poorest
    ratio = q5 / q1  # Ratio: richest divided by poorest
  )


# =============================================================================
# Example 5: Answer the Original Question
# =============================================================================

# Question: "What percentage of poorest households seek care?"

result <- calc_csb_by_wealth_dhs(
  dhs_kr = your_kr_data,
  quintiles = 1  # Poorest only
)

poorest_care_seeking <- result$adm0 |>
  filter(
    wealth_quintile == 1,
    indicator_code == "csb_any"  # Any care sought
  )

cat(sprintf(
  "National estimate: %.1f%% (95%% CI: %.1f%% - %.1f%%) of children in the poorest quintile sought care when they had fever\n",
  poorest_care_seeking$point * 100,
  poorest_care_seeking$ci_l * 100,
  poorest_care_seeking$ci_u * 100
))


# =============================================================================
# Technical Notes
# =============================================================================

# 1. CORRECT calculation:
#    - Filter data to wealth quintile Q
#    - Calculate care-seeking rate among filtered data
#    - Result: P(sought care | quintile Q)

# 2. INCORRECT calculation (DO NOT DO THIS):
#    - Calculate overall care-seeking rate
#    - Multiply by proportion in quintile Q
#    - Result: P(sought care AND quintile Q) - WRONG!

# 3. The functions handle survey weights, clustering, and stratification
#    correctly for both MBG and survey-weighted estimates.

# 4. Wealth variable:
#    - KR recode: v190 (default)
#    - HR recode: hv270
#    - IR recode: v190
#    - PR recode: hv270 (via household linkage)

# 5. Output format matches existing MBG functions, so they work with
#    standard MBG pipelines and spatial modeling tools.
