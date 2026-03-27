# Wealth-Stratified Indicator Functions

## Overview

The sntmethods package now includes functions to calculate DHS indicators stratified by wealth quintile. These functions allow you to answer questions like "What percentage of the **poorest households** seek care when their child has fever?"

## Key Concept

**IMPORTANT:** These functions filter data FIRST by wealth quintile, THEN calculate the indicator. This gives you the correct interpretation:
- ✅ **Correct**: "Care-seeking rate AMONG the poorest quintile" = P(seek care | Q1)
- ❌ **Incorrect**: "Proportion who are both poor AND seeking care" = P(Q1 AND seek care)

## Available Functions

### 1. `calc_csb_by_wealth_mbg()` - MBG Analysis

For spatial modeling and cluster-level data.

```r
csb_mbg <- calc_csb_by_wealth_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data,
  indicators = c("public", "private"),
  quintiles = 1  # Poorest quintile only
)
```

**Returns:** Named list of data.tables
- Keys: `csb_public_q1`, `csb_private_q1`
- Columns: `cluster_id`, `indicator`, `samplesize`, `x`, `y`

### 2. `calc_csb_by_wealth_dhs()` - Survey-Weighted Estimates

For national/regional reporting with confidence intervals.

```r
csb_dhs <- calc_csb_by_wealth_dhs(
  dhs_kr = kr_data,
  quintiles = 1:5,  # All quintiles
  region_var = "v024"  # Optional
)
```

**Returns:** `list(adm0, adm1)`
- `adm0`: National estimates by quintile
- `adm1`: Regional estimates by quintile (if `region_var` provided)
- Columns: `wealth_quintile`, `point`, `ci_l`, `ci_u`, `numerator`, `denominator`

### 3. `prep_csb_by_wealth_mbg()` - Convenience Wrapper

Simplified interface for single indicator.

```r
csb_public <- prep_csb_by_wealth_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data,
  indicator = "public",
  quintiles = 1
)
```

## Integration with MBG Pipeline

**Important:** These are **STANDALONE utility functions**. They are:
- ❌ NOT called automatically by `run_mbg_pipeline()`
- ❌ NOT in the valid indicators list (`.valid_mbg_indicators()`)
- ✅ Called directly by users for specialized analysis

### Why Not in Pipeline?

The pipeline processes standard indicators for all populations. Wealth stratification is a **specialized analysis** that:
1. Produces multiple outputs per indicator (one per quintile)
2. Requires custom interpretation and reporting
3. Is used for equity analysis, not routine monitoring

### How to Use

**For routine monitoring:** Use `run_mbg_pipeline()` with standard indicators
```r
run_mbg_pipeline(
  indicators = c("pfpr", "itn", "csb"),
  ...
)
```

**For wealth equity analysis:** Call wealth functions directly
```r
csb_by_wealth <- calc_csb_by_wealth_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data,
  indicators = c("public", "private"),
  quintiles = c(1, 5)  # Compare poorest vs richest
)
```

## Examples

### Example 1: Answer "% of poorest who seek care"

```r
result <- calc_csb_by_wealth_dhs(
  dhs_kr = kr_data,
  quintiles = 1
)

poorest_care <- result$adm0 %>%
  filter(
    wealth_quintile == 1,
    indicator_code == "csb_any"
  )

sprintf("%.1f%% of children in the poorest quintile sought care (95%% CI: %.1f%%-%.1f%%)",
        poorest_care$point * 100,
        poorest_care$ci_l * 100,
        poorest_care$ci_u * 100)
```

### Example 2: Map Wealth Inequality

```r
# Get cluster-level data for poorest and richest
csb_inequality <- calc_csb_by_wealth_mbg(
  dhs_kr = kr_data,
  gps_data = gps_data,
  indicators = "public",
  quintiles = c(1, 5)
)

# Now you have:
# - csb_public_q1: Poorest quintile by cluster
# - csb_public_q5: Richest quintile by cluster

# Use for MBG modeling or mapping
```

### Example 3: Compare All Quintiles Regionally

```r
regional_wealth <- calc_csb_by_wealth_dhs(
  dhs_kr = kr_data,
  quintiles = 1:5,
  region_var = "v024"
)

# Analyze regional gaps
gaps <- regional_wealth$adm1 %>%
  filter(indicator_code == "csb_any") %>%
  select(adm1, wealth_quintile, point) %>%
  pivot_wider(names_from = wealth_quintile,
              values_from = point,
              names_prefix = "q") %>%
  mutate(gap = q5 - q1)  # Richest minus poorest
```

## Technical Details

### Wealth Variables
- **KR recode**: `v190` (default)
- **HR recode**: `hv270`
- **IR recode**: `v190`
- **PR recode**: `hv270` (via household linkage)

### Output Format Compatibility
✅ MBG output matches standard MBG format (`cluster_id`, `indicator`, `samplesize`, `x`, `y`)
✅ DHS output matches standard long format with added `wealth_quintile` column
✅ Works with existing MBG spatial modeling tools
✅ Compatible with standard reporting pipelines

### Sample Sizes
The functions automatically:
- Remove NA wealth quintiles
- Filter to specified quintiles
- Report sample sizes in output
- Calculate survey-weighted confidence intervals

## Files Created

1. `R/dhs_helpers_wealth_stratified.R` - Core helper functions
2. `R/dhs_calc_csb_by_wealth_mbg.R` - MBG functions
3. `R/dhs_calc_csb_by_wealth_dhs.R` - Survey-weighted functions
4. `inst/examples/wealth_stratified_csb_example.R` - Usage examples
5. `inst/docs/WEALTH_STRATIFIED_INDICATORS.md` - This document

## Exports

All three functions are properly exported in NAMESPACE:
- `calc_csb_by_wealth_mbg`
- `calc_csb_by_wealth_dhs`
- `prep_csb_by_wealth_mbg`

Helper functions are internal (not exported):
- `.add_wealth_quintile`
- `.aggregate_to_mbg_clusters_by_wealth`
- `.compute_dhs_indicator_by_wealth`

## Documentation

Full roxygen2 documentation available:
```r
?calc_csb_by_wealth_mbg
?calc_csb_by_wealth_dhs
?prep_csb_by_wealth_mbg
```

## Limitations

1. Currently only implemented for care-seeking behavior (CSB)
2. Not integrated with automated pipeline (`run_mbg_pipeline`)
3. Requires manual interpretation for equity reporting

## Future Enhancements

Potential extensions:
- Wealth stratification for other indicators (ITN, ACT, PfPR, etc.)
- Add to valid indicators list for optional pipeline integration
- Create equity dashboard templates
- Add wealth gap calculation functions
