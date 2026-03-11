# WMR Survey Analysis Output Specification

## Purpose

This document defines the **exact output format** for all DHS survey
indicator functions in `sntmethods`. The format matches the WHO World
Malaria Report (WMR) `survey_analysis.xlsx` structure. The ACT
indicator function (`calc_act_dhs()`) is the **gold standard
implementation** — all other `calc_*_dhs()` functions should produce
identical output structure.

## Reference Files

| File | Description |
|------|-------------|
| `WMR2022_analysis/_data/survey_analysis.xlsx` | 30,170 rows, 181 unique indicators across 8 families |
| `wmr-debug/02-scripts/2d_act_indicators.R` | Gold standard standalone ACT script |
| `R/dhs_calc_act.R` | Package implementation (gold standard) |

---

## Output Structure

### Return Type

Every `calc_*_dhs()` function returns a **named list of tibbles**,
one per administrative level:

```r
list(
  adm0 = tibble(...),   # National (always present)
  adm1 = tibble(...),   # Admin-1 (when region_var or shapefile)
  adm2 = tibble(...)    # Admin-2 (when shapefile with adm2)
)
```

### Column Schema (Long Format)

Each tibble has **one row per indicator per location**.

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| `survey_id` | chr | `{iso2}{year}{survey_type}` | `"TG2017MIS"` |
| `iso3` | chr | ISO 3166-1 alpha-3 | `"TGO"` |
| `iso2` | chr | DHS 2-letter code | `"TG"` |
| `survey_type` | chr | `"DHS"`, `"MIS"`, or `"AIS"` | `"MIS"` |
| `survey_year` | int | From `v007` | `2017` |
| `adm0` | chr | Country name, UPPERCASE | `"TOGO"` |
| `adm1`* | chr | Admin-1 name, UPPERCASE | `"MARITIME"` |
| `adm2`* | chr | Admin-2 name, UPPERCASE | `"LACS"` |
| `type` | chr | Always `"survey_weighted"` | `"survey_weighted"` |
| `geo_source` | chr | `"survey"` or `"gps"` | `"survey"` |
| `point` | dbl | Proportion (0-1), rounded 3dp | `0.238` |
| `ci_l` | dbl | Lower 95% CI, clamped >= 0 | `0.202` |
| `ci_u` | dbl | Upper 95% CI, clamped <= 1 | `0.280` |
| `counts` | int | Unweighted numerator (N with outcome) | `209` |
| `denominator` | int | Unweighted denominator (N in subgroup) | `753` |
| `indicator` | chr | SCREAMING_SNAKE name | `"ACT_ANTIMALARIAL"` |
| `indicator_code` | chr | Short snake_case code | `"act_antimal"` |
| `indicator_title` | chr | Human-readable title | `"Use of ACTs among antimalarial recipients"` |
| `numerator_description` | chr | What the numerator measures | `"Received ACT treatment"` |
| `denominator_description` | chr | What the denominator measures | `"Febrile U5 who received any antimalarial"` |
| `denominator_code` | chr | Short code for denominator | `"feb_u5_am"` |

*\*`adm1`/`adm2` columns appear only in subnational tabs.*

### WMR `survey_analysis.xlsx` Column Mapping

Our output maps to the WMR reference as follows:

| Our Column | WMR Column | Notes |
|------------|------------|-------|
| `survey_id` | `surveyid` | Same format |
| `iso3` | `iso3` | Same |
| `survey_year` | `surveyyear` | Same |
| `point` | `point` | WMR stores as proportion (0-1) |
| `ci_l` | `ci_l` | Same |
| `ci_u` | `ci_u` | Same |
| `counts` | `counts` | Same |
| `denominator` | `denominator` | WMR uses text description; we use integer + separate description column |
| `indicator` | `INDICATOR` | Same names, SCREAMING_SNAKE |
| `numerator_description` | `NUMERATOR_DESCRIPTION` | Same |
| `denominator_description` | `DENOMINATOR_DESCRIPTION` | Same |
| `indicator_code` | *(new)* | Short code, not in WMR |
| `indicator_title` | *(new)* | Human-readable, not in WMR |
| `denominator_code` | *(new)* | Short code, not in WMR |
| — | `var1` | WMR internal; e.g., `"receive_act"` |
| — | `var2` | WMR internal; e.g., `"I(1)"` |
| — | `varby` | WMR internal; stratifier value |
| — | `country` | We use `adm0` (UPPERCASE) |

---

## Indicator Families (181 in WMR)

### Family 1: ACT (11 indicators) — GOLD STANDARD

**Numerator**: `receive_act` (composite of all ACT drug variables)
**Base population**: Febrile U5 children

| INDICATOR | NUMERATOR_DESCRIPTION | DENOMINATOR_DESCRIPTION |
|-----------|----------------------|------------------------|
| `ACT_CARE_SEEKERS` | Get malaria ACT treatment | Under 5 with fever - seek treatment in public or private sector |
| `ACT_ANTIMALARIAL` | Get malaria ACT treatment | Under 5 with fever and receive antimalarial |
| `ACT_ANY_TREATMENT` | Get malaria ACT treatment | Under 5 with fever - seek treatment in public or private sector and receive antimalarial |
| `ACT_TRAINED_ANTIMALARIAL` | Get malaria ACT treatment | Under 5 with fever - seek treatment in trained provider and receive antimalarial |
| `ACT_PUBLIC_ANTIMALARIAL` | Get malaria ACT treatment | Under 5 with fever - seek treatment in public sector and receive antimalarial |
| `ACT_PUBLIC_NOCHW_ANTIMALARIAL` | Get malaria ACT treatment | Under 5 with fever - seek treatment in public sector (excluding CHW) and receive antimalarial |
| `ACT_PUBLIC_CHW_ANTIMALARIAL` | Get malaria ACT treatment | Under 5 with fever - seek treatment in CHW and receive antimalarial |
| `ACT_PRIVATE_FORMAL_ANTIMALARIAL` | Get malaria ACT treatment | Under 5 with fever - seek treatment in private formal sector and receive antimalarial |
| `ACT_PRIVATE_PHARMACY_ANTIMALARIAL` | Get malaria ACT treatment | Under 5 with fever - seek treatment in pharmacy and receive antimalarial |
| `ACT_PRIVATE_INFORMAL_ANTIMALARIAL` | Get malaria ACT treatment | Under 5 with fever - seek treatment in private informal and receive antimalarial |
| `ACT_PRIVATE_FORMAL_PHA_ANTIMALARIAL` | Get malaria ACT treatment | Under 5 with fever - seek treatment in private formal or pharmacy and receive antimalarial |

### Family 2: ANTIMALARIAL (11 indicators)

**Numerator**: `receive_antimalarial` (any antimalarial drug)
**Base population**: Febrile U5 children

| INDICATOR | NUMERATOR_DESCRIPTION | DENOMINATOR_DESCRIPTION |
|-----------|----------------------|------------------------|
| `ANTIMALARIAL` | Receive antimalarial | Under 5 with fever |
| `ANTIMALARIAL_ANY_TREATMENT` | Receive antimalarial | Under 5 with fever - seek treatment in public or private sector |
| `ANTIMALARIAL_TRAINED` | Receive antimalarial | Under 5 with fever - seek treatment in trained provider |
| `ANTIMALARIAL_PUBLIC` | Receive antimalarial | Under 5 with fever - seek treatment in public sector |
| `ANTIMALARIAL_PUBLIC_NOCHW` | Receive antimalarial | Under 5 with fever - seek treatment in public sector (excluding CHW) |
| `ANTIMALARIAL_CHW` | Receive antimalarial | Under 5 with fever - seek treatment in CHW |
| `ANTIMALARIAL_PRIVATE` | Receive antimalarial | Under 5 with fever - seek treatment in private sector |
| `ANTIMALARIAL_FORMAL` | Receive antimalarial | Under 5 with fever - seek treatment in private formal sector |
| `ANTIMALARIAL_PHARMACY` | Receive antimalarial | Under 5 with fever - seek treatment in pharmacy |
| `ANTIMALARIAL_PRIVATE_INFORMAL` | Receive antimalarial | Under 5 with fever - seek treatment in private informal |
| `ANTIMALARIAL_FORMAL_PHARMACY` | Receive antimalarial | Under 5 with fever - seek treatment in private formal or pharmacy |

### Family 3: CSB — Care-Seeking Behaviour (37 indicators)

**Numerator**: Treatment-seeking by sector
**Base population**: Febrile U5 (+ stratifiers: residence, wealth, education, sex)

Base indicators (7):
`CSB_ANY_TREATMENT_UNDER5_FEVER`, `CSB_PUBLIC`, `CSB_PUBLIC_NO_CHW`, `CSB_CHW`, `CSB_PRIVATE`, `CSB_PRIVATE_FORMAL`, `CSB_PRIVATE_FORMAL_PHARMACY`, `CSB_PHARMACY`, `CSB_PRIVATE_INFORMAL`, `CSB_TRAINED`, `CSB_NOTREATMENT`

Stratified variants (by `_RURAL`, `_URBAN`, `_REFUGEE`, `_EDUCATION_HIGH`, `_EDUCATION_LOW`, `_WEALTH_HIGH`, `_WEALTH_LOW`, `_FEMALE`, `_MALE`).

### Family 4: FEVER (18 indicators)

**Numerator**: `feveryno` (had fever in last 2 weeks)
**Base population**: Under 5 children

Base: `FEVER`
Stratified by: age bands (6), sex (2), residence (3), education (2), wealth (4).

### Family 5: MALARIA_DX — Diagnostic Testing (20 indicators)

**Numerator**: `malaria_dx` (received diagnostic test)
**Base population**: Febrile U5 children

Same CSB sector breakdown as ACT/ANTIMALARIAL, plus antimalarial-receipt cross-tabs.

### Family 6: MALARIA_PARASITES (18 indicators)

**Numerator**: `malaria_parasite` (positive RDT/microscopy)
**Base population**: Under 5 (from PR file, not KR)

Stratified by: age bands, sex, residence, education, wealth.

### Family 7: ITN (48 indicators)

Four sub-families:
- `ACCESS_ITN` (8): Population-level ITN access
- `WITH_ITN` (8): Households with at least 1 ITN
- `ENOUGH_ITN` (8): Households with 1 ITN per 2 people
- `USE_ITN` (24): ITN use (all pop, U5, pregnant women)

Stratified by: wealth, residence.

### Family 8: ANEMIA (18 indicators)

**Numerator**: `anemia_yno` (Hb < 11 g/dL)
**Base population**: Under 5 children (from PR file)

Stratified by: age bands, sex, residence, education, wealth.

---

## Survey Estimation Method

All indicators use `survey::svyciprop()` with **logit confidence
intervals** (not `svymean()`). This produces proper bounded CIs for
proportions near 0 or 1.

```r
svy <- survey::svydesign(
  ids     = ~cluster_id,
  strata  = ~stratum_id,
  weights = ~survey_weight,
  data    = filtered,
  nest    = TRUE
)
est <- survey::svyciprop(
  ~outcome_var, svy,
  method = "logit", na.rm = TRUE
)
ci <- confint(est)
```

Setting: `options(survey.lonely.psu = "adjust")`

---

## Implementation Pattern

### Indicator Dictionary

Each family defines a dictionary with:

```r
list(
  indicator      = "SCREAMING_SNAKE_NAME",
  indicator_code = "short_snake_code",
  indicator_title = "Human-readable title",
  denom_code     = "short_denom_code",
  filter_expr    = quote(condition == 1),
  num_desc       = "Numerator description",
  denom_desc     = "Denominator description"
)
```

### Compute Loop

```r
results <- purrr::map_dfr(dict, function(cond) {
  .compute_wmr_indicator(
    data      = prepared_data,
    condition = cond,
    group_var = group_var
  )
})
```

### Output Assembly

```r
# National tab
meta_cols <- tibble(
  survey_id, iso3, iso2,
  survey_type, survey_year, adm0
)
adm0_tbl <- bind_cols(meta_cols, national_rows) |>
  mutate(type = "survey_weighted",
         geo_source = geo_src)

# Subnational tabs
adm1_tbl <- bind_cols(meta_cols, regional_rows) |>
  mutate(!!admin_col := toupper(location))

list(adm0 = adm0_tbl, adm1 = adm1_tbl)
```

---

## Care-Seeking Behaviour (CSB) Classification

CSB indicators are derived from `h32*` variables in the KR file.
The classification follows the WMR standard:

| CSB Category | h32 Variables | Description |
|-------------|---------------|-------------|
| `public` | h32a-h32i | Government hospital, health centre, health post, etc. |
| `chw` | h32na-h32ne | Community health worker, NGO |
| `private_formal` | h32j-h32m | Private hospital, clinic, doctor |
| `private_informal` | h32s-h32u | Traditional practitioner, other |
| `pharmacy` | h32n-h32r | Pharmacy, drug shop |

Derived indicators:
- `csb_public` = public OR chw
- `csb_private` = private_formal OR private_informal OR pharmacy
- `csb_any_treatment` = csb_public OR csb_private
- `csb_trained_provider` = csb_public OR csb_private_formal_pha
- `csb_private_formal_pha` = private_formal OR pharmacy

---

## Antimalarial Variable Detection

### Drug Series

DHS uses two parallel coding systems for antimalarial drugs:
- **ml13 series** (newer surveys): ml13a-ml13h + multi-letter suffixes (ml13aa, ml13da)
- **h37 series** (older surveys): h37a-h37h

**Never mix** ml13 and h37 in the same composite.

### ACT Detection (Label-Based)

ACT = artemisinin-based **combination** therapy. Detection pattern:

```r
act_pattern <- paste0(
  "\\bact\\b|combin.*artemi|artemi.*combin|",
  "artemether.+lumef|artesunate.+amodiaq|",
  "dihydroartemis|coartem|\\bcta\\b"
)
exclude_pattern <- "rectal|injection|\\biv\\b|monotherapy"
```

### Antimalarial Detection

```r
antimalarial_pattern <- paste0(
  "antimalarial|fansidar|chloroquine|",
  "amodiaquine|quinine|artemether|",
  "artesunate|dihydroartemis|artemisinin|",
  "coartem|\\bsp\\b|\\bcta\\b|\\bact\\b|",
  "mefloquine|piperaquine|lumefantrine"
)
am_exclude <- paste0(
  "don.t know|\\bdk\\b|\\bother\\b|",
  "\\bnone\\b|\\bno \\b|\\bmissing\\b"
)
```

### Two-Pass Approach

When `dhs_read()` standardises labels (via `open_dataset()`),
country-specific drug names are lost. The **two-pass** approach:

1. Read via `dhs_read()` for analysis (standardised schema)
2. Read raw parquet for labels (preserves drug names)
3. Detect ACT/antimalarial vars from raw labels
4. Copy missing variables from raw into analysis dataset

---

## Stratification Suffixes (for future implementation)

WMR indicators use consistent suffixes for stratified variants:

| Suffix | var1 filter | Denominator qualifier |
|--------|-------------|----------------------|
| `_RURAL` | `residence == "rural"` | `residence rural` |
| `_URBAN` | `residence == "urban"` | `residence urban` |
| `_REFUGEE` | `residence == "refugee"` | `residence refugee` |
| `_HIGH_WEALTH` | `wealth == "high"` | `high wealth` |
| `_LOW_WEALTH` | `wealth == "low"` | `low wealth` |
| `_NON_HIGH_WEALTH` | `wealth != "high"` | `non high wealth` |
| `_NON_LOW_WEALTH` | `wealth != "low"` | `non low wealth` |
| `_EDUCATION_HIGH` | `education >= secondary` | `mother secondary or higher` |
| `_EDUCATION_LOW` | `education <= primary` | `mother none or primary` |
| `_FEMALE` | `sex == "female"` | `female` |
| `_MALE` | `sex == "male"` | `male` |
| `_AGE_00_06` | `age %in% 0:6` | `between 0-6 months` |
| `_AGE_06_12` | `age %in% 7:12` | `between 6 and 12 months` |
| `_AGE_12_24` | `age %in% 13:24` | `between 12 and 24 months` |
| `_AGE_24_36` | `age %in% 25:36` | `between 24 and 36 months` |
| `_AGE_36_48` | `age %in% 37:48` | `between 36 and 48 months` |
| `_AGE_48_60` | `age %in% 49:60` | `between 48 and 60 months` |
