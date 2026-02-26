# Cross-Country DHS/MIS Variable Compatibility Assessment

**Countries:** BDI (Burundi), TGO (Togo), CIV (Côte d'Ivoire), GIN (Guinea), BFA (Burkina Faso)
**Audited:** January 2025
**Package version audited:** sntmethods (master)

Survey coverage per country:

| Country | Surveys |
|---------|---------|
| BDI | DHS 2010, MIS 2012, DHS 2016 |
| TGO | DHS 1988, MIS 2017 |
| CIV | DHS 1994, DHS 1998, DHS 2012, DHS 2021 |
| GIN | DHS 2012, DHS 2018, MIS 2021 |
| BFA | DHS 1993 only ⚠️ (incomplete dictionary — HR/IR absent; later surveys not represented) |

> **CIV note:** Dictionary now confirmed to cover DHS 1994, 1998, 2012, 2021.
> Malaria module variables (pfpr, itn, anemia) are only available from 2012 onward.
> Use `min_year >= 2012` for all malaria-specific analyses.

> **BFA note:** Dictionary covers DHS 1993 only — HR and IR datasets absent.
> Burkina Faso has surveys through MIS 2021 not represented here. Almost all
> malaria indicators are absent. Dictionary must be regenerated before running
> any BFA pipeline.

---

## Compatibility Matrix

Legend: ✅ Present | ❌ Absent | ⚠️ Present but wrong meaning | 🔶 Partial (some survey years only)

### Survey design variables (required by all functions)

| Variable | Used as | Recode | BDI | TGO | CIV | GIN | BFA |
|----------|---------|--------|-----|-----|-----|-----|-----|
| `hv001` | cluster ID | HR/PR | ✅ | ✅ | ✅ | ✅ | ✅ |
| `hv005` | weight | HR/PR | ✅ | ✅ | ✅ | ✅ | ✅ |
| `hv013` | household size | HR/PR | ✅ | ✅ | ✅ | ✅ | ✅ |
| `hv021` | PSU/cluster | HR/PR | ✅ | ✅ | ✅ | ✅ | ✅ |
| `hv022` | stratum | HR/PR | ✅ | ✅ | ✅ | ✅ | ✅ |
| `v001` | cluster ID | KR/IR | ✅ | ✅ | ✅ | ✅ | ✅ |
| `v005` | weight | KR/IR | ✅ | ✅ | ✅ | ✅ | ✅ |
| `v021` | PSU/cluster | KR/IR | ✅ | ✅ | ✅ | ✅ | ✅ |
| `v022` | stratum | KR/IR | ✅ | ✅ | ✅ | ✅ | ✅ |
| `hw1` | child age months | KR | ✅ | ✅ | ✅ | ✅ | ✅ |

---

### PfPR — `calc_pfpr_dhs_core` / `calc_pfpr_mbg` (Recode: PR)

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `hml35` | RDT result | 🔶 DHS 2016 + MIS only | ✅ MIS 2017 | ⚠️ 100% missing | 🔶 DHS 2012 + MIS only | ❌ |
| `hml32` | Microscopy result | 🔶 DHS 2016 + MIS only | ✅ MIS 2017 | ⚠️ 100% missing | 🔶 DHS 2012 + MIS only | ❌ |
| `hc1` | child age months | ✅ | ✅ | — | ✅ | — |
| `hv103` | present in household | ✅ | ✅ | — | ✅ | — |
| `hv042` | mother listed | ✅ | ✅ | — | ✅ | — |

**Inconsistencies:**
- `hml35` and `hml32` are absent from DHS 2010 (BDI), DHS 2018 (GIN), DHS 1988 (TGO), and entirely from BFA (DHS 1993 predates modern malaria module). For CIV, columns exist in DHS 2012/2021 PR but all values are missing.
- No fallback exists — if neither variable is present the pipeline will abort.

---

### ITN — `calc_itn_dhs_core` / `calc_itn_mbg` (Recode: HR + PR merged)

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `hml10_*` | ITN ownership (grep `^hml10`) | ✅ | ✅ MIS 2017 | 🔶 DHS 2012/2021 | ✅ | ❌ |
| `hml12` | Slept under ITN | ✅ | ✅ MIS 2017 | 🔶 DHS 2012/2021 | ✅ | ❌ |
| `hml18` | **Pregnancy status** (for pregnant-women sub-indicator) | ✅ | ✅ MIS 2017 | 🔶 DHS 2012/2021 label_mismatch | ✅ | ❌ |

**Inconsistencies:**
- `hml18` is labeled "Pregnancy from individual questionnaire" in all four
  dictionaries. This matches how the function uses it — as a pregnancy flag
  to identify pregnant women for the `itn_use_pregnant` sub-indicator.
  **This is correct.** However, this means the standard DHS "ITN access"
  metric (≥1 net per 2 household members) is NOT stored as a named variable
  in these surveys and cannot be read directly; it must be derived from
  `hml10_*`. Confirm that `calc_itn_dhs_core` derives access this way rather
  than reading `hml18` as an access indicator.
- ITN variables entirely absent from BFA (DHS 1993 predates them; HR dataset also missing).
- TGO ITN data limited to MIS 2017 only.
- CIV ITN data limited to DHS 2012/2021 only.

---

### IRS — `calc_irs_dhs_core` / `calc_irs_mbg` (Recode: HR)

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `hv253` | Household sprayed (primary) | ❌ | ❌ | ❌ | ❌ | ❌ |
| `hv253[a-z]` | Sprayed by… (fallback) | ❌ | ❌ | ❌ | ❌ | ❌ |

**Inconsistencies:**
- IRS absent from all five countries across all survey years.
- The graceful-skip fallback (`hv253` → `hv253a-z` → warn + return NULL)
  is already implemented and will handle this correctly for all countries.
- BFA HR dataset is also absent, so loading will fail before the variable check.
- **No action needed** for existing countries — existing code handles this.

---

### Fever — `calc_fever_dhs_core` / `calc_fever_mbg` (Recode: KR)

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `h22` | Had fever in last 2 weeks | ✅ | ✅ MIS 2017 | ✅ | ✅ | ✅ |
| `b5` | Child alive | ✅ | ✅ | ✅ | ✅ | ✅ |

**Inconsistencies:** None for BDI/TGO/CIV/GIN. BFA DHS 1993 KR has both variables present.

---

### Care-seeking — `calc_csb_dhs_core` (Recode: KR)

Function uses grep pattern `^h32[a-z0-9]+$` to auto-detect available sources.

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `h32a` | Govt hospital | ✅ | ✅ | ✅ | ✅ | ✅ |
| `h32b` | Govt health centre | ✅ | ✅ | ✅ | ✅ | ✅ |
| `h32c` | Govt health post | ✅ | ✅ | ✅ | ✅ | ✅ |
| `h32d` | Mobile clinic | ✅ | ✅ | ✅ | ✅ | ❌ |
| `h32e` | Community health worker | ✅ | ✅ | ❌ | ✅ | ❌ |
| `h32f–h32h` | Other public sector | 🔶 | ✅ | ❌ | 🔶 | ⚠️ non-standard |
| `h32j–h32m` | Private formal | ✅ | ✅ | ❌ | ✅ | ❌ |
| `h32s–h32u` | Private informal/traditional | ✅ | ✅ | ❌ | ✅ | ❌ |

**Inconsistencies:**
- CIV (DHS 1994) only has `h32a–h32d`; CHW (`h32e`) and private sector
  variables (`h32j+`) are absent. The grep-based auto-detection will silently
  produce a narrower public-only CSB indicator without warning.
- BDI and GIN have gaps in the `h32f–h32h` extended public sector slots
  depending on survey year — grep detection handles this gracefully.
- BFA 1993 has h32a/b/c but is missing standard h32d (mobile clinic) and h32e (CHW). It has non-standard h32 variants (h32f, h32g, h32h, h32i, h32k, h32l, h32o, h32p, h32t, h32u, h32x, h32y, h32z) that the grep pattern `^h32[a-z0-9]+$` will auto-detect and include. This could pollute the public-sector care-seeking indicator with unintended country-specific response categories. **New edge case: verify whether the grep-based CSB auto-detection should exclude these BFA-specific non-standard slots.**

---

### Malaria diagnosis — `calc_malaria_dx_dhs_core` (Recode: KR)

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `h47` | Blood taken for test (primary) | ❌ | ❌ | ❌ | ❌ | ❌ |
| `ml1` | Malaria test result (fallback) | ⚠️ | ⚠️ | ❌ | ⚠️ | ❌ |

**Inconsistencies — CRITICAL:**
- `h47` is absent from **all five countries** across all survey years.
- `ml1` is present in BDI, TGO (MIS 2017), and GIN but is labeled
  **"Times took Fansidar during pregnancy"** — this is the IPTp dose count
  variable, not a malaria test result. Using it as the `malaria_dx` fallback
  would produce completely wrong results.
- **The `malaria_dx` indicator cannot be computed for any of these five
  countries.** No graceful skip exists for this path. The pipeline will
  either abort or silently use the wrong variable.
- `act_tested` (ACT among test-positives) shares the same denominator and
  is equally affected.
- **Action required:** Add a graceful NULL-return for the malaria_dx path
  when neither `h47` nor a valid `ml1` is present.

---

### Antimalarial treatment — `calc_antimalarial_dhs_core` (Recode: KR)

Function tries `ml13a–ml13h` first; falls back to `h37a–h37h` if absent.

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `ml13a` | Fansidar | ✅ | ✅ MIS 2017 | 🔶 DHS 2012 only | ✅ | ❌ |
| `ml13b` | Chloroquine | 🔶 MIS+DHS2016 | ✅ | 🔶 DHS 2012 only | ✅ | ❌ |
| `ml13c` | Amodiaquine | 🔶 DHS 2016 only | ✅ | 🔶 DHS 2012 only | ✅ | ❌ |
| `ml13d` | Quinine | ✅ | ✅ | 🔶 DHS 2012 only | ✅ | ❌ |
| `ml13e` | ACT (artemisinin combo) | ✅ | ✅ | 🔶 DHS 2012 only | ✅ | ❌ |
| `ml13h` | Other antimalarial | ✅ | ✅ | 🔶 DHS 2012 only | ✅ | ❌ |
| `h37a–h37e` | Antimalarial alt (older surveys) | ❌ | ✅ | 🔶 DHS 2021 only | 🔶 DHS only | ✅ h37a only |
| `h37e` | ACT alt | ❌ | ✅ | 🔶 DHS 2021 only | 🔶 DHS only | ❌ |

**Inconsistencies:**
- BDI has no `h37` series at all — the primary `ml13` series works for all
  survey years except DHS 2010 which lacks `ml13b` and `ml13c`.
- GIN MIS 2021 has no `h37` series; DHS 2012/2018 have both. The fallback
  logic correctly handles this.
- CIV (DHS 1994 and 1998) has no treatment drug variables at all. DHS 2012 uses `ml13a-h`; DHS 2021 uses `h37a-h`. Neither year has both series.
- TGO: `ml13` series present in MIS 2017; `h37` series also present in
  MIS 2017 (both paths available simultaneously — grep logic will prefer
  `ml13`).
- **CIV variable name shift:** DHS 2012 uses `ml13a-h`; DHS 2021 uses `h37a-h`. Neither year has both series. Multi-year pipelines must handle the naming shift between surveys.
- BFA 1993 has only h37a (Fansidar) — no ACT, quinine, chloroquine, or amodiaquine variables.

---

### ACT — `calc_act_dhs` (Recode: KR)

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `ml13e` | ACT (primary) | ✅ | ✅ MIS 2017 | 🔶 DHS 2012 (ml13e) / DHS 2021 (h37e) | ✅ | ❌ |
| `h37e` | ACT alt (fallback) | ❌ | ✅ | 🔶 DHS 2021 only | 🔶 DHS only | ❌ |

**Inconsistencies:**
- ACT variables available for BDI, TGO (MIS 2017), and GIN. BFA absent.
- BDI relies entirely on `ml13e` (no `h37e` fallback available).
- GIN MIS 2021 has only `ml13e`; DHS 2012/2018 have both.
- CIV: DHS 2012 uses `ml13e`; DHS 2021 uses `h37e` — no year has both. Multi-year pipelines must handle this naming shift.

---

### Anemia — `calc_anemia_mbg` (Recode: PR)

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `hc56` | Haemoglobin adjusted (primary) | ✅ | ✅ MIS 2017 | ⚠️ 100% missing | ✅ | ❌ |
| `hw53` | Haemoglobin unadjusted (KR) | 🔶 DHS only | ✅ MIS 2017 | 🔶 DHS 2012/2021 | 🔶 DHS only | ❌ |

**Inconsistencies:**
- The function uses `hc56` (PR recode) as primary — this is present in BDI
  (all surveys), TGO MIS 2017, and GIN (all surveys). BFA entirely absent.
- `hw53` (KR recode) is absent from BDI MIS 2012 and GIN MIS 2021 — but
  the function does not use `hw53`, so this is not a code issue.
- No graceful skip exists. BFA and TGO DHS 1988 will abort (no PR malaria module).
- **CIV:** `hc56` is 100% missing in both DHS 2012 and DHS 2021 despite the column existing. Use `hw53` from KR for anemia. **New edge case: anemia function uses `hc56` (PR) as primary — for CIV it will silently return empty results unless the function falls back to `hw53`.**

---

### ANC — `calc_anc_dhs_core` (Recode: IR)

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `m14_1` | ANC visit count | 🔶 DHS only | ❌ | ✅ | ✅ | ❌ |
| `v008` | Interview date (CMC) | ✅ | ✅ | ✅ | ✅ | — |
| `b3_01` | Birth date (CMC) | ✅ | ✅ | ✅ | ✅ | — |

**Inconsistencies:**
- `m14_1` absent from BDI MIS 2012 — ANC not collected in that survey.
- `m14_1` entirely absent from TGO across all surveys. ANC will abort for
  TGO. **Graceful skip needed.**
- CIV and GIN have `m14_1` present.
- BFA IR dataset is absent — `m14_1` cannot be accessed. ANC will fail for BFA.

---

### IPTp — `calc_iptp_dhs_core` (Recode: IR)

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `m49a_1` | SP taken in pregnancy (primary) | ✅ | ✅ MIS 2017 | 🔶 DHS 2012/2021 | ✅ | ❌ |
| `ml1_1` | SP dose count (primary for 2+/3+/4+) | ✅ | ✅ MIS 2017 | 🔶 DHS 2012/2021 | ✅ | ❌ |

**Inconsistencies:**
- IPTp works for BDI (all surveys), TGO MIS 2017, and GIN (all surveys).
- CIV (DHS 1994, 1998) predates IPTp policy — both variables absent for those years. Available in DHS 2012/2021.
- TGO DHS 1988 also absent. Pipeline will abort for those survey years
  without a graceful skip.
- BFA IR dataset absent — both variables inaccessible. Pipeline will fail for BFA.

---

### EPI — `calc_epi_dhs_core` (Recode: KR)

| Variable | Function meaning | BDI label | TGO | CIV label | GIN | BFA |
|----------|-----------------|-----------|-----|-----------|-----|-----|
| `h2` | BCG | "Received BCG" ✅ | ❌ | "Received BCG" ✅ | 🔶 DHS 2018 only | ✅ |
| `h5` | DPT3 | — | ❌ | — | — | — |
| `h7` | **Polio dose 2** (function) | "Received DPT 3" ⚠️ | ❌ | "Received DPT 3" ⚠️ | 🔶 DHS 2018 "Received DPT 3" ⚠️ | ✅⚠️ |
| `h9` | Measles dose 1 | "Received MEASLES" ✅ | ❌ | "Received MEASLES" ✅ | 🔶 DHS 2018 only | ✅ |

**Inconsistencies — CRITICAL:**
- **`h7` label conflict.** The function assigns `h7` as "Polio dose 2",
  but in the BDI, CIV, GIN, and BFA dictionaries `h7` is explicitly labeled
  **"Received DPT 3"**. In standard DHS coding, `h7` = Polio2 and `h5` = DPT3,
  but some survey editions swap or shift these. This means either:
  - The function is using the wrong variable for DPT3 (should be `h5`
    but the data stores it in `h7`), or
  - The dictionary labels are wrong.
  - **Must verify `h5` vs `h7` for DPT3 against the raw KR codebook
    for each country before running EPI indicators.**
- EPI entirely absent from TGO (MIS survey design omits vaccination module).
- GIN EPI only available in DHS 2018; absent from DHS 2012 and MIS 2021.
- Graceful skip needed for TGO and for GIN DHS 2012 / MIS 2021.
- BFA 1993 has h2/h7/h9 (same DPT3 label issue on h7) but data is from 1993 and not analytically useful.

---

### U5MR — `calc_u5mr_dhs_core` (Recode: KR)

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `b3` | Date of birth (CMC) | ✅ | ✅ | ✅ | ✅ | ✅ |
| `b5` | Child alive | ✅ | ✅ | ✅ | ✅ | ✅ |
| `b7` | Age at death (months) | 🔶 DHS only | 🔶 DHS only | ✅ | 🔶 DHS only | ⚠️ 100% missing |
| `v008` | Interview date (CMC) | ✅ | ✅ | ✅ | ✅ | ✅ |

**Inconsistencies:**
- `b7` is absent from MIS surveys in BDI (MIS 2012), TGO (MIS 2017), and
  GIN (MIS 2021). U5MR cannot be computed from MIS data in any of these
  countries — MIS survey design omits the detailed birth history needed for
  mortality estimation.
- The pipeline will fail for MIS survey years. Graceful skip needed.
- BFA 1993 has b7 as a column but it is 100% missing (n_unique=0). This may indicate very few or no recorded deaths in the 1993 cohort, or a data quality issue. Verify against raw KR recode.

---

### SMC — `calc_smc_dhs_core` / `calc_smc_mbg` (Recode: KR)

| Variable | Used as | BDI | TGO | CIV | GIN | BFA |
|----------|---------|-----|-----|-----|-----|-----|
| `hml43` | SMC received (primary) | ❌ | ❌ | ❌ | ❌ | ❌ |
| `ml13g` | SMC alt (fallback) | ❌ | ⚠️ | ❌ | ⚠️ | ❌ |

**Inconsistencies — CRITICAL:**
- `hml43` absent from all five countries across all survey years.
- `ml13g` is present in TGO (MIS 2017) and GIN (limited records) but is
  labeled **"CS antimalarial taken for fever/cough"** — a country-specific
  antimalarial treatment slot, NOT an SMC receipt indicator.
- Using `ml13g` as the SMC fallback in TGO or GIN would silently produce
  wrong results (measuring a specific fever treatment drug, not SMC).
- **No valid SMC data exists in any of these five countries.**
- The pipeline will abort (no graceful skip). **Graceful skip required
  before running any of these countries.**

---

## Summary: Actions Required Before Running

| Fix needed | Affects | Priority |
|-----------|---------|----------|
| Graceful skip for `malaria_dx` / `act_tested` when `h47` absent and `ml1` carries wrong meaning | BDI, TGO, CIV, GIN, BFA — all surveys | 🔴 Critical |
| Graceful skip for SMC (`hml43` absent; `ml13g` is wrong variable) | BDI, TGO, CIV, GIN, BFA — all surveys | 🔴 Critical |
| Verify `h5` vs `h7` for DPT3 — label says "DPT 3" but code assigns `h7` = Polio2 | BDI, CIV, GIN, BFA | 🔴 Critical |
| Regenerate BFA dictionary from post-1993 surveys (DHS 2010, MIS 2014/2017/2021) | BFA — all indicators | 🔴 Critical |
| Add graceful skip for missing HR/IR datasets (BFA loading will fail without them) | BFA | 🔴 Critical |
| Graceful skip for anemia when `hc56` absent | TGO DHS 1988, BFA | 🟠 High |
| Graceful skip for ANC (`m14_1`) | TGO all surveys, BDI MIS 2012, BFA | 🟠 High |
| Graceful skip for EPI when `h2`/`h7`/`h9` absent | TGO all surveys, GIN DHS 2012 + MIS 2021 | 🟠 High |
| Graceful skip for U5MR when `b7` absent | BDI MIS 2012, TGO MIS 2017, GIN MIS 2021 | 🟠 High |
| Verify BFA non-standard h32 variants (h32f/g/h/i/k/l/o/p/t/u/x/y/z) excluded from CSB grep auto-detection | BFA, potentially others | 🟠 High |
| CIV anemia: `hc56` is 100% missing; confirm function falls back to `hw53` (KR) | CIV DHS 2012/2021 | 🟠 High |
| Graceful skip for IPTp when `m49a_1` / `ml1_1` absent | CIV DHS 1994/1998, TGO DHS 1988, BFA | 🟡 Medium |
| Graceful skip for PfPR when `hml35`/`hml32` absent | BDI DHS 2010, GIN DHS 2018, TGO DHS 1988, BFA | 🟡 Medium |
| Confirm `hml18` = pregnancy (not ITN access) in raw PR recodes | BDI, TGO, GIN | 🟡 Medium |
| CIV pfpr: `hml35`/`hml32` are 100% missing despite columns existing — pipeline should skip gracefully rather than return empty results | CIV | 🟡 Medium |
| CIV antimalarial variable shift: 2012 uses `ml13a-h`, 2021 uses `h37a-h` — ensure multi-year pipeline handles naming difference | CIV | 🟡 Medium |
| Graceful skip for SMC when `ml13g` is present but carries wrong meaning | TGO, GIN | 🟡 Medium |

---

## Cross-country variable presence at a glance

| Variable | BDI | TGO | CIV | GIN | BFA | Function |
|----------|-----|-----|-----|-----|-----|----------|
| `hml35` | 🔶 | 🔶 | ⚠️ 100% missing | 🔶 | ❌ | pfpr |
| `hml32` | 🔶 | 🔶 | ⚠️ 100% missing | 🔶 | ❌ | pfpr |
| `hml10_*` | ✅ | 🔶 | 🔶 DHS 2012/2021 | ✅ | ❌ | itn |
| `hml12` | ✅ | 🔶 | 🔶 DHS 2012/2021 | ✅ | ❌ | itn |
| `hml18` | ✅ | 🔶 | 🔶 DHS 2012/2021 ⚠️ | ✅ | ❌ | itn (pregnancy flag) |
| `hv253` | ❌ | ❌ | ❌ | ❌ | ❌ | irs |
| `hv253[a-z]` | ❌ | ❌ | ❌ | ❌ | ❌ | irs fallback |
| `h22` | ✅ | 🔶 | ✅ | ✅ | ✅ | fever / csb / malaria_dx |
| `h32a-d` | ✅ | ✅ | ✅ | ✅ | 🔶 | csb (public) — BFA: a/b/c only; d absent |
| `h32e+` | ✅ | ✅ | ❌ | ✅ | ❌ | csb (CHW/private) — BFA: standard absent; non-standard present |
| `h47` | ❌ | ❌ | ❌ | ❌ | ❌ | malaria_dx |
| `ml1` (test) | ⚠️ | ⚠️ | ❌ | ⚠️ | ❌ | malaria_dx fallback (wrong meaning) |
| `ml13e` | ✅ | 🔶 | 🔶 DHS 2012 | ✅ | ❌ | act |
| `h37e` | ❌ | ✅ | 🔶 DHS 2021 | 🔶 | ❌ | act fallback |
| `hc56` | ✅ | 🔶 | ⚠️ 100% missing | ✅ | ❌ | anemia |
| `m14_1` | 🔶 | ❌ | ✅ | ✅ | ❌ | anc — BFA: IR absent |
| `m49a_1` | ✅ | 🔶 | 🔶 DHS 2012/2021 | ✅ | ❌ | iptp — BFA: IR absent |
| `ml1_1` | ✅ | 🔶 | 🔶 DHS 2012/2021 | ✅ | ❌ | iptp dose count — BFA: IR absent |
| `h2` (BCG) | ✅ | ❌ | ✅ | 🔶 | ✅ | epi |
| `h7` (DPT3?) | ✅⚠️ | ❌ | ✅⚠️ | 🔶⚠️ | ✅⚠️ | epi — label conflict |
| `h9` (measles) | ✅ | ❌ | ✅ | 🔶 | ✅ | epi |
| `b3` | ✅ | ✅ | ✅ | ✅ | ✅ | u5mr |
| `b5` | ✅ | ✅ | ✅ | ✅ | ✅ | u5mr / fever |
| `b7` | 🔶 | 🔶 | ✅ | 🔶 | ⚠️ 100% missing | u5mr (absent MIS years; BFA: column exists but all missing) |
| `hml43` | ❌ | ❌ | ❌ | ❌ | ❌ | smc |
| `ml13g` | ❌ | ⚠️ | ❌ | ⚠️ | ❌ | smc fallback (wrong meaning) |
