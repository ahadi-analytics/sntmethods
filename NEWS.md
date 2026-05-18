# sntmethods 0.9.0

## New `epi_*` outbreak detection module

A tidymodels-style framework for univariate outbreak detection on weekly
count series, with four composable verbs:

- `epi_detect()` — run one or more detectors against a baseline / target
  split. Returns an `epi_detection_run` S3 object with print, summary,
  `as_tibble`, plot, and autoplot methods.
- `epi_evaluate()` — grade detector performance against labelled or
  simulated truth. Computes standard metrics (sens / spec / PPV / NPV /
  accuracy / F1 / F-beta), AUC (Mann–Whitney), AUPRC (trapezoidal),
  surveillance metrics (time-to-detect, false-alarm rate, persistence,
  shoulder fraction), and a configurable cost. Headline diagnostic is the
  leave-one-method-out (LOO) ensemble delta. Includes a `k`-of-`M`
  majority-vote sweep, operating-point analysis via outbreak injection,
  FP-shoulder analysis, and a nine-plot diagnostic suite available through
  `autoplot()`.
- `epi_ensemble()` — combine per-method alarms via `majority_vote`,
  `weighted_vote`, or `score_average`. Weights may be derived from an
  `epi_evaluation` object. Returns an `epi_ensemble` S3 object.
- `epi_recommend()` — profile a series and emit a ranked, rationalised
  list of recommended detectors. Three tie-break strategies:
  `simplicity` (default), `evidence` (per-method AUC), and
  `cost_minimising`. Includes `profile_series()` as a public helper.

### Built-in detectors (14 total)

Hand-rolled: `threshold` (mean / median / Q3 flavours), `endemic_channel`,
`stl_residual`.  Wrappers around `surveillance`: `ears_c1`, `ears_c2`,
`ears_c3`, `cusum_classical`, `farrington` (Farrington Flexible), `glrnb`.
Wrappers around external packages: `trending` (NB-GLM with harmonics),
`arima` (`forecast::auto.arima`), `anomalize_stl` (`anomalize::anomalize`),
`changepoint_pelt` (`changepoint::cpt.meanvar`), `bayesian_changepoint`
(`Rbeast::beast`).

### Registry and extension

- `register_detector()` lets users plug in third-party detectors via the
  `new_epi_detector()` constructor and the `fit_detector()` /
  `predict_detector()` generics. Third-party detectors only need to honour
  the four-column predict contract (`date`, `alarm`, `score`,
  `upper_threshold`); failure isolation, `method` / `group_id` /
  `failed` / `error_message` columns are added by the framework.
- `available_detectors()` returns the current registry.

### Dependency notes

The framework verbs use only base-tidyverse Imports. Detector packages
without straightforward installations (`Rbeast`, `forecast`, `anomalize`,
`changepoint`, `MASS`) are declared as Suggests and gated at call time with
an actionable error message. `surveillance` and `trending` are required
Imports because the majority of detectors depend on them.

### Vignette

See `vignette("epi_outbreak_detection")` for a worked example walking
through the four verbs end-to-end.
