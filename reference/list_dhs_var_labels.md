# List DHS variables and their haven labels by name pattern

Generic utility to inspect a (typically DHS) data frame and return a
tidy inventory of every column whose name matches a given prefix or
regex, together with its haven `label` attribute. This is useful when
building specs for `custom_csb_indicator` (or any indicator that routes
columns by label / variable name), since DHS recodes share many label
conventions across surveys but the exact set of populated columns varies
by country and round.

## Usage

``` r
list_dhs_var_labels(
  data,
  pattern,
  regex = FALSE,
  only_observed = FALSE,
  duplicate_label = TRUE
)
```

## Arguments

- data:

  A data frame (typically a DHS recode such as the KR file read by
  [`dhs_read()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md)).
  Columns may be plain vectors or haven-labelled vectors; only the name
  and `label` attribute are inspected.

- pattern:

  Character scalar. Either a prefix (default) or a regex (when
  `regex = TRUE`). Must be non-empty.

- regex:

  Logical. If `FALSE` (default), `pattern` is treated as a prefix and
  the matched regex is `paste0("^", pattern, "[a-z0-9]+$")`. If `TRUE`,
  `pattern` is used as the full regex.

- only_observed:

  Logical. If `TRUE`, drop variables with no non-NA values, or - for
  numeric 0/1 columns - no observed `1`. Default `FALSE`.

- duplicate_label:

  Logical. If `TRUE` (default), add a `duplicate_label` logical column
  flagging variables whose label is shared with at least one other
  matched variable.

## Value

A tibble with columns:

- `variable`: matched column name.

- `label`: haven `label` attribute (or `NA_character_` if absent).

- `n_nonmissing`: number of non-NA values in the column.

- `n_ones`: number of values equal to `1` (or `NA` for non-numeric
  columns).

- `duplicate_label` (only if `duplicate_label = TRUE`): logical, `TRUE`
  if another matched variable has the same non-NA label.

Variables are returned in the order they appear in `data`. If no
variables match, an empty tibble with the same columns is returned and a
`cli` info message is emitted.

## Details

Two matching modes are supported:

- Prefix mode (default, `regex = FALSE`): the helper matches columns
  whose names start with `pattern` followed by one or more lowercase
  letters or digits. For example, `pattern = "h32"` will match `h32a`,
  `h32x`, `h32a1`, but not `h32_recoded` or `h32`.

- Regex mode (`regex = TRUE`): `pattern` is used directly as a regular
  expression. For example, `pattern = "^ml13[a-z]$"` will match
  ITN-source variables `ml13a`..`ml13h`.

Optionally, the helper can:

- Mark variables whose haven label is shared with another variable in
  the same inventory (`duplicate_label = TRUE`). This is the situation
  that motivates routing a `custom_csb_indicator` spec by variable
  **name** rather than by label (e.g. h32e and h32n in some surveys both
  carry the label "Fever/cough: comm.health wrkr").

- Drop variables that have no observed `1` (or no non-NA values at all
  for non-binary columns) via `only_observed = TRUE`. This keeps the
  inventory focused on slots actually populated by the survey.

The function never modifies `data`; it only reads column names and the
`label` attribute (as set by
[`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html)
or
[`sntmethods::dhs_read()`](https://ahadi-analytics.github.io/sntmethods/reference/dhs_read.md)).

## Examples

``` r
if (FALSE) { # \dontrun{
  kr_data <- sntmethods::dhs_read(
    path        = path_dhs_parquet,
    file_type   = "KR",
    survey_type = "DHS"
  )

  # All h32* treatment-seeking source columns
  list_dhs_var_labels(kr_data, "h32")

  # Only h32 columns with at least one observed `1`, flagging duplicates
  list_dhs_var_labels(kr_data, "h32", only_observed = TRUE)

  # ITN source columns ml13a..ml13h via regex
  list_dhs_var_labels(kr_data, "^ml13[a-z]$", regex = TRUE)
} # }
```
