# Convert DHS Century Month Code to Date

Converts DHS Century Month Code (CMC) values to R Date objects. CMC is a
date coding system used by DHS where CMC = (year - 1900) \* 12 + month.

## Usage

``` r
cmc_to_date(cmc)
```

## Arguments

- cmc:

  Numeric vector of CMC values.

## Value

Vector of Date objects (mid-month: day 15).

## Details

The DHS Century Month Code (CMC) encodes dates as the number of months
since December 1899. The formula is: CMC = (year - 1900) \* 12 + month.

For example:

- CMC 1 = January 1900

- CMC 1324 = January 2010

- CMC 1404 = August 2016

## Examples

``` r
if (FALSE) { # \dontrun{
cmc_to_date(1)     # 1900-01-15
cmc_to_date(1324)  # 2010-01-15
cmc_to_date(1404)  # 2016-08-15
} # }
```
