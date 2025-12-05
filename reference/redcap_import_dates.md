# REDCap Date Conversion

This function prepares date values in a data table for import in REDCap.
Dates in Excel can be entered in a variety of formats. This function
attempts to account for the most common ways dates may have been entered
and converts them into a format compatible with REDCap.

## Usage

``` r
redcap_import_dates(var, unk_day = "01", unk_month = "01", format = "european")
```

## Arguments

- var:

  Variable to convert

- unk_day:

  Day to use if unknown, i.e. if only the year or only the month + year
  is found. The default is 01 (2022 -\> 2022-01-01).

- unk_month:

  Month to use if unknown, i.e. if only the year is found. The default
  is 01 (2022 -\> 2022-01-01).

- format:

  Date format to be used: "european" (DMY) or "american" (MDY). Dates
  that match both formats will be converted accordingly. Default =
  "european".

## Value

converted variable

## Examples

``` r
var <-c("01.12.2022", "12.2022", "2022", "01/12/2022", "31341")
redcap_import_dates(var)
#> [1] "2022-12-01" "2022-12-01" "2022-01-01" "2022-12-01" "1985-10-21"
```
