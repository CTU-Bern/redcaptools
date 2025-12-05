# REDCap Date-Time Conversion

This function prepares date-time values in a data table for import in
REDCap. It parses date and time values and processes them using
`redcap_import_dates` and `redcap_import_times`. This ensures that
date-time entries, which may have been entered in various formats in
Excel, are converted into a format compatible with REDCap.

## Usage

``` r
redcap_import_datetime(
  var,
  args_rc_dates = list(),
  args_rc_times = list(),
  date_only = FALSE
)
```

## Arguments

- var:

  Variable to convert

- args_rc_dates:

  List with arguments for `redcap_import_dates` (e.g. args_rc_dates =
  list(unk_day = 01,format = "american"))

- args_rc_times:

  List with arguments for `redcap_import_times` (e.g. args_rc_times =
  list(unk_min = 01,unk_sec = 01))

- date_only:

  If TRUE, only the date will be included in the output and the time
  value will be removed. Default = FALSE.

## Value

converted variable

## Examples

``` r
var <-c("1.2.24 11:11:00","1.2.22 11:11","1.2.24 11", "31341.34375")
redcap_import_datetime(var)
#> [1] "2024-02-01 11:11:00" "2022-02-01 11:11:00" "2024-02-01 11:00:00"
#> [4] "1985-10-21 08:15:00"
```
