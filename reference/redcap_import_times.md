# REDCap Time Conversion

This function prepares time values in a data table for import in REDCap.
In Excel, time values can be entered in various formats. This function
attempts for the most common ways time values may have been entered and
converts them into a format compatible with REDCap.

## Usage

``` r
redcap_import_times(var, unk_min = "00", unk_sec = "00")
```

## Arguments

- var:

  Variable to convert

- unk_min:

  Minutes to use if unknown. The default is 00 (11 -\> 11:00).

- unk_sec:

  Seconds to use if unknown. The default is 00 (11:11 -\> 11:11:00).

## Value

converted variable

## Examples

``` r
var <-c("11:11:00","11:11","11","0.34375" )
redcap_import_times(var)
#> [1] "11:11:00" "11:11:00" "11:00:00" "08:15:00"
```
