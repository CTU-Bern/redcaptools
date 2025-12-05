# Deprecated functions These functions have been renamed to be more consistent with the rest of the package. They may be removed in a future version.

Deprecated functions These functions have been renamed to be more
consistent with the rest of the package. They may be removed in a future
version.

## Usage

``` r
rc_prep(
  data,
  metadata,
  rep = FALSE,
  rep_date = rep,
  rep_datetime = rep,
  rep_singlechoice = rep,
  rep_multichoice = rep,
  app_date = "_date",
  app_datetime = "_datetime",
  app_singlechoice = "_factor",
  app_multichoice = "_factor",
  ...
)

rc_dates(data, metadata, replace = FALSE, append = "_date")

rc_datetimes(data, metadata, replace = FALSE, append = "_datetime", ...)

split_by_form(data, metadata)
```

## Arguments

- data:

  dataframe

- metadata:

  datadictionary as exported from REDCap or downloaded from the API

- rep:

  replace variables. If FALSE, encoded versions of the variable will be
  created

- rep_date, rep_datetime, rep_singlechoice, rep_multichoice:

  replace the indicated variable type

- app_date, app_datetime, app_singlechoice, app_multichoice:

  text to append to the newly generated variables name (if `rep_*` is
  FALSE)

- ...:

  options passed to/from other methods

- replace:

  indicator of whether to replace original variables or not

- append:

  text to append to the newly generated variables name (if `replace` is
  TRUE)

## Value

list of dataframes

## Functions

- `rc_prep()`: original function name for `redcap_prep`

- `rc_dates()`: original function name for `redcap_dates`

- `rc_datetimes()`: original function name for `redcap_datetimes`

- `split_by_form()`: deprecated in favour of `redcap_toform` Split a
  manually exported REDCap dataset into forms
