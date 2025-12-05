# Convert REDCap variable types (dates, datetimes, factors) and apply labels

Convert REDCap variable types (dates, datetimes, factors) and apply
labels

## Usage

``` r
redcap_prep(
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
```

## Arguments

- data:

  dataframe

- metadata:

  data dictionary from REDCap

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

## Value

dataframe with converted factors, dates, POSIX, ...
