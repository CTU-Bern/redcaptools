# Convert dates stored as strings to `Date` variables

Converts the string values returned from REDCap to Dates. This function
also applies labels to the variable itself, based on the option label.

## Usage

``` r
redcap_prep_dates(data, metadata, replace = FALSE, append = "_date")

redcap_prep_datetimes(
  data,
  metadata,
  replace = FALSE,
  append = "_datetime",
  ...
)
```

## Arguments

- data:

  the data.frame to modify

- metadata:

  metadata/datadictionary

- replace:

  whether to overwrite the existing data .

- append:

  text to append to the variable name if not overwriting

- ...:

  options passed to/from other methods

## Value

input data.frame with additional date variables/variables converted to
dates.

## Functions

- `redcap_prep_datetimes()`: input data.frame with date-time variables
  reformated to POSIX
