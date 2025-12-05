# create factors for multiple choice variables

Converts the numeric values returned from REDCap to factors (with levels
Yes/No). This function also applies labels to the variable itself, based
on the option label.

## Usage

``` r
multichoice_factor(
  data,
  metadata,
  replace = FALSE,
  append = "_factor",
  include_vlabel = FALSE,
  vlabel_sep = ": "
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

- include_vlabel:

  logical indicating whether to include the variable label before the
  value label

- vlabel_sep:

  text to use for separating vlabel and label

## Value

input data.frame with additional factor variables.
