# create factors for single choice variables

Converts the numeric values returned from REDCap to factors. This
function also applies labels to the variable itself.

## Usage

``` r
singlechoice_factor(data, metadata, replace = FALSE, append = "_factor")
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

## Value

dataframe with factor variables
