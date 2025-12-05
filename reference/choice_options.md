# Get options for single and multi choice questions

Get options for single and multi choice questions

## Usage

``` r
singlechoice_opts(metadata)

multichoice_opts(metadata)
```

## Arguments

- metadata:

  data.frame containing the metadata

## Value

data.frame with variables `var` (variable), `label` (the variable
label), `vals` (possible values for the variable) and `labs` (the labels
related to each value in `vals`)

data.frame with variables `ovar` (the variable as it appears in the data
dictionary/metadata), `var` (the variable as it appears in the data
itself), `vlabel` (the variable label), `vals` (possible values for the
variable) and `labs` (the labels related to each value in `vals`)

## Details

Multiple choice variables exist in REDCap data as a set of
0/1/TRUE/FALSE variables, where 1/TRUE represents a selected/checked
answer. Hence, for a single multiple choice 'question' in the
datadictionary/metadata with `n` options, there are `n` variables. Each
variable is the variable name (e.g. morbidities) followed by 3
underscores (`___`) and the option number (e.g. 1) - `morbidities___1`.
