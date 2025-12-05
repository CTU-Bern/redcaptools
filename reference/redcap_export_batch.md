# Export data in batches

Exports of large databases may fail using the standard export methods
implemented in [redcap_export_tbl](redcap_export_tbl.md) and
[redcap_export_byform](redcap_export_byform.md). To remedy this, the
`redcap_export_batch` function exports data in smaller chunks (of 1000
records by default)

## Usage

``` r
redcap_export_batch(
  token,
  url,
  batchsize = 1000,
  meta = NULL,
  byform = FALSE,
  remove_empty = TRUE,
  ...
)
```

## Arguments

- token:

  REDcap API token

- url:

  address of the API

- batchsize:

  number of records per batch

- meta:

  metadata from `redcap_export_meta` (will be downloaded if not
  provided)

- byform:

  logical. Download data by form (see
  [redcap_export_byform](redcap_export_byform.md))

- remove_empty:

  when using byform: should empty rows be removed from the dataset
  (REDCap automatically creates all forms for an event when any form in
  the event is created)

- ...:

  other parameters passed to the API (see your REDCap API documentation
  for options)

## Value

depending on `byform`, either a list of dataframes or a single dataframe

## See also

[redcap_export_tbl](redcap_export_tbl.md),
[redcap_export_byform](redcap_export_byform.md)

## Examples

``` r
# token <- "some_really_long_string_provided_by_REDCap"
# as a single dataframe
# redcap_export_batch(token, "https://www.some_redcap_url.com/api/")
# as a list of dataframes (forms)
# redcap_export_batch(token, "https://www.some_redcap_url.com/api/", byform = TRUE)
```
