# Export REDCap data by form

Export REDCap data by form

## Usage

``` r
redcap_export_byform(
  token,
  url,
  meta = NULL,
  remove_empty = TRUE,
  wait = 0.2,
  ...
)
```

## Arguments

- token:

  REDcap API token

- url:

  address of the API

- meta:

  metadata from `redcap_export_meta` (will be downloaded if not
  provided)

- remove_empty:

  should empty rows be removed from the dataset (REDCap automatically
  creates all forms for an event when any form in the event is created)

- wait:

  seconds to wait between API calls

- ...:

  other parameters passed to the API (see your REDCap API documentation
  for options)

## Value

list of dataframes

## Examples

``` r
# token <- "some_really_long_string_provided_by_REDCap"
# redcap_export_byform(token, "https://www.some_redcap_url.com/api/")
```
