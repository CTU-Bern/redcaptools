# Export tables from REDCap

Export tables from REDCap

## Usage

``` r
redcap_export_tbl(token, url, content, ...)
```

## Arguments

- token:

  REDcap API token

- url:

  address of the API

- content:

  content to download

- ...:

  other parameters passed to the API (see your REDCap API documentation
  for options)

## Value

dataframe

## Examples

``` r
# token <- "some_really_long_string_provided_by_REDCap"
# redcap_export_tbl(token, "https://www.some_redcap_url.com/api/", "record")
```
