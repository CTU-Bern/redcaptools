# REDCap Select and Rename

This function loops through all the variable names of a data set and
lets the user compare them with the variable names set up in REDCap.  
The REDCap data dictionary can either be directly provided or downloaded
from the REDCap project by providing an API token and matching URL.  
For variables with matching names in REDCap, the function can be run so
that they will be automatically selected without renaming. If
auto-selecting is turned off, the user can decide to not select these
variables at all or to select and rename them.  
For variables without matching names in REDCap, the function can be run
so that they will be automatically skipped. If auto-skipping is turned
off, the user can decide to select these variables anyway (helpful e.g.
when they need to be split for checkbox fields) or to select and rename
them.  
The function returns a data frame with the selected/renamed variables,
writes an overview csv-table, and the executed code to a txt-file for
copy-pasting and adjusting/reusing.

## Usage

``` r
redcap_import_select(
  import_data,
  dict = NULL,
  rc_token,
  rc_url,
  forms = NULL,
  start_var = 1,
  auto_match = TRUE,
  auto_skip_nomatch = FALSE,
  no_match_suggestion = 0.5,
  skip_intro = FALSE,
  continue = TRUE,
  suppress_txt = FALSE,
  log = TRUE,
  log_code = "redcap_import_select_code.txt",
  log_table = "redcap_import_select_overview.csv",
  log_unused = TRUE,
  wait = 2
)
```

## Arguments

- import_data:

  Data frame to be imported

- dict:

  Data dictionary (e.g. as downloaded from REDCap or via
  `redcap_export_meta(rc_token, rc_url)$meta`). If not supplied, this
  will be downloaded from the API using `rc_token` and `rc_url`.

- rc_token:

  REDCap API token

- rc_url:

  Link to REDCap API

- forms:

  Character vector of the forms as set up in REDCap of which variable
  names will be displayed. Default = all forms.

- start_var:

  Define in which column of the import data the loop should start.
  Default = 1.

- auto_match:

  If TRUE, variables with matching names will be automatically selected.
  If FALSE, the user can decide if the variable shall be imported or
  not. Default = TRUE.

- auto_skip_nomatch:

  If TRUE, variables without matching names will be automatically
  skipped. If FALSE, the user can decide to select and rename the
  variable. Default = FALSE.

- no_match_suggestion:

  For variables without matching names, similar names in REDCap will be
  suggested. With this numeric similarity index between 0 (no similarity
  at all = shows all items) and 1 (identical = shows only perfect
  matches) the number of suggestions can be adjusted. Type '0' to turn
  off similarity suggestions. Default = 0.5.

- skip_intro:

  If TRUE, the introduction messages will be skipped. Default = FALSE

- continue:

  If TRUE, a question to continue will be asked before moving along the
  loop. Default = TRUE.

- suppress_txt:

  If TRUE, all text output will be suppressed (not recommended). Default
  = FALSE.

- log:

  If TRUE, an overview csv-table, and a txt-file are stored in the
  working directory. Default = TRUE.

- log_code:

  Name and location of the txt-file containing the executed code.
  Default = redcap_import_select_code.txt.

- log_table:

  Name and location of the csv.table containing the tabular overview.
  Default = redcap_import_select_overview.csv.

- log_unused:

  IF TRUE, all REDCap variable names that have not been matched with the
  data dictionary will be listed in the end of the csv-table. Default =
  TRUE.

- wait:

  Allows you to set the latency time between the steps. Default = 2s.

## Value

Data frame with selected/renamed data. Log-file with executed code.
CSV-table with overview.

## Examples

``` r
# data(importdemo_data)
# data(importdemo_dict)
# redcap_import_select(importdemo_data, importdemo_dict)

# if using local data:
# token <- "xxxxx"
# url <- "xxxxx"
# file <- "data.csv"
# redcap_import_select(file, rc_token = token, rc_url = url)
```
