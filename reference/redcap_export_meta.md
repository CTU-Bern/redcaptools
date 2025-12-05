# Export the most important REDCap metadata tables

The REDCap API has a large number of API endpoints. Those that are
metadata-type details are listed on this page. The

## Usage

``` r
redcap_export_meta(
  token,
  url,
  tabs = c("metadata", "event", "formEventMapping", "instrument"),
  ...
)
```

## Arguments

- token:

  REDcap API token

- url:

  address of the API

- tabs:

  tables to export. `project` is always added.

- ...:

  other parameters passed to the API (see your REDCap API documentation
  for options)

## Value

list of dataframes

## Details

Allowed tabs are

- `arm` - labels of a projects arms

- `dag` - data access groups (DAGs)

- `userDagMapping` - mapping between users and DAGs

- `event` - list of events in the project (only available for
  longitudinal projects)

- `exportFieldNames` - list of the fields that the API returns

- `instrument` - list of instruments (eCRFs/forms) in the project

- `formEventMapping` - mapping between instruments (forms) and events
  (only available for longitudinal projects)

- `metadata` - the data dictionary

- `project` - information on the project

- `record` - the data itself. The method has many options. See the API
  help page on your REDCap instance

- `repeatingFormsEvents` - which forms can repeat on which events

- `report` - access custom reports defined in REDCap

- `version` - REDCap version

- `user` - list of users

- `userRole` - rights for each role

- `userRoleMapping` - user-roll mapping

## Note

tables that are not relevant for non-longitudinal projects (e.g.
formEventMapping and event) are silently removed

## Examples

``` r
# token <- "some_really_long_string_provided_by_REDCap"
# redcap_export_meta(token, "https://www.some_redcap_url.com/api/")
```
