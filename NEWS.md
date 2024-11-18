# redcaptools 0.5.0

* finalizing `redcap_import_recode` with user-friendly introduction, more useful output, automated options and tests
* improving `redcap_import_dates` to prepare dates for import
* addition of `redcap_import_times` and `redcap_import_datetime` to prepare time or date-time values for import

# redcaptools 0.4.0

* some data preparation functions have been renamed to improve consistency and clarity. The old functions remain to work, but issue a warning about deprecation.
  * `rc_prep` is now `redcap_prep`
  * `rc_dates` is now `redcap_prep_dates`
  * `rc_datetimes` is now `redcap_prep_datetimes`
* improved support for when no single or multiple choice variables exist in a database
* addition of `redcap_toform` which splits a dataframe into a list of dataframes, one for each form.
* addition of a new vignette covering some details on the API endpoints offered by REDCap.

# redcaptools 0.3.2

* finalizing `redcap_import_select` with user-friendly introduction, more automated options and tests
* new function `redcap_import_recode`

# redcaptools 0.3.1

* bugfix for `redcap_export_batch`: caused an error if detected variable types in batches differed. 

# redcaptools 0.3.0

* addition of `redcap_select_rename`: a user-guided loop to select and rename variables for a REDCap import.
* addition of `redcap_recode`: a user-guided loop to reformat and recode variables for a REDCap import.

# redcaptools 0.2.8

* export additional dataprep helpers (see #15)

# redcaptools 0.2.7

* addition of `redcap_export_batch` for those times when data gets too big to export with other methods. It can be used to return either a single dataframe (as per `redcap_export_tbl`) or a list of forms (as per `redcap_export_byform`).

# redcaptools 0.2.6

* `redcap_export_byform` now allows exporting simple non-longitudinal projects
* more testing
* addition of `convert_to_logical` for converting binary (e.g. variables coded 'Yes' and 'No') variables to logical

# redcaptools 0.2.5

* option to include the variable label as well as the option label for multiple choice variables and to use a separator between them
* some testing

# redcaptools 0.2.4

* add some tests
* support for different primary key (normally `record_id`)

# redcaptools 0.2.3

* specify version of `httr2` that is required

# redcaptools 0.2.2

* better support for forms - removes empty rows during API export
* some support for splitting manually exporting exports into forms `split_by_form`
* `rc_prep` has added options for replacing variable types and text to append (if not replacing)
* improve the vignette

# redcaptools 0.2.1

* add support for `yesno` variables

# redcaptools 0.2.0

* Functions for labelling and converting to factors and dates

# redcaptools 0.1.0

* Added a `NEWS.md` file to track changes to the package.
