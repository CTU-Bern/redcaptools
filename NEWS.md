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
