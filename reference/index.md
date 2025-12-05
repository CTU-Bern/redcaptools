# Package index

## Exporting data

Functions for communicating with the REDCap API

- [`redcap_export_tbl()`](redcap_export_tbl.md) : Export tables from
  REDCap
- [`redcap_export_byform()`](redcap_export_byform.md) : Export REDCap
  data by form
- [`redcap_export_batch()`](redcap_export_batch.md) : Export data in
  batches
- [`redcap_export_meta()`](redcap_export_meta.md) : Export the most
  important REDCap metadata tables

## Working with manually exported datasets

REDcap can export data through the web interface too. These functions
may be helpful in such cases.

- [`redcap_toform()`](redcap_toform.md) : Convert manually downloaded
  REDCap data into a list of forms

## Preparing data for analysis

Functions for converting from REDCap’s data representation to a more
useful format for analysis

- [`redcap_prep()`](redcap_prep.md) : Convert REDCap variable types
  (dates, datetimes, factors) and apply labels

- [`redcap_prep_dates()`](redcap_prep_dates.md)
  [`redcap_prep_datetimes()`](redcap_prep_dates.md) :

  Convert dates stored as strings to `Date` variables

- [`singlechoice_factor()`](singlechoice_factor.md) : create factors for
  single choice variables

- [`multichoice_factor()`](multichoice_factor.md) : create factors for
  multiple choice variables

- [`label_others()`](label_others.md) :

  Label non-single/multiple choice/date(time) fields
  `singlechoice_factor`, `multichoice_factor`, `rc_date` and
  `rc_datetime`

- [`convert_to_logical()`](convert_to_logical.md) : Convert variables to
  logical

- [`remove_empty_rows()`](remove_empty_rows.md) : Analagous to
  \`janitor::remove_empty(..., "rows")\`, but allows ignoring specific
  variables

## Exploring metadata

Functions for exploring the metadata of a REDCap project

- [`singlechoice_opts()`](choice_options.md)
  [`multichoice_opts()`](choice_options.md) : Get options for single and
  multi choice questions

## Preparing data for import INTO REDCap

REDCap expects data to be in a specific format for import. These
functions help to format the data appropriately, including guiding the
user through converting from an old data format to the REDCap format.

- [`redcap_import_select()`](redcap_import_select.md) : REDCap Select
  and Rename
- [`redcap_import_recode()`](redcap_import_recode.md) : REDCap Recode
- [`redcap_import_dates()`](redcap_import_dates.md) : REDCap Date
  Conversion
- [`redcap_import_times()`](redcap_import_times.md) : REDCap Time
  Conversion
- [`redcap_import_datetime()`](redcap_import_datetime.md) : REDCap
  Date-Time Conversion

### Demonstration datasets

- [`importdemo_data`](importdemo_data.md) : Example import data
- [`importdemo_dict`](importdemo_dict.md) : Example data dictionary

## Deprecated functions

- [`rc_prep()`](deprecated.md) [`rc_dates()`](deprecated.md)
  [`rc_datetimes()`](deprecated.md) [`split_by_form()`](deprecated.md) :
  Deprecated functions These functions have been renamed to be more
  consistent with the rest of the package. They may be removed in a
  future version.
