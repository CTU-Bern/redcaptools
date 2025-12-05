# redcaptools

``` r
library(redcaptools)
```

`redcaptools` assists in exporting data from REDCap via the API,
converting variable types (e.g. radio-buttons to factors), labelling
etc.

### Exporting from REDCap

REDCap exports generally come in a single files, with repeating elements
as new rows, lots of empty variables (e.g. because they don’t exist for
a given visit). To use the data, it must often be subsetted by form or
event, variables selected, etc.

`redcaptools` eases this by exporting the data by form from the
beginning with the `redcap_export_byform` function.

``` r
token <- "my-redcap-token"
url <- "https://redcap.mydomain.com/api/"

data <- redcap_export_byform(token, url)
```

Other export functions are `redcap_export_meta` (for downloading various
metadata objects - currently the data dictionary, form-event-mapping and
event tables) and `redcap_export_tbl`, which can be used to use whatever
parameters you like for the API (see the REDCap documentation for
details). The dataset that REDCap exports through the website interface
is accessed for instance via:

``` r
record <- redcap_export_tbl(token, url, "record")
```

#### Using manual exports

REDCap also allows exporting data manually through the web interface.
While `redcaptools` is primarily intended to used with the API, there
much of it will also work with manually exported datasets. Important to
note is that the data dictionary must be provided manually in this case.

For example, the `redcap_toform` function tries to split a manually
downloaded dataset into it’s constituent forms. As less information is
available, it’s not as robust as the API version, but it may still be
useful.

``` r
# a small manually downloaded testing dataset
data <- readRDS(system.file("extdata/test.rda", package = "redcaptools"))
# the data dictionary
dd <- read.csv(system.file("extdata/DataDictionary.csv", package = "redcaptools"))

formdata <- redcap_toform(data, dd)
```

### Processing REDCap data

To make the data more usable in R, it’s often nice to label data, create
factor, convert dates etc. `redcaptools` has functions to help with that
too. Each of the following functions also label variables.

The `redcap_prep` function is the easiest to use. It wraps all of the
functions in the following section.

``` r
meta <- redcap_export_meta(token, url)
# could also use a data dictionary exported manually

# where all data is in a single dataframe
prepped <- redcap_prep(record, meta$metadata)

# for data split by forms, use sapply
prepped <- sapply(data, redcap_prep, metadata = meta$metadata)
```

#### Create factors

REDCap has both single and multiple choice factor type variables. These
need handling separately, so there are two sets of functions.

Lists of the options can be obtained with the `singlechoice_opts` and
`multichoice_opts` functions. These are rather for your information.

``` r
singlechoice_opts(meta$metadata)
mutlichoice_opts(meta$metadata)
```

Factors can be created easily with

``` r
prepped <- sapply(data, singlechoice_factor, metadata = meta$metadata)
prepped <- sapply(prepped, multichoice_factor, metadata = meta$metadata)
```

#### Create `Date`s and `POSIX` variables

As with single and multiple choice variables, `Date` and `POSIX`
variables also need handling separately.

``` r
prepped <- sapply(prepped, redcap_prep_dates, metadata = meta$metadata)
prepped <- sapply(prepped, redcap_prep_datetimes, metadata = meta$metadata)
```
