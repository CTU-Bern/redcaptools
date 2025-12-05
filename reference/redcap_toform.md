# Convert manually downloaded REDCap data into a list of forms

Similar to `redcap_export_byform`, this function tries to split a
manually downloaded dataset into it's constituent forms. While use of
the API allows individual forms to be downloaded, with a manual
download, only the data dictionary is available as auxillary
information. If no data dictionary is available, the function will use
the variable names to guess the forms (see details).

## Usage

``` r
redcap_toform(data, datadict = NULL, metadata = NULL, guess_events = TRUE, ...)
```

## Arguments

- data:

  imported REDCap data

- datadict:

  data dictionary downloaded manually from REDCap

- metadata:

  metadata downloaded from REDCap API

- guess_events:

  restrict forms to events (rows) where data exists (see details)

- ...:

  additional arguments passed to other functions (currently unused)

## Details

In a longitudinal data collection with many forms, a REDCap dataset will
have a large degree of empty cells. The `guess_events` argument uses
missingness as an indicator of a row not being part of the form in
question. If all user variables (i.e. those that do not start with
`redcap`) are empty, the row will be removed from the dataset.

If neither `datadict` nor `metadata` are provided, the function will
attempt to guess the forms based on the variable names, specifically the
`form_complete` variables which denote the state of the form. This is
not a foolproof method: there may be other variables in the data that
end with `_complete`.

## Examples

``` r
data <- readRDS(system.file("extdata/test.rda", package = "redcaptools"))
metadata <- readRDS(system.file("extdata/meta.rda", package = "redcaptools"))
dd <- read.csv(system.file("extdata/DataDictionary.csv", package = "redcaptools"))
redcap_toform(data, dd)
#> $form1
#>   record_id redcap_event_name redcap_repeat_instrument redcap_repeat_instance
#> 1         1     event_1_arm_1                                              NA
#> 2         2       first_arm_2                                              NA
#> 3         3     event_1_arm_1                                              NA
#>   textvar ynvar    datevar      datetimevar numvar singlecvar multicvar_2___1
#> 1     foo     1 2022-09-02 2022-09-02 16:24   1984          1               0
#> 2     bar     0 2022-09-02 2022-07-05 06:26      5          2               1
#> 3 potatoe     1 2022-09-08 2022-09-16 17:15   1956          2               1
#>   multicvar_2___2 tfvar slidervar
#> 1               1     1        20
#> 2               0     0        73
#> 3               1     1        39
#> 
#> $form2
#>   record_id redcap_event_name redcap_repeat_instrument redcap_repeat_instance
#> 1         1     event_1_arm_1                                              NA
#> 2         2       first_arm_2                                              NA
#>     datevar2   datevar3 yesnovar2
#> 1 2022-09-04 2022-09-01         0
#> 2 2022-09-13 2022-09-02         0
#> 
#> $form3
#>   record_id redcap_event_name redcap_repeat_instrument redcap_repeat_instance
#> 1         1     event_1_arm_1                                              NA
#> 2         2       first_arm_2                    form3                      1
#> 3         2       first_arm_2                    form3                      2
#> 4         2      second_arm_2                    form3                      1
#> 5         2      second_arm_2                    form3                      2
#>      repdate repnum repdrop
#> 1 2022-09-02    654       3
#> 2 2022-09-05     69       2
#> 3 2022-09-14     89       1
#> 4 2022-09-30     78       3
#> 5 2022-08-01     66       1
#> 
redcap_toform(data, metadata = metadata)
#> $form1
#>   record_id redcap_event_name redcap_repeat_instrument redcap_repeat_instance
#> 1         1     event_1_arm_1                                              NA
#> 2         2       first_arm_2                                              NA
#> 3         3     event_1_arm_1                                              NA
#>   textvar ynvar    datevar      datetimevar numvar singlecvar multicvar_2___1
#> 1     foo     1 2022-09-02 2022-09-02 16:24   1984          1               0
#> 2     bar     0 2022-09-02 2022-07-05 06:26      5          2               1
#> 3 potatoe     1 2022-09-08 2022-09-16 17:15   1956          2               1
#>   multicvar_2___2 tfvar slidervar
#> 1               1     1        20
#> 2               0     0        73
#> 3               1     1        39
#> 
#> $form2
#>   record_id redcap_event_name redcap_repeat_instrument redcap_repeat_instance
#> 1         1     event_1_arm_1                                              NA
#> 2         2       first_arm_2                                              NA
#>     datevar2   datevar3 yesnovar2
#> 1 2022-09-04 2022-09-01         0
#> 2 2022-09-13 2022-09-02         0
#> 
#> $form3
#>   record_id redcap_event_name redcap_repeat_instrument redcap_repeat_instance
#> 1         1     event_1_arm_1                                              NA
#> 2         2       first_arm_2                    form3                      1
#> 3         2       first_arm_2                    form3                      2
#> 4         2      second_arm_2                    form3                      1
#> 5         2      second_arm_2                    form3                      2
#>      repdate repnum repdrop
#> 1 2022-09-02    654       3
#> 2 2022-09-05     69       2
#> 3 2022-09-14     89       1
#> 4 2022-09-30     78       3
#> 5 2022-08-01     66       1
#> 
redcap_toform(data)
#> Warning: No metadata provided, guessing forms based on variable names
#> $form1
#>   record_id redcap_event_name redcap_repeat_instrument redcap_repeat_instance
#> 1         1     event_1_arm_1                                              NA
#> 2         2       first_arm_2                                              NA
#> 3         3     event_1_arm_1                                              NA
#>   textvar ynvar    datevar      datetimevar numvar singlecvar multicvar_2___1
#> 1     foo     1 2022-09-02 2022-09-02 16:24   1984          1               0
#> 2     bar     0 2022-09-02 2022-07-05 06:26      5          2               1
#> 3 potatoe     1 2022-09-08 2022-09-16 17:15   1956          2               1
#>   multicvar_2___2 tfvar slidervar form1_complete
#> 1               1     1        20              0
#> 2               0     0        73              0
#> 3               1     1        39              0
#> 
#> $form2
#>   record_id redcap_event_name redcap_repeat_instrument redcap_repeat_instance
#> 1         1     event_1_arm_1                                              NA
#> 2         2       first_arm_2                                              NA
#> 3         3     event_1_arm_1                                              NA
#>     datevar2   datevar3 yesnovar2 form2_complete
#> 1 2022-09-04 2022-09-01         0              0
#> 2 2022-09-13 2022-09-02         0              0
#> 3                              NA              0
#> 
#> $form3
#>   record_id redcap_event_name redcap_repeat_instrument redcap_repeat_instance
#> 1         1     event_1_arm_1                                              NA
#> 2         2       first_arm_2                    form3                      1
#> 3         2       first_arm_2                    form3                      2
#> 4         2      second_arm_2                    form3                      1
#> 5         2      second_arm_2                    form3                      2
#> 6         3     event_1_arm_1                                              NA
#>      repdate repnum repdrop form3_complete
#> 1 2022-09-02    654       3              0
#> 2 2022-09-05     69       2              0
#> 3 2022-09-14     89       1              0
#> 4 2022-09-30     78       3              0
#> 5 2022-08-01     66       1              0
#> 6                NA      NA              0
#> 
```
