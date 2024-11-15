data("importdemo_data")
data("importdemo_dict")

test_that("redcap_import_recode works", {
  expect_error(redcap_import_recode(importdemo_data, importdemo_dict,
                                    auto_recode = TRUE,
                                    skip_intro = TRUE,
                                    continue = FALSE,
                                    suppress_txt = TRUE,
                                    log = FALSE,
                                    wait = 0), NA)

  x <- redcap_import_recode(importdemo_data, importdemo_dict,
                            auto_recode = TRUE,
                            skip_intro = TRUE,
                            continue = FALSE,
                            suppress_txt = TRUE,
                            log = FALSE,
                            wait = 0)

  expect_s3_class(x, "data.frame")


 dates <- importdemo_dict$field_name[grepl("date_dmy|date_ymd",importdemo_dict$text_validation_type_or_show_slider_number)]
 convertible_to_date <- sapply(x[names(x) %in% dates], function(col) {
   all(is.na(col) | !is.na(as.Date(col)))
 })
 expect_true(all(convertible_to_date))

 ints <- importdemo_dict$field_name[importdemo_dict$text_validation_type_or_show_slider_number == "integer"]
 convertible_to_int <- sapply(x[names(x) %in% ints], function(col) {
   all(is.na(col) | (!is.na(as.numeric(col)) & as.numeric(col) %% 1 == 0))
 })
 expect_true(all(convertible_to_int))

 nums <- importdemo_dict$field_name[importdemo_dict$text_validation_type_or_show_slider_number == "number"]
 convertible_to_num <- sapply(x[names(x) %in% nums], function(col) {
   all(is.na(col) | !is.na(as.numeric(col)))
 })
 expect_true(all(convertible_to_num))

 num2 <- importdemo_dict$field_name[importdemo_dict$text_validation_type_or_show_slider_number == "number_2dp"]
 convertible_to_num2 <- sapply(x[names(x) %in% num2], function(col) {
   all(is.na(col) | (!is.na(as.numeric(col)) & grepl("^\\d+\\.\\d{2}$", as.character(col))))
 })
 expect_true(all(convertible_to_num2))

 num4 <- importdemo_dict$field_name[importdemo_dict$text_validation_type_or_show_slider_number == "number_4dp"]
 convertible_to_num4 <- sapply(x[names(x) %in% num4], function(col) {
   all(is.na(col) | (!is.na(as.numeric(col)) & grepl("^\\d+\\.\\d{4}$", as.character(col))))
 })
 expect_true(all(convertible_to_num4))

 times <- importdemo_dict$field_name[importdemo_dict$text_validation_type_or_show_slider_number == "time"]
 convertible_to_time <- sapply(x[names(x) %in% times], function(col) {
   all(is.na(col) | !is.na(as.POSIXct(col,format = '%H:%M')))
 })
 expect_true(all(convertible_to_time))


})

