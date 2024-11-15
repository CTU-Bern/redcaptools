test_that("check_url", {
  expect_error(check_url(c(2, 4)))
  expect_warning(check_url("potato"))
})
test_that("check_token", {
  expect_error(check_token(c(2, 4)))
})
test_that("check_content", {
  expect_error(check_content(c(2, 4)))
  expect_error(check_content("potato"), "unsupported")
})
test_that("check_meta", {
  expect_error(check_meta(c(2, 4)))
  expect_error(check_meta(list(arm = "", dag = "")), "must include")
  expect_error(check_meta(list(project = list(is_longitudinal = 0))), "instrument")
  expect_warning(check_meta(list(project = list(is_longitudinal = 0),
                               instrument = "", foo = "")), "unrecognised")
  expect_error(check_meta(list(project = list(is_longitudinal = 1),
                               instrument = "")), "formEventMapping")
})
test_that("check_dict", {
  expect_error(check_dict(c(2, 4)))
  expect_error(check_dict(data.frame(x = "")), "field_name")
  expect_error(check_dict(data.frame(field_name = "")), "form_name")
  expect_error(check_dict(data.frame(field_name = "", form_name = "")), "field_type")
  expect_error(check_dict(data.frame(field_name = "", form_name = "",
                                     field_type = "")), "field_label")
  expect_error(check_dict(data.frame(field_name = "", form_name = "",
                                     field_type = "", field_label = "")), "select_choices_or_calculations")
  expect_error(check_dict(data.frame(field_name = "", form_name = "",
                                     field_type = "", field_label = "",
                                     select_choices_or_calculations = "")), "text_validation_type_or_show_slider_number")
  expect_error(check_dict(data.frame(field_name = "", form_name = "",
                                     field_type = "", field_label = "",
                                     select_choices_or_calculations = "",
                                     text_validation_type_or_show_slider_number = "")), "text_validation_min")
  expect_error(check_dict(data.frame(field_name = "", form_name = "",
                                     field_type = "", field_label = "",
                                     select_choices_or_calculations = "",
                                     text_validation_type_or_show_slider_number = "",
                                     text_validation_min = "")), "text_validation_max")
  expect_error(check_dict(data.frame(field_name = "", form_name = "",
                                     field_type = "", field_label = "",
                                     select_choices_or_calculations = "",
                                     text_validation_type_or_show_slider_number = "",
                                     text_validation_min = "", text_validation_max = "")), "branching_logic")

})
test_that("check_forms", {
  expect_error(check_forms(c(1, 2)))
})
test_that("check_data", {
  expect_error(check_data(c(1, 2)))
})
test_that("check_missing_codes", {
  expect_error(check_missing_codes(c(1,2)))
  expect_error(check_missing_codes(data.frame(x=1,y=2,z=3)))
  expect_error(check_missing_codes(data.frame(x=1,label=2)))
  expect_error(check_missing_codes(data.frame(code=1,y=2)))
  expect_error(check_missing_codes(data.frame(code=NA,label=2)))
  expect_error(check_missing_codes(data.frame(code=1,label=NA)))
})


