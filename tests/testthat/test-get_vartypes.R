data(proj)
library(dplyr)

test_that("get_vartypes no NAs", {
  expect_true(any(!is.na(get_vartypes(proj)$class)))
})

test_that("get_vartypes single choice", {
  sc_var <- proj$metadata |>
    filter(field_type %in% c("yesno", "dropdown", "radio")) |>
    pull(field_name)
  sc_class <- get_vartypes(proj) |> filter(field_name %in% sc_var) |> pull(class)
  expect_equal(sc_class,rep("integer",length(sc_class)))
})

test_that("get_vartypes multiple choice", {

  cb_var <- proj$metadata |>
    filter(field_type == "checkbox") |>
    pull(field_name)

  if (is.na(proj$project$missing_data_codes)) {
    cb <- 1
  } else if (!is.na(proj$project$missing_data_codes)) {
    cb <- 2
  }
  cb_nbr <- proj$metadata |>
    filter(field_type == "checkbox") |>
    mutate(n=str_count(select_choices_or_calculations,pattern = '\\|')+cb,1) |>
    summarise_at("n", sum) |>
    pull(n)
  cb_class <- get_vartypes(proj) |> filter(field_name %in% cb_var) |> pull(class)
  expect_equal(cb_class,rep("integer",cb_nbr))

})


test_that("get_vartypes numeric", {

  int_var <- proj$metadata |>
    filter(field_type == "text" & text_validation_type_or_show_slider_number == 'integer') |>
    pull(field_name)
  int_class <- get_vartypes(proj) |> filter(field_name %in% int_var) |> pull(class)
  expect_equal(int_class,rep("integer",length(int_class)))

  num_var <- proj$metadata |>
    filter(field_type == "calc" | (field_type == "text" & grepl("number",text_validation_type_or_show_slider_number))) |>
    pull(field_name)
  num_class <- get_vartypes(proj) |> filter(field_name %in% num_var) |> pull(class)
  expect_equal(num_class,rep("numeric",length(num_class)))

})


test_that("get_vartypes everything else", {

  else_var <- proj$metadata |>
    filter(!field_type %in% c("yesno", "dropdown", "radio", "checkbox", "calc"),
           text_validation_type_or_show_slider_number != 'integer',
           !grepl("number",text_validation_type_or_show_slider_number)) |>
    pull(field_name)
  else_class <- get_vartypes(proj) |> filter(field_name %in% else_var) |> pull(class)
  expect_equal(else_class,rep("character",length(else_class)))
})

