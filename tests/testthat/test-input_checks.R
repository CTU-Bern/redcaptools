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
