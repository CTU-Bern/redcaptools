data(proj)

test_that("get_vartypes", {
  expect_true(any(!is.na(get_vartypes(proj)$class)))
})
