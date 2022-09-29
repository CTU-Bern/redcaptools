
data(mtcars)
var_label(mtcars$vs) <- "VS"
test_that("convert_to_logical replace", {
  expect_error(convert_to_logical(mtcars, "am", 1), NA)
  conv <- convert_to_logical(mtcars, "am", 1)
  expect_true(!"am_logical" %in% names(conv))
  expect_true(class(conv$am) == "logical")

  expect_error(convert_to_logical(mtcars, c("am", "vs"), 1), NA)
  conv2 <- convert_to_logical(mtcars, c("am", "vs"), 1)
  expect_true(!"vs_logical" %in% names(conv))
  expect_true(class(conv2$vs) == "logical")
  expect_true(var_label(conv2$vs) == "VS")

})

test_that("convert_to_logical append", {
  expect_error(convert_to_logical(mtcars, "am", 1, FALSE), NA)
  conv <- convert_to_logical(mtcars, "am", 1, FALSE)
  expect_true("am_logical" %in% names(conv))
  expect_true("am" %in% names(conv))
  expect_true(class(conv$am_logical) == "logical")
  expect_true(class(conv$am) == "numeric")

  expect_error(convert_to_logical(mtcars, c("am", "vs"), 1, FALSE), NA)
  conv2 <- convert_to_logical(mtcars, c("am", "vs"), 1, FALSE)
  expect_true("vs_logical" %in% names(conv2))
  expect_true(class(conv2$vs_logical) == "logical")
  expect_true(var_label(conv2$vs_logical) == "VS")

  expect_error(convert_to_logical(mtcars, c("am", "vs"), 1, FALSE, "_lgl"), NA)
  conv2 <- convert_to_logical(mtcars, c("am", "vs"), 1, FALSE, "_lgl")
  expect_true("vs_lgl" %in% names(conv2))
  expect_true(class(conv2$vs_lgl) == "logical")
  expect_true(var_label(conv2$vs_lgl) == "VS")

})

test_that("convert_to_logical failure", {
  expect_error(convert_to_logical(mtcars, "potato", 1))
})
