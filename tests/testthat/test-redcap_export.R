
token <- get_token()
token2 <- get_token2()
url <- "https://redcap.ctu.unibe.ch/api/"



# save some data for other tests
# x <- redcap_export_tbl(token, url, "record")
# saveRDS(x, "inst/extdata/test.rda")
# x <- redcap_export_tbl(token, url, "metadata")
# saveRDS(x, "inst/extdata/meta.rda")

test_that("export_tbl record works", {
  x <- redcap_export_tbl(token, url, "record")
  expect_s3_class(x, "data.frame")
  x <- redcap_export_tbl(token2, url, "record")
  expect_s3_class(x, "data.frame")
})

test_that("default export_meta works", {
  x <- redcap_export_meta(token, url)
  expect_type(x, "list")
  expect_equal(length(x), 5)
  expect_equal(names(x),
               c("metadata", "event", "formEventMapping", "instrument", "project"))
  expect_error(redcap_export_meta(token, url, "foo"), "unsupported")
})

test_that("export_byform record works", {
  x <- redcap_export_byform(token, url)
  expect_type(x, "list")
  expect_equal(length(x), 3)
  x <- redcap_export_byform(token2, url)
  expect_type(x, "list")
  expect_equal(length(x), 3)
  expect_error(redcap_export_byform(token2, url, "foo"))
})

test_that("batched export works", {
  expect_error(redcap_export_batch(token, url), NA)
  expect_error(redcap_export_batch(token, url, byform = TRUE), NA)

  x <- redcap_export_batch(token, url)
  expect_s3_class(x, "data.frame")

  x <- redcap_export_batch(token, url, byform = TRUE)
  expect_type(x, "list")
  expect_equal(length(x), 3)
})

test_that("batched byform comparible to export_byform", {
  x <- redcap_export_batch(token, url, byform = TRUE, batchsize = 2)
  y <- redcap_export_byform(token, url)
  expect_equal(x, y, ignore_attr = TRUE)
})

test_that("batched byform comparible to export_tbl", {
  x <- redcap_export_batch(token, url, byform = FALSE, batchsize = 2)
  y <- redcap_export_tbl(token, url, "record")
  expect_equal(x, y, ignore_attr = TRUE)
})
