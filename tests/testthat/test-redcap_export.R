
token <- Sys.getenv("RC_TOKEN")
url <- "https://redcap.ctu.unibe.ch/api/"

test_that("export_tbl record works", {
  x <- redcap_export_tbl(token, url, "record")
  expect_s3_class(x, "data.frame")
})

test_that("default export_meta works", {
  x <- redcap_export_meta(token, url)
  expect_type(x, "list")
  expect_equal(length(x), 4)
  expect_equal(names(x),
               c("metadata", "event", "formEventMapping", "instrument"))
})




