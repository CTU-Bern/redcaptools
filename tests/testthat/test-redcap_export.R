
token <- get_token()
word <- get_token2()
url <- "https://redcap.ctu.unibe.ch/api/"

print(word)

# save some data for other tests
# x <- redcap_export_tbl(token, url, "record")
# saveRDS(x, "inst/extdata/test.rda")
# x <- redcap_export_tbl(token, url, "metadata")
# saveRDS(x, "inst/extdata/meta.rda")

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


x <- redcap_export_tbl(token, url, "record")
y <- redcap_export_tbl(token, url, "metadata")
rc_prep(x,y, rep = TRUE, rep_date = FALSE)
