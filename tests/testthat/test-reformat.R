
x <- readRDS(system.file("extdata", "test.rda", package = "redcaptools"))
meta <- readRDS(system.file("extdata", "meta.rda", package = "redcaptools"))

# test_that("rc_prep no error", {
#   expect_error(rc_prep(x, meta), NA)
#   expect_error(rc_prep(x, meta, rep = TRUE), NA)
#   expect_error(rc_prep(x, meta, rep = TRUE, include_vlabel = TRUE, vlabel_sep = " "), NA)
#   expect_error(rc_prep(x, meta, rep_date = TRUE), NA)
# })

test_that("redcap_prep_dates works with replace", {
  y <- redcap_prep_dates(x, meta)
  expect_true(all(c("datevar_date", "datevar2_date", "datevar3_date", "repdate_date") %in% names(y)))
  expect_true(all(sapply(y[, c("datevar_date", "datevar2_date", "datevar3_date", "repdate_date")], class) == "Date"))
  expect_true(all(sapply(y[, c("datevar", "datevar2", "datevar3", "repdate")], class) == "character"))
})
test_that("redcap_prep_dates works with replace, append dt", {
  y <- redcap_prep_dates(x, meta, append = "_dt")
  expect_true(all(c("datevar_dt", "datevar2_dt", "datevar3_dt", "repdate_dt") %in% names(y)))
  expect_true(all(sapply(y[, c("datevar_dt", "datevar2_dt", "datevar3_dt", "repdate_dt")], class) == "Date"))
  expect_true(all(sapply(y[, c("datevar", "datevar2", "datevar3", "repdate")], class) == "character"))
})
test_that("redcap_prep_dates works without replace", {
  y <- redcap_prep_dates(x, meta, replace = TRUE)
  expect_true(all(!c("datevar_date", "datevar2_date", "datevar3_date", "repdate_date") %in% names(y)))
  expect_true(all(sapply(y[, c("datevar", "datevar2", "datevar3", "repdate")], class) == "Date"))
})

test_that("warnings from deprecated functions", {
  expect_warning(rc_prep(x, meta, rep = TRUE), "rc_prep is deprecated")
  expect_warning(rc_dates(x, meta, rep = TRUE), "rc_dates is deprecated")
  expect_warning(rc_datetimes(x, meta, rep = TRUE), "rc_datetimes is deprecated")
})



token <- get_token()
token2 <- get_token2()
url <- "https://redcap.ctu.unibe.ch/api/"

x <- redcap_export_tbl(token, url, "record")
y <- redcap_export_tbl(token, url, "metadata")
redcap_prep(x,y, rep = TRUE, rep_date = FALSE)

rc_datetimes(x, y)
# str(rc_datetimes(x, y))
# str(x)



