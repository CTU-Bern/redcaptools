
x <- readRDS(system.file("extdata", "test.rda", package = "redcaptools"))
meta <- readRDS(system.file("extdata", "meta.rda", package = "redcaptools"))

test_that("rc_prep no error", {
  expect_error(rc_prep(x, meta), NA)
  expect_error(rc_prep(x, meta, rep = TRUE), NA)
  expect_error(rc_prep(x, meta, rep = TRUE, include_vlabel = TRUE, vlabel_sep = " "), NA)
  expect_error(rc_prep(x, meta, rep_date = TRUE), NA)
})

test_that("rc_dates works with replace", {
  y <- rc_dates(x, meta)
  expect_true(all(c("datevar_date", "datevar2_date", "datevar3_date", "repdate_date") %in% names(y)))
  expect_true(all(sapply(y[, c("datevar_date", "datevar2_date", "datevar3_date", "repdate_date")], class) == "Date"))
  expect_true(all(sapply(y[, c("datevar", "datevar2", "datevar3", "repdate")], class) == "character"))
  y <- rc_dates(x, meta, append = "dt")
  expect_true(all(c("datevar_dt", "datevar2_dt", "datevar3_dt", "repdate_dt") %in% names(y)))
  expect_true(all(sapply(y[, c("datevar_dt", "datevar2_dt", "datevar3_dt", "repdate_dt")], class) == "Date"))
  expect_true(all(sapply(y[, c("datevar", "datevar2", "datevar3", "repdate")], class) == "character"))
})

test_that("rc_dates works without replace", {
  y <- rc_dates(x, meta, replace = TRUE)
  expect_true(all(!c("datevar_date", "datevar2_date", "datevar3_date", "repdate_date") %in% names(y)))
  expect_true(all(sapply(y[, c("datevar", "datevar2", "datevar3", "repdate")], class) == "Date"))
})
