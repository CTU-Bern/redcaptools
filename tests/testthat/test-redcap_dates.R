test_that("redcap_dates", {
  expect_equal(suppressWarnings(redcap_dates("01.12.2022")),"2022-12-01")
  expect_equal(suppressWarnings(redcap_dates("01/12/2022")),"2022-12-01")
  expect_equal(suppressWarnings(redcap_dates("01-12-2022")),"2022-12-01")
  expect_equal(suppressWarnings(redcap_dates("3.4.22")),"2022-04-03")
  expect_equal(suppressWarnings(redcap_dates("12.2022")),"2022-12-01")
  expect_equal(suppressWarnings(redcap_dates("12.2022",unk_day = "05")),"2022-12-05")
  expect_equal(suppressWarnings(redcap_dates("2022")),"2022-01-01")
  expect_equal(suppressWarnings(redcap_dates("2022",unk_day = "06", unk_month = "07")),"2022-07-06")
  expect_equal(suppressWarnings(redcap_dates("22")),"2022-01-01")
  expect_equal(suppressWarnings(redcap_dates("22",unk_day = "06", unk_month = "07")),"2022-07-06")
  expect_equal(suppressWarnings(redcap_dates("22",unk_day = "06", unk_month = "07",unk_cent = "19")),"1922-07-06")

})
