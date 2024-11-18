test_that("redcap_import_dates", {
  expect_equal(redcap_import_dates("01.12.2022"),as.Date("2022-12-01"))
  expect_equal(redcap_import_dates("15.12.2022"),as.Date("2022-12-15"))
  expect_equal(redcap_import_dates("01/12/2022"),as.Date("2022-12-01"))
  expect_equal(redcap_import_dates("01-12-2022"),as.Date("2022-12-01"))
  expect_equal(redcap_import_dates("3.4.22"),as.Date("2022-04-03"))
  expect_equal(redcap_import_dates("12.2022"),as.Date("2022-12-01"))
  expect_equal(redcap_import_dates("12.2022",unk_day = "05"),as.Date("2022-12-05"))
  expect_equal(redcap_import_dates("2022"),as.Date("2022-01-01"))
  expect_equal(redcap_import_dates("2022",unk_day = "06", unk_month = "07"),as.Date("2022-07-06"))
  expect_equal(redcap_import_dates("22"),as.Date("2022-01-01"))
  expect_equal(redcap_import_dates("22",unk_day = "06", unk_month = "07"),as.Date("2022-07-06"))
  expect_equal(redcap_import_dates("33333"),as.Date("1991-04-05"))
  expect_equal(redcap_import_dates("02.22",unk_day = "30"),as.Date(NA))

  expect_equal(redcap_import_dates("01.12.2022",format = "american"),as.Date("2022-01-12"))
  expect_equal(redcap_import_dates("15.12.2022",format = "american"),as.Date("2022-12-15"))
  expect_equal(redcap_import_dates("01/12/2022",format = "american"),as.Date("2022-01-12"))
  expect_equal(redcap_import_dates("01-12-2022",format = "american"),as.Date("2022-01-12"))
  expect_equal(redcap_import_dates("3.4.22",format = "american"),as.Date("2022-03-04"))
  expect_equal(redcap_import_dates("12.2022",format = "american"),as.Date("2022-12-01"))
  expect_equal(redcap_import_dates("12.2022",unk_day = "05",format = "american"),as.Date("2022-12-05"))
  expect_equal(redcap_import_dates("2022",format = "american"),as.Date("2022-01-01"))
  expect_equal(redcap_import_dates("2022",unk_day = "06", unk_month = "07",format = "american"),as.Date("2022-07-06"))
  expect_equal(redcap_import_dates("22",format = "american"),as.Date("2022-01-01"))
  expect_equal(redcap_import_dates("22",unk_day = "06", unk_month = "07",format = "american"),as.Date("2022-07-06"))
  expect_equal(redcap_import_dates("33333",format = "american"),as.Date("1991-04-05"))


})


test_that("redcap_import_times", {
  expect_equal(redcap_import_times("11:11:00"),"11:11:00")
  expect_equal(redcap_import_times("11-11-00"),"11:11:00")
  expect_equal(redcap_import_times("11/11/00"),"11:11:00")
  expect_equal(redcap_import_times("11.11.00"),"11:11:00")
  expect_equal(redcap_import_times("111100"),"11:11:00")
  expect_equal(redcap_import_times("11:11"),"11:11:00")
  expect_equal(redcap_import_times("11"),"11:00:00")
  expect_equal(redcap_import_times("11:11",unk_sec = "30"),"11:11:30")
  expect_equal(redcap_import_times("11",unk_min = "15",unk_sec = "30"),"11:15:30")
  expect_equal(redcap_import_times("0.34375"),"08:15:00")
  expect_equal(redcap_import_times("11:11:00"),"11:11:00")

})


test_that("redcap_import_datetime", {

  expect_equal(redcap_import_datetime("1.2.24 11:11:00"),"2024-02-01 11:11:00")
  expect_equal(redcap_import_datetime("2.24 11:11:00"),"2024-02-01 11:11:00")
  expect_equal(redcap_import_datetime("24 11:11:00"),"2024-01-01 11:11:00")
  expect_equal(redcap_import_datetime("1.2.24 11:11"),"2024-02-01 11:11:00")
  expect_equal(redcap_import_datetime("2.24 11:11"),"2024-02-01 11:11:00")
  expect_equal(redcap_import_datetime("24 11:11"),"2024-01-01 11:11:00")
  expect_equal(redcap_import_datetime("1.2.24 11"),"2024-02-01 11:00:00")
  expect_equal(redcap_import_datetime("2.24 11"),"2024-02-01 11:00:00")
  expect_equal(redcap_import_datetime("24 11"),"2024-01-01 11:00:00")
  expect_equal(redcap_import_datetime("1.2.24"),"2024-02-01 00:00:00")
  expect_equal(redcap_import_datetime("2.24"),"2024-02-01 00:00:00")
  expect_equal(redcap_import_datetime("24"),"2024-01-01 00:00:00")
  expect_equal(redcap_import_datetime("33333.34375"),"1991-04-05 08:15:00")
  expect_equal(redcap_import_datetime("1.2.24 11:11:00",date_only = TRUE),"2024-02-01")

  expect_equal(redcap_import_datetime("1.2.24 11:11:00",
                                      args_rc_dates = list(format = "american")),
               "2024-01-02 11:11:00")
  expect_equal(redcap_import_datetime("2.24 11:11:00",
                                      args_rc_dates = list(unk_day = "15")),
               "2024-02-15 11:11:00")
  expect_equal(redcap_import_datetime("24 11:11:00",
                                      args_rc_dates = list(unk_day = "15",unk_month = "10")),
               "2024-10-15 11:11:00")
  expect_equal(redcap_import_datetime("1.2.24 11:11",
                                      args_rc_times = list(unk_sec = "30")),
               "2024-02-01 11:11:30")
  expect_equal(redcap_import_datetime("2.24 11:11",
                                      args_rc_dates = list(unk_day = "15"),
                                      args_rc_times = list(unk_sec = "30")),
               "2024-02-15 11:11:30")
  expect_equal(redcap_import_datetime("24 11:11",
                                      args_rc_dates = list(unk_day = "15",unk_month = "10"),
                                      args_rc_times = list(unk_sec = "30")),
               "2024-10-15 11:11:30")
  expect_equal(redcap_import_datetime("1.2.24 11",
                                      args_rc_times = list(unk_min = "15",unk_sec = "30")),
               "2024-02-01 11:15:30")
  expect_equal(redcap_import_datetime("2.24 11",
                                      args_rc_dates = list(unk_day = "15"),
                                      args_rc_times = list(unk_min = "15",unk_sec = "30")),
               "2024-02-15 11:15:30")
  expect_equal(redcap_import_datetime("24 11",
                                      args_rc_dates = list(unk_day = "15",unk_month = "10"),
                                      args_rc_times = list(unk_min = "15",unk_sec = "30")),
               "2024-10-15 11:15:30")
  expect_equal(redcap_import_datetime("1.2.24",
                                      args_rc_times = list(unk_min = "15",unk_sec = "30")),
               "2024-02-01 00:00:00")
  expect_equal(redcap_import_datetime("2.24",
                                      args_rc_dates = list(unk_day = "15"),
                                      args_rc_times = list(unk_min = "15",unk_sec = "30")),
               "2024-02-15 00:00:00")
  expect_equal(redcap_import_datetime("24",
                                      args_rc_dates = list(unk_day = "15",unk_month = "10"),
                                      args_rc_times = list(unk_sec = "30")),
               "2024-10-15 00:00:00")


})


