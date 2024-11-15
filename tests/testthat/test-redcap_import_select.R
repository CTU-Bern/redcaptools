data("importdemo_data")
data("importdemo_dict")

test_that("redcap_import_select works", {
  expect_error(redcap_import_select(importdemo_data, importdemo_dict,
                                    auto_skip_nomatch = TRUE,
                                    skip_intro = TRUE,
                                    continue = FALSE,
                                    suppress_txt = TRUE,
                                    log = FALSE,
                                    wait = 0), NA)

  x <- redcap_import_select(importdemo_data, importdemo_dict,
                            auto_skip_nomatch = TRUE,
                            skip_intro = TRUE,
                            continue = FALSE,
                            suppress_txt = TRUE,
                            log = FALSE,
                            wait = 0)

  expect_s3_class(x, "data.frame")

  expect_equal(names(x)[1],"record_id")

  matches <- intersect(names(importdemo_data), importdemo_dict$field_name)
  expect_equal(names(x),matches)

  no_matches <- setdiff(names(importdemo_data), importdemo_dict$field_name)
  expect_equal(any(no_matches %in% names(x)),FALSE)
})

