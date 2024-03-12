#' Deprecated functions
#' These functions have been renamed to be more consistent with the rest of the package. They may be removed in a future version.
#' @name deprecated
#' @param data dataframe
#' @param metadata data dictionary from REDCap
#' @param rep replace variables. If FALSE, encoded versions of the variable will be created
#' @param rep_date,rep_datetime,rep_singlechoice,rep_multichoice replace the indicated variable type
#' @param app_date,app_datetime,app_singlechoice,app_multichoice text to append to the newly generated variables name (if \code{rep_*} is FALSE)
#' @param append text to append to the newly generated variables name (if \code{replace} is TRUE)
#' @param replace indicator of whether to replace original variables or not
#' @param ... options passed to/from other methods
NULL

#' @describeIn deprecated original function name for \code{redcap_prep}
#' @export
rc_prep <- function(data, metadata,
                    rep = FALSE,
                    rep_date = rep, rep_datetime = rep,
                    rep_singlechoice = rep, rep_multichoice = rep,
                    app_date = "_date", app_datetime = "_datetime",
                    app_singlechoice = "_factor", app_multichoice = "_factor",
                    ...){

  warning("rc_prep is deprecated, please use redcap_prep")
  redcap_prep(data, metadata,
              rep = rep,
              rep_date = rep_date, rep_datetime = rep_datetime,
              rep_singlechoice = rep_singlechoice, rep_multichoice = rep_multichoice,
              app_date = app_date, app_datetime = app_datetime,
              app_singlechoice = app_singlechoice, app_multichoice = app_multichoice,
              ...)

}

#' @describeIn deprecated original function name for \code{redcap_dates}
#' @export
rc_dates <- function(data, metadata, replace = FALSE, append = "_date"){
  warning("rc_dates is deprecated, use redcap_prep_dates instead")
  redcap_prep_dates(data, metadata, replace, append)
}

#' @describeIn deprecated original function name for \code{redcap_datetimes}
#' @export
rc_datetimes <- function(data, metadata, replace = FALSE, append = "_datetime", ...){
  warning("rc_datetimes is deprecated, use redcap_prep_datetimes instead")
  redcap_prep_datetimes(data, metadata, replace, append, ...)
}

#' @describeIn deprecated deprecated in favour of \code{redcap_toform}
#' Split a manually exported REDCap dataset into forms
#'
#' @param data dataframe
#' @param metadata datadictionary as exported from REDCap or downloaded from the API
#'
#' @return list of dataframes
#' @export
split_by_form <- function(data, metadata){
  warning("split_by_form is deprecated, use redcap_toform instead")
  metadata$regex <- ifelse(metadata$field_type == "checkbox",
                           paste0(metadata$field_name, "___"),
                           metadata$field_name)
  sapply(unique(metadata$form_name), function(x){
    regex <- paste(metadata$field_name[1], "^redcap",
                   paste0("^", metadata$regex[metadata$form_name == x],
                          collapse = "|"), sep = "|")
    dd <- data[, grepl(regex, names(data))]
    remove_empty_rows(dd)
  })
}
