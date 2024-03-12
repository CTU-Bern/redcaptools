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
