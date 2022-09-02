#' Get options for single and multi choice questions
#'
#' @rdname choice_options
#' @param metadata data.frame containing the metadata
#'
#' @return data.frame with variables \code{var} (variable), \code{label} (the
#' variable label), \code{vals} (possible values for the variable) and
#' \code{labs} (the labels related to each value in \code{vals})
#' @export
singlechoice_opts <- function(metadata){
  radio <- metadata[metadata$field_type %in% c("radio", "dropdown", "yesno"), ]
  radio$select_choices_or_calculations[radio$field_type == "yesno"] <- "0, No | 1, Yes"
  fn <- function(var, choices, label){
    opts <- choices
    opts <- trimws(unlist(strsplit(opts, "|", fixed = TRUE)))
    n <- length(opts)
    opts2 <- strsplit(opts, ",")

    vals <- trimws(sapply(opts2, function(x) x[1], simplify = TRUE))
    labs <- trimws(sapply(opts2, function(x) paste(x[-1], collapse = ","), simplify = TRUE))
    labvals <- data.frame(var = rep(var, n), label = rep(label, n), val = vals, lab = labs)
    labvals
  }
  radio_labs <- do.call("rbind", apply(radio, 1, function(x) fn(x["field_name"], x["select_choices_or_calculations"], x["field_label"])))
  row.names(radio_labs) <- NULL
  return(radio_labs)
}


#' @rdname choice_options
#' @details Multiple choice variables exist in REDCap data as a set of 0/1/TRUE/FALSE
#' variables, where 1/TRUE represents a selected/checked answer. Hence, for a
#' single multiple choice 'question' in the datadictionary/metadata with \code{n} options,
#' there are \code{n} variables. Each variable is the variable name (e.g. morbidities)
#' followed by 3 underscores (\code{___}) and the option number (e.g. 1) -
#' \code{morbidities___1}.
#' @export
#' @return data.frame with variables \code{ovar} (the variable as it appears in
#' the data dictionary/metadata), \code{var} (the variable as it appears in
#' the data itself), \code{vlabel} (the variable label), \code{vals} (possible
#' values for the variable) and \code{labs} (the labels related to each value in
#' \code{vals})
multichoice_opts <- function(metadata){
  tmp <- metadata[metadata$field_type == "checkbox", ]
  fn <- function(var, choices, label){
    opts <- choices
    opts <- trimws(unlist(strsplit(opts, "|", fixed = TRUE)))
    n <- length(opts)
    opts2 <- strsplit(opts, ",")

    vals <- trimws(sapply(opts2, function(x) x[1], simplify = TRUE))
    labs <- trimws(sapply(opts2, function(x) paste(x[-1], collapse = ","), simplify = TRUE))
    labvals <- data.frame(ovar = rep(var, n), var = rep(var, n), vlabel = rep(label, n), val = vals, label = labs)
    labvals
  }
  tmp_labs <- do.call("rbind", apply(tmp, 1, function(x) fn(x["field_name"], x["select_choices_or_calculations"], x["field_label"])))
  row.names(tmp_labs) <- NULL
  tmp_labs$var <- paste0(tmp_labs$var, "___", tmp_labs$val)
  return(tmp_labs)
}


#' create factors for single choice variables
#'
#' Converts the numeric values returned from REDCap to factors. This function also
#' applies labels to the variable itself.
#'
#' @param data the data.frame to modify
#' @param metadata metadata/datadictionary
#' @param replace whether to overwrite the existing data .
#' @param append text to append to the variable name if not overwriting
#'
#' @return dataframe with factor variables
#' @importFrom labelled var_label var_label<-
singlechoice_factor <- function(data, metadata, replace = FALSE, append = "_factor"){
  radios <- singlechoice_opts(metadata)
  radios <- radios[radios$var %in% names(data), ]
  if(nrow(radios) > 0){
    for(i in unique(radios$var)){
      tmp <- radios[radios$var == i, ]
      v <- if(replace) i else  paste0(i, append)
      data[, v] <- factor(data[, i], levels = tmp$val, labels = tmp$lab)
      var_label(data[, i]) <- unique(tmp$label)
      if(!replace) var_label(data[, v]) <- unique(tmp$label)
    }
  }
  return(data)
}

#' create factors for multiple choice variables
#'
#' Converts the numeric values returned from REDCap to factors (with levels Yes/No).
#' This function also applies labels to the variable itself, based on the option label.
#'
#' @param data the data.frame to modify
#' @param metadata metadata/datadictionary
#' @param replace whether to overwrite the existing data .
#' @param append text to append to the variable name if not overwriting
#'
#' @return input data.frame with additional factor variables.
#' @export
multichoice_factor <- function(data, metadata, replace = FALSE, append = "_factor"){
  checks <- multichoice_opts(metadata)
  checks <- checks[checks$var %in% names(data), ]
  if(nrow(checks) > 0){
    for(i in 1:nrow(checks)){
      ov <- checks$var[i]
      l <- checks$label[i]
      v <- if(replace) ov else paste0(ov, append)
      data[, v] <- factor(data[, ov], levels = c(0, 1), labels = c("No", "Yes"))
      var_label(data[, ov]) <- l
      if(!replace) var_label(data[, v]) <- l
    }
  }
  return(data)
}

#' Convert dates stored as strings to \code{Date} variables
#'
#' Converts the string values returned from REDCap to Dates.
#' This function also applies labels to the variable itself, based on the option label.
#'
#' @rdname rc_date
#' @param data the data.frame to modify
#' @param metadata metadata/datadictionary
#' @param replace whether to overwrite the existing data .
#' @param append text to append to the variable name if not overwriting
#'
#' @return input data.frame with additional date variables/variables converted to dates.
#' @importFrom labelled var_label var_label<-
#' @importFrom lubridate as_date
#' @export
rc_dates <- function(data, metadata, replace = FALSE, append = "_date"){
  tmp <- subset(metadata, metadata$text_validation_type_or_show_slider_number == "date_dmy")
  tmp <- tmp[tmp$field_name %in% names(data), ]
  if(nrow(tmp) > 0){
    for(i in 1:nrow(tmp)){
      ov <- tmp$field_name[i]
      # print(ov)
      v <- if(replace) ov else paste0(ov, append)
      data[, v] <- as_date(data[, ov])
      var_label(data[, ov]) <- tmp$field_label[i]
      if(!replace) var_label(data[, v]) <- tmp$field_label[i]
    }
  }
  return(data)
}

#' @describeIn rc_date input data.frame with date-time variables reformated to POSIX
#' @importFrom labelled var_label var_label<-
#' @importFrom lubridate as_datetime
rc_datetimes <- function(data, metadata, replace = FALSE, append = "_datetime"){
  tmp <- subset(metadata, metadata$text_validation_type_or_show_slider_number == "datetime_dmy")
  tmp <- tmp[tmp$field_name %in% names(data), ]
  if(nrow(tmp) > 0){
    for(i in 1:nrow(tmp)){
      ov <- tmp$field_name[i]
      # print(ov)
      v <- if(replace) ov else paste0(ov, append)
      data[, v] <- as_datetime(data[, ov])
      var_label(data[, ov]) <- unique(tmp$field_label[i])
      if(!replace) var_label(data[, v]) <- unique(tmp$field_label[i])
    }
  }
  return(data)
}


#' Label non-single/multiple choice/date(time) fields
#' \code{singlechoice_factor}, \code{multichoice_factor}, \code{rc_date} and \code{rc_datetime}
#'
#' @param data dataframe
#' @param metadata redcap data dictionary
#' @export
#' @importFrom labelled var_label var_label<-
label_others <- function(data, metadata){
  tmp <- metadata[!metadata$field_type %in% c("checkbox", "radio", "dropdown") & !metadata$text_validation_type_or_show_slider_number %in% c("date_dmy", "datetime_dmy"), ]
  tmp <- tmp[tmp$field_name %in% names(data), ]
  if(nrow(tmp) > 0){
    for(i in seq_along(tmp$field_type)){
      var_label(data[, tmp$field_name[i]]) <- tmp$field_label[i]
    }
  }
  return(data)
}


#' Convert REDCap variable types (dates, datetimes, factors) and apply labels
#'
#' @param data dataframe
#' @param metadata data dictionary from REDCap
#' @param rep_date,rep_datetime,rep_singlechoice,rep_multichoice replace the indicated variable type
#' @param app_date,app_datetime,app_singlechoice,app_multichoice text to append to the newly generated variables name (if \code{rep_*} is FALSE)
#'
#' @return dataframe with converted factors, dates, POSIX, ...
#' @export
#'
rc_prep <- function(data, metadata,
                    rep_date = FALSE, rep_datetime = FALSE,
                    rep_singlechoice = FALSE, rep_multichoice = FALSE,
                    app_date = "_date", app_datetime = "_datetime",
                    app_singlechoice = "_factor", app_multichoice = "_factor"
                    ){

  tmp <- singlechoice_factor(data, metadata,
                             replace = rep_singlechoice,
                             append = app_singlechoice)
  tmp <- multichoice_factor(tmp, metadata,
                            replace = rep_multichoice,
                            append = app_multichoice)
  tmp <- rc_dates(tmp, metadata,
                  replace = rep_date,
                  append = app_date)
  tmp <- rc_datetimes(tmp, metadata,
                      replace = rep_datetime,
                      append = app_datetime)
  tmp <- label_others(tmp, metadata)
  return(tmp)

}
