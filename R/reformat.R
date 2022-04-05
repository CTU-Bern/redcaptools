
# get radio button options into a dataframe
singlechoice_opts <- function(metadata){
  radio <- metadata[metadata$field_type %in% c("radio", "dropdown"), ]
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


# get checkbox button options into a dataframe
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



# create factors for radio buttons
singlechoice_factor <- function(data, metadata, replace = FALSE, append = "_factor"){
  require(Hmisc)
  radios <- singlechoice_opts(metadata)
  radios <- radios[radios$var %in% names(data), ]
  if(nrow(radios) > 0){
    for(i in unique(radios$var)){
      tmp <- radios[radios$var == i, ]
      v <- if(replace) i else  paste0(i, append)
      data[, v] <- factor(data[, i], levels = tmp$val, labels = tmp$lab)
      labelled::var_label(data[, i]) <- unique(tmp$label)
      if(!replace) labelled::var_label(data[, v]) <- unique(tmp$label)
    }
  }
  return(data)
}

# create factors for checkbox buttons
multichoice_factor <- function(data, metadata, replace = FALSE, append = "_factor"){
  require(Hmisc)
  checks <- multichoice_opts(metadata)
  checks <- checks[checks$var %in% names(data), ]
  if(nrow(checks) > 0){
    for(i in 1:nrow(checks)){
      ov <- checks$var[i]
      l <- checks$label[i]
      v <- if(replace) ov else paste0(ov, append)
      data[, v] <- factor(data[, ov], levels = c(0, 1), labels = c("No", "Yes"))
      labelled::var_label(data[, ov]) <- l
      if(!replace) labelled::var_label(data[, v]) <- l
    }
  }
  return(data)
}

# format dates
rc_dates <- function(data, metadata, replace = FALSE, append = "_date"){
  tmp <- subset(metadata, metadata$text_validation_type_or_show_slider_number == "date_dmy")
  tmp <- tmp[tmp$field_name %in% names(data), ]
  if(nrow(tmp) > 0){
    for(i in 1:nrow(tmp)){
      ov <- tmp$field_name[i]
      # print(ov)
      v <- if(replace) ov else paste0(ov, append)
      data[, v] <- lubridate::as_date(data[, ov])
      labelled::var_label(data[, ov]) <- tmp$field_label[i]
      if(!replace) labelled::var_label(data[, v]) <- tmp$field_label[i]
    }
  }
  return(data)
}

# format datetimes
rc_datetimes <- function(data, metadata, replace = FALSE, append = "_datetime"){
  tmp <- subset(metadata, metadata$text_validation_type_or_show_slider_number == "datetime_dmy")
  tmp <- tmp[tmp$field_name %in% names(data), ]
  if(nrow(tmp) > 0){
    for(i in 1:nrow(tmp)){
      ov <- tmp$field_name[i]
      # print(ov)
      v <- if(replace) ov else paste0(ov, append)
      data[, v] <- lubridate::as_datetime(data[, ov])
      labelled::var_label(data[, ov]) <- unique(tmp$field_label[i])
      if(!replace) labelled::var_label(data[, v]) <- unique(tmp$field_label[i])
    }
  }
  return(data)
}


# label non-radio/checkbox/date(time) fields
label_others <- function(data, metadata){
  tmp <- metadata[!metadata$field_type %in% c("checkbox", "radio", "dropdown") & !metadata$text_validation_type_or_show_slider_number %in% c("date_dmy", "datetime_dmy"), ]
  tmp <- tmp[tmp$field_name %in% names(data), ]
  for(i in 1:nrow(tmp)){
    labelled::var_label(data[, tmp$field_name[i]]) <- tmp$field_label[i]
  }
  return(data)
}


# do all of the above
rc_prep <- function(data, metadata){

  if(!is.null(data)){

    tmp <- singlechoice_factor(data, metadata)
    tmp <- multichoice_factor(tmp, metadata)
    tmp <- rc_dates(tmp, metadata)
    tmp <- rc_datetimes(tmp, metadata)
    tmp <- label_others(tmp, metadata)
    return(tmp)
  } else {

    return(data)

  }

}
