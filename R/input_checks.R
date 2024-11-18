check_url <- function(url){
  if(length(url) > 1) stop("'url' should be length 1")
  if(!grepl("api/$", url)) warning("'url' should point to the api e.g. 'domain.com/api/'")
}

check_token <- function(token){
  if(length(token) > 1) stop("'token' must have length 1")
}

check_content <- function(x){
  if(length(x) != 1) stop("'content' must have length 1")
  if(!x %in% api_meta()) stop("unsupported content - see ?redcap_export_meta")
}

check_meta <- function(x){

  if(!is.list(x)) stop("meta should be a list")
  if(!all(sapply(names(x), function(x) x %in% api_meta()))) warning("unrecognised content name in meta")

  if(!"project" %in% names(x)) stop("meta must include 'project'")

  longitudinal <- x$project$is_longitudinal == 1

  # check meta includes the relevant elements
  if(!"instrument" %in% names(x)) stop("meta must contain 'instrument'")
  if(!"formEventMapping" %in% names(x) & longitudinal) stop("meta must contain 'formEventMapping'")

}

check_dict <- function(x) {

  if(!is.data.frame(x)) stop("dict should be a data frame")

  # check that dict includes the relevant columns
  if(!"field_name" %in% names(x)) stop("dict must include 'field_name'")
  if(!"form_name" %in% names(x)) stop("dict must include 'form_name'")
  if(!"field_type" %in% names(x)) stop("dict must include 'field_type'")
  if(!"field_label" %in% names(x)) stop("dict must include 'field_label'")
  if(!"select_choices_or_calculations" %in% names(x)) stop("dict must include 'select_choices_or_calculations'")
  if(!"text_validation_type_or_show_slider_number" %in% names(x)) stop("dict must include 'text_validation_type_or_show_slider_number'")
  if(!"text_validation_min" %in% names(x)) stop("dict must include 'text_validation_min'")
  if(!"text_validation_max" %in% names(x)) stop("dict must include 'text_validation_max'")
  if(!"branching_logic" %in% names(x)) stop("dict must include 'branching_logic'")

}

check_forms <- function(x) {
  if(!is.character(x)) stop("forms should be a character vector")
}

check_data <- function(x) {
  if(!is.data.frame(x)) stop("import data should be a data frame")
}

check_missing_codes <- function(x) {

  if(!is.data.frame(x) |
     length(x) != 2 |
     names(x)[1] != "code" |
     names(x)[2] != "label" |
     any(is.na(x))) {
    stop("missing codes could not be parsed correctly")
  }

}
