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
