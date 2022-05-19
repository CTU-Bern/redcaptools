#' Export tables from REDCap
#'
#' @param token REDcap API token
#' @param url address of the API
#' @param content content to download
#' @param ... other parameters passed to the API (see your REDCap API helpfile for options)
#'
#' @return dataframe
#' @export
#' @importFrom httr2 request req_headers req_body_form req_perform resp_status resp_body_string
#'
#' @examples
#' # token <- "some_really_long_string_provided_by_REDCap"
#' # redcap_export_tbl(token, "https://www.some_redcap_url.com/api/", "record")
redcap_export_tbl <- function(token, url, content, ...){

  if(length(token) != 1) stop("'token' must have length 1")
  if(length(url) != 1) stop("'url' must have length 1")
  if(length(content) != 1) stop("'content' must have length 1")

  if(!grepl("/$", url)){
    warning("url should end with '/'")
    url <- paste0(url, "/")
  }

  req <- httr2::request(url) %>%
    httr2::req_headers() %>%
    httr2::req_body_form(list(token = token,
                              content = content,
                              format = "csv",
                              ...))
  resp <- req %>% httr2::req_perform()
  if(httr2::resp_status(resp) == 200){
    body <- resp %>% httr2::resp_body_string()
    if(nchar(body) > 1){
      return(read.csv(textConnection(body)))
    }
  }
}



#' Export most important REDCap metadata tables
#'
#' @inheritParams redcap_export_tbl
#' @param tabs tables to export
#'
#' @return list of dataframes
#' @export
#'
#' @examples
#' # token <- "some_really_long_string_provided_by_REDCap"
#' # redcap_export_meta(token, "https://www.some_redcap_url.com/api/")
redcap_export_meta <- function(token,
                               url,
                               tabs = c("metadata", "event", "formEventMapping"),
                               ...){
  out <- lapply(tabs,
         function(x){
           redcap_export_tbl(token,
                             url,
                             content = x,
                             ...)
         })
  names(out) <- tabs
  return(out)
}



#' Export REDCap data by form
#'
#' @inheritParams redcap_export_tbl
#' @param meta metadata from \code{redcap_export_meta} (will be downloaded if not provided)
#'
#' @return list of dataframes
#' @export
#'
#' @examples
#' # token <- "some_really_long_string_provided_by_REDCap"
#' # redcap_export_byform(token, "https://www.some_redcap_url.com/api/")
#'
redcap_export_byform <- function(token,
                                 url,
                                 meta = NULL,
                                 ...) {

  if(is.null(meta)) meta <- redcap_export_meta(token, url)

  db_sheets <- unique(meta$formEventMapping$form)

  formmapping <- meta$formEventMapping

  tabs <- lapply(db_sheets,
                 function(x){

                   events <- subset(formmapping, formmapping$form == x)$unique_event_name
                   events <- paste0(events, collapse = ",")

                   redcap_export_tbl(token, url,
                                     content = "record",
                                     forms = x,
                                     events = events,
                                     'fields[0]' = "record_id",
                                     ...)
                   })

  names(tabs) <- db_sheets

  return(tabs)
}
