#' Export tables from REDCap
#'
#' @param token REDcap API token
#' @param url address of the API
#' @param content content to download
#' @param ... other parameters passed to the API (see your REDCap API documentation for options)
#'
#' @return dataframe
#' @export
#' @importFrom httr2 request req_headers req_body_form req_perform resp_status resp_body_string
#'
#' @examples
#' # token <- "some_really_long_string_provided_by_REDCap"
#' # redcap_export_tbl(token, "https://www.some_redcap_url.com/api/", "record")
redcap_export_tbl <- function(token, url, content, ...){
  check_token(token)
  check_url(url)
  check_content(content)

  if(!grepl("/$", url)){
    warning("url should end with '/'")
    url <- paste0(url, "/")
  }

  req <- httr2::request(url) %>%
    httr2::req_headers() %>%
    httr2::req_body_form(token = token,
                         content = content,
                         format = "csv",
                         ...)

  resp <- req %>% httr2::req_perform()
  if(httr2::resp_status(resp) == 200){
    body <- resp %>% httr2::resp_body_string()
    if(nchar(body) > 1){
      return(read.csv(textConnection(body)))
    }
  }
}



#' Export the most important REDCap metadata tables
#'
#' The REDCap API has a large number of API endpoints. Those that are metadata-type
#' details are listed on this page. The
#'
#' @details
#' Allowed tabs are
#' * `arm` - labels of a projects arms
#' * `dag` - data access groups (DAGs)
#' * `userDagMapping` - mapping between users and DAGs
#' * `event` - list of events in the project (only
#'    available for longitudinal projects)
#' * `exportFieldNames` - list of the fields that the API returns
#' * `instrument` - list of instruments (eCRFs/forms) in the project
#' * `formEventMapping` - mapping between instruments (forms) and events (only
#'    available for longitudinal projects)
#' * `metadata` - the data dictionary
#' * `project` - information on the project
#' * `record` - the data itself. The method has many options. See the API help
#'   page on your REDCap instance
#' * `repeatingFormsEvents` - which forms can repeat on which events
#' * `report` - access custom reports defined in REDCap
#' * `version` - REDCap version
#' * `user` - list of users
#' * `userRole` - rights for each role
#' * `userRoleMapping` - user-roll mapping
#' @inheritParams redcap_export_tbl
#' @param tabs tables to export. `project` is always added.
#' @note tables that are not relevant for non-longitudinal projects (e.g.
#'   formEventMapping and event) are silently removed
#' @return list of dataframes
#' @export
#' @md
#'
#' @examples
#' # token <- "some_really_long_string_provided_by_REDCap"
#' # redcap_export_meta(token, "https://www.some_redcap_url.com/api/")
redcap_export_meta <- function(token,
                               url,
                               tabs = c("metadata", "event", "formEventMapping", "instrument"),
                               ...){

  proj <- redcap_export_tbl(token, url = url, "project")

  if(proj$is_longitudinal == 0){
    if(any(c("event", "formEventMapping") %in% tabs)){
      tabs <- tabs[!tabs %in% c("event", "formEventMapping")]
    }
  }

  out <- sapply(tabs,
         function(x){
           redcap_export_tbl(token,
                             url,
                             content = x,
                             ...)
         })

  out$project <- proj

  return(out)
}



#' Export REDCap data by form
#'
#' @inheritParams redcap_export_tbl
#' @param meta metadata from \code{redcap_export_meta} (will be downloaded if not provided)
#' @param remove_empty should empty rows be removed from the dataset (REDCap automatically
#' creates all forms for an event when any form in the event is created)
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
                                 remove_empty = TRUE,
                                 ...) {

  if(is.null(meta)) meta <- redcap_export_meta(token, url)

  check_meta(meta)

  longitudinal <- meta$project$is_longitudinal == 1

  idvar <- meta$metadata$field_name[1]

  db_sheets <- unique(meta$instrument$instrument_name)

  formmapping <- meta$formEventMapping

  tabs <- sapply(db_sheets,
                 function(x){

                   if(longitudinal){
                     events <- subset(formmapping, formmapping$form == x)$unique_event_name
                     events <- paste0(events, collapse = ",")

                     d <- redcap_export_tbl(token, url,
                                            content = "record",
                                            forms = x,
                                            events = events,
                                            'fields[0]' = idvar,
                                            ...)

                     if(remove_empty & !is.null(d)) d <- remove_empty_rows(d)
                   } else {
                     d <- redcap_export_tbl(token, url,
                                            content = "record",
                                            forms = x,
                                            'fields[0]' = idvar,
                                            ...)
                   }

                   return(d)

                   })

  return(tabs)
}


