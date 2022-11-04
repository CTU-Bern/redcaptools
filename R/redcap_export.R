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
#' @importFrom magrittr %>%
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
      return(read.csv(textConnection(body), na.strings = c("NA", "")))
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
#' @param wait seconds to wait between API calls
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
                                 wait = .2,
                                 ...) {

  if(is.null(meta)) meta <- redcap_export_meta(token, url)

  check_meta(meta)

  longitudinal <- meta$project$is_longitudinal == 1

  idvar <- meta$metadata$field_name[1]

  db_sheets <- unique(meta$instrument$instrument_name)

  formmapping <- meta$formEventMapping

  tabs <- sapply(db_sheets,
                 function(x){

                   Sys.sleep(wait)

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

#' Export data in batches
#'
#' Exports of large databases may fail using the standard export methods implemented
#' in \link{redcap_export_tbl} and \link{redcap_export_byform}. To remedy this,
#' the \code{redcap_export_batch} function exports data in smaller chunks (of 1000
#' records by default)
#'
#' @inheritParams redcap_export_byform
#' @param batchsize number of records per batch
#' @param byform logical. Download data by form (see \link{redcap_export_byform})
#'
#' @return depending on \code{byform}, either a list of dataframes or a single dataframe
#' @export
#' @importFrom dplyr bind_rows
#' @seealso \link{redcap_export_tbl}, \link{redcap_export_byform}
#'
#' @examples
#' # token <- "some_really_long_string_provided_by_REDCap"
#' # as a single dataframe
#' # redcap_export_batch(token, "https://www.some_redcap_url.com/api/")
#' # as a list of dataframes (forms)
#' # redcap_export_batch(token, "https://www.some_redcap_url.com/api/", byform = TRUE)
redcap_export_batch <- function(token,
                                url,
                                batchsize = 1000,
                                meta = NULL,
                                byform = FALSE,
                                ...){

  if(is.null(meta)) meta <- redcap_export_meta(token, url)
  idvar <- meta$metadata$field_name[1]

  ids <- redcap_export_tbl(token, url, "record", fields = idvar)
  ids <- unique(ids[[idvar]])
  nbatch <- ceiling(length(ids)/batchsize)
  batches <- rep(1:nbatch, each = batchsize)
  batches <- batches[1:length(ids)]

  if(byform){

    db_sheets <- unique(meta$instrument$instrument_name)

    tmp <- tapply(ids, batches, function(x){
      ids <- paste(x, collapse = ",")
      redcap_export_byform(token, url, meta,
                           # ...,
                           records = ids)
    })

    out <- sapply(db_sheets, function(x){
      lapply(tmp, function(y){
        tmp <- "[["(y, x)
        out <- if(nrow(tmp) > 0){tmp} else {NULL}
        out
      }) %>% bind_rows
    })

  } else {
    out <- tapply(ids, batches, function(x){
      ids <- paste(x, collapse = ",")
      redcap_export_tbl(token, url, "record", records = ids)
    }) %>% bind_rows()

  }

  return(out)

}
