#' Convert manually downloaded REDCap data into a list of forms
#'
#' Similar to \code{redcap_export_byform}, this function tries to split a
#' manually downloaded dataset into it's constituent forms. While use of the API
#' allows individual forms to be downloaded, with a manual download, only the data
#' dictionary is available as auxillary information. If no data dictionary is
#' available, the function will use the variable names to guess the forms (see
#' details).
#'
#' @param data imported REDCap data
#' @param datadict data dictionary downloaded manually from REDCap
#' @param metadata metadata downloaded from REDCap API
#' @param guess_events restrict forms to events (rows) where data exists (see details)
#' @param ... additional arguments passed to other functions (currently unused)
#'
#' @details
#' In a longitudinal data collection with many forms, a REDCap dataset will have
#' a large degree of empty cells. The \code{guess_events} argument uses missingness
#' as an indicator of a row not being part of the form in question. If all user
#' variables (i.e. those that do not start with \code{redcap}) are empty, the row
#' will be removed from the dataset.
#'
#' If neither \code{datadict} nor \code{metadata} are provided, the function will
#' attempt to guess the forms based on the variable names, specifically the
#' \code{form_complete} variables which denote the state of the form. This is
#' not a foolproof method: there may be other variables in the data that end with
#' \code{_complete}.
#'
#' @importFrom dplyr if_else pull filter mutate select everything across where slice matches
#' @importFrom tidyr fill
#' @importFrom stringr str_detect str_extract
#' @export
#' @examples
#' data <- readRDS(system.file("extdata/test.rda", package = "redcaptools"))
#' metadata <- readRDS(system.file("extdata/meta.rda", package = "redcaptools"))
#' dd <- read.csv(system.file("extdata/DataDictionary.csv", package = "redcaptools"))
#' redcap_toform(data, dd)
#' redcap_toform(data, metadata = metadata)
#' redcap_toform(data)
redcap_toform <- function(data,
                          datadict = NULL,
                          metadata = NULL,
                          guess_events = TRUE,
                          ...){

  field_name <- form_name <- field_type <- regex <- NULL


  if(!is.null(datadict)){
    # a manually downloaded data dictionary has different variable names to the
    #   API version
    metadata <- harmonize_datadict(datadict)
  }
  if(is.null(datadict) & is.null(metadata)){
    warning("No metadata provided, guessing forms based on variable names")
    metadata <- data.frame(field_name = names(data)) |>
      mutate(complete = str_detect(field_name, "_complete$"),
             form_name = str_extract(field_name, ".*(?=_complete)"),
             field_type = "") |>
      fill(form_name, .direction = "up")

    metadata <- metadata |>
      filter(!grepl("^redcap", field_name))
  }



  forms <- metadata$form_name |> unique()
  names(forms) <- forms
  idvar <- metadata$field_name[1]
  redcap_vars <- names(data)[grepl("^redcap", names(data))]

  generic_regex <- regex_many(c(idvar, redcap_vars))

  lapply(forms,
         function(form){
           formmeta <- metadata |> slice(-1) |> filter(form_name == form)
           # construct regex to select variables
           form_regex <- formmeta |>
             mutate(regex = if_else(field_type == "checkbox",
                                    paste0("^", field_name, "___"),
                                    regex_single(field_name))) |>
             pull(regex) |> paste(collapse = "|")

           # extract forms
           tmp <- data |>
             select(matches(generic_regex), matches(form_regex))

           if(guess_events){
             tmp2 <- tmp |>
               select(matches(form_regex)) |>
               mutate(across(
                 everything(),
                 ~ !is.na(.x) & .x != ""),

               ) |> rowSums(na.rm = TRUE)
             tmp <- tmp |> filter(tmp2 > 0)
           }
           tmp
         })

}

regex_single <- function(x){
  paste0("^", x, "$")
}
regex_many <- function(x){
  paste0(regex_single(x), collapse = "|")
}

# regex_single("redcap_event_name")
# regex_many(c("redcap_event_name", "redcap_repeat_instance"))

harmonize_datadict <- function(datadict){
  if(!ncol(datadict) == 18)
    stop("data dictionary must have 18 columns")
  # convert factors to character
  datadict <- datadict |>
    mutate(across(where(is.factor), as.character))
  # set names
  names(datadict) <- c("field_name", "form_name", "section_header", "field_type",
                       "field_label", "select_choices_or_calculations", "field_note",
                       "text_validation_type_or_show_slider_number",
                       "text_validation_min", "text_validation_max", "identifier",
                       "branching_logic", "required_field", "custom_alignment",
                       "question_number", "matrix_group_name", "matrix_ranking",
                       "field_annotation")
  return(datadict)
}
