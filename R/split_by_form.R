#' Split a manually exported REDCap dataset into forms
#'
#' @param data dataframe
#' @param metadata datadictionary as exported from REDCap or downloaded from the API
#'
#' @return list of dataframes
#' @export
split_by_form <- function(data, metadata){
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
