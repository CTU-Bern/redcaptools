#' Analagous to `janitor::remove_empty(..., "rows")`, but allows ignoring specific variables
#'
#' @param data a dataframe
#' @param ignore regex identifying variables to ignore
#'
#' @return dataframe
#' @export
#'
#' @examples
#' x <- data.frame(a = 1:10, b = rep(c("b", NA), 5))
#' remove_empty_rows(x, "a")
remove_empty_rows <- function(data, ignore = "^(record_id|redcap)|_complete$"){
  tmp <- data[, !grepl(ignore, names(data)), drop = FALSE]
  mask_keep <- rowSums(is.na(tmp) | tmp == "") != ncol(tmp)
  data[mask_keep, , drop = FALSE]
}
