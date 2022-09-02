#' Analagous to `janitor::remove_empty(..., "rows")`, but allows ignoring specific variables
#'
#' @param data a dataframe
#' @param ignore regex identifying variables to ignore
#'
#' @return dataframe
#' @export
#'
#' @examples
#' x <- data.frame(a = c(1:9, NA), b = rep(c("b", NA), 5))
#' remove_empty_rows(x, "a")
#' remove_empty_rows(x, FALSE)
remove_empty_rows <- function(data, ignore = "^(record_id|redcap)|_complete$"){
  if(is.logical(ignore)){
    if(ignore) stop("'ignore should be FALSE or a regex string'")
    ignore_vars <- rep(TRUE, ncol(data))
  } else {
    if(ignore == "") warning('ignore = "" will ignore everything')
    ignore_vars <- !grepl(ignore, names(data))
  }
  tmp <- data[, ignore_vars, drop = FALSE]
  mask_keep <- rowSums(is.na(tmp) | tmp == "") != ncol(tmp)
  data[mask_keep, , drop = FALSE]
}
