#' Convert variables to logical
#'
#' This is particularly useful for binary variables that have been encoded with
#' e.g. Yes and No as options. Variable labels are retained, which may or may not
#' make sense, depending on the variable
#'
#' @param data dataframe
#' @param vars character string of variables to convert
#' @param true value which should become `TRUE`
#' @param replace Replace the indicated variables
#' @param append text to append to new variables (when `replace = TRUE`)
#'
#' @return `data` with modified variables, potentially with additional variables
#' (if `replace = TRUE`)
#' @export
#' @md
#'
#' @examples
#' data(mtcars)
#' convert_to_logical(mtcars, "am", 1)
#' convert_to_logical(mtcars, c("am", "vs"), 1)
#' convert_to_logical(mtcars, c("am", "vs"), 1, FALSE)
#' convert_to_logical(mtcars, c("am", "vs"), 1, FALSE, "_lgl")
convert_to_logical <- function(data, vars, true = "Yes", replace = TRUE, append = "_logical"){
  if(!all(vars %in% names(data))) stop("some 'vars' not found in 'data'")

  for(i in vars){
    v <- var_label(data[, i])
    if(!replace){
      n <- paste0(i, append)
    } else {
      n <- i
    }
    data[, n] <- data[, i] == true
    var_label(data[, n]) <- v
  }
  return(data)
}
