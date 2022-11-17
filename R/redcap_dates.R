#' REDCap Date Conversion
#'
#' This function is trying to take all (most)
#' possibilities to enter dates into account and converts them to as.Date
#' format.
#'
#' @param var variable to convert
#'
#' @return converted variable
#' @export
#' @importFrom crayon bold underline red

#'
#' @examples
#' var <- "dataframe$var"
#' redcap_dates(var)


redcap_dates <- function(var) {

  # (as.Date() does not work in ifelse statement -> as.character(as.Date()) does!)
  # (^ .... $ for exact match)
  # \\. = 'dot'
  # between the year format (yy Vs. yyyy) needs to be distinguished (with %y Vs. %Y)
  # (e.g., 22 with %Y becomes '0022' or 2022 with %y becomes '2020')
  # for months and days always %d or %m should be used
  # (e.g., 01.01. with %m = '01.01.' but 1.1. with %D = '01.10.')

  cat(red(bold(underline("Warning:"),
               "\nThis conversion is only valid with the european date format (days before months) and will lead to wrong results otherwise!!",
               "\nIf only the year is found, 01.01. is prefixed!",
               "\nIf only the month and year is found, 01. is prefixed!")))

  # convert to character
  var <- as.character(var)

  # sometimes entries are separated with / -> replace with .
  var <- gsub("/",".",var)

  # if only year was entered, append 01.01. ("2011" -> "01.01.2011")
  var <- ifelse(grepl("^[1-2]{1}[0-9]{3}$",var),                                          # yyyy
                as.character(as.Date(paste0("01.01.",var),format = "%d.%m.%Y")),
                var)

  # if only month and year was entered, append 01. ("01.2011" -> "01.01.2011")
  var <- ifelse(grepl("^[1-9]{1}\\.[1-2]{1}[0-9]{3}$",var) |                              # m.yyyy
                  grepl("^[0-1]{1}[0-9]{1}\\.[1-2]{1}[0-9]{3}$",var),                     # mm.yyyy
                as.character(as.Date(paste0("01.",var),format = "%d.%m.%Y")),
                var)
  var <- ifelse(grepl("^[1-9]{1}\\.[0-9]{2}",var) |                                       # m.yy
                  grepl("^[0-1]{1}[0-9]{1}\\.[0-9]{2}$",var),                             # mm.yy
                as.character(as.Date(paste0("01.",var),format = "%d.%m.%y")),
                var)

  # if regular date was entered
  var <- ifelse(grepl("^[1-9]{1}\\.[1-9]{1}\\.[1-2]{1}[0-9]{3}$",var) |                   # d.m.yyyy
                  grepl("^[1-9]{1}\\.[0-1]{1}[0-9]{1}\\.[1-2]{1}[0-9]{3}$",var) |         # d.mm.yyyy
                  grepl("^[0-3]{1}[0-9]{1}\\.[1-9]{1}\\.[1-2]{1}[0-9]{3}$",var) |         # dd.m.yyyy
                  grepl("^[0-3]{1}[0-9]{1}\\.[0-1]{1}[0-9]{1}\\.[1-2]{1}[0-9]{3}$",var),  # dd.mm.yyyy
                as.character(as.Date(var,format = "%d.%m.%Y")),
                var)
  var <- ifelse(grepl("^[1-9]{1}\\.[1-9]{1}\\.[0-9]{2}$",var) |                           # d.m.yy
                  grepl("^[1-9]{1}\\.[0-1]{1}[0-9]{1}\\.[0-9]{2}$",var) |                 # d.mm.yy
                  grepl("^[0-3]{1}[0-9]{1}\\.[1-9]{1}\\.[0-9]{2}$",var) |                 # dd.m.yy
                  grepl("^[0-3]{1}[0-9]{1}\\.[0-1]{1}[0-9]{1}\\.[0-9]{2}$",var),          # dd.mm.yy
                as.character(as.Date(var,format = "%d.%m.%y")),
                var)

  # if date was entered as number
  var <- ifelse(grepl("^[1-2]{1}[0-9]{3}[0-1]{1}[0-9]{1}[0-3]{1}[0-9]{1}$",var),          # yyyymmdd
                as.character(as.Date(var,format = "%Y%m%d")),
                var)
  var <- ifelse(grepl("^[0-3]{1}[0-9]{1}[0-1]{1}[0-9]{1}[1-2]{1}[0-9]{3}$",var),          # ddmmyyyy
                as.character(as.Date(var,format = "%d%m%Y")),
                var)

  return(var)


}
