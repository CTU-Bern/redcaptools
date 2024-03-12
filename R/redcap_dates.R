#' REDCap Date Conversion
#'
#' This function is used to prepare date fields in a data table for import in
#' REDCap. The function tries to take all (most)possibilities to enter dates
#' into account and converts them to as.Date format.
#'
#' @param var variable to convert
#' @param unk_day Day to use if unknown, i.e. if only the year or only the month
#'   + year is found. The default is 01 (2022 -> 2022-01-01).
#' @param unk_month Month to use if unknown, i.e. if only the year is found. The
#'   default is 01 (2022 -> 2022-01-01).
#' @param unk_cent Century to use if unknown, i.e. if only the year is found.
#' The default is 20 (22 -> 2022)
#'
#' @return converted variable
#' @export
#' @importFrom crayon bold underline red

#'
#' @examples
#' var <-c("01.12.2022", "12.2022", "2022", "01/12/2022")
#' redcap_dates(var)


redcap_dates <- function(var,
                         unk_day = "01",
                         unk_month = "01",
                         unk_cent = "20") {

  # (as.Date() does not work in ifelse statement -> as.character(as.Date()) does!)
  # (^ .... $ for exact match)
  # \\. = 'dot'
  # between the year format (yy Vs. yyyy) needs to be distinguished (with %y Vs. %Y)
  # (e.g., 22 with %Y becomes '0022' or 2022 with %y becomes '2020')
  # for months and days always %d or %m should be used
  # (e.g., 01.01. with %m = '01.01.' but 1.1. with %D = '01.10.')

  warning("This conversion is (currently) only valid with the european date format (days before months) and will lead to wrong results otherwise!!\n\n")

  # convert to character
  var <- as.character(var)

  # sometimes entries are separated with / -> replace with .
  var <- gsub("/",".",var)
  # sometimes entries are separated with - -> replace with .
  var <- gsub("-",".",var)

  # if only year was entered, append accordingly ("2011" -> "01.01.2011")
  var <- ifelse(grepl("^[1-2]{1}[0-9]{3}$",var),                                          # yyyy
                as.character(as.Date(paste0(unk_day,".",unk_month,".",var),format = "%d.%m.%Y")),
                var)
  var <- ifelse(grepl("^[0-9]{2}$",var),                                                  # yy
                as.character(as.Date(paste0(unk_day,".",unk_month,".",unk_cent,var),format = "%d.%m.%Y")),
                var)

  # if only month and year was entered, append accordingly ("01.2011" -> "01.01.2011")
  var <- ifelse(grepl("^[1-9]{1}\\.[1-2]{1}[0-9]{3}$",var) |                              # m.yyyy
                  grepl("^[0-1]{1}[0-2]{1}\\.[1-2]{1}[0-9]{3}$",var),                     # mm.yyyy
                as.character(as.Date(paste0(unk_day,".",var),format = "%d.%m.%Y")),
                var)
  var <- ifelse(grepl("^[1-9]{1}\\.[0-9]{2}",var) |                                       # m.yy
                  grepl("^[0-1]{1}[0-2]{1}\\.[0-9]{2}$",var),                             # mm.yy
                as.character(as.Date(paste0(unk_day,".",var),format = "%d.%m.%y")),
                var)

  # if regular date was entered
  var <- ifelse(grepl("^[1-9]{1}\\.[1-9]{1}\\.[1-2]{1}[0-9]{3}$",var) |                   # d.m.yyyy
                  grepl("^[1-9]{1}\\.[0-1]{1}[0-2]{1}\\.[1-2]{1}[0-9]{3}$",var) |         # d.mm.yyyy
                  grepl("^[0-3]{1}[0-9]{1}\\.[1-9]{1}\\.[1-2]{1}[0-9]{3}$",var) |         # dd.m.yyyy
                  grepl("^[0-3]{1}[0-9]{1}\\.[0-1]{1}[0-2]{1}\\.[1-2]{1}[0-9]{3}$",var),  # dd.mm.yyyy
                as.character(as.Date(var,format = "%d.%m.%Y")),
                var)
  var <- ifelse(grepl("^[1-9]{1}\\.[1-9]{1}\\.[0-9]{2}$",var) |                           # d.m.yy
                  grepl("^[1-9]{1}\\.[0-1]{1}[0-2]{1}\\.[0-9]{2}$",var) |                 # d.mm.yy
                  grepl("^[0-3]{1}[0-9]{1}\\.[1-9]{1}\\.[0-9]{2}$",var) |                 # dd.m.yy
                  grepl("^[0-3]{1}[0-9]{1}\\.[0-1]{1}[0-2]{1}\\.[0-9]{2}$",var),          # dd.mm.yy
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
