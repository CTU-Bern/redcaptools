#' REDCap Date Conversion
#'
#' This function prepares date values in a data table for import in REDCap.
#' Dates in Excel can be entered in a variety of formats. This function attempts
#' to account for the most common ways dates may have been entered and converts
#' them into a format compatible with REDCap.
#'
#' @param var Variable to convert
#' @param unk_day Day to use if unknown, i.e. if only the year or only the month
#'   + year is found. The default is 01 (2022 -> 2022-01-01).
#' @param unk_month Month to use if unknown, i.e. if only the year is found. The
#'   default is 01 (2022 -> 2022-01-01).
#' @param format Date format to be used: "european" (DMY) or "american" (MDY).
#'   Dates that match both formats will be converted accordingly. Default =
#'   "european".
#'
#'
#' @return converted variable
#' @export
#' @importFrom dplyr case_when mutate pull
#' @importFrom lubridate dmy mdy ymd

#'
#' @examples
#' var <-c("01.12.2022", "12.2022", "2022", "01/12/2022", "31341")
#' redcap_import_dates(var)


redcap_import_dates <- function(var,
                         unk_day = "01",
                         unk_month = "01",
                         format = "european") {

  input <- output <- NULL


  df_out <- data.frame(input = as.character(var))

  df_out <- df_out |>
    mutate(
      # sometimes entries are separated with / or -
      # will be replaced with .
      modified = gsub("[/-]",".",input),



      # European format ----

      euro = case_when(
        grepl("^[1-9]{1}\\.[1-9]{1}\\.[1-2]{1}[0-9]{3}$",modified) |                   # d.m.yyyy
          grepl("^[1-9]{1}\\.[0-1]{1}[0-9]{1}\\.[1-2]{1}[0-9]{3}$",modified) |         # d.mm.yyyy
          grepl("^[0-3]{1}[0-9]{1}\\.[1-9]{1}\\.[1-2]{1}[0-9]{3}$",modified) |         # dd.m.yyyy
          grepl("^[0-3]{1}[0-9]{1}\\.[0-1]{1}[0-9]{1}\\.[1-2]{1}[0-9]{3}$",modified) | # dd.mm.yyyy
          grepl("^[1-9]{1}\\.[1-9]{1}\\.[0-9]{2}$",modified) |                         # d.m.yy
          grepl("^[1-9]{1}\\.[0-1]{1}[0-9]{1}\\.[0-9]{2}$",modified) |                 # d.mm.yy
          grepl("^[0-3]{1}[0-9]{1}\\.[1-9]{1}\\.[0-9]{2}$",modified) |                 # dd.m.yy
          grepl("^[0-3]{1}[0-9]{1}\\.[0-1]{1}[0-9]{1}\\.[0-9]{2}$",modified)           # dd.mm.yy
        ~ suppressWarnings(dmy(modified))
      ),



      # American format ----

      ami = case_when(
        grepl("^[1-9]{1}\\.[1-9]{1}\\.[1-2]{1}[0-9]{3}$",modified) |                   # m.d.yyyy
          grepl("^[0-1]{1}[0-9]{1}\\.[1-9]{1}\\.[1-2]{1}[0-9]{3}$",modified) |         # mm.d.yyyy
          grepl("^[1-9]{1}\\.[0-3]{1}[0-9]{1}\\.[1-2]{1}[0-9]{3}$",modified) |         # m.dd.yyyy
          grepl("^[0-1]{1}[0-9]{1}\\.[0-3]{1}[0-9]{1}\\.[1-2]{1}[0-9]{3}$",modified) | # mm.dd.yyyy
          grepl("^[1-9]{1}\\.[1-9]{1}\\.[0-9]{2}$",modified) |                         # m.d.yy
          grepl("^[0-1]{1}[0-9]{1}\\.[1-9]{1}\\.[0-9]{2}$",modified) |                 # mm.d.yy
          grepl("^[1-9]{1}\\.[0-3]{1}[0-9]{1}\\.[0-9]{2}$",modified) |                 # m.dd.yy
          grepl("^[0-1]{1}[0-9]{1}\\.[0-3]{1}[0-9]{1}\\.[0-9]{2}$",modified)           # mm.dd.yy
        ~ suppressWarnings(mdy(modified))
      ),



      output = case_when(


        # append dates ----

        # if only year was entered, append accordingly
        # ("2011" -> "01.01.2011")
        # ("11" -> 01.01.2011")

        grepl("^[1-2]{1}[0-9]{3}$",modified) |                                     # yyyy
          grepl("^[0-9]{2}$",modified)                                             # yy
        ~ suppressWarnings(dmy(paste0(unk_day,".",unk_month,".",modified))),


        # if only month and year was entered, append accordingly
        # ("01.2011" -> "01.01.2011")
        grepl("^[1-9]{1}\\.[1-2]{1}[0-9]{3}$",modified) |                          # m.yyyy
          grepl("^[0-1]{1}[0-2]{1}\\.[1-2]{1}[0-9]{3}$",modified) |                # mm.yyyy
          grepl("^[1-9]{1}\\.[0-9]{2}",modified) |                                 # m.yy
          grepl("^[0-1]{1}[0-2]{1}\\.[0-9]{2}$",modified)                          # mm.yy
        ~ suppressWarnings(dmy(paste0(unk_day,".",modified))),


        # other formats ----

        # date entered without separator
        grepl("^[1-2]{1}[0-9]{3}[0-1]{1}[0-9]{1}[0-3]{1}[0-9]{1}$",modified)          # yyyymmdd
        ~ suppressWarnings(ymd(modified)),
        grepl("^[0-3]{1}[0-9]{1}[0-1]{1}[0-9]{1}[1-2]{1}[0-9]{3}$",modified)          # ddmmyyyy
        ~ suppressWarnings(dmy(modified)),

        # Excel format Excel date system counts days starting from January 1,
        # 1900 (or December 30, 1899, depending on the software version).Each
        # whole number represents a day. For example, 1 corresponds to January
        # 1, 1900, 2 corresponds to January 2, 1900, and so on.
        grepl("^\\d{4,5}$",modified) & suppressWarnings(as.numeric(modified)) <= 50000
        ~ suppressWarnings(as.Date(as.numeric(modified), origin = "1899-12-30")),

      # date was already in data format (yyyy-mm-dd)
      grepl("^[1-2]{1}[0-9]{3}\\-[0-1]{1}[0-9]{1}\\-[0-3]{1}[0-9]{1}$",input)
      ~ suppressWarnings(ymd(input))

      ),


      # decide which format ----

      output = case_when(
        (!is.na(euro) & is.na(ami)) |
          (!is.na(euro) & !is.na(ami) & format == "european")
        ~ euro,
        (is.na(euro) & !is.na(ami)) |
          (!is.na(euro) & !is.na(ami) & format == "american")
        ~ ami,


        TRUE ~ output

      )
    ) |>
    pull(output)



  return(df_out)


}




#' REDCap Time Conversion
#'
#' This function prepares time values in a data table for import in REDCap. In
#' Excel, time values can be entered in various formats. This function attempts
#' for the most common ways time values may have been entered and converts them
#' into a format compatible with REDCap.
#'
#' @param var Variable to convert
#' @param unk_min Minutes to use if unknown. The default is 00 (11 -> 11:00).
#' @param unk_sec Seconds to use if unknown. The default is 00 (11:11 ->
#'   11:11:00).
#'
#'
#' @return converted variable
#' @export
#' @importFrom dplyr case_when mutate if_else pull select
#' @importFrom stringr str_detect
#' @importFrom tidyr separate

#'
#' @examples
#' var <-c("11:11:00","11:11","11","0.34375" )
#' redcap_import_times(var)


redcap_import_times <- function(var,
                                unk_min = "00",
                                unk_sec = "00") {

  excel_time <- excel_hours <- excel_minutes <- excel_seconds <- input <- modified <- output <- NULL


  df_out <- data.frame(input = as.character(var)) |>

    # Excel time values ----

    # Excels time system is count as decimals: 0.0 represents midnight
    # (00:00:00), 0.5 represents 12:00 PM (noon), 1.0 represents the next day at
    # midnight (i.e., 24 hours later)

    # convert to hours_minutes_seconds if number starts with a 0, followed by a
    # decimal point, followed by one or more numbers after the decimal point

    mutate(excel_time = suppressWarnings(if_else(str_detect(var, "^0\\.\\d+$"),as.numeric(var) * 24,NA)),
           excel_hours = floor(excel_time),
           excel_minutes = floor((excel_time - excel_hours) * 60),
           excel_seconds = round((((excel_time - excel_hours) * 60) - excel_minutes) * 60)
    ) |>
    mutate(input = if_else(!is.na(excel_time),sprintf("%02d:%02d:%02d", excel_hours, excel_minutes, excel_seconds),input)) |>
    select(input) |>


    # reformat ----
    mutate(modified = case_when(

      # sometimes entries are separated with / or - or .
      # will be replaced with ':'
      grepl("[/.-]",input) ~ gsub("[/.-]",":",input),

      # sometime ':' is missing
      grepl("^[0-2]{1}[0-9]{1}[0-5]{1}[0-9]{1}$",input) ~ gsub("(\\d{2})(\\d{2})", "\\1:\\2", input),
      grepl("^[0-2]{1}[0-9]{1}[0-5]{1}[0-9]{1}[0-5]{1}[0-9]{1}$",input) ~ gsub("(\\d{2})(\\d{2})(\\d{2})", "\\1:\\2:\\3", input),
      TRUE ~ input
    )) |>


    # conversion ----
    separate(modified, into = c("h", "m","s"), sep = ":", fill = "right") |>
    mutate(
      h = case_when(
        grepl("^[0-9]$",h) ~ paste0("0",h),
        is.na(as.numeric(h)) ~ NA,
        as.numeric(h) > 24 ~ NA,
        TRUE ~ h),
      m = case_when(
        grepl("^[0-9]$",m) ~ paste0("0",m),
        as.numeric(m) > 60 ~ NA,
        !is.na(h) & is.na(m) ~ as.character(unk_min),
        TRUE ~ m),
      s = case_when(
        grepl("^[0-9]$",s) ~ paste0("0",s),
        as.numeric(s) > 60 ~ NA,
        !is.na(h) & is.na(s) ~ as.character(unk_sec),
        TRUE ~ s),
      output = case_when(
        !is.na(h) & !is.na(m) & !is.na(s) ~ paste0(h,":",m,":",s),
        !is.na(h) & !is.na(m) & is.na(s) ~ paste0(h,":",m),
      )
    ) |>
    pull(output)

  return(df_out)

}


#' REDCap Date-Time Conversion
#'
#' This function prepares date-time values in a data table for import in REDCap.
#' It parses date and time values and processes them using
#' \code{redcap_import_dates} and \code{redcap_import_times}. This ensures that
#' date-time entries, which may have been entered in various formats in Excel,
#' are converted into a format compatible with REDCap.
#'
#' @param var Variable to convert
#' @param args_rc_dates List with arguments for \code{redcap_import_dates} (e.g.
#'   args_rc_dates = list(unk_day = 01,format = "american"))
#' @param args_rc_times List with arguments for \code{redcap_import_times} (e.g.
#'   args_rc_times = list(unk_min = 01,unk_sec = 01))
#' @param date_only If TRUE, only the date will be included in the output and
#'   the time value will be removed. Default = FALSE.
#'
#' @return converted variable
#' @export
#' @importFrom dplyr case_when mutate pull
#' @importFrom stringr str_detect str_split
#' @importFrom tidyr separate
#'
#' @examples
#' var <-c("1.2.24 11:11:00","1.2.22 11:11","1.2.24 11", "31341.34375")
#' redcap_import_datetime(var)
#'

redcap_import_datetime <- function(var,
                                   args_rc_dates = list(),
                                   args_rc_times = list(),
                                   date_only = FALSE) {

  input <- Date <- Time <- out <- NULL

  df_out <- data.frame(input = as.character(var)) |>

    # Excel date-time values:
    #
    # Excel date system counts days starting from January 1, 1900 (or December
    # 30, 1899, depending on the software version).Each whole number represents
    # a day. For example, 1 corresponds to January 1, 1900, 2 corresponds to
    # January 2, 1900, and so on. Excels time system is count as decimals: 0.0
    # represents midnight (00:00:00), 0.5 represents 12:00 PM (noon), 1.0
    # represents the next day at midnight (i.e., 24 hours later). To discover
    # them, separate them if it is number starting with four or more digits
    # (^\\d{4,}), followed by a decimal point (\\.), ending with at least one
    # digit after the decimal point (\\d+$)
    mutate(input = ifelse(str_detect(input, "^\\d{4,5}\\.\\d+$"),paste0(str_split(input, "\\.", simplify = TRUE)[,1]," 0.",str_split(input, "\\.", simplify = TRUE)[,2]),input)) |>

    # separate date and time
    separate(input, into = c("Date", "Time"), sep = "\\s+", fill = "right") |>


    # run redcap_import_dates and redcap_import_times on date/time values
    mutate(Date = do.call(redcap_import_dates,c(list(Date),args_rc_dates)),
           Time = do.call(redcap_import_times,c(list(Time),args_rc_times)),

           # prepare output
           out = case_when(
             # only date
             !is.na(Date) & date_only ~ as.character(Date),
             # date and time separated by " "
             !is.na(Date) & !is.na(Time) & !date_only~ paste0(Date," ",Time),
             # append midnight if no time value found
             !is.na(Date) & is.na(Time) & !date_only~ paste0(as.character(Date)," 00:00:00"),
           )
    ) |>
    pull(out)

  return(df_out)
}




