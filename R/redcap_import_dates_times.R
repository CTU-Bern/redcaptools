#' REDCap Date Conversion
#'
#' This function is used to prepare dates in a data table for import in REDCap.
#' In an Excel, dates can be entered in various shapes and forms. This function
#' tries to take most possibilities how dates could have been entered into
#' account and converts them to a format that can be imported into REDCap.
#'
#' @param var variable to convert
#' @param unk_day Day to use if unknown, i.e. if only the year or only the month
#'   + year is found. The default is 01 (2022 -> 2022-01-01).
#' @param unk_month Month to use if unknown, i.e. if only the year is found. The
#'   default is 01 (2022 -> 2022-01-01).
#' @param format Date format to be used: "European" (DMY) or "American" (MDY).
#'   Dates that match both formats will be converted accordingly.
#'
#'
#' @return converted variable
#' @export
#' @importFrom dplyr case_when
#' @importFrom lubridate dmy mdy ymd

#'
#' @examples
#' var <-c("01.12.2022", "12.2022", "2022", "01/12/2022")
#' redcap_import_dates(var)


redcap_import_dates <- function(var,
                         unk_day = "01",
                         unk_month = "01",
                         format = "european") {



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

        # Excel format
        grepl("^[0-9]{5}$",modified)
        ~ suppressWarnings(as.Date(as.numeric(modified), origin = "1899-12-30"))
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
#' This function is used to prepare time-values in a data table for import in REDCap.
#' In an Excel, time-values can be entered in various shapes and forms. This function
#' tries to take most possibilities how time-values could have been entered into
#' account and converts them to a format that can be imported into REDCap.
#'
#' @param var variable to convert
#' @param unk_min Minutes to use if unknown. The default is 00 (11 -> 11:00).
#' @param unk_sec Seconds to use if unknown. The default is 00 (11:11 -> 11:11:00).
#'
#'
#' @return converted variable
#' @export
#' @importFrom dplyr case_when mutate
#' @importFrom tidyr separate

#'
#' @examples
#' var <-c("11:11:00","11:11","11")
#' redcap_import_times(var)


redcap_import_times <- function(var,
                                unk_min = "00",
                                unk_sec = "00") {


  df_out <- data.frame(input = as.character(var)) |>

    mutate(modified = case_when(
      # sometimes entries are separated with / or -
      # will be replaced with ':'
      grepl("[/-]",input) ~ gsub("[/-]",":",input),

      # missing ':'
      grepl("^[0-2]{1}[0-9]{1}[0-5]{1}[0-9]{1}$",input) ~ gsub("(\\d{2})(\\d{2})", "\\1:\\2", input),
      grepl("^[0-2]{1}[0-9]{1}[0-5]{1}[0-9]{1}[0-5]{1}[0-9]{1}$",input) ~ gsub("(\\d{2})(\\d{2})(\\d{2})", "\\1:\\2:\\3", input),
      TRUE ~ input
    )) |>

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
#' This function is used to prepare date-time-values in a data table for import
#' in REDCap by parsing date and time values and running \code{redcap_import_dates} and
#' \code{redcap_import_times}
#'
#' @param var variable to convert
#'
#' @return converted variable
#' @export
#'
#' @examples
#' var <-c("11:11:00","11:11","11")
#' redcap_import_times(var)
#'

redcap_import_datetime <- function(var,
                                   args_rc_dates = list(),
                                   args_rc_times = list()) {

  converted_var <- data.frame(var) |>
    separate(var, into = c("Date", "Time"), sep = "\\s+", fill = "right") |>
    mutate(Date = do.call(redcap_import_dates,c(list(Date),args_rc_dates)),
           Time = do.call(redcap_import_times,c(list(Time),args_rc_times)),
           out = case_when(
             !is.na(Date) & !is.na(Time) ~ paste0(Date," ",Time),
             !is.na(Date) & is.na(Time) ~ paste0(as.character(Date)," 00:00:00"),
           )
    ) |>
    pull(out)

  return(converted_var)
}




