#'REDCap Recode
#'
#'This function loops through all the variables of a data set and compares them
#'with the field type and validation of the variables as set up in REDCap. \cr
#'The REDCap data dictionary can either be directly provided or downloaded from
#'the REDCap project by providing an API token and matching URL. \cr Variables
#'can be converted automatically or manually to match the type and validation in
#'REDCap. \cr The script will then summarize all values that do not match the
#'expected format and will look for values that could potentially indicate
#'missing values (such as 'missing', 'excluded',...). \cr In a second step,
#'these values can be recoded automatically or manually if missing data codes
#'have defined in REDCap Additional Customizations (or simply set to NA). \cr If
#'a variable has been converted to a factor (e.g., radio button field), the
#'recoding is (additionally) prompted for all factor levels. \cr The function
#'returns a data frame with the recoded variables, writes an overview csv-table,
#'and the executed code to a txt-file for copy-pasting and adjusting/reusing.
#'\cr It is advised to use \code{redcap_import_select} on the data first, before
#'running this function.
#'
#'
#'@param selected_data Data to be recoded
#'@param dict Data dictionary (e.g. as downloaded from REDCap or via
#'  \code{redcap_export_meta(rc_token, rc_url)$meta}). If not supplied, this
#'  will be downloaded from the API using \code{rc_token} and \code{rc_token}.
#'@param missing_codes If a data dictionary is provided by the user, Missing
#'  Data Codes as defined in REDCap Additional Customizations can be provided
#'  here (if set up accordingly). The Missing Data Codes should be provided in a
#'  single string with [code] [label] separated by a comma and a pipe between
#'  the options (e.g., "-99, Missing | EXCL, Excluded | NA, not available"). If
#'  no data dictionary is provided, the codes will be downloaded from the API
#'  using \code{rc_token} and \code{rc_token} (if set up accordingly).
#'@param rc_token REDCap API token
#'@param rc_url Link to REDCap API
#'@param start_var Define in which column of the data the loop should start.
#'  Default = 1.
#'@param pot_miss The provided data is inspected for potential missing values
#'  that could be recoded. This is mainly helpful for text variables.
#'  Expressions can simply be defined in a character vector and a text-search is
#'  applied to search through the data. Default =
#'  c("miss","unknown","excluded","^0$","NA","N.A."). To disable this search set
#'  pot_miss to NULL.
#'@param if_empty Sets a default value for empty cells. This value can be
#'  changed for each variable when using manual recoding. Default = NA (meaning
#'  the cell remains empty).
#'@param auto_conv If TRUE, the variable will be auto-converted according to the
#'  best matching field type and validation in REDCap. If FALSE, the user can
#'  decide how the variable should be converted. If the option to continue is
#'  active (see below), this auto-conversion can be switched on and off while
#'  running the script. Default = TRUE.
#'@param auto_recode If TRUE, the values that need recoding will be auto-recoded
#'  by matching them with codes and labels as set up in REDCap. If FALSE, the
#'  user can decide to recode the values as suggested or to recode each value
#'  individually. Default = FALSE.
#'@param auto_recode_precision The values that need recoding are compared with
#'  codes and labels as set up in REDCap. With this numeric similarity index
#'  between 0 (no similarity at all = shows basically all code/labels as
#'  similar) and 1 (identical = shows only perfect codes/labels) the number of
#'  suggestions can be adjusted. If auto-recoding is switched off, this index
#'  can be adjusted while running the script. If multiple matches are found, the
#'  value will be set to NA. Default = 0.5.
#'@param skip_intro If TRUE, the introduction messages will be skipped. Default
#'  = FALSE
#'@param continue If TRUE, a question to continue will be asked before moving
#'  along the loop. Default = TRUE.
#'@param suppress_txt If TRUE, all text output will be suppressed when used with
#'  auto-conversion and auto-recoding. This is not recommended and should only
#'  be used for testing. Default = FALSE.
#'@param log If TRUE, an overview csv-table, and a txt-file are stored in the
#'  working directory. Default = TRUE.
#'@param log_code Name and location of the txt-file containing the executed
#'  code. Default = redcap_import_recode_code.txt.
#'@param log_table Name and location of the csv.table containing the tabular
#'  overview. Default = redcap_import_recode_overview.csv.
#'@param wait Allows you to set the latency time between the steps. Default =
#'  2s.
#'@param ... other parameters used for \code{redcap_import_dates} or
#'  \code{redcap_import_times}
#'
#'@return Data frame with recoded data. Log-file with executed code.
#'@export
#'@importFrom stringr str_split
#'@importFrom crayon bold underline blue red
#'@importFrom dplyr select mutate na_if across
#'@importFrom tidyselect everything
#'@importFrom utils str write.table
#'@importFrom knitr kable
#'@importFrom lubridate hm hms ms
#'
#' @examples
#' # data(importdemo_data)
#' # data(importdemo_dict)
#' # redcap_import_recode(importdemo_data, importdemo_dict)
#'
#' # if using local data:
#' # token <- "xxxxx"
#' # url <- "xxxxx"
#' # file <- "data.csv"
#' # redcap_import_recode(file, rc_token = token, rc_url = url)



redcap_import_recode <- function(selected_data,
                                 dict = NULL,
                                 missing_codes = NULL,
                                 rc_token,
                                 rc_url,
                                 start_var = 1,
                                 pot_miss = c("miss","unknown","excluded","^0$","NA","N.A."),
                                 if_empty = NA,
                                 auto_conv = TRUE,
                                 auto_recode = FALSE,
                                 auto_recode_precision = 0.5,
                                 skip_intro = FALSE,
                                 continue = TRUE,
                                 suppress_txt = FALSE,
                                 log = TRUE,
                                 log_code = 'redcap_import_recode_code.txt',
                                 log_table = 'redcap_import_recode_overview.csv',
                                 wait = 2,
                                 ...) {

  field_name <- field_type <- select_choices_or_calculations <- text_validation_type_or_show_slider_number <- field_label <- text_validation_min <- text_validation_max <- NULL

  # evaluate inputs ----

  # data
  check_data(selected_data)


  # dict

  if(is.null(dict)) {
    dict_downloaded <- TRUE
    check_token(rc_token)
    check_url(rc_url)

    meta <- redcap_export_meta(rc_token, rc_url)
    dict <- meta$meta
    missing_codes <- meta$project$missing_data_codes

  } else {
    dict_downloaded <- FALSE
  }

  check_dict(dict)

  # missing codes

  if (!is.null(missing_codes)) {
    if (!is.na(missing_codes)) {
      if(!is.character(missing_codes) | length(missing_codes) != 1) stop("missing_codes should be a single string")

      missing_choices_sep <- str_split(missing_codes, pattern = '\\|')[[1]]
      missing_codes_sep <- str_split(missing_choices_sep, pattern = "\\,",simplify = TRUE,n=2)[,1]
      missing_options_sep <- str_split(missing_choices_sep, pattern = "\\,",simplify = TRUE,n=2)[,2]
      missing_codes <- data.frame(code = missing_codes_sep,
                                  label = missing_options_sep) |>
        mutate(across(everything(), trimws),
               across(everything(), ~ na_if(.x, ""))
        )

      check_missing_codes(missing_codes)

    }
  }


  # other arguments

  if(length(start_var) != 1) {
    stop("start_var should be a single integer")
  } else if (start_var %% 1 != 0)  {
    stop("start_var should be a single integer")
  }
  if(!is.null(pot_miss)) if(!is.character(pot_miss)) stop("pot_miss should be a character vector")
  if(length(if_empty) != 1) {
    stop("if_empty should be a single string or NA")
  } else {
    if(!is.na(if_empty)) if(!is.character(if_empty)) stop("if_empty should be a single string or NA")
  }
  if(!is.logical(auto_conv)) stop("auto_conv should be logical (TRUE/FALSE)")
  if(!is.logical(auto_recode)) stop("auto_recode should be logical (TRUE/FALSE)")
  if(length(auto_recode_precision) != 1) {
    stop("auto_recode_precision should be a number between 0 and 1")
  } else {
    if(!is.numeric(auto_recode_precision) | auto_recode_precision < 0 | auto_recode_precision > 1) stop("auto_recode_precision should be a number between 0 and 1")
  }
  if(!is.logical(skip_intro)) stop("skip_intro should be logical (TRUE/FALSE)")
  if(!is.logical(continue)) stop("continue should be logical (TRUE/FALSE)")
  if(!is.logical(suppress_txt)) stop("suppress_txt should be logical (TRUE/FALSE)")
  if(!is.logical(log)) stop("log should be logical (TRUE/FALSE)")
  if(!is.character(log_code) || length(log_code) != 1 || !grepl("\\.txt$", log_code)) stop("please provide a valid path for txt-file")
  if(!is.character(log_table) || length(log_table) != 1 || !grepl("\\.csv$", log_table)) stop("please provide a valid path for csv-table")
  if(length(wait) != 1) {
    stop("wait should be a single integer")
  } else if (wait %% 1 != 0) {
    stop("wait should be a single integer")
  }


  # parse additional arguments for redcap_import_dates_times
  additional_args <- list(...)

  args_rc_dates <- additional_args[names(additional_args) %in% c("unk_day","unk_month","format")]
  log_rc_dates <- character()
  for (arg in names(args_rc_dates)) {
    log_rc_dates <- c(log_rc_dates,(paste0(arg," = '",args_rc_dates[[arg]],"'")))
  }

  args_rc_times <- additional_args[names(additional_args) %in% c("unk_min","unk_sec")]
  log_rc_times <- character()
  for (arg in names(args_rc_times)) {
    log_rc_times <- c(log_rc_times,(paste0(arg," = '",args_rc_times[[arg]],"'")))
  }


  # intro ----

  if (!skip_intro) {
    cat(underline("\nHello and welcome!\n"))
    cat("Let's start with some info about this script and your selections.\n")
    cat("It's best to use fullscreen while working with this script.\n")
    cat("To turn off this introduction, set 'skip_intro = TRUE'.\n\n\n")
    Sys.sleep(wait)

    cat(underline("What this script will do:\n"))
    cat("This script will loop through the variables in your data table and compares them with the field type and validation in a REDCap data dictionary.\n")
    cat("Variables can be converted to match the type and validation in REDCap. The script will then summarize all values that do not match the expected format. Furthermore, the script will look for values that could potentially indicate missing values (such as 'missing', 'excluded',...).\n")
    cat("In a second step, you will be prompted to recode these values, and you can do so automatically (by just accepting the suggestions provided with a similarity index) or manually.\n")
    cat("If a variable has been converted to a factor (e.g., radio button field), the recoding is (additionally) prompted for all factor levels.\n")
    cat("\n\n")


    intro_ans <- ""
    while (intro_ans != 1) {
      intro_ans <- readline(prompt="Type '1' to continue: ")
      if (intro_ans != 1) {
        cat("Please check your answer! (Type 'Esc' to cancel)")
      }
    }

    cat("\n\n")
    cat(underline("Let's have a look at your choices that you provided as function arguments:\n\n"))

    if(start_var != 1) {
      cat(bold("Start-Variable:\n"))
      cat(paste0("Your script will start at column: ",as.character(start_var),"\n"))
    }
    Sys.sleep(wait+1)

    cat("\n\n")
    cat(bold("Data Dictionary:\n"))
    if (dict_downloaded) {
      cat("No data dictionary has been provided!\n")
      cat("The dictionary will be downloaded from REDCap with the URL and token you have provided.\n")
      cat("Field types and validation will be read from this dictionary.\n")
      cat("If there are missing data codes defined in REDCap Additional Customizations, they will also be read from this dictionary.\n")
    } else {
      cat("A data dictionary has been provided.\n")
      cat("Variable names will be read from this dictionary.\n")
      cat("If there are missing data codes defined in REDCap Additional Customizations, please make sure to provide them accordingly.\n")
      cat("They should be provided in a single string with [code] [label] separated by a comma and a pipe between the options:\n")
      cat("E.g., missing_codes = '-99, Missing | EXCL, Excluded | NA, not available'\n")
      if (!is.null(missing_codes)) {
        cat(paste0("Currently you have provided the following missing codes: ",missing_codes,"\n"))
      } else {
        cat("You have currently not provided any missing codes.\n")
      }
    }
    Sys.sleep(wait+1)

    cat("\n\n")
    cat(bold("Auto-Conversion:\n"))
    if (auto_conv) {
      cat("Auto-Conversion has been turned on!\n")
      cat("The variables will be auto-converted according to the best matching field type and validation in REDCap.\n")
      cat("To turn it off, set auto_conv = FALSE.\n")
    } else {
      cat("Auto-Conversion has been turned off!\n")
      cat("You are prompted to decide how the variable should be converted.\n")
      cat("To turn it on, set auto_conv = TRUE.\n")
    }
    Sys.sleep(wait+1)

    cat("\n\n")
    cat(bold("Auto-Recoding:\n"))
    if (auto_recode) {
      cat("Auto-Recoding has been turned on!\n")
      cat("The values that need recoding will be auto-recoded by matching them with codes and labels as set up in REDCap.\n")
      cat("To turn it off, set auto_recode = FALSE.\n")
    } else {
      cat("Auto-Recoding has been turned off!\n")
      cat("You can decide to recode the values as suggested or to recode each value individually.\n")
      cat("To turn it on, set auto_recode = TRUE.\n")
    }
    cat("\n")
    cat(paste0("The similarity index for auto-recoding or recoding-suggestions has been set at: ",italic(auto_recode_precision),"\n"))
    cat("You can change it by providing a number between 0 (no similarity at all = shows basically all code/labels as similar) and 1 (identical = shows only perfect codes/labels) for auto_recode_precision.\n")
    cat("Please be careful with your selection as values with no or multiple matches will be set to NA.\n")
    Sys.sleep(wait+1)

    cat("\n\n")
    cat(bold("Continue:\n"))
    if (continue) {
      cat("Continue has been turned on.\n")
      cat("You will be given the chance to re-do the conversion/recoding before moving along the loop.\n")
      cat("Various arguments (such as auto_conv, auto_recode, auto_recode_precision, pot_miss) can be changed as well in this step.\n")
    } else {
      cat("Continue has been turned off.\n")
      cat("You won't be asked if you would like to re-do the conversion/recoding and the script will move on automatically.\n")
      cat("No arguments (such as auto_conv, auto_recode, auto_recode_precision, pot_miss) can be changed while running the function.\n")
    }
    Sys.sleep(wait+1)

    cat("\n\n")
    cat(bold("Potential Missings:\n"))
    cat("The provided data will be inspected for potential missing values according to the following patterns:\n")
    if (!is.null(pot_miss)) {
       cat(paste(pot_miss,collapse = ", "))
     } else {
       cat("(No check for potential missings has been defined!)")
     }
     cat("\nThis text-search can be changed by providing a character vector for pot_miss.\n")
     cat("E.g., pot_miss = c('miss','unknown','excluded','^0$','NA','N.A.')\n")
     cat("To disable the search for potential missings, set 'pot_miss = NULL'!\n")
     Sys.sleep(wait+1)

     cat("\n\n")
     cat(bold("Empty Cells:\n"))
     cat("The default value for empty cells has been set as:\n")
     if (is.na(if_empty)) {
       cat("<NA> (field will stay empty)")
     } else {
       cat(italic(if_empty))
     }
     cat("\nTo change it, provide a different value for if_empty (e.g., if_empty = 'N.A.')\n")
     Sys.sleep(wait+1)

     cat("\n\n\n")
     cat("There are other things that you can change when running the function.\n")
     cat("Feel free to have a look at the documentation.\n\n\n")


     intro_ans <- ""
     while (intro_ans != 1) {
       intro_ans <- readline(prompt="Type '1' to continue: ")
       if (intro_ans != 1) {
         cat("Please check your answer! (Type 'Esc' to cancel)")
       }
     }

     cat("\n\n")
     cat(bold("WHEN MOVING ALONG THE LOOP, PLEASE BE CAREFUL WITH YOUR CHOICES AS IT IS NOT POSSIBLE TO GO BACK!!\n\n"))
     cat("You can press 'Esc' any time to stop the function but it is advised to finish the loop properly! This can be done by either typing 'exit' in the prompt or by looping through all the variables. It makes sure that the executed code is properly written in the log-file and can be copy-pasted into your R-script and adjusted manually at a later time.\n\n")


    cat("Are you ready to begin? \n 1 = YES\n'esc' = STOP")
    intro_ans <- ""
    while (intro_ans != 1) {
      intro_ans <- readline(prompt="Answer: ")
      if (intro_ans != 1) {
        cat("Please check your answer! \n 1 = YES\n'esc' = STOP")
        intor_ans <- ""
      }
    }

    cat("\nGreat! Let's begin!\n")
    cat("\n--------------------------------------------------------------------------\n\n")
    Sys.sleep(wait)
  }


  # open log-files ----
  if(log) {
    write.table(paste0("\n\n",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),":\n\nrecoded_data <- mutate(",deparse(substitute(selected_data))), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("\n, across(everything(), as.character)"), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table(paste0("\n\n",format(Sys.time(), "%Y-%m-%d %H:%M:%S")), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(",Variable,Old Coding,New Coding\n\n", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  }


  # read data & dict, prepare output variables ----

  name_vars <- colnames(selected_data)

  rc_spec <- dict |>
    select(field_name,
           field_label,
           field_type,
           choices = select_choices_or_calculations,
           validation = text_validation_type_or_show_slider_number,
           min = text_validation_min,
           max = text_validation_max)

  vars_recode <- list()




  # start variable for-loop ----

  for (i in seq_along(name_vars)[start_var:ncol(selected_data)]) {

    # initiate option to go back with a while loop
    goback = TRUE

    # in order to directly exit the variable for-loop or move to the next variable, additional options are needed (for_break and for_skip)
    # they can be set to TRUE during the while-loop, 'break' is then used to exit the while-loop, and the options are evaluated in the variable for-loop
    # (for_break will cause the variable for-loop to exit, for_skip will cause the variable for-loop to move to the next variable)
    for_break = FALSE
    for_skip = FALSE

    # later in the script, the user can manually change expressions that will be used to search for missings for one variable only
    # this option makes sure that only the part about changing expressions is repeated in the while-loop
    change_pot_miss <- FALSE

    # the same goes for rounding of numeric values (but there the conversion needs to happen again)
    change_round <- FALSE



    # start conversion while-loop ----
    while (goback) {

      ## summarize data and RC dict ----

      var <- selected_data[,i]
      name <- name_vars[i]


      if (!change_pot_miss) { # this part will be skipped if only the missing terms are adjusted

        if(!change_round) {

          if (!suppress_txt) cat(paste0("\n\nVariable: ",blue(bold(underline(name))),"\n\n"))
          Sys.sleep(wait)

          if (!suppress_txt) cat("--------------------------------- REDCap ---------------------------------")

          # var not in redcap:
          if (!any(rc_spec$field_name == name_vars[i])) {

            if (!suppress_txt) {
              cat("\n\nThis variable is NOT part of the data dictionary you provided and will be skipped.\n")
              cat("It will remain in the data but will not be changed.")
              cat("\n\n")
              cat("-------------------------------------------------------------------------")
            }
            # 'break' exits the conversion while-loop here
            # for_skip will be set to TRUE which will cause the variable for-loop to move to the next variable below
            # (see "end conversion while-loop")
            for_skip = TRUE
            break

          }


          # var in redcap:

          if (!suppress_txt) cat("\n\nVariable found in REDCap!")
          rc_label <- rc_spec$field_label[which(rc_spec$field_name == name)]
          rc_type <- rc_spec$field_type[which(rc_spec$field_name == name)]
          rc_val <- rc_spec$validation[which(rc_spec$field_name == name)]
          rc_choices <- rc_spec$choices[which(rc_spec$field_name == name)]

          if (!suppress_txt) {
            cat(paste0("\n\nVariable Label: ",italic(rc_label)))
            cat(paste0("\nField Type: ", italic(rc_type)))
            if (rc_type == "text") cat(paste0("\nField Validation: ",italic(rc_val)))
            cat("\n\n\n")
          }
          Sys.sleep(wait)

          if (!suppress_txt) {
            cat("------------------------------- Data File -------------------------------")
            cat("\n\nData Summary:\n")
            print(summary(var))
            cat(str(var))
            cat("\n\n")
            cat("-------------------------------------------------------------------------")
          }
          Sys.sleep(wait)



          # auto conversion ----
          conv_to <- ''

          # character
          if ((rc_type == "text" & is.na(rc_val)) |
              rc_type == "notes") {
            conv_to <- "txt"
            conv_label <- "Unvalidated Text"

            # factor
          } else if (rc_type == "truefalse" |
                     rc_type == "yesno" |
                     rc_type == "dropdown" |
                     rc_type == "radio") {
            conv_to <- "sc"
            conv_label <- "Single-Choice"

            # integer
          } else if ((rc_type == "text" & !is.na(rc_val) & rc_val == "integer")) {
            conv_to <- "int"
            conv_label <- "Integer"

            # numeric
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val == "number") {
            conv_to <- "num"
            conv_label <- "Number (not further specified)"

            # number 1DP
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val == "number_1dp") {
            conv_to <- "num1"
            conv_label <- "Number with 1 decimal place"

            # number 2DP
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val == "number_2dp") {
            conv_to <- "num2"
            conv_label <- "Number with 2 decimal places"

            # number 3DP
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val == "number_3dp") {
            conv_to <- "num3"
            conv_label <- "Number with 3 decimal places"

            # number 4DP
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val == "number_4dp") {
            conv_to <- "num4"
            conv_label <- "Number with 4 decimal places"

            # date
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val %in% c("date_dmy","date_ymd")) {
            conv_to <- "dt"
            conv_label <- "Date"

            # datetime
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val %in% c("datetime_dmy","datetime_ymd","datetime_seconds_dmy","datetime_seconds_ymd")) {
            conv_to <- "dt_tm"
            conv_label <- "Date-Time"

            # time_hms
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val %in% c("time_hh_mm_ss","time_seconds_hms")) {
            conv_to <- "time_hms"
            conv_label <- "Time (HH:MM:SS)"

            # time_hm
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val == "time") {
            conv_to <- "time_hm"
            conv_label <- "Time (HH:MM)"

            # time_ms
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val == "time_mm_ss") {
            conv_to <- "time_ms"
            conv_label <- "Time (MM:SS)"

            # email
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val == "email") {
            conv_to <- "email"
            conv_label <- "Email"

            # letter
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val == "alpha_only") {
            conv_to <- "letter"
            conv_label <- "Letter only"

            # phone
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val == "phone_ch") {
            conv_to <- "phone"
            conv_label <- "Phone CH"

            # pseudonym
          } else if (rc_type == "text" & !is.na(rc_val) & rc_val == "pseudonym") {
            conv_to <- "pseudo"
            conv_label <- "Pseudonym"

            # slider
          } else if (rc_type == "slider") {
              conv_to <- "slider"
            conv_label <- "Slider Variable"
            # currently min/max are hard checked during import, so they need to be included
            # if empty, they are the defaults from 0 to 100
            rc_min <- rc_spec$min[which(rc_spec$field_name == name)]
            if (is.na(rc_min)) rc_min <- 0
            rc_max <- rc_spec$max[which(rc_spec$field_name == name)]
            if (is.na(rc_max)) rc_max <- 100

            # calc
          } else if (rc_type == "calc") {
            if (!suppress_txt) {
              cat("\n\n")
              cat(bold(underline("Note:"),"\nValues for calculations will not be imported by REDCap!\nThey will be calculated automatically during the import process."))
              cat("\n-------------------------------------------------------------------------\n\n")
            }
            for_skip = TRUE
            break

            # checkbox
          } else if (rc_type == "checkbox") {
            if (!suppress_txt) {
              cat("\n\n")
              cat(bold(underline("Note:"),"\nCheckbox variable will be skipped as it has to be split into multiple variables.\nThis will be done in a separate step!"))
              cat("\n\n-------------------------------------------------------------------------\n\n")
            }
            for_skip = TRUE
            break

            # not defined
          } else {
            if (!suppress_txt) {
              cat("\n\nVariable Type/Validation not yet supported in this script!")
              cat("\n\n-------------------------------------------------------------------------\n\n")
            }
            for_skip = TRUE
            break
          }



          if (!suppress_txt) {
            cat(bold(paste0("\n\nSuggested Conversion to: ",italic(conv_label))))
            cat("\n\n\n")
          }
          Sys.sleep(wait)


          # auto or manual conversion? ----

          if (auto_conv) {
            if (!suppress_txt) cat(paste0(bold(underline("NOTE:")," Auto-conversion is active!\n(To turn it off, set 'auto_conv = FALSE' or type 'off' when prompted to continue.)\n\n\n")))
            Sys.sleep(wait)

          } else {

            ## INPUT ----
            cat("Would you like to convert the variable as suggested?")
            cat("\n 1 = YES (convert as suggested)")
            cat("\n 0 = NO (convert manually)")
            cat("\n 'skip' = skip this variable")
            cat("\n 'exit' = stop loop, exit code")
            manconv_ans <- ""

            while (manconv_ans != '1' &
                   manconv_ans != '0' &
                   manconv_ans != 'skip' &
                   manconv_ans != 'exit') {

              manconv_ans <- readline(prompt="Answer: ")

              if (manconv_ans != '1' &
                  manconv_ans != '0' &
                  manconv_ans != 'skip' &
                  manconv_ans != 'exit') {

                cat("Please check your answer!")
                cat("\n 1 = YES (convert as suggested)")
                cat("\n 0 = NO (convert manually)")
                cat("\n 'skip' = skip this variable")
                cat("\n 'exit' = stop loop, exit code")
                manconv_ans <- ""
              }
            }

            # exit:
            # break conversion while-loop here
            # for_break will be set to TRUE which will break the variable for-loop below and exit the code
            # (see "end conversion while-loop")

            if (manconv_ans == 'exit') {
              for_break = TRUE
              break
            }

            # skip:
            # break conversion while-loop here
            # for_skip will be set to TRUE which will cause the variable for-loop to move to the next variable below
            # (see "end conversion while-loop")

            if (manconv_ans == "skip") {
              for_skip = TRUE
              break
            }



            # manual conversion ----
            if (manconv_ans == "0") {

              ### INPUT ----
              cat("\nHow would you like to convert this variable?")
              manual_choices <- data.frame(conv_to = c('txt',
                                                       'sc',
                                                       'int',
                                                       'num',
                                                       'num1',
                                                       'num2',
                                                       'num3',
                                                       'num4',
                                                       'dt',
                                                       'dt_tm',
                                                       'time_hms',
                                                       'time_hm',
                                                       'time_ms',
                                                       'email',
                                                       'letter',
                                                       'phone',
                                                       'pseudo',
                                                       'slider',
                                                       'skip',
                                                       'exit'),
                                           conv_label = c("unvalidated text",
                                                          "single-choice (radiobutton, dropdown)",
                                                          "integer",
                                                          "number (not further specified)",
                                                          "number with 1 decimal place",
                                                          "number with 2 decimal places",
                                                          "number with 3 decimal places",
                                                          "number with 4 decimal places",
                                                          "date",
                                                          "datetime",
                                                          "time (HH:MM:SS)",
                                                          "time (HH:MM)",
                                                          "time (MM:SS)",
                                                          "email",
                                                          "letters only",
                                                          "Swiss phone number",
                                                          "pseudonym",
                                                          "Slider Variable",
                                                          "do NOT convert and move to next item",
                                                          "do NOT convert and stop loop"))

              print(kable(manual_choices,col.names = c("Options:","Description:")))
              conv_to <- ""

              while (conv_to != 'txt' &
                     conv_to != 'sc' &
                     conv_to != 'int' &
                     conv_to != 'num' &
                     conv_to != 'num1' &
                     conv_to != 'num2' &
                     conv_to != 'num3' &
                     conv_to != 'num4' &
                     conv_to != 'dt' &
                     conv_to != 'dt_tm' &
                     conv_to != 'time_hms' &
                     conv_to != 'time_hm' &
                     conv_to != 'time_ms' &
                     conv_to != 'email' &
                     conv_to != 'letter' &
                     conv_to != 'phone' &
                     conv_to != 'pseudo' &
                     conv_to != 'slider' &
                     conv_to != 'skip' &
                     conv_to != 'exit') {

                conv_to <- readline(prompt="Answer: ")

                if (conv_to != 'txt' &
                    conv_to != 'sc' &
                    conv_to != 'int' &
                    conv_to != 'num' &
                    conv_to != 'num1' &
                    conv_to != 'num2' &
                    conv_to != 'num3' &
                    conv_to != 'num4' &
                    conv_to != 'dt' &
                    conv_to != 'dt_tm' &
                    conv_to != 'time_hms' &
                    conv_to != 'time_hm' &
                    conv_to != 'time_ms' &
                    conv_to != 'email' &
                    conv_to != 'letter' &
                    conv_to != 'phone' &
                    conv_to != 'pseudo' &
                    conv_to != 'slider' &
                    conv_to != 'skip' &
                    conv_to != 'exit') {

                  cat("\nPlease check your answer!")
                  print(kable(manual_choices,col.names = c("Options:","Description:")))
                  conv_to <- ""
                }
              }
            } # end if (manconv_ans == 0)
          } # end if (auto_conv == FALSE)


          # exit:
          # break conversion while-loop here
          # for_break will be set to TRUE which will break the variable for-loop below and exit the code
          # (see "end conversion while-loop")

          if (conv_to == 'exit') {
            for_break = TRUE
            break
          }

          # skip:
          # break conversion while-loop here
          # for_skip will be set to TRUE which will cause the variable for-loop to move to the next variable below
          # (see "end conversion while-loop")

          if (conv_to == 'skip') {
            for_skip = TRUE
            break
          }

        } # # end if (!change_round), this part will be skipped if only the numeric values should be rounded




        # CONVERSIONS ----
        # Note:
        # this is mainly about providing a new summary, potential missings, and all values that do not match the chosen format
        # the actual "conversion" is done later after the recoding (see WHAT TO DO)
        # only values that are not recoded (i.e., are not potential missings and match the format) will be converted accordingly

        # suppress warnings as conversion would generate many warnings that will be shown in the very end
        suppressWarnings({

          ## unvalidated text ----
          if (conv_to == 'txt') {
            converted_var <- as.character(var)
            log_default <- paste0("as.character(",name,")")


            ## factor ----
          } else if (conv_to == 'sc') {
            converted_var <- as.factor(var)
            log_default <- paste0("as.character(",name,")")


            ## integer ----
          } else if (conv_to == 'int') {

            if (change_round) {
              converted_var <- if_else(round(as.numeric(var)) %% 1 == 0,as.integer(var),NA)
              log_default <- paste0("as.character(as.integer(round(as.numeric(",name,"))))")

            } else {
              converted_var <- if_else(as.numeric(var) %% 1 == 0,as.integer(var),NA)
              log_default <- paste0("as.character(as.integer(",name,"))")
            }


            ## numeric ----
          } else if (conv_to == 'num') {
            converted_var <- as.numeric(var)
            log_default <- paste0("as.character(as.numeric(",name,"))")


            ## number 1 DP ----
          } else if (conv_to == 'num1') {

            if (change_round) {
              converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1})?$", as.character(round(as.numeric(var),digits = 1))),sprintf(as.numeric(var),fmt = '%#.1f'),NA)
              special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1})?$", as.character(round(as.numeric(var),digits = 1))),round(as.numeric(var),digits = 1),NA)
              log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.1f')")

            } else {
              converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1})?$", var),sprintf(as.numeric(var),fmt = '%#.1f'),NA)
              special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1})?$", var),round(as.numeric(var),digits = 1),NA)
              log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.1f')")
            }


            ## number 2 DP ----
          } else if (conv_to == 'num2') {

            if (change_round) {
              converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,2})?$", as.character(round(as.numeric(var),digits = 2))),sprintf(as.numeric(var),fmt = '%#.2f'),NA)
              special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,2})?$", as.character(round(as.numeric(var),digits = 2))),round(as.numeric(var),digits = 2),NA)
              log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.2f')")

            } else {
              converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,2})?$", var),sprintf(as.numeric(var),fmt = '%#.2f'),NA)
              special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,2})?$", var),round(as.numeric(var),digits = 2),NA)
              log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.2f')")
            }


            ## number 3 DP ----
          } else if (conv_to == 'num3') {

            if (change_round) {
              converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,3})?$", as.character(round(as.numeric(var),digits = 3))),sprintf(as.numeric(var),fmt = '%#.3f'),NA)
              special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,3})?$", as.character(round(as.numeric(var),digits = 3))),round(as.numeric(var),digits = 3),NA)
              log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.3f')")

            } else {
              converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,3})?$", var),sprintf(as.numeric(var),fmt = '%#.3f'),NA)
              special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,3})?$", var),round(as.numeric(var),digits = 3),NA)
              log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.3f')")
            }


            ## number 4 DP ----
          } else if (conv_to == 'num4') {

            if (change_round) {
              converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,4})?$", as.character(round(as.numeric(var),digits = 4))),sprintf(as.numeric(var),fmt = '%#.4f'),NA)
              special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,4})?$", as.character(round(as.numeric(var),digits = 4))),round(as.numeric(var),digits = 4),NA)
              log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.4f')")

            } else {
              converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,4})?$", var),sprintf(as.numeric(var),fmt = '%#.4f'),NA)
              special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,4})?$", var),round(as.numeric(var),digits = 4),NA)
              log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.4f')")
            }


            ## date ----
          } else if (conv_to == 'dt') {

            if (change_round) {
              converted_var <- as.Date(redcap_import_datetime(var,args_rc_dates,args_rc_times,date_only = TRUE))
              log_default <- paste0("redcap_import_datetime(",name,
                                    ", args_rc_dates = list(",
                                    paste0(log_rc_dates,collapse = ", "),
                                    "), args_rc_times = list(",
                                    paste0(log_rc_times,collapse = ", "),
                                    "),date_only = TRUE)")

            } else {
            converted_var <- do.call(redcap_import_dates,c(list(var = var),args_rc_dates))
            # sometimes date-times are entered in Excel-format: they are recoded into a special variable for displaying the no-matches
            # because it's easier to understand what the no-match is about (instead of displaying e.g. 31341.34375)
            if (any(grepl("^\\d{4,}\\.\\d+$",var))) {
              special_no_match <- if_else(grepl("^\\d{4,}\\.\\d+$",var),redcap_import_datetime(var,args_rc_dates,args_rc_times),var)
            } else {
              special_no_match <- var
            }
            log_default <- paste0("format(redcap_import_dates(",name,", ",paste0(log_rc_dates,collapse = ", "),"))")
            }


            ## date-time ----
          } else if (conv_to == 'dt_tm') {
            converted_var <- as.POSIXct(redcap_import_datetime(var,args_rc_dates,args_rc_times))
            log_default <- paste0("redcap_import_datetime(",name,
                                  ", args_rc_dates = list(",
                                  paste0(log_rc_dates,collapse = ", "),
                                  "), args_rc_times = list(",
                                  paste0(log_rc_times,collapse = ", "),
                                  "))")


            ## time HMS ----
          } else if (conv_to == 'time_hms') {

            converted_var <- do.call(redcap_import_times,c(list(var = var),args_rc_times))
            special_sum_var <- hms(converted_var)
            log_default <- paste0("redcap_import_times(",name,", ",
                                  paste0(log_rc_times,collapse = ", "),
                                  ")")


            ## time HM ----
          } else if (conv_to == 'time_hm') {

            if (change_round) {
              converted_var <- format(as.POSIXct(do.call(redcap_import_times,c(list(var = var),args_rc_times)), format = "%H:%M:%S"), format = "%H:%M")
              special_sum_var <- hm(converted_var)
              log_default <- paste0("format(as.POSIXct(redcap_import_times(",name,", ",
                                    paste0(log_rc_times,collapse = ", "),
                                    "), format = '%H:%M:%S'), format = '%H:%M')")

            } else {
              converted_var <- if_else(format(as.POSIXct(do.call(redcap_import_times,c(list(var = var),args_rc_times)), format = "%H:%M:%S"), format = "%S") == "00",
                                       format(as.POSIXct(do.call(redcap_import_times,c(list(var = var),args_rc_times)), format = "%H:%M:%S"), format = "%H:%M"),NA)
              special_sum_var <- hm(converted_var)
              log_default <- paste0("format(as.POSIXct(redcap_import_times(",name,", ",
                                    paste0(log_rc_times,collapse = ", "),
                                    "), format = '%H:%M:%S'), format = '%H:%M')")
            }


            ## time MS ----
            # values in this variable are interpreted as min:sec
            # if there are more than 4 values, it is not possible to do a conversion (rounding makes no sense here)
          } else if (conv_to == 'time_ms') {

            converted_var <- if_else(is.na(as.POSIXct(redcap_import_times(var,unk_sec = NA), format = "%H:%M:%S")),
                                     format(as.POSIXct(redcap_import_times(var,unk_sec = NA), format = "%M:%S"), format = "%M:%S"),NA)
            special_sum_var <- ms(converted_var)
            log_default <- paste0("format(as.POSIXct(redcap_import_times(",name,", unk_sec = NA), format = '%M:%S'), format = '%M:%S')")


            ## email ----
          } else if (conv_to == 'email') {
            converted_var <- if_else(grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$",var),var,NA)
            log_default <- paste0("as.character(",name,")")


            ## letter ----
          } else if (conv_to == 'letter') {
            converted_var <- if_else(grepl("^[a-zA-Z]+$",var),var,NA)
            log_default <- paste0("as.character(",name,")")


            ## phone ----
          } else if (conv_to == 'phone') {
            converted_var <- if_else(grepl("\\b((0041|\\+41|0)(\\s?\\(0\\))?(\\s)?[1-9]{2}(\\s)?[0-9]{3}(\\s)?[0-9]{2}(\\s)?[0-9]{2})\\b", var),var,NA)
            log_default <- paste0("as.character(",name,")")


            ## pseudonym ----
          } else if (conv_to == 'pseudo') {
            converted_var <- if_else(grepl("^[0-9]{3}\\-[0-9]{3}\\-[0-9]{2}[0-9X]$",var),var,NA)
            log_default <- paste0("as.character(",name,")")


            ## slider ----
            # slider needs to be an integer (no missing data codes possible)
            # min/max are hard checked
          } else if (conv_to == 'slider') {
            converted_var <- if_else(as.numeric(var) %% 1 == 0 &
                                       as.numeric(var) >= rc_min &
                                       as.numeric(var) <= rc_max,
                                     as.integer(var),NA)
            log_default <- paste0("if_else(as.numeric(",name,") %% 1 == 0 &
                                       as.numeric(",name,") >= ",rc_min," &
                                       as.numeric(",name,") <= ",rc_max,",
                                     as.integer(",name,"),NA)")


          }

        }) # end suppressWarnings

      } # end if (!change_pot_miss), this part will be skipped if only the missings expressions should be adjusted



      # New Summary ----

      if (!suppress_txt) {
        cat("\n------------------------------- Conversion ------------------------------")
        cat("\n\nData Summary:\n")
        if(conv_to %in% c("num1","num2","num3","num4","time_hms","time_hm","time_ms")) {
          print(summary(special_sum_var))
        } else {
          print(summary(converted_var))
        }
        cat(str(converted_var))
      }

      if (!is.factor(converted_var)) {

        ## Potential Missings ----
        if (!is.null(pot_miss)) {
          potential_missings <- grepl(paste(pot_miss,collapse="|"),var, ignore.case = TRUE)
        } else {
          potential_missings <- FALSE
        }

        if (any(potential_missings)) {
          if (!suppress_txt) {
            cat("\n\nPotential Missings:")
            summary_table <- data.frame(Value = names(summary(as.factor(var[potential_missings]))),
                                        Count = as.numeric(summary(as.factor(var[potential_missings])))
            )
            print(kable(summary_table))
          }
        }


        # Not matching Format ----
        no_match <- is.na(converted_var) &  !is.na(var) & !potential_missings

        if (any(no_match)) {
          if (!suppress_txt) {
            cat("\n\nNot possible to convert:")
            if (conv_to == 'dt') {
              summary_table <- data.frame(Value = names(summary(as.factor(special_no_match[no_match]))),
                                          Count = as.numeric(summary(as.factor(var[no_match])))
              )
            } else {
              summary_table <- data.frame(Value = names(summary(as.factor(var[no_match]))),
                                          Count = as.numeric(summary(as.factor(var[no_match])))
              )
            }
            print(kable(summary_table))
          }
        }


      } else {
        potential_missings <- NULL
        no_match <- NULL
      }


      ## Empty ----

      empty_cells <- is.na(var)

      if (any(empty_cells)) {
        if (!suppress_txt) cat(paste0("\n\nEmpty cells: ",sum(empty_cells)))
      }

      if (!suppress_txt) cat("\n\n-------------------------------------------------------------------------")
      Sys.sleep(wait)



      # continue? ----

      ## INPUT ----
      if(continue) {
        cat("\n\nContinue?")
        cat("\n 1 = YES (start recoding)")
        cat("\n 2 = change expressions indicating missing values in this variable and the following")
        if(conv_to %in% c("int","num1","num2","num3","num4","time_hm","dt")) {
          cat("\n 3 = round values with too many decimals (or delete unneccessary time info)")
        }
        if (!auto_conv) {
          cat("\n 0 = NO (start over)")
          cat("\n 'on' = turn on auto conversion")
        } else {
          cat("\n 'off' = turn off auto conversion")
        }
        cat("\n 'skip' = skip this variable")
        cat("\n 'exit' = stop loop, exit code")
        contconv_ans <- ""

        while (contconv_ans != '1' &
               contconv_ans != '2' &
               contconv_ans != '3' &
               contconv_ans != 'on' &
               contconv_ans != 'off' &
               contconv_ans != '0' &
               contconv_ans != 'skip' &
               contconv_ans != 'exit') {

          contconv_ans <- readline(prompt="Answer: ")

          if (contconv_ans != '1' &
              contconv_ans != '2' &
              contconv_ans != '3' &
              contconv_ans != 'on' &
              contconv_ans != 'off' &
              contconv_ans != '0' &
              contconv_ans != 'skip' &
              contconv_ans != 'exit') {

            cat("Please check your answer!")
            cat("\n 1 = YES (start recoding)")
            cat("\n 2 = change expressions indicating missing values in this variable and the following")
            if(conv_to %in% c("int","num1","num2","num3","num4","time_hm","dt")) {
              cat("\n 3 = round values with too many decimals (or delete unneccessary time info)")
            }
            if (!auto_conv) {
              cat("\n 0 = NO (start over)")
              cat("\n 'on' = turn on auto conversion")
            } else {
              cat("\n 'off' = turn off auto conversion")
            }
            cat("\n 'skip' = skip this variable")
            cat("\n 'exit' = stop loop, exit code")
            contconv_ans <- ""
          }
        }
      } else contconv_ans <- ""


      ### change potential missings ----

      if (contconv_ans == '2') {

        # set change_pot_miss to TRUE so that conversion-part is skipped
        change_pot_miss <- TRUE

        #### INPUT ----

        cat("\nCurrently set as Potential Missing Expressions:\n")
        if (!is.null(pot_miss)) {
          cat(paste(pot_miss,collapse = "\n"))
        } else {
          cat("(No check for potential missings has been defined!)")
        }
        cat("\n\nExpressions can be defined in a character vector and a text-search is applied to search the data.\nE.g., c('miss','unknown','excluded','^0$','NA','N.A.')\n")
        cat("\nTo disable the search for potential missings, type 'NULL'!\n")
        pot_miss <- NULL

        while (!is.character(pot_miss)) {
          pot_miss <- readline(prompt="Answer: ")

          if (grepl("^c\\(.*\\)$", pot_miss)) {
            pot_miss <- eval(parse(text = pot_miss))
          }

          if (!is.character(pot_miss)) {

            cat("Please check your answer!")
            cat("Expressions can be defined in a character vector and a text-search is applied to search the data.\nE.g., c('miss','unknown','excluded','^0$','NA','N.A.')\n")
            pot_miss <- NULL
          }
        }

        if (length(pot_miss) == 1) {
          if (pot_miss == "NULL") {
            pot_miss <- NULL
          }
        }

      }

      ### round numeric or time values ----

      if (contconv_ans == '3') {

        # set change_round to TRUE so questions are skipped and rounding performed in conversion-part
        change_round <- TRUE

      }

      ### start over ----
      if (contconv_ans == '0') {
        change_pot_miss <- FALSE
        change_round <- FALSE
      }


      ### turn on auto conversion ----
      if (contconv_ans == 'on') {
        change_pot_miss <- FALSE
        change_round <- FALSE
        auto_conv <- TRUE
      }

      ### turn off auto conversion ----
      if (contconv_ans == 'off') {
        change_pot_miss <- FALSE
        change_round <- FALSE
        auto_conv <- FALSE
      }

      # exit:
      # break conversion while-loop here
      # for_break will be set to TRUE which will break the variable for-loop below and exit the code
      # (see "end conversion while-loop")

      if (contconv_ans == 'exit') {
        for_break = TRUE
        break
      }

      # skip:
      # break conversion while-loop here
      # for_skip will be set to TRUE which will cause the variable for-loop to move to the next variable below
      # (see "end conversion while-loop")

      if (contconv_ans == "skip") {
        for_skip = TRUE
        break
      }


      # end conversion while-loop ----

      # go-back option will be set to false which exits the while-loop

      if (!continue | contconv_ans == '1') {
        goback = FALSE
        change_pot_miss <- FALSE
        change_round <- FALSE
      }


    } # end while-loop



    # if for_break has been set to TRUE, the code will break the variable for-loop and exit the code
    if (for_break) {
      break
    }

    # if for_skip has been set to TRUE, the code will go to next variable in variable for-loop
    if (for_skip) {
      next
    }





    # check for recoding ----

    if (!suppress_txt) cat("\n\n------------------------------- Recoding --------------------------------")


    if (!is.factor(converted_var) & !any(potential_missings) & !any(no_match) & !any(empty_cells)) {

      to_recode <- NULL
      if (!suppress_txt) cat("\n\nNo recoding necessary!\n\n")

      if (!identical(var,converted_var)) {

        recoded_var <- as.character(converted_var)
        write.table(paste0("\n, ",name," = ",log_default), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

      } else {
        recoded_var <- as.character(var)
      }

      vars_recode[[length(vars_recode)+1]] <- recoded_var
      names(vars_recode)[length(vars_recode)] <- name



    } else {

      to_recode <- c()
      coded_options <- data.frame(code = character(),
                                  label = character())
    }

    ### if empty cells ----

    if (any(empty_cells)) {
      to_recode <- c(to_recode,NA)
    }

    ### if factor ----
    if (is.factor(converted_var)) {

      add_levels <- levels(converted_var)
      to_recode <- c(to_recode,levels(converted_var))

      if (rc_type == 'yesno') {
        rc_choices <- "1, Yes | 0, No"
      } else if (rc_type == 'truefalse') {
        rc_choices <- "1, True | 0, False"
      }

      if (is.na(rc_choices)) {
        if (!suppress_txt) {
          cat(bold(underline("\n\nNote:\n")))
          cat("No choices have been defined for this variable in REDCap!\n")
          cat("Please make sure to define choices in REDCap first.\n")
          cat("The variable will be skipped!")
          cat("\n\n")
          cat("-------------------------------------------------------------------------")
        }
        next
      }
      choices_sep <- str_split(rc_choices, pattern = ' \\| ')[[1]]
      codes_sep <- str_split(choices_sep, pattern = "\\, ",simplify = TRUE,n=2)[,1]
      options_sep <- str_split(choices_sep, pattern = "\\, ",simplify = TRUE,n=2)[,2]

      coded_options <- coded_options |>
        rbind(data.frame(code = codes_sep,
                         label = options_sep))

    }

    ### if missings ----
    if (any(potential_missings)) {

      add_miss <- levels(as.factor(var[potential_missings]))
      to_recode <- c(to_recode,add_miss)
    }


    ### if mismatches ----

    if (any(no_match)) {

      add_nomatch <- levels(as.factor(var[no_match]))
      to_recode <- c(to_recode,add_nomatch)
    }

    ### slider ----
    if (conv_to == 'slider') {
      recoded_var <- as.character(converted_var)
      vars_recode[[length(vars_recode)+1]] <- recoded_var
      names(vars_recode)[length(vars_recode)] <- name
      write.table(paste0("\n, ",name," = ",log_default), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

      to_recode <- NULL
      if (!suppress_txt) {
      cat("\n\nCurrently missing data codes can't be imported for slider variables (06.11.2024).\n")
      cat("All cells with values indicating potential missings or with values not matching the format will be set to NA (empty).\n")
      cat("Empty cells will remain empty.\n\n")
      }
    }


    if (!is.null(to_recode)) {

      # add missing codes to codebook
      if (!is.null(missing_codes)) {
        coded_options <- coded_options |>
          rbind(missing_codes)
      }

      # initiate option to go back with a while loop
      # in order to exit the for-loop or move to the next variable additional variables are needed which can be set to true during the loop
      goback = TRUE
      for_break = FALSE
      for_skip = FALSE


      # start recoding while-loop ----

      while (goback) {

        if (!suppress_txt) cat(paste0("\n\nRecoding for: ",bold(underline(name)),"   ",italic(rc_label)))
        Sys.sleep(wait)

        ## summarize values & codes ----
        if (!suppress_txt) {

          cat(underline("\n\n\nPotential values to recode in data table:"))
          summary_table <- data.frame(Value = names(summary(factor(var[var %in% to_recode],levels = to_recode))),
                                      Count = as.numeric(summary(factor(var[var %in% to_recode],levels = to_recode)))
          )
          print(kable(summary_table))

          cat(underline("\n\nPotential codes to use from REDCap codebook:"))
          print(kable(coded_options))
        }

        Sys.sleep(wait)



        # auto recoding ----

        # to change the similarity index, initiate a new while-loop

        change_auto_recode_precision <- TRUE
        while (change_auto_recode_precision) {

          sugg_coding <- data.frame(data = character(),
                                    to = character(),
                                    rc = character(),
                                    code = character(),
                                    stringsAsFactors = FALSE)


          ## start auto-recoding for-loop ----
          for (l in seq_along(to_recode)) {

            lvl = to_recode[l]

            if(!is.na(lvl)) {

              # compare code
              sim_code <- stringdist::stringsim(tolower(lvl),tolower(coded_options$code))
              # compare option label
              sim_option <-stringdist::stringsim(tolower(lvl),tolower(coded_options$label))

              ### no match ----
              if (!any(sim_code >= auto_recode_precision) & !any(sim_option >= auto_recode_precision)) {
                sugg_coding <- sugg_coding |>
                  rbind(data.frame(data = lvl,
                                   to = "   ",
                                   rc = "(no match found)",
                                   code = NA,
                                   stringsAsFactors = FALSE
                  ))
                next
              }

              ## multiple matches ----
              if (sum(sim_code >= auto_recode_precision) > 1 | sum(sim_option >= auto_recode_precision) > 1) {
                sugg_coding <- sugg_coding |>
                  rbind(data.frame(data = lvl,
                                   to = "   ",
                                   rc = "(mutliple matches found)",
                                   code = NA,
                                   stringsAsFactors = FALSE
                  ))
                next
              }

              ## code and option match----
              if (any(sim_code >= auto_recode_precision) & any(sim_option >= auto_recode_precision)) {
                sim_option <- NULL # code is more relevant than label
              }

              ## code match ----
              if (any(sim_code >= auto_recode_precision)) {
                sugg_coding <- sugg_coding |>
                  rbind(data.frame(data = lvl,
                                   to = "   ",
                                   rc = paste0(coded_options$code[sim_code >= auto_recode_precision]," (",rc_option = coded_options$label[sim_code >= auto_recode_precision],")"),
                                   code = coded_options$code[sim_code >= auto_recode_precision],
                                   stringsAsFactors = FALSE
                  ))
              }

              ## option match ----
              if (any(sim_option >= auto_recode_precision)) {
                sugg_coding <- sugg_coding |>
                  rbind(data.frame(data = lvl,
                                   to = "   ",
                                   rc = paste0(coded_options$code[sim_option >= auto_recode_precision]," (",rc_option = coded_options$label[sim_option >= auto_recode_precision],")"),
                                   code = coded_options$code[sim_option >= auto_recode_precision],
                                   stringsAsFactors = FALSE
                  )
                  )
              }
            }
          } # end auto-recoding loop

          ## suggestion ----
          if (!suppress_txt) cat(underline("\n\nSuggestion:"))

          if (!suppress_txt & nrow(sugg_coding) > 0) {
            print(kable(sugg_coding[1:3],col.names = c("Value in Data Table","","Suggested Code")))
            cat("\nValues for which no/multiple matches are found will be set to <NA>!")
          }


          ### empty cells ----

          if (any(empty_cells)) {
            if (!suppress_txt) cat("\n\nEmpty cells (NA's) will be converted to: ")
            if (is.na(if_empty)) {
              if (!suppress_txt) cat("<NA>")
              recode_empty <- NA
            } else {
              if (any(str_detect(coded_options$code,paste0("^",if_empty,"$")))) {
                if (!suppress_txt) cat(paste0(coded_options$code[str_detect(coded_options$code,paste0("^",if_empty,"$"))]," (",coded_options$label[str_detect(coded_options$code,paste0("^",if_empty,"$"))],")"))
                recode_empty <- coded_options$code[str_detect(coded_options$code,paste0("^",if_empty,"$"))]
              } else {
                if (!suppress_txt) cat("<NA> (provided code not recognized)")
                recode_empty <- NA
              }
            }
          }

          Sys.sleep(wait)



          # auto or manual recoding? ----

          ## INPUT ----

          if (auto_recode) {
            if (!suppress_txt) cat(paste0("\n\n\n",bold(underline("NOTE:")," Auto-recoding is active!\n(To turn it off, set 'auto_recode = FALSE' or type 'off' when prompted to continue.)\n\n\n")))
            Sys.sleep(wait)
            manrec_ans <- "1"

          } else {

            cat("\n\n\nWould you like to recode this variable as suggested or do it manually?")
            cat("\n 1 = recode as suggested")
            if (nrow(sugg_coding) > 0) cat("\n 2 = change precision of auto-matching")
            cat("\n 0 = recode manually")
            cat("\n 'skip' = do NOT recode, skip this variable")
            cat("\n 'exit' = stop loop, exit code")
            manrec_ans <- ""

            while (manrec_ans != '1' &
                   manrec_ans != '2' &
                   manrec_ans != '0' &
                   manrec_ans != "skip" &
                   manrec_ans != "exit") {

              manrec_ans <- readline(prompt="Answer: ")

              if (manrec_ans != '1' &
                  manrec_ans != '2' &
                  manrec_ans != '0' &
                  manrec_ans != "skip" &
                  manrec_ans != "exit") {

                cat("Please check your answer!")
                cat("\n 1 = recode as suggested")
                if (nrow(sugg_coding) > 0) cat("\n 2 = change precision of auto-matching")
                cat("\n 0 = recode manually")
                cat("\n 'skip' = do NOT recode, skip this variable")
                cat("\n 'exit' = stop loop, exit code")
                manrec_ans <- ""
              }
            }
          }


          ### change similarity index ----
          if (manrec_ans == '2') {

            cat(paste0("\nCurrent similarity index: ",auto_recode_precision))
            cat("\n\nPlease enter a new similariy index between 0 and 1.\n")
            auto_recode_precision <- NA

            while (!is.numeric(auto_recode_precision) |
                   auto_recode_precision < 0 |
                   auto_recode_precision > 1) {

              auto_recode_precision <- readline(prompt="Answer: ")

              if (suppressWarnings(!is.na(as.numeric(auto_recode_precision)))) {
                auto_recode_precision <- as.numeric(auto_recode_precision)
              }

              if (!is.numeric(auto_recode_precision) |
                  auto_recode_precision < 0 |
                  auto_recode_precision > 1) {

                cat("\nPlease check your answer!")
                cat("\nPlease enter a new similariy index between 0 and 1.\n")
                auto_recode_precision <- NA
              }
            }

          } else {
            # if any other option has been chosen, end the change_auto_recode_precision while-loop
            change_auto_recode_precision <- FALSE
          }
        } # end change_auto_recode_precision while-loop


        # exit:
        # break recoding while-loop here
        # for_break will be set to TRUE which will break the variable for-loop below and exit the code
        # (see "end recoding while-loop")

        if (manrec_ans == 'exit') {
          for_break = TRUE
          break
        }

        # skip:
        # break recoding while-loop here
        # for_skip will be set to TRUE which will cause the variable for-loop to move to the next variable below
        # (see "end recoding while-loop")

        if (manrec_ans == 'skip') {
          for_skip = TRUE
          break
        }



        # RECODING ----

        ## auto ----

        if (manrec_ans == '1') {

          recoded_var <- as.factor(var)
          log_recode_code <- character()
          log_recode_table <- character()


          if (any(empty_cells)) {
            # recode empty cells first:
            recoded_var <- as.character(recoded_var)
            recoded_var[is.na(recoded_var)] <- recode_empty
            recoded_var <- as.factor(recoded_var)

            if (is.na(recode_empty)) {
              log_recode_code <- c(log_recode_code,", NA ~ NA")
              log_recode_table <- c(log_recode_table,",,(empty),(empty)")
            } else {
              log_recode_code <- c(log_recode_code,paste0(", NA ~ '",recode_empty,"'"))
              log_recode_table <- c(log_recode_table,paste0(",,(empty),",recode_empty))
            }
          }



          for (l in seq_along(to_recode)) {

            # recode all levels, ignore NAs
            if (!is.na(to_recode[l])) {

              lvl <- as.character(to_recode[l])
              levels(recoded_var)[which(levels(recoded_var) == lvl)] <- sugg_coding$code[sugg_coding$data == to_recode[l]]

              if (is.na(sugg_coding$code[sugg_coding$data == to_recode[l]])) {
                log_recode_code <- c(log_recode_code,paste0(", '",lvl,"' ~ NA"))
                log_recode_table <- c(log_recode_table,paste0(",,",gsub(","," ",lvl),",(empty)"))
              } else {
                log_recode_code <- c(log_recode_code,paste0(", '",lvl,"' ~ '",sugg_coding$code[sugg_coding$data == to_recode[l]],"'"))
                log_recode_table <- c(log_recode_table,paste0(",,",gsub(","," ",lvl),",",sugg_coding$code[sugg_coding$data == to_recode[l]]))
              }
            }
          }
        }


        ## manual recoding----

        if (manrec_ans == '0') {

          recoded_var <- as.factor(var)
          log_recode_code <- character()
          log_recode_table <- character()

          cat("\n\n")
          cat("--------------------------- Manual Recoding -----------------------------")
          cat("\n\n")
          cat(paste0("Variable: ",bold(underline(name)),"   ",italic(rc_label)))
          cat("\n\nIn the following, the code will walk you through all values of the variable that might need recoding.\n")
          cat("You can decide to either change the value to a code as defined in the codebook in REDCap, to set the value to NA, or to leave it unchanged.\n")
          cat("If you are not sure what to do and want to consult with the study team first, I suggest to leave it unchanged and manually adjust the code output later on.\n")
          cat("Let's begin!\n\n\n")


          ### start manual-recoding for-loop ----
          # loop through values to recode and compare with codes in REDCap

          for (l in seq_along(to_recode)) {

            if (l == 1 & any(empty_cells)) {
              cat(bold(underline("What to do with empty cells?")))

            } else {
              lvl <- as.character(to_recode[l])
              cat("\nValue to recode:", bold(underline(lvl)))
            }

            cat("\n\nCodes to use from codebook in REDCap:")
            print(kable(coded_options))


            ### INPUT ----

            cat("\nPlease type matching code from the codebook or choose one of the following options:")
            cat("\n 'empty' = field will be set to NA (or stays NA)")
            cat("\n 'skip' = do NOT recode this value, value will stay as it is")
            cat("\n 'stop' = stop recoding for this variable, move to next variable")
            cat("\n 'exit' = stop loop, exit code")
            rec_ans <- ""

            while (!any(str_detect(coded_options$code,paste0("^",rec_ans,"$"))) &
                   rec_ans != 'empty' &
                   rec_ans != 'stop' &
                   rec_ans != 'skip' &
                   rec_ans != 'exit') {

              rec_ans <- readline(prompt="Answer: ")

              if (!any(str_detect(coded_options$code,paste0("^",rec_ans,"$"))) &
                  rec_ans != 'empty' &
                  rec_ans != 'stop' &
                  rec_ans != 'skip' &
                  rec_ans !='exit') {

                cat("Code not recognized: Please try again!")
                cat("\n\nPlease type matching code from the codebook or choose one of the following options:")
                cat("\n 'empty' = field will be set to NA (or stays NA)")
                cat("\n 'skip' = do NOT recode this value, value will stay as it is")
                cat("\n 'stop' = stop recoding for this variable, move to next variable")
                cat("\n 'exit' = stop loop, exit code")
                rec_ans <- ""
              }
            }


            # exit:
            # break manual-recoding for-loop here
            # for_break will be set to TRUE which will first break the recoding while-loop (see "end manual-recoding for-loop")
            # then, for_break will also break the variable for-loop and exit the code (see "end recoding while-loop")

            if (rec_ans == 'exit') {
              for_break = TRUE
              break
            }

            # stop:
            # break manual-recoding for-loop here
            # for_skip will be set to TRUE which will first break the recoding while-loop (see "end manual-recoding for-loop")
            # then, for_skip will cause the variable for-loop to move to the next variable (see "end recoding while-loop")

            if (rec_ans == 'stop') {
              for_skip = TRUE
              break
            }

            # skip:
            # moves to next item in manual-recoding for-loop
            if (rec_ans == 'skip') {
              log_recode_code <- c(log_recode_code,paste0(", '",lvl,"' ~ '",lvl,"'"))
              log_recode_table <- c(log_recode_table,paste0(",,",gsub(","," ",lvl),",",gsub(","," ",lvl)))
              next
            }

            # set to missing
            if (rec_ans == 'empty') {

              # empty cells
              if (l == 1 & any(empty_cells)) {
                recoded_var <- recoded_var
                log_recode_code <- c(log_recode_code,", NA ~ NA")
                log_recode_table <- c(log_recode_table,",,(empty),(empty)")

                # any other value
              } else {
                levels(recoded_var)[which(levels(recoded_var) == lvl)] <- NA
                log_recode_code <- c(log_recode_code,paste0(", '",lvl,"' ~ NA"))
                log_recode_table <- c(log_recode_table,paste0(",,",gsub(","," ",lvl),",(empty)"))
              }
            }


            # recode
            if (any(str_detect(coded_options$code,paste0("^",rec_ans,"$")))) {

              # empty cells
              if (l == 1 & any(empty_cells)) {
                recoded_var <- as.character(recoded_var)
                recoded_var[is.na(recoded_var)] <- rec_ans
                recoded_var <- as.factor(recoded_var)

                log_recode_code <- c(log_recode_code,paste0(", NA ~ '",rec_ans,"'"))
                log_recode_table <- c(log_recode_table,paste0(",,(empty),",rec_ans))

                # any other value
              } else {
                levels(recoded_var)[which(levels(recoded_var) == lvl)] <- rec_ans
                log_recode_code <- c(log_recode_code,paste0(", '",lvl,"' ~ '",rec_ans,"'"))
                log_recode_table <- c(log_recode_table,paste0(",,",gsub(","," ",lvl),",",rec_ans))
              }
            }







            ## end manual-recoding for-loop ----

          } # end for loop



          # if for_break has been set to TRUE, break the recoding while-loop here
          # for_break will also break the variable for-loop below and exit the code (see "end recoding while-loop")
          if (for_break) {
            break
          }

          # if for_skip has been set to TRUE, break the recoding while-loop here
          # for_skip will also cause the variable for-loop to move to the next variable below (see "end recoding while-loop")
          if (for_skip) {
            break
          }


        } # end if-statement (manual recoding)

        # output needs to be character
        recoded_var <- as.character(recoded_var)


        ### New Summary ----


        if (!suppress_txt) {

          cat("\n\n-------------------------- Recoding completed ---------------------------")

          cat(paste0("\n\nYou have reached the end of the recoding loop for:\n\n",bold(underline(name)),"   ",italic(rc_label)))
          cat(underline("\n\n\nREDCap Codebook:"))
          print(kable(coded_options))

          cat(underline("\n\nSummary before recoding:"))
          summary_table <- data.frame(Value = names(summary(as.factor(var[var %in% to_recode]))),
                                      Count = as.numeric(summary(as.factor(var[var %in% to_recode])))
          )
          print(kable(summary_table))

          cat(underline("\n\nSummary after recoding:"))
          summary_table <- data.frame(Value = names(summary(as.factor(recoded_var[var %in% to_recode]))),
                                      Count = as.numeric(summary(as.factor(recoded_var[var %in% to_recode])))
                                      )
          print(kable(summary_table))

          cat("\n-------------------------------------------------------------------------")
        }
        Sys.sleep(wait)


        ### continue? ----

        #### INPUT ----
        if(continue) {
          cat("\n\nContinue?")
          cat("\n 1 = YES")
          if (!auto_recode) {
            cat("\n 0 = NO (repeat recoding)")
            cat("\n 'on' = turn on auto recoding")
          } else {
            cat("\n 'off' = turn off auto recoding")
          }
          cat("\n 'skip' = skip this variable")
          cat("\n 'exit' = stop loop, exit code")
          contrec_ans <- ""

          while (contrec_ans != '1' &
                 contrec_ans != '0' &
                 contrec_ans != 'on' &
                 contrec_ans != 'off' &
                 contrec_ans != 'skip' &
                 contrec_ans != 'exit') {

            contrec_ans <- readline(prompt="Answer: ")

            if (contrec_ans != '1' &
                contrec_ans != '0' &
                contrec_ans != 'on' &
                contrec_ans != 'off' &
                contrec_ans != 'skip' &
                contrec_ans != 'exit') {

              cat("Please check your answer!")
              cat("\n 1 = YES")
              if (!auto_recode) {
                cat("\n 0 = NO (repeat recoding)")
                cat("\n 'on' = turn on auto recoding")
              } else {
                cat("\n 'off' = turn off auto recoding")
              }
              cat("\n 'skip' = skip this variable")
              cat("\n 'exit' = stop loop, exit code")
              contrec_ans <- ""
            }
          }
        } else contrec_ans <- ""


        ##### turn on auto recoding ----
        if (contrec_ans == 'on') {
          auto_recode <- TRUE
          change_auto_recode_precision <- FALSE
        }
        ##### turn off auto recoding ----
        if (contrec_ans == 'off') {
          auto_recode <- FALSE
          change_auto_recode_precision <- FALSE
        }

        # exit:
        # break recoding while-loop here
        # for_break will be set to TRUE which will break the variable for-loop below and exit the code
        # (see "end recoding while-loop")

        if (contrec_ans == 'exit') {
          for_break = TRUE
          break
        }

        # skip:
        # break recoding while-loop here
        # for_skip will be set to TRUE which will cause the variable for-loop to move to the next variable below
        # (see "end recoding while-loop")

        if (contrec_ans == 'skip') {
          for_skip = TRUE
          break
        }


        # WHAT TO DO ----

        if (!continue | contrec_ans == '1') {

          # output needs to be character
          converted_var <- as.character(converted_var)
          var <- as.character(var)

          # take the converted values or, if recording, the recorded values
          final_var <- data.frame(var,
                                  converted_var,
                                  recoded_var
          ) |>
            mutate(final_var =
                     if_else(
                       # if var has been recoded
                       recoded_var != var |
                         # if var has not been recoded but var is already a missing data code
                         (recoded_var == var & recoded_var %in% missing_codes$code) |
                         # if var is empty and has been recoded to a missing data code
                         (is.na(var) & !is.na(recoded_var)) |
                         # if var should not be recoded but does not match the format
                         (var == recoded_var & is.na(converted_var)),
                       #... then use the recoded var
                       recoded_var,
                       # ...otherwise the converted var
                       converted_var)) |>
            pull(final_var)

          vars_recode[[length(vars_recode)+1]] <- final_var
          names(vars_recode)[length(vars_recode)] <- name

          if(log) {

            write.table(paste0("\n, ",name," = case_match(",name), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste0(log_recode_code, sep=""), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste0(", .default = ",log_default,")"), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

            write.table(paste0(",",name), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste0(log_recode_table), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table("\n", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

          }




          # end recoding while-loop ----

          # go-back option will be set to false which exits the while-loop
          goback = FALSE
        }

      } # end while-loop


      # if for_break has been set to TRUE, the code will break the variable for-loop and exit the code
      if (for_break) {
        break
      }


      # if for_skip has been set to TRUE, the code will go to next variable in variable for-loop ('skip')
      if (for_skip) {
        next
      }

    } # end if-statement (anything to recode)

    if (!suppress_txt) cat("\n-------------------------------------------------------------------------\n\n")
    Sys.sleep(wait)

  } # end for-loop






  # execute code ----
  recoded_data <- mutate(selected_data,!!!vars_recode)


  # finalize log-file ----
  write.table(")\n\n--------------------------------------------------------------------------------------------------\n", log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


  # Return Output ----
  if (!suppress_txt) {
    cat("\nALL DONE!!!\n\n")
    cat("Thanks for using this script!\n")
    cat("Make sure to check the code and the summary in the log-files!\n")
    cat("If you encountered any problems while running the script, please let me know!\n\n")
  }

  return(recoded_data)




}




