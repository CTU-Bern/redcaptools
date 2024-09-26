#'REDCap Recode
#'
#'This function loops through all the variables of a data set and lets the user
#'compare them with the variables set up in REDCap. An API token is needed to
#'download the variable names from REDCap. The class/type can be changed to
#'output a new summary and to match REDCap. If 'factor' is chosen as class, the
#'script loops through all the factor levels and compares them with the coding
#'as defined in REDCap. The factor levels can then be matched with the
#'respective codes in REDCap. The function returns a data frame with the recoded
#'variables and writes the executed code to a log-file for copy-pasting and
#'adjusting/reusing.
#'
#'
#'@param selected_data Data to be recoded
#'@param dict Data dictionary (e.g. as downloaded from REDCap or via
#'  \code{redcap_export_meta(rc_token, rc_url)$meta}). If not supplied, this
#'  will be downloaded from the API using \code{rc_token} and \code{rc_token}.
#'@param missing_codes If a data dictionary is provided by the user, missing
#'  codes as defined in REDCap must be provided as well (if applicable). The
#'  missing codes should be provided in a single string with [code] [label]
#'  separated by a comma for each option and a pipe between the options (e.g.,
#'  "EXCL, Excluded | NA, not available"). If no data dictionary is provided,
#'  the codes will be downloaded from the API using \code{rc_token} and
#'  \code{rc_token} (if applicable).
#'@param rc_token REDCap API token
#'@param rc_url Link to REDCap API
#'@param start_var Define in which column of the import data the loop should
#'  start. Default = 1.
#'@param pot_miss The data is inspected for potential missing values that can be
#'  recoded. Expressions can be defined in a character vector and a text-search
#'  is applied to search through the data. Default =
#'  c("miss","unknown","excluded","^0$","NA","N.A.").
#'@param auto_conv If TRUE, ....
#'@param auto_recode If TRUE, ....
#'@param skip_intro If TRUE, the introduction messages will be skipped. Default
#'  = FALSE
#'@param continue If TRUE, a question to continue will be asked before moving
#'  along the loop. Default = TRUE.
#'@param suppress_txt If TRUE, all text output will be suppressed (not
#'  recommended). Default = FALSE.
#'@param log If TRUE, an overview csv-table, and a txt-file are stored in the
#'  working directory. Default = TRUE.
#'@param log_code Name and location of the txt-file containing the executed
#'  code. Default = redcap_import_recode_code.txt.
#'@param log_table Name and location of the csv.table containing the tabular
#'  overview. Default = redcap_import_recode_overview.csv.
#'@param wait Allows you to set the latency time between the steps. Default =
#'  2s.
#'
#'@return Data frame with recoded data. Log-file with executed code.
#'@export
#'@importFrom stringr str_split
#'@importFrom crayon bold underline blue red
#'@importFrom dplyr select mutate na_if across
#'@importFrom tidyselect everything
#'@importFrom utils str write.table
#'@importFrom knitr kable
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
                                 skip_intro = FALSE,
                                 continue = TRUE,
                                 suppress_txt = FALSE,
                                 log = TRUE,
                                 log_code = 'redcap_import_recode_code.txt',
                                 log_table = 'redcap_import_recode_overview.csv',
                                 wait = 2,
                                 ...) {

  field_name <- field_type <- select_choices_or_calculations <- text_validation_type_or_show_slider_number <- NULL

  # evaluate inputs ----

  # data
  check_data(selected_data)


  # dict

  if(is.null(dict)) {
    check_token(rc_token)
    check_url(rc_url)

    meta <- redcap_export_meta(rc_token, rc_url)
    dict <- meta$meta
    missing_codes <- meta$project$missing_data_codes

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
  #TODO: pot_miss, auto_conv und auto_recode
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


  # parse additional arguments for redcap_import_dates and redcap_import_times
  additional_args <- list(...)

  args_rc_dates <- additional_args[names(additional_args) %in% c("unk_day","unk_month","format")]
  log_rc_dates <- character()
  for (arg in names(args_rc_dates)) {
    log_rc_dates <- c(log_rc_dates,(paste0(arg," = ",args_rc_dates[[arg]])))
  }

  args_rc_times <- additional_args[names(additional_args) %in% c("unk_min","unk_sec")]
  log_rc_times <- character()
  for (arg in names(args_rc_times)) {
    log_rc_times <- c(log_rc_times,(paste0(arg," = ",args_rc_times[[arg]])))
  }


  # intro ----

  if (!skip_intro) {
    cat("\nHello and welcome!\n\n")
    cat("Let's start with some info about this script and your selections.\n")
    cat("It's best to use fullscreen while working with this script.\n")
    cat("(To turn off this introduction, set 'skip_intro = TRUE')\n\n\n\n")
    Sys.sleep(wait+1)

    cat("This script will loop you through the variable contents (e.g., coding, number format) in the provided data table and compares them with the setup in a REDCap data dictionary.\n\n\n")
    cat("Let's have a look at your choices first as provided as function arguments:\n\n")

    #TODO: describe other input arguments in intro!!


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
    write.table(paste0("\n\n",format(Sys.time(), "%Y-%m-%d %H:%M:%S")), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  }


  # read data & dict, prepare output variables ----

  name_vars <- colnames(selected_data)

  rc_spec <- dict |>
    select(field_name,
           field_label,
           field_type,
           choices = select_choices_or_calculations,
           validation = text_validation_type_or_show_slider_number)

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

      var <- as.character(selected_data[,i])
      name <- name_vars[i]


      if (!change_pot_miss) { # this part will be skipped if only the missing terms are adjusted

        if(!change_round) {

          cat(paste0("\n\nVariable: ",blue(bold(underline(name))),"\n\n"))
          Sys.sleep(wait)

          cat("--------------------------------- REDCap ---------------------------------")

          # var not in redcap:
          if (!any(rc_spec$field_name == name_vars[i])) {

            cat("\n\nThis variable is NOT part of the data dictionary you provided and will be skipped.\n")
            cat("It will remain in the data but will not be changed.")
            cat("\n\n")
            cat("-------------------------------------------------------------------------")

            # 'break' exits the conversion while-loop here
            # for_skip will be set to TRUE which will cause the variable for-loop to move to the next variable below
            # (see "end conversion while-loop")
            for_skip = TRUE
            break

          }


          # var in redcap:

          cat("\n\nVariable found in REDCap!")
          rc_label <- rc_spec$field_label[which(rc_spec$field_name == name)]
          rc_type <- rc_spec$field_type[which(rc_spec$field_name == name)]
          rc_val <- rc_spec$validation[which(rc_spec$field_name == name)]
          rc_choices <- rc_spec$choices[which(rc_spec$field_name == name)]

          cat(paste0("\n\nVariable Label: ",italic(rc_label)))
          cat(paste0("\nField Type: ", italic(rc_type)))
          if (rc_type == "text") cat(paste0("\nField Validation: ",italic(rc_val)))
          cat("\n\n\n")
          Sys.sleep(wait)


          cat("------------------------------- Data File -------------------------------")
          cat("\n\nData Summary:\n")
          print(summary(var))
          cat(str(var))
          cat("\n\n")
          cat("-------------------------------------------------------------------------")
          Sys.sleep(wait)



          # auto conversion ----
          conv_to <- ''

          # character
          if ((rc_type == "text" & is.na(rc_val)) |
              rc_type == "notes") {
            conv_to <- "txt"
            conv_label <- "Unvalidated Text"
          }

          # factor
          else if (rc_type == "truefalse" |
                   rc_type == "yesno" |
                   rc_type == "dropdown" |
                   rc_type == "radio") {
            conv_to <- "sc"
            conv_label <- "Single-Choice"
          }

          # integer
          else if (rc_type == "text" & !is.na(rc_val) & rc_val == "integer") {
            conv_to <- "int"
            conv_label <- "Integer"
          }

          # numeric
          else if (rc_type == "text" & !is.na(rc_val) & rc_val == "number") {
            conv_to <- "num"
            conv_label <- "Number (not further specified)"
          }

          # number 1DP
          else if (rc_type == "text" & !is.na(rc_val) & rc_val == "number_1dp") {
            conv_to <- "num1"
            conv_label <- "Number with 1 decimal place"
          }

          # number 2DP
          else if (rc_type == "text" & !is.na(rc_val) & rc_val == "number_2dp") {
            conv_to <- "num2"
            conv_label <- "Number with 2 decimal places"
          }

          # number 3DP
          else if (rc_type == "text" & !is.na(rc_val) & rc_val == "number_3dp") {
            conv_to <- "num3"
            conv_label <- "Number with 3 decimal places"
          }

          # number 4DP
          else if (rc_type == "text" & !is.na(rc_val) & rc_val == "number_4dp") {
            conv_to <- "num4"
            conv_label <- "Number with 4 decimal places"
          }

          # date
          else if (rc_type == "text" & !is.na(rc_val) & rc_val %in% c("date_dmy","date_ymd")) {
            conv_to <- "dt"
            conv_label <- "Date"
          }

          # datetime
          else if (rc_type == "text" & !is.na(rc_val) & rc_val %in% c("datetime_dmy","datetime_ymd","datetime_seconds_dmy","datetime_seconds_ymd")) {
            conv_to <- "dt_tm"
            conv_label <- "Date-Time"
          }

          # email
          else if (rc_type == "text" & !is.na(rc_val) & rc_val == "email") {
            conv_to <- "email"
            conv_label <- "Email"
          }

          # letter
          else if (rc_type == "text" & !is.na(rc_val) & rc_val == "alpha_only") {
            conv_to <- "letter"
            conv_label <- "Letter only"
          }

          # not defined
          else {
            cat("\n\nVariable Type not yet supported!")
            for_skip = TRUE
            break
          }

          #TODO: other validation types!

          cat(bold(paste0("\n\nSuggested Conversion to: ",italic(conv_label))))
          cat("\n\n\n")
          Sys.sleep(wait)


          # auto or manual conversion? ----

          if (auto_conv) {
            cat(paste0(underline("NOTE"),": Auto-conversion has been turned on! (To turn it off, set 'auto_conv = FALSE')\n\n\n"))
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

              #TODO: put answer options in a table to show it more aligned
              cat("\nHow would you like to convert this variable?\n")
              cat(" 'txt' = unvalidated text\n")
              cat(" 'sc' = single-choice (radiobutton, dropdown)\n")
              cat(" 'int' = integer\n")
              cat(" 'num' = number (not further specified)\n")
              cat(" 'num1' = number with 1 decimal place\n")
              cat(" 'num2' = number with 2 decimal places\n")
              cat(" 'num3' = number with 3 decimal places\n")
              cat(" 'num4' = number with 4 decimal places\n")
              cat(" 'dt' = date\n")
              cat(" 'dt_tm' = datetime\n")
              cat(" 'email' = email")
              cat(" 'letter' = letter only")
              # cat(" 'tm1' = time (HH:MM:SS)\n")
              # cat(" 'tm2' = time (HH:MM)\n")
              # cat(" 'tm3' = time (MM:SS)\n")
              cat(" 'exit' = do NOT convert and stop loop\n")
              cat(" 'skip' = do NOT convert and move to next item")
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
                     conv_to != 'email' &
                     conv_to != 'letter' &
                     # conv_to != 'dt_tm2' &
                     # conv_to != 'tm1' &
                     # conv_to != 'tm2' &
                     # conv_to != 'tm3' &
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
                    conv_to != 'email' &
                    conv_to != 'letter' &
                    # conv_to != 'dt_tm2' &
                    # conv_to != 'tm1' &
                    # conv_to != 'tm2' &
                    # conv_to != 'tm3' &
                    conv_to != 'skip' &
                    conv_to != 'exit') {

                  cat("Please check your answer! \n")
                  cat(" 'txt' = unvalidated text\n")
                  cat(" 'sc' = single-choice (radiobutton, dropdown)\n")
                  cat(" 'int' = integer\n")
                  cat(" 'num' = number (not further specified)\n")
                  cat(" 'num1' = number with 1 decimal place\n")
                  cat(" 'num2' = number with 2 decimal places\n")
                  cat(" 'num3' = number with 3 decimal places\n")
                  cat(" 'num4' = number with 4 decimal places\n")
                  cat(" 'dt' = date\n")
                  cat(" 'dt_tm' = datetime\n")
                  cat(" 'email' = email")
                  cat(" 'letter' = letter only")

                  # cat(" 'dt_tm1' = datetime (D-M-Y H:M)\n")
                  # cat(" 'dt_tm2' = datetime (Y-M-D H:M)\n")
                  # cat(" 'tm1' = time (HH:MM:SS)\n")
                  # cat(" 'tm2' = time (HH:MM)\n")
                  # cat(" 'tm3' = time (MM:SS)\n")
                  cat(" 'exit' = do NOT convert and stop loop\n")
                  cat(" 'skip' = do NOT convert and move to next item")
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




        # CONVERSION ----
        # Note:
        # this is mainly about providing a new summary, potential missings, and all values that do not match the chosen format
        # the actual "conversion" is done later after the recoding (see WHAT TO DO)
        # only values that are not recoded (i.e., are not potential missings and match the format) will be converted accordingly

        # suppress warnings as conversion would generate many warnings that will be shown in the very end
        suppressWarnings({

        # unvalidated text
        if (conv_to == 'txt') {
          converted_var <- as.character(var)
          log_default <- paste0("as.character(",name,")")
        }

        # factor
        #TODO
        if (conv_to == 'sc') {
          converted_var <- as.factor(var)
          log_default <- paste0(", ",name," = as.factor(",name,")")
        }

        # integer
        if (conv_to == 'int') {

          if (change_round) {
            converted_var <- if_else(round(as.numeric(var)) %% 1 == 0,as.integer(var),NA)
            log_default <- paste0("as.character(as.integer(round(as.numeric(",name,"))))")

          } else {
            converted_var <- if_else(as.numeric(var) %% 1 == 0,as.integer(var),NA)
            log_default <- paste0("as.character(as.integer(",name,"))")
          }

        }

        # numeric
        if (conv_to == 'num') {
          converted_var <- as.numeric(var)
          log_default <- paste0("as.character(as.numeric(",name,"))")
        }

        # number 1 DP
        if (conv_to == 'num1') {

          if (change_round) {
            converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1})?$", as.character(round(as.numeric(var),digits = 1))),sprintf(as.numeric(var),fmt = '%#.1f'),NA)
            special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1})?$", as.character(round(as.numeric(var),digits = 1))),round(as.numeric(var),digits = 1),NA)
            log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.1f')")

          } else {
            converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1})?$", var),sprintf(as.numeric(var),fmt = '%#.1f'),NA)
            special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1})?$", var),round(as.numeric(var),digits = 1),NA)
            log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.1f')")
          }

        }

        # number 2 DP
        if (conv_to == 'num2') {

          if (change_round) {
            converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,2})?$", as.character(round(as.numeric(var),digits = 2))),sprintf(as.numeric(var),fmt = '%#.2f'),NA)
            special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,2})?$", as.character(round(as.numeric(var),digits = 2))),round(as.numeric(var),digits = 2),NA)
            log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.2f')")

          } else {
            converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,2})?$", var),sprintf(as.numeric(var),fmt = '%#.2f'),NA)
            special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,2})?$", var),round(as.numeric(var),digits = 2),NA)
            log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.2f')")
          }

        }

        # number 3 DP
        if (conv_to == 'num3') {

          if (change_round) {
            converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,3})?$", as.character(round(as.numeric(var),digits = 3))),sprintf(as.numeric(var),fmt = '%#.3f'),NA)
            special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,3})?$", as.character(round(as.numeric(var),digits = 3))),round(as.numeric(var),digits = 3),NA)
            log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.3f')")

          } else {
            converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,3})?$", var),sprintf(as.numeric(var),fmt = '%#.3f'),NA)
            special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,3})?$", var),round(as.numeric(var),digits = 3),NA)
            log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.3f')")
          }

        }

        # number 4 DP
        if (conv_to == 'num4') {

          if (change_round) {
            converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,4})?$", as.character(round(as.numeric(var),digits = 4))),sprintf(as.numeric(var),fmt = '%#.4f'),NA)
            special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,4})?$", as.character(round(as.numeric(var),digits = 4))),round(as.numeric(var),digits = 4),NA)
            log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.4f')")

          } else {
            converted_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,4})?$", var),sprintf(as.numeric(var),fmt = '%#.4f'),NA)
            special_sum_var <- if_else(grepl("^[+-]?[0-9]+(\\.[0-9]{1,4})?$", var),round(as.numeric(var),digits = 4),NA)
            log_default <- paste0("sprintf(as.numeric(",name,"),fmt = '%#.4f')")
          }

        }

        # date
        if (conv_to == 'dt') {
          converted_var <- redcap_import_dates(var,args_rc_dates)
          log_default <- paste0("format(redcap_import_dates(",name,", ",paste0(log_rc_dates,collapse = ", "),"))")
        }

        # date-time
        if (conv_to == 'dt_tm') {
          converted_var <- as.POSIXct(redcap_import_datetime(var,args_rc_dates,args_rc_times))
          log_default <- paste0("redcap_import_datetime(",name,
                                ", args_rc_dates = list(",
                                paste0(log_rc_dates,collapse = ", "),
                                "), args_rc_times = list(",
                                paste0(log_rc_times,collapse = ", "),
                                "))")
        }

        # email
        if (conv_to == 'email') {
          converted_var <- if_else(grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$",var),var,NA)
          log_default <- paste0("as.character(",name,")")
        }

        # letter
        if (conv_to == 'letter') {
          converted_var <- if_else(grepl("^[a-zA-Z]+$",var),var,NA)
          log_default <- paste0("as.character(",name,")")
        }


        }) # end suppressWarnings




        # } else if (conv_to == 'dt_tm2') {                                                # convert to DATETIME (Y-M-D H:M)
        #   cat(red(bold(underline("Warning:"),
        #                "\nThis conversion has not yet been defined!",
        #                "\nNothing will happen here!")))
        #   converted_var <- var
        #
        # } else if (conv_to == 'tm1') {                                                   # convert to TIME (HH:MM:SS)
        #   cat(red(bold(underline("Warning:"),
        #                "\nThis conversion has not yet been defined!",
        #                "\nNothing will happen here!")))
        #   converted_var <- var
        #
        # } else if (conv_to == 'tm2') {                                                   # convert to TIME (HH:MM)
        #   cat(red(bold(underline("Warning:"),
        #                "\nThis conversion has not yet been defined!",
        #                "\nNothing will happen here!")))
        #   converted_var <- var
        #
        # } else if (conv_to == 'tm3') {                                                   # convert to TIME (MM:SS)
        #   cat(red(bold(underline("Warning:"),
        #                "\nThis conversion has not yet been defined!",
        #                "\nNothing will happen here!")))
        #   converted_var <- var
        #
        # }


      } # end if (!change_pot_miss), this part will be skipped if only the missings expressions should be adjusted

      # New Summary ----

      cat("\n------------------------------- Conversion ------------------------------")

      cat("\n\nData Summary:\n")
      if(conv_to %in% c("num1","num2","num3","num4")) {
        print(summary(special_sum_var))
      } else {
        print(summary(converted_var))
      }
      cat(str(converted_var))

      ## Potential Missings ----

      potential_missings <- grepl(paste(pot_miss,collapse="|"),var, ignore.case = TRUE)

      if (any(potential_missings)) {
        cat("\n\nPotential Missings:")
        print(kable(summary(as.factor(var[potential_missings])),col.names = "Cases:"))
      }


      ## Not matching Format ----

      no_match <- is.na(converted_var) &  !is.na(var) & !potential_missings

      if (any(no_match)) {
        cat("\n\nNot possible to convert:")
        print(kable(summary(as.factor(var[no_match])),col.names = "Cases:"))
      }

      ## Empty ----

      empty_cells <- is.na(var)

      if (any(empty_cells)) {
        cat(paste0("\n\nEmpty cells: ",sum(empty_cells)))
      }

      cat("\n\n")
      cat("-------------------------------------------------------------------------")
      Sys.sleep(wait)



      # continue? ----

      ## INPUT ----
      if(continue) {
        cat("\n\nContinue?")
        cat("\n1 = YES (start recoding)")
        cat("\n2 = change expressions indicating missing values in this variable")
        if(conv_to %in% c("int","num1","num2","num3","num4")) {
          cat("\n3 = round values with too many decimals")
        }
        cat("\n0 = NO (start over)")
        contconv_ans <- ""

        while (contconv_ans != '1' &
               contconv_ans != '2' &
               contconv_ans != '3' &
               contconv_ans != '0') {

          contconv_ans <- readline(prompt="Answer: ")

          if (contconv_ans != '1' &
              contconv_ans != '2' &
              contconv_ans != '3' &
              contconv_ans != '0') {

            cat("Please check your answer!")
            cat("\n1 = YES (start recoding)")
            cat("\n2 = change expressions indicating missing values in this variable")
            if(conv_to %in% c("int","num1","num2","num3","num4")) {
              cat("\n3 = round values with too many decimals")
            }
            cat("\n0 = NO (start over)")
            contconv_ans <- ""
          }
        }
      } else contconv_ans <- ""


      ### change potential missings ----

      if (contconv_ans == '2') {

        # set change_pot_miss to TRUE so that conversion-part is skipped
        change_pot_miss <- TRUE

        #### INPUT ----

        # TODO: add option to switch off check for pot miss!
        cat("\nCurrently set as Potential Missing Expressions:\n")
        cat(paste(pot_miss,collapse = "\n"))
        cat("\n\nExpressions can be defined in a character vector and a text-search is applied to search the data.\nE.g., c('miss','unknown','excluded','^0$','NA','N.A.')\n")
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
      }

      ### round numeric values ----
      if (contconv_ans == '3') {

        # set change_round to TRUE so questions are skipped and rounding performed in conversion-part
        change_round <- TRUE

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

    cat("\n\n")
    cat("------------------------------- Recoding --------------------------------")


    if (!is.factor(converted_var) & !any(potential_missings) & !any(no_match) & !any(empty_cells)) {

      cat("\n\nNo recoding necessary!\n\n")
      recoded_var <- var
      to_recode <- NULL

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

      to_recode <- c(to_recode,levels(converted_var))

      if (rc_type == 'yesno') {
        rc_choices <- "1, Yes | 0, No"
      } else if (rc_type == 'truefalse') {
        rc_choices <- "1, True | 0, False"
      }

      choices_sep <- str_split(rc_choices, pattern = ' \\| ')[[1]]
      codes_sep <- str_split(choices_sep, pattern = "\\, ",simplify = TRUE,n=2)[,1]
      options_sep <- str_split(choices_sep, pattern = "\\, ",simplify = TRUE,n=2)[,2]

      coded_options <- coded_options |>
        rbind(data.frame(codes_sep,options_sep))

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




# TODO:
# just because there is nothing to recode, does not mean that the var does not need to be converted
# if all values match the suggested format, the conversion still needs to be made (e.g., for numbers)
# => else-statement is needed here + something like (if var !=converted_var), otherwise many useless things are in the code-log
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

        cat(paste0("\n\nRecoding for: ",bold(underline(name)),"   ",italic(rc_label)))
        Sys.sleep(wait)

        ## summarize values & codes ----

        cat(underline("\n\n\nPotential values to recode in data table:"))
        print(kable(summary(factor(var[var %in% to_recode],levels = to_recode)),col.names = "Cases:"))

        cat(underline("\n\nPotential codes to use from REDCap codebook:"))
        print(kable(coded_options))

        Sys.sleep(wait)



        # auto recoding ----

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
            if (!any(sim_code > 0.8) & !any(sim_option > 0.8)) {
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
            if (sum(sim_code > 0.8) > 1 | sum(sim_option > 0.8) > 1) {
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
            if (any(sim_code > 0.8) & any(sim_option > 0.8)) {
              sim_option <- NULL # code is more relevant than label
            }

            ## code match ----
            if (any(sim_code > 0.8)) {
              sugg_coding <- sugg_coding |>
                rbind(data.frame(data = lvl,
                                 to = "   ",
                                 rc = paste0(coded_options$code[sim_code > 0.8]," (",rc_option = coded_options$label[sim_code > 0.8],")"),
                                 code = coded_options$code[sim_code > 0.8],
                                 stringsAsFactors = FALSE
                ))
            }

            ## option match ----
            if (any(sim_option > 0.8)) {
              sugg_coding <- sugg_coding |>
                rbind(data.frame(data = lvl,
                                 to = "   ",
                                 rc = paste0(coded_options$code[sim_option > 0.8]," (",rc_option = coded_options$label[sim_option > 0.8],")"),
                                 code = coded_options$code[sim_option > 0.8],
                                 stringsAsFactors = FALSE
                                 )
                      )
            }
          }
        } # end auto-recoding loop

        ## suggestion ----
        cat(underline("\n\nSuggestion:"))
        print(kable(sugg_coding[1:3],col.names = c("Value in Data Table","","Suggested Code")))

        ### empty cells ----
        cat("\nValues for which no/multiple matches are found will be set to <NA>!")
        if (any(empty_cells)) {
          cat("\n\nEmpty cells (NA's) will be converted to: ")
          if (is.na(if_empty)) {
            cat("<NA>")
            recode_empty <- NA
          } else {
            if (any(str_detect(coded_options$code,paste0("^",if_empty,"$")))) {
              cat(paste0(coded_options$code[str_detect(coded_options$code,paste0("^",if_empty,"$"))]," (",coded_options$label[str_detect(coded_options$code,paste0("^",if_empty,"$"))],")"))
              recode_empty <- coded_options$code[str_detect(coded_options$code,paste0("^",if_empty,"$"))]
            } else {
              cat("<NA> (provided code not recognized)")
              recode_empty <- NA
            }
          }
        }

        Sys.sleep(wait)



        # auto or manual recoding? ----

        ## INPUT ----

        if (!auto_recode) {

          cat("\n\n\nWould you like to recode this variable as suggested or do it manually?")
          cat("\n 1 = recode as suggested")
          #TODO: add option to change matching
          cat("\n 0 = recode manually")
          cat("\n 'skip' = do NOT recode, skip this variable")
          cat("\n 'exit' = stop loop, exit code")
          manrec_ans <- ""

          while (manrec_ans != '1' &
                 manrec_ans != '0' &
                 manrec_ans != "skip" &
                 manrec_ans != "exit") {

            manrec_ans <- readline(prompt="Answer: ")

            if (manrec_ans != '1' &
                manrec_ans != '0' &
                manrec_ans != "skip" &
                manrec_ans != "exit") {

              cat("Please check your answer!")
              cat("\n 1 = recode as suggested")
              cat("\n 0 = recode manually")
              cat("\n 'skip' = do NOT recode, skip this variable")
              cat("\n 'exit' = stop loop, exit code")
              manrec_ans <- ""
            }
          }
        }


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

          log_recode <- character()
          recoded_var <- as.factor(var)

          if (any(empty_cells)) {
            # recode empty cells first:
            recoded_var <- as.character(recoded_var)
            recoded_var[is.na(recoded_var)] <- recode_empty
            recoded_var <- as.factor(recoded_var)

            if (is.na(recode_empty)) {
              log_recode <- c(log_recode,", NA ~ NA")
            } else {
              log_recode <- c(log_recode,paste0(", NA ~ '",recode_empty,"'"))
            }
          }



          for (l in seq_along(to_recode)) {

            # recode all levels, ignore NAs
            if (!is.na(to_recode[l])) {

              lvl <- as.character(to_recode[l])
              levels(recoded_var)[which(levels(recoded_var) == lvl)] <- sugg_coding$code[sugg_coding$data == to_recode[l]]

              if (is.na(sugg_coding$code[sugg_coding$data == to_recode[l]])) {
                log_recode <- c(log_recode,paste0(", '",lvl,"' ~ NA"))
              } else {
                log_recode <- c(log_recode,paste0(", '",lvl,"' ~ '",sugg_coding$code[sugg_coding$data == to_recode[l]],"'"))
              }
            }
          }
        }


        ## manual recoding----

        if (manrec_ans == '0') {

          log_recode <- character()
          recoded_var <- as.factor(var)

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
              cat(bold(underline("First let's decide what to do with empty cells!")))

            } else {
              lvl <- as.character(to_recode[l])
              cat("\nValue to recode:", bold(underline(lvl)))
            }

            cat("\n\nCodes to use from codebook in REDCap:")
            print(kable(coded_options))


            ### INPUT ----

            cat("\nPlease type matching code from the codebook or choose one of the following options:")
            cat("\n 'delete' = delete value, field will be set to NA (or stays NA)")
            cat("\n 'skip' = do NOT recode this value, value will stay as it is")
            cat("\n 'stop' = stop recoding for this variable, move to next variable")
            cat("\n 'exit' = stop loop, exit code")
            rec_ans <- ""

            while (!any(str_detect(coded_options$code,paste0("^",rec_ans,"$"))) &
                   rec_ans != 'delete' &
                   rec_ans != 'stop' &
                   rec_ans != 'skip' &
                   rec_ans != 'exit') {

              rec_ans <- readline(prompt="Answer: ")

              if (!any(str_detect(coded_options$code,paste0("^",rec_ans,"$"))) &
                  rec_ans != 'delete' &
                  rec_ans != 'stop' &
                  rec_ans != 'skip' &
                  rec_ans !='exit') {

                cat("Code not recognized: Please try again!")
                cat("\n\nPlease type matching code from the codebook or choose one of the following options:")
                cat("\n 'delete' = delete value, field will be set to NA (or stays NA)")
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
              next
            }

            # set to missing
            if (rec_ans == 'delete') {

              # empty cells
              if (l == 1 & any(empty_cells)) {
                recoded_var <- recoded_var
                log_recode <- c(log_recode,", NA ~ NA")

                # any other value
                } else {
                  levels(recoded_var)[which(levels(recoded_var) == lvl)] <- NA
                  log_recode <- c(log_recode,paste0(", '",lvl,"' ~ NA"))
                }
            }


            # recode
            if (any(str_detect(coded_options$code,paste0("^",rec_ans,"$")))) {

              # empty cells
              if (l == 1 & any(empty_cells)) {
                recoded_var <- as.character(recoded_var)
                recoded_var[is.na(recoded_var)] <- rec_ans
                recoded_var <- as.factor(recoded_var)

                log_recode <- c(log_recode,paste0(", NA ~ '",rec_ans,"'"))

              # any other value
              } else {
                levels(recoded_var)[which(levels(recoded_var) == lvl)] <- rec_ans
                log_recode <- c(log_recode,paste0(", '",lvl,"' ~ '",rec_ans,"'"))
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


        cat("\n\n")
        cat("-------------------------- Recoding completed ---------------------------")

        cat(paste0("\n\nYou have reached the end of the recoding loop for:\n\n",bold(underline(name)),"   ",italic(rc_label)))
        cat(underline("\n\n\nREDCap Codebook:"))
        print(kable(coded_options))

        cat(underline("\n\nSummary before recoding:"))
        print(kable(summary(as.factor(var[var %in% to_recode])),col.names = "Cases:"))

        cat(underline("\n\nSummary after recoding:"))
        print(kable(summary(as.factor(recoded_var[var %in% to_recode])),col.names = "Cases:"))

        cat("\n-------------------------------------------------------------------------")
        Sys.sleep(wait)


        ### continue? ----

        #### INPUT ----
        cat("\n\nContinue?")
        cat("\n1 = YES")
        cat("\n0 = NO (repeat recoding)")
        contrec_ans <- ""

        while (contrec_ans != '1' &
               contrec_ans != '0') {

          contrec_ans <- readline(prompt="Answer: ")

          if (contrec_ans != '1' &
              contrec_ans != '0') {

            cat("Please check your answer!")
            cat("\n1 = YES")
            cat("\n0 = NO (repeat recoding)")
            contrec_ans <- ""
          }
        }


        # WHAT TO DO ----

        if (contrec_ans == '1') {

          # output needs to be character
          converted_var <- as.character(converted_var)

          # take the converted values or, if recording, the recorded values
          final_var <- data.frame(var,
                                  converted_var,
                                  recoded_var
          ) |>
            mutate(final_var = if_else(recoded_var != var |
                                         (recoded_var == var & recoded_var %in% missing_codes$code) |
                                         (is.na(var) & !is.na(recoded_var)),
                                       recoded_var,
                                       converted_var)) |>
            pull(final_var)

          vars_recode[[length(vars_recode)+1]] <- final_var
          names(vars_recode)[length(vars_recode)] <- name

          if(log) {
            write.table(paste0("\n, ",name," = case_match(",name), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste0(log_recode, sep=""), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste0(", .default = ",log_default,")"), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
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

    cat("\n-------------------------------------------------------------------------\n\n")
    Sys.sleep(wait)

  } # end for-loop






  # execute code ----
  recoded_data <- mutate(selected_data,!!!vars_recode)


  # finalize log-file ----
  write.table(")\n\n--------------------------------------------------------------------------------------------------\n", log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


  # Return Output ----
  cat("\nALL DONE!!!\n\n")
  cat("Thanks for using this script!\n")
  cat("Make sure to check the code and the summary in the log-files!\n")
  cat("If you encountered any problems while running the script, please let me know!\n\n")

  return(recoded_data)




}




