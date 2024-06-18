#'REDCap Recode
#'
#'This function loops through all the variable classes of a data set and lets
#'the user compare them with the variable types set up in REDCap. An API token
#'is needed to download the variable names from REDCap. The class/type can be
#'changed to output a new summary and to match REDCap. If 'factor' is chosen as
#'class, the script loops through all the factor levels and compares them with
#'the coding as defined in REDCap. The factor levels can then be matched with
#'the respective codes in REDCap. The function returns a data frame with the
#'recoded variables and writes the executed code to a log-file for copy-pasting
#'and adjusting/reusing.
#'
#'
#'@param selected_data Data to be recoded
#'#'@param dict Data dictionary (e.g. as downloaded from REDCap or via
#'  \code{redcap_export_meta(rc_token, rc_url)$meta}). If not supplied, this will
#'  be downloaded from the API using \code{rc_token}.
#'@param missing_codes have to be provided as string: e.g., "EXCL, Excluded | NA, not available"
#'@param rc_token REDCap API token
#'@param rc_url Link to REDCap API
#'@param skip_intro If set to TRUE, the introduction messages will be skipped.
#'  Default = FALSE
#'@param suppress_txt If set TRUE, all text output will be suppressed (not
#'  recommended). Default = FALSE.
#'@param log If TRUE, an overview csv-table, and a log-file are stored in the
#'  working directory. Default = TRUE.
#'@param wait Allows you to set the latency time between the steps. Default =
#'  2s.
#'
#'@return Data frame with recoded data. Log-file with executed code.
#'@export
#'@importFrom stringr str_split
#'@importFrom crayon bold underline blue red
#'@importFrom dplyr select mutate
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
                                 auto_conv = FALSE,
                                 auto_recode = FALSE,
                                 skip_intro = FALSE,
                                 suppress_txt = FALSE,
                                 log = TRUE,
                                 wait = 2) {

  field_name <- field_type <- select_choices_or_calculations <- text_validation_type_or_show_slider_number <- NULL

  # evaluate inputs ----
  check_data(selected_data)
  name_vars <- colnames(selected_data)


  if(is.null(dict)) {
    check_token(rc_token)
    check_url(rc_url)
    meta <- redcap_export_meta(rc_token, rc_url)
    dict <- meta$meta
    missing_codes <- meta$project$missing_data_codes
  }
  check_dict(dict)

  if(!is.logical(skip_intro)) stop("skip_intro should be logical (TRUE/FALSE)")
  if(!is.logical(suppress_txt)) stop("suppress_txt should be logical (TRUE/FALSE)")
  if(!is.logical(log)) stop("log should be logical (TRUE/FALSE)")
  if(!is.numeric(wait) || length(wait) != 1) stop("wait should be a single number")



  # intro ----

  if (!skip_intro) {
    cat("\nHello and welcome!\n\n")
    cat("Let's start with some info about this script and your selections.\n")
    cat("It's best to use fullscreen while working with this script.\n")
    cat("(To turn off this introduction, set 'skip_intro = TRUE')\n\n\n\n")
    Sys.sleep(wait+1)


    cat("Are you ready to begin? \n 1 = YES\n'esc' = STOP")
    intro_ans <- ""
    while (intro_ans != 1) {
      intro_ans <- readline(prompt="Answer= ")
      if (intro_ans != 1) {
        cat("Please check your answer! \n 1 = YES\n'esc' = STOP")
        intor_ans <- ""
      }
    }

    cat("\nGreat! Let's begin!\n")
    cat("\n--------------------------------------------------------------------------\n\n")
    Sys.sleep(wait)
  }


  # open log-file ----
  if(log) {
    log_file <- "redcap_import_recode.txt"
    write.table(paste0(Sys.time(),":\n\nrecoded_data <- mutate(selected_data"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  }

  # read dict, prepare output variables ----

  rc_spec <- dict |>
    select(field_name,
           field_label,
           field_type,
           choices = select_choices_or_calculations,
           validation = text_validation_type_or_show_slider_number)

  vars_recode <- list()

  # start loop ----

  for (i in seq_along(name_vars)) {

    goback = TRUE
    for_break = FALSE
    for_skip = FALSE

    while (goback) {

      ## summarise data and rc dict ----

      var <- selected_data[,i]
      name <- name_vars[i]
      cat(paste0("\nVariable: ",blue(bold(underline(name))),"\n\n"))
        Sys.sleep(wait)

      cat("--------------------------------- REDCap ---------------------------------")

      if (!any(rc_spec$field_name == name_vars[i])) {
        cat("\n\nThis variable is NOT part of the data dictionary you provided and will be skipped.\n")
        cat("Please run redcap_import_select() to choose and rename variables before recoding.\n\n")
        for_skip = TRUE
        break
      }

      cat("\n\nVariable found in REDCap!")
      rc_label <- rc_spec$field_label[which(rc_spec$field_name == name)]
      rc_type <- rc_spec$field_type[which(rc_spec$field_name == name)]
      rc_val <- rc_spec$validation[which(rc_spec$field_name == name)]
      choices <- rc_spec$choices[which(rc_spec$field_name == name)]

      cat(paste0("\n\nVariable Label: ",italic(rc_label)))
      cat(paste0("\nField Type: ", italic(rc_type)))
      if (rc_type == "text") cat(paste0("\nField Validation: ",italic(rc_val)))
      cat("\n\n\n")
      Sys.sleep(wait)


      cat("------------------------------- Data File -------------------------------")
      cat(paste0("\n\nClass in Data File: ", italic(class(var))))
      cat("\n\nData Summary:\n")
      print(summary(var))
      cat(str(var))
      cat("\n\n")
      cat("-------------------------------------------------------------------------")
      Sys.sleep(wait)


      # auto conversion ----
      conv_ans <- ''

      ## character ----
      if ((rc_type == "text" && is.na(rc_val)) |
          rc_type == "notes") {
        conv_ans <- "char"
        conv_label <- "Character"
      }

      ## factor ----
      if (rc_type == "truefalse" |
          rc_type == "yesno" |
          rc_type == "dropdown" |
          rc_type == "radio") {
        conv_ans <- "fct"
        conv_label <- "Factor"
      }

      ## integer ----
      if (rc_type == "text" && !is.na(rc_val) && rc_val == "integer") {
        conv_ans <- "int"
        conv_label <- "Integer"
      }

      ## numeric ----
      if (rc_type == "text" && !is.na(rc_val) && rc_val == "number") {
        conv_ans <- "num"
        conv_label <- "Number (not further specified)"
      }

      ## number 1DP ----
      if (rc_type == "text" && !is.na(rc_val) && rc_val == "number_1dp") {
        conv_ans <- "num1"
        conv_label <- "Number with 1 decimal place"
      }

      ## number 2DP ----
      if (rc_type == "text" && !is.na(rc_val) && rc_val == "number_2dp") {
        conv_ans <- "num2"
        conv_label <- "Number with 2 decimal places"
      }

      ## number 3DP ----
      if (rc_type == "text" && !is.na(rc_val) && rc_val == "number_3dp") {
        conv_ans <- "num3"
        conv_label <- "Number with 3 decimal places"
      }

      ## number 4DP ----
      if (rc_type == "text" && !is.na(rc_val) && rc_val == "number_4dp") {
        conv_ans <- "num4"
        conv_label <- "Number with 4 decimal places"
      }

      ## date dmy ----
      if (rc_type == "text" && !is.na(rc_val) && rc_val == "date_dmy") {
        conv_ans <- "dt1"
        conv_label <- "Date (D-M-Y)"
      }

      ## date ymd ----
      if (rc_type == "text" && !is.na(rc_val) && rc_val == "date_ymd") {
        conv_ans <- "dt2"
        conv_label <- "Date (Y-M-D)"
      }

      cat(bold(paste0("\n\nSuggested conversion to Class: ",italic(conv_label))))
      cat("\n\n\n")
      Sys.sleep(wait)


      # manual conversion ----

      ### INPUT ----

      if (auto_conv) {
        cat("Auto-conversion has been turned on! (To turn it off, set 'auto_conv = FALSE')\n\n\n")
        Sys.sleep(wait)

      } else {

        cat("Would you like to convert or manually change the type/class?")
        cat("\n 1 = convert as suggested")
        cat("\n 0 = change manually")
        cat("\n 'skip' = skip this variable")
        cat("\n 'exit' = stop loop")
        manconv_ans <- ""

        while (manconv_ans != '1' &&
               manconv_ans != '0' &&
               manconv_ans != 'skip' &&
               manconv_ans != 'exit') {

          manconv_ans <- readline(prompt="Answer= ")

          if (manconv_ans != '1' &&
              manconv_ans != '0' &&
              manconv_ans != 'skip' &&
              manconv_ans != 'exit') {

            cat("Please check your answer!")
            cat("\n 1 = convert as suggested")
            cat("\n 0 = change manually")
            cat("\n 'skip' = skip this variable")
            cat("\n 'exit' = stop loop")
            manconv_ans <- ""
          }
        }

        #### exit ----
        # (break while-loop here, for-loop will be broken below)

        if (manconv_ans == 'exit') {
          for_break = TRUE
          break
        }

        ### skip ----
        # do not change type/class
        # (break while-loop here, for-loop will be nexted below)

        if (manconv_ans == "skip") {
          for_skip = TRUE
          break
        }

        ### change manually ----

        if (manconv_ans == "0") {

          ##### INPUT ----

          cat("\nWhat should the new type/class be?\n")
          cat(" 'char' = character\n")
          cat(" 'fct' = factor (e.g., Radiobutton)\n")
          cat(" 'int' = integer\n")
          cat(" 'num' = number (not further specified)\n")
          cat(" 'num1' = number with 1 decimal place\n")
          cat(" 'num2' = number with 2 decimal places\n")
          cat(" 'num3' = number with 3 decimal places\n")
          cat(" 'num4' = number with 4 decimal places\n")
          cat(" 'dt1' = date (D-M-Y)\n")
          cat(" 'dt2' = date (Y-M-D)\n")
          cat(" 'dt_tm1' = datetime (D-M-Y H:M)\n")
          cat(" 'dt_tm2' = datetime (Y-M-D H:M)\n")
          cat(" 'tm1' = time (HH:MM:SS)\n")
          cat(" 'tm2' = time (HH:MM)\n")
          cat(" 'tm3' = time (MM:SS)\n")
          cat(" 'exit' = do NOT change and stop loop\n")
          cat(" 'skip' = do NOT change and move to next item")
          conv_ans <- ""

          while (conv_ans != 'char' &&
                 conv_ans != 'int' &&
                 conv_ans != 'num' &&
                 conv_ans != 'num1' &&
                 conv_ans != 'num2' &&
                 conv_ans != 'num3' &&
                 conv_ans != 'num4' &&
                 conv_ans != 'dt1' &&
                 conv_ans != 'dt2' &&
                 conv_ans != 'dt_tm1' &&
                 conv_ans != 'dt_tm2' &&
                 conv_ans != 'tm1' &&
                 conv_ans != 'tm2' &&
                 conv_ans != 'tm3' &&
                 conv_ans != 'fct' &&
                 conv_ans != 'skip' &&
                 conv_ans != 'exit') {

            conv_ans <- readline(prompt="Answer= ")

            if (conv_ans != 'char' &&
                conv_ans != 'int' &&
                conv_ans != 'num' &&
                conv_ans != 'num1' &&
                conv_ans != 'num2' &&
                conv_ans != 'num3' &&
                conv_ans != 'num4' &&
                conv_ans != 'dt1' &&
                conv_ans != 'dt2' &&
                conv_ans != 'dt_tm1' &&
                conv_ans != 'dt_tm2' &&
                conv_ans != 'tm1' &&
                conv_ans != 'tm2' &&
                conv_ans != 'tm3' &&
                conv_ans != 'fct' &&
                conv_ans != 'skip' &&
                conv_ans != 'exit') {

              cat("Please check your answer! \n")
              cat(" 'char' = character\n")
              cat(" 'fct' = factor (e.g., Radiobutton)\n")
              cat(" 'int' = integer\n")
              cat(" 'num' = number (not further specified)\n")
              cat(" 'num1' = number with 1 decimal place\n")
              cat(" 'num2' = number with 2 decimal places\n")
              cat(" 'num3' = number with 3 decimal places\n")
              cat(" 'num4' = number with 4 decimal places\n")
              cat(" 'dt1' = date (D-M-Y)\n")
              cat(" 'dt2' = date (Y-M-D)\n")
              cat(" 'dt_tm1' = datetime (D-M-Y H:M)\n")
              cat(" 'dt_tm2' = datetime (Y-M-D H:M)\n")
              cat(" 'tm1' = time (HH:MM:SS)\n")
              cat(" 'tm2' = time (HH:MM)\n")
              cat(" 'tm3' = time (MM:SS)\n")
              cat(" 'exit' = do NOT change and stop loop\n")
              cat(" 'skip' = do NOT change and move to next item")
              conv_ans <- ""
            }
          }
        } # end if (answer = 0)
      } # end if (auto = FALSE)


      #### exit ----
      # (break while-loop, for-loop will be broken below)
      if (conv_ans == 'exit') {
        for_break = TRUE
        break
      }

      #### skip ----
      # (break while-loop, for-loop will be nexted below)
      if (conv_ans == 'skip') {
        for_skip = TRUE
        break
      }

      ## CONVERSION ----
      if (conv_ans == 'char') {                                                           # convert to CHARACTER
        new_var <- as.character(var)
        log <- paste0(", ",name," = as.character(",name,")")

      } else if (conv_ans == 'fct') {                                                   # convert to FACTOR
        new_var <- as.factor(var)
        log <- paste0(", ",name," = as.factor(",name,")")

      } else if (conv_ans == 'int') {                                                   # convert to INTEGER
        new_var <- as.integer(var)
        log <- paste0(", ",name," = as.integer(",name,")")

      } else if (conv_ans == 'num') {                                                   # convert to NUMBER
        new_var <- as.numeric(var)
        log <- paste0(", ",name," = as.numeric(",name,")")

      } else if (conv_ans == 'num1') {                                                  # convert to NUMBER WITH 1 DECIMAL PLACE
        new_var <- ifelse(!is.na(var),sprintf(var, fmt = '%#.1f'),var)
        log <- paste0(", ",name," = ifelse(!is.na(",name,"),sprintf(",name,", fmt = '%#.1f'),",name,")")

      } else if (conv_ans == 'num2') {                                                  # convert to NUMBER WITH 2 DECIMAL PLACES
        new_var <- ifelse(!is.na(var),sprintf(var, fmt = '%#.2f'),var)
        log <- paste0(", ",name," = ifelse(!is.na(",name,"),sprintf(",name,", fmt = '%#.2f'),",name,")")

      } else if (conv_ans == 'num3') {                                                  # convert to NUMBER WITH 3 DECIMAL PLACES
        new_var <- ifelse(!is.na(var),sprintf(var, fmt = '%#.3f'),var)
        log <- paste0(", ",name," = ifelse(!is.na(",name,"),sprintf(",name,", fmt = '%#.3f'),",name,")")

      } else if (conv_ans == 'num4') {                                                  # convert to NUMBER WITH 4 DECIMAL PLACES
        new_var <- ifelse(!is.na(var),sprintf(var, fmt = '%#.4f'),var)
        log <- paste0(", ",name," = ifelse(!is.na(",name,"),sprintf(",name,", fmt = '%#.4f'),",name,")")

      } else if (conv_ans == 'dt1') {                                                   # convert to DATE (D-M-Y)
        var <- redcap_dates(var)
        new_var <- format(as.Date(var), "%d-%m-%Y")
        log <- paste0(", ",name," = format(as.Date(redcap_dates(",name,")), '%d-%m-%Y')")

      } else if (conv_ans == 'dt2') {                                                   # convert to DATE (Y-M-D)
        var <- redcap_dates(var)
        new_var <- as.Date(var)
        log <- paste0(", ",name," = as.Date(redcap_dates(",name,"))")

      } else if (conv_ans == 'dt_tm1') {                                                # convert to DATETIME (D-M-Y H:M)
        cat(red(bold(underline("Warning:"),
                     "\nThis conversion has not yet been defined!",
                     "\nNothing will happen here!")))
        new_var <- var

      } else if (conv_ans == 'dt_tm2') {                                                # convert to DATETIME (Y-M-D H:M)
        cat(red(bold(underline("Warning:"),
                     "\nThis conversion has not yet been defined!",
                     "\nNothing will happen here!")))
        new_var <- var

      } else if (conv_ans == 'tm1') {                                                   # convert to TIME (HH:MM:SS)
        cat(red(bold(underline("Warning:"),
                     "\nThis conversion has not yet been defined!",
                     "\nNothing will happen here!")))
        new_var <- var

      } else if (conv_ans == 'tm2') {                                                   # convert to TIME (HH:MM)
        cat(red(bold(underline("Warning:"),
                     "\nThis conversion has not yet been defined!",
                     "\nNothing will happen here!")))
        new_var <- var

      } else if (conv_ans == 'tm3') {                                                   # convert to TIME (MM:SS)
        cat(red(bold(underline("Warning:"),
                     "\nThis conversion has not yet been defined!",
                     "\nNothing will happen here!")))
        new_var <- var

      }



      ### show new summary ----

      cat("------------------------------- Conversion ------------------------------")
      cat(paste0("\n\nNew Class: ",italic(class(new_var))))
      cat("\n\nData Summary:\n")
      print(summary(new_var))
      cat(str(new_var))
      cat("\n\n")
      cat("-------------------------------------------------------------------------")
      Sys.sleep(wait)


      ## continue ----

      ### INPUT ----
      cat("\n\nContinue?")
      cat("\n1 = YES")
      cat("\n0 = NO (repeat conversion)")
      contconv_ans <- ""

      while (contconv_ans != '1' &&
             contconv_ans != '0') {

        contconv_ans <- readline(prompt="Answer= ")

        if (contconv_ans != '1' &&
            contconv_ans != '0') {

          cat("Please check your answer!")
          cat("\n1 = YES")
          cat("\n0 = NO (repeat conversion)")
          contconv_ans <- ""
        }
      }


      # finish conversion ----
      # append outcome variable
      # write log-file
      # break go-back option ( = exit while-loop, continue with for-loop)

      if (contconv_ans == '1') {
        vars_recode[[length(vars_recode)+1]] <- new_var
        names(vars_recode)[length(vars_recode)] <- name
        write.table(log, log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
        goback = FALSE
      }
    }

    # end while-loop ----

    # break for-loop
    if (for_break) {
      break
    }

    # go to next item in for-loop
    if (for_skip) {
      next
    }


    # recode ----
    # if factor

    if (is.factor(new_var)) {

      cat("\n\n")
      cat("------------------------------- Recoding --------------------------------")

      goback = TRUE
      for_break = FALSE
      for_skip = FALSE

      while (goback) {

        cat(paste0("\n\nRecoding for: ",bold(underline(name)),"   ",italic(rc_label)))
        Sys.sleep(wait)

        ## summarize codes & options ----

        if (rc_type == 'yesno') {
          choices <- "1, Yes | 0, No"
        } else if (rc_type == 'truefalse') {
          choices <- "1, True | 0, False"
        }

        choices_sep <- str_split(choices, pattern = ' \\| ')[[1]]
        codes_sep <- str_split(choices_sep, pattern = "\\, ",simplify = TRUE,n=2)[,1]
        options_sep <- str_split(choices_sep, pattern = "\\, ",simplify = TRUE,n=2)[,2]
        coded_options <- data.frame(codes_sep,options_sep)
        names(coded_options) <- c("code","label")

        # add missing codes
        if (!is.na(missing_codes)) {
          missing_choices_sep <- str_split(missing_codes, pattern = ' \\| ')[[1]]
          missing_codes_sep <- str_split(missing_choices_sep, pattern = "\\, ",simplify = TRUE,n=2)[,1]
          missing_options_sep <- str_split(missing_choices_sep, pattern = "\\, ",simplify = TRUE,n=2)[,2]
          coded_options <- coded_options |>
            rbind(data.frame(code = missing_codes_sep,
                             label = missing_options_sep
                             ))
        }

        levels <- levels(new_var)

        cat(underline("\n\nFactor levels found in data table:\n\n"))
        cat(levels, sep = "\n")
        cat(underline("\n\nCoding according to REDCap:\n\n"))
        print(coded_options,row.names = FALSE)
        Sys.sleep(wait)



        ### auto recoding ----

        sugg_coding <- data.frame(data = character(),
                                  to = character(),
                                  rc = character(),
                                  code = character(),
                                  stringsAsFactors = FALSE)

        for (l in seq_along(levels)) {

          level = levels[l]

          # compare code
          sim_code <- stringdist::stringsim(tolower(level),tolower(coded_options$code))
          # compare option label
          sim_option <-stringdist::stringsim(tolower(level),tolower(coded_options$label))

          # no match
          if (!any(sim_code > 0.8) && !any(sim_option > 0.8)) {
            sugg_coding <- sugg_coding |>
              rbind(data.frame(data = level,
                               to = "   ",
                               rc = "(no match found)",
                               code = NA,
                               stringsAsFactors = FALSE
              ))
            next
          }

          # multiple matches
          if (sum(sim_code > 0.8) > 1 | sum(sim_option > 0.8) > 1) {
            sugg_coding <- sugg_coding |>
              rbind(data.frame(data = level,
                               to = "   ",
                               rc = "(mutliple matches found)",
                               code = NA,
                               stringsAsFactors = FALSE
              ))
            next
          }

          # code and option match
          if (any(sim_code > 0.8) && any(sim_option > 0.8)) {
            sim_option <- NULL # code is more relevant than label
          }

          # code match
          if (any(sim_code > 0.8)) {
            sugg_coding <- sugg_coding |>
              rbind(data.frame(data = level,
                               to = "   ",
                               rc = paste0(coded_options$code[sim_code > 0.8]," (",rc_option = coded_options$label[sim_code > 0.8],")"),
                               code = coded_options$code[sim_code > 0.8],
                               stringsAsFactors = FALSE
              ))
          }

          # option match
          if (any(sim_option > 0.8)) {
            sugg_coding <- sugg_coding |>
              rbind(data.frame(data = level,
                               to = "   ",
                               rc = paste0(coded_options$code[sim_option > 0.8]," (",rc_option = coded_options$label[sim_option > 0.8],")"),
                               code = coded_options$code[sim_option > 0.8],
                               stringsAsFactors = FALSE
              ))
          }
        }

        cat(underline("\n\nSuggestion:"))
        print(kable(sugg_coding[1:3],col.names = c("Factor Level in Data Table","","Suggested Recoding")))
        Sys.sleep(wait)


        ### manual recoding ----

        #### INPUT ----

        if (!auto_recode) {

          cat("\n\nWould you like to recode this variable as suggested or do it manually?")
          cat("\n 1 = recode as suggested")
          cat("\n 0 = recode manually")
          cat("\n 'skip' = skip this variable")
          cat("\n 'exit' = stop loop")
          manrec_ans <- ""

          while (manrec_ans != '1' &&
                 manrec_ans != '0' &&
                 manrec_ans != "skip" &&
                 manrec_ans != "exit") {

            manrec_ans <- readline(prompt="Answer= ")

            if (manrec_ans != '1' &&
                manrec_ans != '0' &&
                manrec_ans != "skip" &&
                manrec_ans != "exit") {

              cat("Please check your answer!")
              cat("\n 1 = recode as suggested")
              cat("\n 0 = recode manually")
              cat("\n 'skip' = skip this variable")
              cat("\n 'exit' = stop loop")
              manrec_ans <- ""
            }
          }
        }


        ##### exit ----
        # (break while-loop here, for-loop will be broken below)

        if (manrec_ans == 'exit') {
          for_break = TRUE
          break
        }

        ##### skip ----
        # (break while-loop here, for-loop will be nexted below)

        if (manrec_ans == 'skip') {
          for_skip = TRUE
          break
        }

        ### RECODING ----

        ##### auto ----

        if (manrec_ans == '1') {

          log <- character()
          recoded_var <- new_var

          for (l in seq_along(levels)) {

            level <- levels[l]
            levels(recoded_var)[which(levels(recoded_var) == level)] <- sugg_coding$code[l]
            log <- c(log,paste0(", '",level,"' = ",sugg_coding$code[l]))
          }
        }


        ##### manual ----

        if (manrec_ans == '0') {

          stop_rec = FALSE
          log <- character()
          recoded_var <- new_var

          cat("\n\n")
          cat("--------------------------- Manual Recoding -----------------------------")
          cat("\n\nIn the following, we will go through the factor levels of the variable in the data table.\n")
          cat("The levels will be compared to the codes as defined in the codebook in REDCap.\n\n")
          cat(paste0("Variable: ",bold(underline(name)),"   ",italic(rc_label)))


          # loop through factor levels and compare with codes in REDCap
          for (l in seq_along(levels)) {
            level <- as.character(levels[l])

            cat("\n\n\nFactor level:", bold(underline(level)))
            cat("\n\n\nCodebook:\n\n")
            print(coded_options,row.names = FALSE)

            ###### INPUT ----

            cat("\nPlease type matching code from the codebook.")
            cat("\n 'empty' = delete value, field will be empty")
            cat("\n 'skip' = do NOT recode this option")
            cat("\n 'stop' = stop recoding, skip this variable")
            cat("\n 'exit' = stop loop")
            rec_ans <- ""

            while (!any(str_detect(coded_options$code,paste0("^",rec_ans,"$"))) &&
                   rec_ans != 'empty' &&
                   rec_ans != 'stop' &&
                   rec_ans != 'skip' &&
                   rec_ans != 'exit') {

              rec_ans <- readline(prompt="Code= ")

              if (!any(str_detect(coded_options$code,paste0("^",rec_ans,"$"))) &&
                  rec_ans != 'empty' &&
                  rec_ans != 'stop' &&
                  rec_ans != 'skip' &&
                  rec_ans !='exit') {

                cat("Code not recognized: Please try again!")
                cat("\n 'empty' = delete value, field will be empty")
                cat("\n 'skip' = do NOT recode this option")
                cat("\n 'stop' = stop recoding, skip this variable")
                cat("\n 'exit' = stop loop")
                rec_ans <- ""
              }
            }

            ###### exit ----
            # (break for-loop, while-loop will be broken below)

            if (rec_ans == 'exit') {
              for_break = TRUE
              break
            }

            ###### stop ----
            # break for-loop
            if (rec_ans == 'stop') {
              for_skip = TRUE
              break
            }

            ###### skip ----
            # continue for-loop
            if (rec_ans == 'skip') {
              next
            }

            ###### set to missing ----
            if (rec_ans == 'empty') {
              levels(recoded_var)[which(levels(recoded_var) == level)] <- NA
              log <- c(log,paste0(", '",level,"' = ",rec_ans))
            }

            ###### recode ----
            if (any(str_detect(coded_options$code,paste0("^",rec_ans,"$")))) {
              levels(recoded_var)[which(levels(recoded_var) == level)] <- rec_ans
              log <- c(log,paste0(", '",level,"' = ",rec_ans))
            }
          }

          ###### end for-loop ----

          # (break while-loop here, for-loop will be broken below)
          if (for_break) {
            break
          }

          # (break while-loop here, for-loop will be nexted below)
          if (for_skip) {
            break
          }

        } # end manual recoding




        ### show new summary ----

        if (identical(new_var,recoded_var)) {
          cat("\n\nThe coding matches with REDCap. No conversion needed!")
        } else if (!identical(new_var,recoded_var)) {
          cat("\n\n")
          cat("-------------------------- Recoding completed ---------------------------")
          cat(paste0("\n\nSummary for: ",bold(underline(name)),"   ",italic(rc_label)))
          cat(underline("\n\n\nCodebook:\n\n"))
          print(coded_options,row.names = FALSE)
          cat(underline("\n\nSummary before recoding:\n\n"))
          print(summary(new_var))
          cat(underline("\n\nSummary after recoding:\n\n"))
          print(summary(recoded_var))
        }

        cat("-------------------------------------------------------------------------")
        Sys.sleep(wait)


        ### continue ----

        #### INPUT ----
        cat("\n\nContinue?")
        cat("\n1 = YES")
        cat("\n0 = NO (repeat recoding)")
        contrec_ans <- ""

        while (contrec_ans != '1' &&
               contrec_ans != '0') {

          contrec_ans <- readline(prompt="Answer= ")

          if (contrec_ans != '1' &&
              contrec_ans != '0') {

            cat("Please check your answer!")
            cat("\n1 = YES")
            cat("\n0 = NO (repeat recoding)")
            contrec_ans <- ""
          }
        }

        ### finish recoding ----
        if (contrec_ans == '1') {
          vars_recode[[length(vars_recode)+1]] <- recoded_var
          names(vars_recode)[length(vars_recode)] <- name
          write.table(paste0(", ",name," = recode(",name), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
          write.table(paste0(log, sep=""), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
          write.table(")", log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
          goback = FALSE
        }
      }

      ## end while-loop ----
      # break for-loop
      if (for_break) {
        break
      }


      # go to next item in for-loop
      if (for_skip) {
        next
      }



    } # end if(is.factor)

  } # end for-loop






  # RECODE DATA
  recoded_data <- mutate(selected_data,!!!vars_recode)


  # finalize log-file
  write.table(")\n--------------------------------------------------------------------------------------------------\n", log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


  # Return Output
  cat("ALL DONE!!")
  return(recoded_data)




}




