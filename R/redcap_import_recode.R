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
#'@param rc_token REDCap API token
#'@param rc_url Link to REDCap API. Default: https://redcap.ctu.unibe.ch/api/
#'@param dict Data dictionary (e.g. as downloaded from REDCap or via
#'  \code{redcap_export_meta(rc_token, rc_url)$meta}). If not supplied, this will
#'  be downloaded from the API using \code{rc_token}.
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
                                 rc_token,
                                 rc_url,
                                 auto_conv = FALSE,
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
    dict <- redcap_export_meta(rc_token, rc_url)$meta
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
    ans <- ""
    while (ans != 1) {
      ans <- readline(prompt="Answer= ")
      if (ans != 1) {
        cat("Please check your answer! \n 1 = YES\n'esc' = STOP")
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

    ## initiate go-back option ----
    goback = TRUE
    for_break = FALSE
    for_skip = FALSE

    while (goback) {

      ## summarise data and rc dict ----

      var <- selected_data[,i]
      name <- name_vars[i]
      cat(paste0("\n",blue(bold(underline(name))),"\n\n"))

      cat("--------------------------------- REDCap ---------------------------------")

      if (!any(str_detect(rc_spec$field_name,paste0("^",name_vars[i],"$")))) {
        cat("\n\nThis variable is NOT part of the data dictionary you provided and will be skipped.\n")
        cat("Please run redcap_import_select() to choose and rename variables before recoding.\n\n")
        for_skip = TRUE
        break
      }

      cat("\n\nVariable found in REDCap!")
      rc_label <- rc_spec$field_label[grep(paste0("^",name,"$"),rc_spec$field_name)]
      rc_type <- rc_spec$field_type[grep(paste0("^",name,"$"),rc_spec$field_name)]
      rc_val <- rc_spec$validation[grep(paste0("^",name,"$"),rc_spec$field_name)]
      choices <- rc_spec$choices[grep(paste0("^",name,"$"),rc_spec$field_name)]


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


      ## auto conversion ----
      conv <- ''

      ### character ----
      if (rc_type == "text" && is.na(rc_val)) {
        conv <- "char"
        conv_label <- "Character"
      }


      ### factor ----
      if (rc_type == "truefalse") {
        conv <- "fct"
        conv_label <- "Factor"
      }


      cat(bold(paste0("\n\nSuggested conversion to Class: ",italic(conv_label))))
      cat("\n\n\n")


      ## manual conversion ----

      ### INPUT ----

      if (!auto_conv) {

        cat("Would you like to convert or manually change the type/class?")
        cat("\n 1 = convert as suggested")
        cat("\n 0 = change manually")
        cat("\n 'skip' = skip this variable")
        cat("\n 'exit' = stop loop")
        ans <- ""

        while (ans != '1' &&
               ans != '0' &&
               ans != 'skip' &&
               ans != 'exit') {

          ans <- readline(prompt="Answer= ")

          if (ans != '1' &&
              ans != '0' &&
              ans != 'skip' &&
              ans != 'exit') {

            cat("Please check your answer!")
            cat("\n 1 = convert as suggested")
            cat("\n 0 = change manually")
            cat("\n 'skip' = skip this variable")
            cat("\n 'exit' = stop loop")
            ans <- ""
          }
        }
      } else {
        cat("Auto-conversion has been turned on! (To turn it off, set 'auto_conv = FALSE')\n\n\n")
        Sys.sleep(wait)
      }


      ### EVALUATE ----

      #### exit ----
      # (break while-loop here, for-loop will be broken below)

      if (ans == 'exit') {
        for_break = TRUE
        break
      }

      #### skip ----
      # do not change type/class
      # (break while-loop here, for-loop will be nexted below)

      if (ans == "skip") {
        for_skip = TRUE
        break
      }

      #### change manually ----

      ##### INPUT ----
      if (ans == "0") {

        cat("\nWhat should the new type/class be?\n")
        cat(" 'char' = character\n")
        cat(" 'fct' = factor (e.g., Radiobutton)\n")
        cat(" 'int' = integer\n")
        cat(" 'num' = number\n")
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
        conv <- ""

        while (conv != 'char' &&
               conv != 'int' &&
               conv != 'num' &&
               conv != 'num1' &&
               conv != 'num2' &&
               conv != 'num3' &&
               conv != 'num4' &&
               conv != 'dt1' &&
               conv != 'dt2' &&
               conv != 'dt_tm1' &&
               conv != 'dt_tm2' &&
               conv != 'tm1' &&
               conv != 'tm2' &&
               conv != 'tm3' &&
               conv != 'fct' &&
               conv != 'skip' &&
               conv != 'exit') {

          conv <- readline(prompt="Answer= ")                                         # input for change type/class

          if (conv != 'char' &&
              conv != 'int' &&
              conv != 'num' &&
              conv != 'num1' &&
              conv != 'num2' &&
              conv != 'num3' &&
              conv != 'num4' &&
              conv != 'dt1' &&
              conv != 'dt2' &&
              conv != 'dt_tm1' &&
              conv != 'dt_tm2' &&
              conv != 'tm1' &&
              conv != 'tm2' &&
              conv != 'tm3' &&
              conv != 'fct' &&
              conv != 'skip' &&
              conv != 'exit') {

            cat("Please check your answer! \n")
            cat(" 'char' = character\n")
            cat(" 'fct' = factor (e.g., Radiobutton)\n")
            cat(" 'int' = integer\n")
            cat(" 'num' = number\n")
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
            conv <- ""
          }
        }
      }



      ##### EVALUATE ----

      ###### exit ----
      # (break while-loop, for-loop will be broken below)
      if (conv == 'exit') {
        for_break = TRUE
        break
      }

      ###### skip ----
      # (break while-loop, for-loop will be nexted below)
      if (conv == 'skip') {
        for_skip = TRUE
        break
      }

      ###### convert ----
      if (conv == 'char') {                                                           # convert to CHARACTER
        new_var <- as.character(var)
        log <- paste0(", ",name," = as.character(",name,")")

      } else if (conv == 'fct') {                                                   # convert to FACTOR
        new_var <- as.factor(var)
        log <- paste0(", ",name," = as.factor(",name,")")

      } else if (conv == 'int') {                                                   # convert to INTEGER
        new_var <- as.integer(var)
        log <- paste0(", ",name," = as.integer(",name,")")

      } else if (conv == 'num') {                                                   # convert to NUMBER
        new_var <- as.numeric(var)
        log <- paste0(", ",name," = as.numeric(",name,")")

      } else if (conv == 'num1') {                                                  # convert to NUMBER WITH 1 DECIMAL PLACE
        new_var <- ifelse(!is.na(var),sprintf(var, fmt = '%#.1f'),var)
        log <- paste0(", ",name," = ifelse(!is.na(",name,"),sprintf(",name,", fmt = '%#.1f'),",name,")")

      } else if (conv == 'num2') {                                                  # convert to NUMBER WITH 2 DECIMAL PLACES
        new_var <- ifelse(!is.na(var),sprintf(var, fmt = '%#.2f'),var)
        log <- paste0(", ",name," = ifelse(!is.na(",name,"),sprintf(",name,", fmt = '%#.2f'),",name,")")

      } else if (conv == 'num3') {                                                  # convert to NUMBER WITH 3 DECIMAL PLACES
        new_var <- ifelse(!is.na(var),sprintf(var, fmt = '%#.3f'),var)
        log <- paste0(", ",name," = ifelse(!is.na(",name,"),sprintf(",name,", fmt = '%#.3f'),",name,")")

      } else if (conv == 'num4') {                                                  # convert to NUMBER WITH 4 DECIMAL PLACES
        new_var <- ifelse(!is.na(var),sprintf(var, fmt = '%#.4f'),var)
        log <- paste0(", ",name," = ifelse(!is.na(",name,"),sprintf(",name,", fmt = '%#.4f'),",name,")")

      } else if (conv == 'dt1') {                                                   # convert to DATE (D-M-Y)
        var <- redcap_dates(var)
        new_var <- format(as.Date(var), "%d-%m-%Y")
        log <- paste0(", ",name," = format(as.Date(redcap_dates(",name,")), '%d-%m-%Y')")

      } else if (conv == 'dt2') {                                                   # convert to DATE (Y-M-D)
        var <- redcap_dates(var)
        new_var <- as.Date(var)
        log <- paste0(", ",name," = as.Date(redcap_dates(",name,"))")

      } else if (conv == 'dt_tm1') {                                                # convert to DATETIME (D-M-Y H:M)
        cat(red(bold(underline("Warning:"),
                     "\nThis conversion has not yet been defined!",
                     "\nNothing will happen here!")))
        new_var <- var

      } else if (conv == 'dt_tm2') {                                                # convert to DATETIME (Y-M-D H:M)
        cat(red(bold(underline("Warning:"),
                     "\nThis conversion has not yet been defined!",
                     "\nNothing will happen here!")))
        new_var <- var

      } else if (conv == 'tm1') {                                                   # convert to TIME (HH:MM:SS)
        cat(red(bold(underline("Warning:"),
                     "\nThis conversion has not yet been defined!",
                     "\nNothing will happen here!")))
        new_var <- var

      } else if (conv == 'tm2') {                                                   # convert to TIME (HH:MM)
        cat(red(bold(underline("Warning:"),
                     "\nThis conversion has not yet been defined!",
                     "\nNothing will happen here!")))
        new_var <- var

      } else if (conv == 'tm3') {                                                   # convert to TIME (MM:SS)
        cat(red(bold(underline("Warning:"),
                     "\nThis conversion has not yet been defined!",
                     "\nNothing will happen here!")))
        new_var <- var

      }



      ## show new summary ----

      cat("------------------------------- Conversion ------------------------------")
      cat(paste0("\n\nNew Class: ",italic(class(new_var))))
      cat("\n\nData Summary:\n")
      print(summary(new_var))
      cat(str(new_var))
      cat("\n\n")
      cat("-------------------------------------------------------------------------")


      ## continue ----

      ### INPUT ----
      cat("\n\nContinue?")
      cat("\n1 = YES")
      cat("\n0 = NO (repeat conversion)")
      ans <- ""

      while (ans != '1' &&
             ans != '0') {

        ans <- readline(prompt="Answer= ")

        if (ans != '1' &&
            ans != '0') {

          cat("Please check your answer!")
          cat("\n1 = YES")
          cat("\n0 = NO (repeat conversion)")
        }
      }


      ### EVALUATE ----

      ## finish conversion ----
      # append outcome variable
      # write log-file
      # break go-back option ( = exit while-loop, continue with for-loop)

      if (ans == '1') {
        vars_recode[[length(vars_recode)+1]] <- new_var
        names(vars_recode)[length(vars_recode)] <- name
        write.table(log, log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
        goback = FALSE
      }
    }

    ## exit ----
    # break for-loop
    if (for_break) {
      break
    }

    ## skip ----
    # go to next item in for-loop
    if (for_skip) {
      next
    }


    # recode  ----
    # if factor

    if (is.factor(new_var)) {

      cat(paste0("\n",blue(bold(underline(name))),"\n"))

      ## initiate new go-back option ----
      goback = TRUE
      while_break = FALSE

      while (goback) {

        ### summarize codes & options ----

        if (rc_type == 'yesno') {
          choices <- "1, Yes | 0, No"
        } else if (rc_type == 'truefalse') {
          choices <- "1, True | 0, False"
        }

        choices_sep <- str_split(choices, pattern = ' \\| ')[[1]]
        codes_sep <- str_split(choices_sep, pattern = "\\, ",simplify = TRUE,n=2)[,1]
        options_sep <- str_split(choices_sep, pattern = "\\, ",simplify = TRUE,n=2)[,2]
        coded_options <- data.frame(codes_sep,options_sep)
        names(coded_options) <- c("codes","options")
        levels <- levels(new_var)

        cat(underline("\n\nFactor levels found in data table:\n\n"))
        cat(levels, sep = "\n")
        cat(underline("\n\nCoding according to REDCap:\n\n"))
        print(coded_options,row.names = FALSE)



        ### auto recoding ----

        #cat(underline("\n\nSuggested recoding:"))

        for (l in seq_along(levels)) {
          sim_code <- stringdist::stringsim(tolower(levels[l]),tolower(coded_options$codes))
          sim_option <-stringdist::stringsim(tolower(levels[l]),tolower(coded_options$options))

          # HIER!!!!
          # try to build an output dataframe that can be printed afterwards!
          # take either the code or the option depending on where the match was made
          # but show always all three (level in data table, code in rc, label in rc)
          # write NA if no match
          # also write NA if multiple matches (should not happen ot often)

          coded_options$options[sim_option > 0.8]
          cat(levels[l],coded_options$options[sim_option > 0.8])

        }


        # cat("\n\nWould you like to recode this variable as suggested or do it manually?")
        # cat("\n 1 = recode as suggested")
        # cat("\n 0 = recode manually")
        # cat("\n 'skip' = skip this variable")
        # cat("\n 'exit' = stop loop")


        cat("Do you want to recode this variable?")
        cat("\n  1 = YES (start recoding now)")
        cat("\n  0 = NO (move to next variable)")
        ans <- ""

        while (ans != '1' && ans != '0') {                                         # recoding?
          ans <- readline(prompt="Answer= ")
          if (ans != '1' && ans != '0') {
            cat("Please check your answer!")
            cat("\n  1 = YES (start recoding now)")
            cat("\n  0 = NO (move to next variable)")
          }
        }

        if (ans == '0') {
          break
        } else if (ans == '1') {
          cat(bold("\n\n----------------START RECODING----------------\n\n"))
          cat(bold(underline(name)))
          log <- character()
          recoded_var <- new_var

          for (l in seq_along(levels)) {                                               # loop through factor levels and compare with codes in REDCap
            lvl <- as.character(levels[l])

            if (!any(str_detect(coded_options$codes,paste0("^",lvl,"$")))) {

              cat("\n\n\nNo matching code found for factor level:", bold(underline(lvl)))
              cat("\n\nPlease choose maching code from REDCap Dictionnary:\n")
              print(coded_options)

              cat("\nPlease type matching code from the list above.")
              cat("\n 'NA' = set to missing value")
              cat("\n 'exit' = do Not recode this item")
              cat("\n 'skip' = do NOT recode this option")
              ans <- ""

              while (!any(str_detect(coded_options$codes,paste0("^",ans,"$"))) &&
                     ans != 'exit' &&
                     ans != 'skip' &&
                     ans != 'NA') {

                ans <- readline(prompt="Code= ")

                if (!any(str_detect(coded_options$codes,paste0("^",ans,"$"))) &&
                    ans !='exit' &&
                    ans != 'skip' &&
                    ans != 'NA') {

                  cat("Code not recognized: Please try again!")
                  cat("\n 'NA' = set to missing value")
                  cat("\n 'exit' = do Not recode this item")
                  cat("\n 'skip' = do NOT recode this option")
                  ans <- ""
                }
              }

              if (ans == 'exit') {                                                         # check answers:
                while_break = TRUE
                break                                                                      # exit (break for-loop, while-loop will be broken below)

              } else if (ans == 'skip') {
                next                                                                       # do not recode

              } else if (ans == 'NA') {                                                    # set to missing
                levels(recoded_var)[grep(paste0("^",lvl,"$"),levels(recoded_var))] <- NaN
                log <- c(log,paste0(", '",lvl,"' = ",ans))

              } else {                                                                     # recode
                levels(recoded_var)[grep(paste0("^",lvl,"$"),levels(recoded_var))] <- ans
                log <- c(log,paste0(", '",lvl,"' = ",ans))
              }
            }
          }

          if (while_break) {                                                              # break while-loop, continue for-loop
            break
          }


          # show new summary if changes have been made
          if (identical(new_var,recoded_var)) {
            cat("\n\nThe coding matches with REDCap. No conversion needed!")
            goback = FALSE
          } else if (!identical(new_var,recoded_var)) {
            cat("\n----------------RECODING COMPLETED----------------")
            cat(paste0("\n\nSUMMARY FOR: ",bold(underline(name))))
            cat("\n\nCodebook:\n\n")
            print(coded_options)
            cat("\n\nSummary before recoding:\n\n")
            print(table(original = new_var, recoded = recoded_var, useNA = "ifany"))
            # cat("\n\nSummary with recoded options:\n\n")
            # print(summary(recoded_var))

            cat("\nContinue?
          1 = YES
          0 = NO repeat recoding")
            ans <- ""
            while (ans != '1' && ans != '0') {                                         # go back?
              ans <- readline(prompt="Answer= ")
              if (ans != '1' && ans != '0') {
                cat("Please check your answer!
          1 = YES
          0 = NO repeat conversion")
              }
            }

            if (ans == '1') {                                                          # exit while-loop, continue for-loop
              vars_recode[[length(vars_recode)+1]] <- recoded_var
              names(vars_recode)[length(vars_recode)] <- name
              write.table(paste0(", ",name," = recode(",name), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
              write.table(paste0(log, sep=""), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
              write.table(")", log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
              goback = FALSE
            }
          }
        }
      }
    }
  }




  # RECODE DATA
  recoded_data <- mutate(selected_data,!!!vars_recode)


  # finalize log-file
  write.table(")\n--------------------------------------------------------------------------------------------------\n", log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


  # Return Output
  cat("ALL DONE!!")
  return(recoded_data)




}




