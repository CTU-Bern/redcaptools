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
#' # redcap_recode(importdemo_data, importdemo_dict)
#'
#' # if using local data:
#' # token <- "xxxxx"
#' # url <- "xxxxx"
#' # file <- "data.csv"
#' # redcap_recode(file, rc_token = token, rc_url = url)



redcap_recode <- function(selected_data,
                          dict = NULL,
                          rc_token,
                          rc_url) {

  field_name <- field_type <- select_choices_or_calculations <- text_validation_type_or_show_slider_number <- NULL


  # load data
  name_vars <- colnames(selected_data)

  if(is.null(dict)) dict <- redcap_export_meta(rc_token, rc_url)$meta
  rc_spec <- select(dict,
                    field_name,
                    field_type,
                    choices = select_choices_or_calculations,
                    validation = text_validation_type_or_show_slider_number)

  # open log-file
  log_file <- "redcap_recode.txt"
  write.table(paste0(Sys.time(),":\n\nrecoded_data <- mutate(selected_data"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

  # prepare output variables
  vars_recode <- list()

  for (i in seq_along(name_vars)) {

    goback = TRUE                                                                  # initiate go-back option
    for_break = FALSE
    for_skip = FALSE
    while (goback) {


      var <- selected_data[,i]
      name <- name_vars[i]
      rc_class <- rc_spec$field_type[grep(paste0("^",name,"$"),rc_spec$field_name)]
      rc_val <- rc_spec$validation[grep(paste0("^",name,"$"),rc_spec$field_name)]
      choices <- rc_spec$choices[grep(paste0("^",name,"$"),rc_spec$field_name)]

      cat(paste0("\n",blue(bold(underline(name)))))
      cat("\n\nSummary:\n")
      print(summary(var))
      cat(str(var))

      cat(paste0("\nCurrent Class: ", class(var)))
      cat(paste0("\nType according to REDCap: ", rc_class," ",rc_val,"\n"))

      cat("\nWould you like to change the type/class?
      1 = YES
      0 = NO
      'exit' = stop loop")
      ans <- ""
      while (ans != '1' && ans != '0' && ans != 'exit') {                          # input for change type/class
        ans <- readline(prompt="Answer= ")
        if (ans != '1' && ans != '0' && ans != 'exit') {
          cat("Please check your answer!
          1 = YES
          0 = NO
          'exit' = stop loop")
        }
      }


      if (ans == 'exit') {                                                         # check answers:
        for_break = TRUE
        break                                                                      # exit (break while-loop, for-loop will be broken below)

      } else  if (ans == "0") {
        for_skip = TRUE
        break                                                                      # do not change type/class (break while-loop, for-loop will be nexted below)

      } else if (ans == "1") {                                                     # change type/class

        cat("\nWhat should the new type/class be? \n
        'fct' = factor (e.g., Radiobutton) \n
        'int' = integer
        'num' = number
        'num1' = number with 1 decimal place
        'num2' = number with 2 decimal places
        'num3' = number with 3 decimal places
        'num4' = number with 4 decimal places\n
        'dt1' = date (D-M-Y)
        'dt2' = date (Y-M-D)
        'dt_tm1' = datetime (D-M-Y H:M)
        'dt_tm2' = datetime (Y-M-D H:M)\n
        'tm1' = time (HH:MM:SS)
        'tm2' = time (HH:MM)
        'tm3' = time (MM:SS)\n
        'exit' = do NOT change and stop loop
        'skip' = do NOT change and move to next item")
        ans <- ""
        while (ans != 'int' && ans !=  'num' && ans != 'num1' && ans != 'num2' && ans != 'num3' && ans != 'num4' && ans != 'dt1' && ans != 'dt2' && ans != 'dt_tm1' && ans != 'dt_tm2' && ans != 'tm1' && ans != 'tm2' && ans != 'tm3' && ans != 'fct' && ans != 'skip' && ans != 'exit') {
          ans <- readline(prompt="Answer= ")                                         # input for change type/class
          if (ans != 'int' && ans !=  'num' && ans != 'num1' && ans != 'num2' && ans != 'num3' && ans != 'num4' && ans != 'dt1' && ans != 'dt2' && ans != 'dt_tm1' && ans != 'dt_tm2' && ans != 'tm1' && ans != 'tm2' && ans != 'tm3' && ans != 'fct' && ans != 'skip' && ans != 'exit') {
            cat("Please check your answer! \n
        'fct' = factor (e.g., Radiobutton)\n
        'int' = integer
        'num' = number
        'num1' = numeric with 1 decimal place
        'num2' = numeric with 2 decimal places
        'num3' = numeric with 3 decimal places
        'num4' = numeric with 4 decimal places\n
        'dt1' = date (D-M-Y)
        'dt2' = date (Y-M-D)
        'dt_tm1' = datetime (D-M-Y H:M)
        'dt_tm2' = datetime (Y-M-D H:M)\n
        'tm1' = time (HH:MM:SS)
        'tm2' = time (HH:MM)
        'tm3' = time (MM:SS)\n
        'exit' = do NOT change and stop loop
        'skip' = do NOT change and move to next item")
          }
        }


        if (ans == 'exit') {                                                         # check answers:
          for_break = TRUE
          break                                                                      # exit (break while-loop, for-loop will be broken below)

        } else if (ans == 'skip') {
          for_skip = TRUE
          break                                                                      # skip (break while-loop, for-loop will be nexted below)

        } else if (ans == 'fct') {                                                   # convert to FACTOR
          new_var <- as.factor(var)
          log <- paste0(", ",name," = as.factor(",name,")")

        } else if (ans == 'int') {                                                   # convert to INTEGER
          new_var <- as.integer(var)
          log <- paste0(", ",name," = as.integer(",name,")")

        } else if (ans == 'num') {                                                   # convert to NUMBER
          new_var <- as.numeric(var)
          log <- paste0(", ",name," = as.numeric(",name,")")

        } else if (ans == 'num1') {                                                  # convert to NUMBER WITH 1 DECIMAL PLACE
          new_var <- ifelse(!is.na(var),sprintf(var, fmt = '%#.1f'),var)
          log <- paste0(", ",name," = ifelse(!is.na(",name,"),sprintf(",name,", fmt = '%#.1f'),",name,")")

        } else if (ans == 'num2') {                                                  # convert to NUMBER WITH 2 DECIMAL PLACES
          new_var <- ifelse(!is.na(var),sprintf(var, fmt = '%#.2f'),var)
          log <- paste0(", ",name," = ifelse(!is.na(",name,"),sprintf(",name,", fmt = '%#.2f'),",name,")")

        } else if (ans == 'num3') {                                                  # convert to NUMBER WITH 3 DECIMAL PLACES
          new_var <- ifelse(!is.na(var),sprintf(var, fmt = '%#.3f'),var)
          log <- paste0(", ",name," = ifelse(!is.na(",name,"),sprintf(",name,", fmt = '%#.3f'),",name,")")

        } else if (ans == 'num4') {                                                  # convert to NUMBER WITH 4 DECIMAL PLACES
          new_var <- ifelse(!is.na(var),sprintf(var, fmt = '%#.4f'),var)
          log <- paste0(", ",name," = ifelse(!is.na(",name,"),sprintf(",name,", fmt = '%#.4f'),",name,")")

        } else if (ans == 'dt1') {                                                   # convert to DATE (D-M-Y)
          var <- redcap_dates(var)
          new_var <- format(as.Date(var), "%d-%m-%Y")
          log <- paste0(", ",name," = format(as.Date(redcap_dates(",name,")), '%d-%m-%Y')")

        } else if (ans == 'dt2') {                                                   # convert to DATE (Y-M-D)
          var <- redcap_dates(var)
          new_var <- as.Date(var)
          log <- paste0(", ",name," = as.Date(redcap_dates(",name,"))")

        } else if (ans == 'dt_tm1') {                                                # convert to DATETIME (D-M-Y H:M)
          cat(red(bold(underline("Warning:"),
                       "\nThis conversion has not yet been defined!",
                       "\nNothing will happen here!")))
          new_var <- var

        } else if (ans == 'dt_tm2') {                                                # convert to DATETIME (Y-M-D H:M)
          cat(red(bold(underline("Warning:"),
                       "\nThis conversion has not yet been defined!",
                       "\nNothing will happen here!")))
          new_var <- var

        } else if (ans == 'tm1') {                                                   # convert to TIME (HH:MM:SS)
          cat(red(bold(underline("Warning:"),
                       "\nThis conversion has not yet been defined!",
                       "\nNothing will happen here!")))
          new_var <- var

        } else if (ans == 'tm2') {                                                   # convert to TIME (HH:MM)
          cat(red(bold(underline("Warning:"),
                       "\nThis conversion has not yet been defined!",
                       "\nNothing will happen here!")))
          new_var <- var

        } else if (ans == 'tm3') {                                                   # convert to TIME (MM:SS)
          cat(red(bold(underline("Warning:"),
                       "\nThis conversion has not yet been defined!",
                       "\nNothing will happen here!")))
          new_var <- var

        }

        # show new summary
        cat(paste0("\n",bold(underline(name))))
        cat("\n\nNew Summary:\n")
        print(summary(new_var))
        cat(str(new_var))
        cat(paste0("\nNew Class: ", class(new_var)))
        cat(paste0("\nType according to REDCap: ", rc_class," ",rc_val,"\n"))

        cat("\nContinue?
          1 = YES
          0 = NO repeat conversion")
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
          vars_recode[[length(vars_recode)+1]] <- new_var
          names(vars_recode)[length(vars_recode)] <- name
          write.table(log, log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
          goback = FALSE
        }
      }
    }

    if (for_break) {                                                               # break for-loop
      break
    }
    if (for_skip) {                                                                # go to next item in for-loop
      next
    }


    if (is.factor(new_var)) {                                                      # recode options if factor!

      cat(bold(paste0("\nVariable ",underline(name)),"is a factor. Please compare coding to REDCap!!"))

      goback = TRUE                                                                  # initiate new go-back option
      while_break = FALSE
      while (goback) {

        # parse and separate codes from options

        if (rc_class == 'yesno') {
          choices <- "1, Yes | 0, No"
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
        print(coded_options)

        cat("\n\nDo you want to recode this variable?\n  1 = YES \n  0 = NO (move to next variable)")
        ans <- ""
        while (ans != '1' && ans != '0') {                                         # recoding?
          ans <- readline(prompt="Answer= ")
          if (ans != '1' && ans != '0') {
            cat("Please check your answer!
            1 = YES start recoding now
            0 = NO move to next variable")
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
              cat("\nPlease type matching code from the list above.\n 'NA' = set to missing value \n 'exit' = do Not recode this item \n 'skip' = do NOT recode this option")
              ans <- ""
              while (!any(str_detect(coded_options$codes,paste0("^",ans,"$"))) && ans != 'exit' && ans != 'skip' && ans != 'NA') {
                ans <- readline(prompt="Code= ")
                if (!any(str_detect(coded_options$codes,paste0("^",ans,"$"))) && ans !='exit' && ans != 'skip' && ans != 'NA') {
                  cat("Code not recognized: Please try again!\n 'NA' = set to missing value \n 'exit' = do Not recode this item \n 'skip' = do NOT recode this option")
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




