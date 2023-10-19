#'REDCap Select and Rename
#'
#'This function loops through all the variable names of a data set and lets the
#'user compare them with the variable names set up in REDCap. \cr The REDCap
#'data dictionary can either be directly provided or downloaded from the REDCap
#'project by providing an API token and matching URL. \cr For variables with
#'matching names in REDCap, the user can decide to automatically select them
#'without renaming. If auto-selecting is turned off, the user can decide to not
#'select these variables at all or to select and rename them. \cr For variables
#'without matching names in REDCap, the user will always be prompted to decide
#'either to not select these variables at all or to select and rename them. \cr
#'The function returns a data frame with the selected/renamed variables, writes
#'an overview csv-table, and a short summary with the executed code to a
#'log-file for copy-pasting and adjusting/reusing.
#'
#'
#'@param import_data Data frame to be imported
#'@param dict Data dictionary (e.g. as
#'  downloaded from REDCap or via \code{redcap_export_meta(rc_token,
#'  rc_url)$meta}). If not supplied, this will be downloaded from the API using
#'  \code{rc_token} and \code{rc_url}.
#'@param rc_token REDCap API token
#'@param rc_url Link to REDCap API
#'@param forms Character vector of the forms as set up in REDCap of which
#'  variable names will be displayed. Default = all forms.
#'@param auto_match If TRUE, variables with matching names will be automatically
#'  selected. If FALSE, the user can decide if the variable shall be imported or
#'  not. Default = TRUE.
#'@param auto_skip_nomatch If TRUE, variables without matching names will be
#'  automatically skipped. If FALSE, the user can decide to select and rename
#'  the variable. Default = FALSE.
#'@param skip_intro If set to TRUE, the introduction messages will be skipped.
#'  Default = FALSE
#'@param suppress_txt If set TRUE, all text output will be suppressed (not
#'  recommended). Default = FALSE.
#'@param log If TRUE, an overview csv-table, and a log-file are stored in the
#'  working directory. Default = TRUE.
#'@param wait Allows you to set the latency time between the steps. Default =
#'  2s.
#'
#'@return Data frame with selected/renamed data. Log-file with executed code.
#'  CSV-table with overview.
#'@export
#'@importFrom stringr str_detect
#'@importFrom crayon bold underline blue italic
#'@importFrom dplyr select filter
#'@importFrom utils write.table
#'
#' @examples
#' # data(importdemo_data)
#' # data(importdemo_dict)
#' # redcap_import_select(importdemo_data, importdemo_dict)
#'
#' # if using local data:
#' # token <- "xxxxx"
#' # url <- "xxxxx"
#' # file <- "data.csv"
#' # redcap_import_select(file, rc_token = token, rc_url = url)




redcap_import_select <- function(import_data,
                                 dict = NULL,
                                 rc_token,
                                 rc_url,
                                 forms = NULL,
                                 auto_match = TRUE,
                                 auto_skip_nomatch = FALSE,
                                 skip_intro = FALSE,
                                 suppress_txt = FALSE,
                                 log = TRUE,
                                 wait = 2) {

  form_name <- field_name <- field_label <- NULL



  # evaluate inputs

  check_data(import_data)
  imp_vars <- colnames(import_data)

  if(is.null(dict)) {
    check_token(rc_token)
    check_url(rc_url)
    dict <- redcap_export_meta(rc_token, rc_url)$meta
  }
  check_dict(dict)


  if(!is.null(forms)) {
    check_forms(forms)
    if(!all(forms %in% dict$form_name)) {
      stop(paste0("The following forms have not been found in the data dictionary:\n",paste(setdiff(forms,dict$form_name),collapse = "\n")))
    }
    dict <- filter(dict, form_name %in% forms)
  }

  if(!is.logical(auto_match)) stop("auto_match should be logical (TRUE/FALSE)")
  if(!is.logical(auto_skip_nomatch)) stop("auto_skip_nomatch should be logical (TRUE/FALSE)")
  if(!is.logical(skip_intro)) stop("skip_intro should be logical (TRUE/FALSE)")
  if(!is.logical(suppress_txt)) stop("suppress_txt should be logical (TRUE/FALSE)")
  if(!is.logical(log)) stop("log should be logical (TRUE/FALSE)")
  if(!is.numeric(wait) || length(wait) != 1) stop("wait should be a single number")




  # intro

  if (!skip_intro) {
    cat("\nHello and welcome!\n\n")
    cat("Let's start with some info about this script and your selections.\n")
    cat("It's best to use fullscreen while working with this script.\n")
    cat("(To turn off this introduction, set 'skip_intro = TRUE')\n\n\n\n")
    Sys.sleep(wait+1)

    cat("This script will loop you through the variable names in the provided data table and compares them with the names in a REDCap data dictionary.\n\n")
    if (auto_match) {
      cat("Auto-selecting of matching variables has been turned on! (To turn it off, set 'auto_match = FALSE')\n")
      cat("If a matching variable name is found in the REDCap dictionary, the variable will be automatically selected without renaming.\n\n")
    } else {
      cat("Auto-selecting of matching variables has been turned off! (To turn it on set 'auto_match = TRUE')\n")
      cat("If a matching variable name is found in the REDCap dictionary, you can decide to not select this variable at all or to select and rename it.\n\n")
    }
    if (auto_skip_nomatch) {
      cat("Auto-skipping of non-matching variables has been turned on! (To turn it off, set 'auto_skip_nomatch = FALSE')\n")
      cat("If a variable name is found with no matching name in the REDCap dictionary, the variable will be automatically skipped.\n\n")
    } else {
      cat("Auto-selecting of non-matching variables has been turned off! (To turn it on set 'auto_skip_nomatch = TRUE')\n")
      cat("If a matching variable name is found with no matching name in the REDCap dictionary, you can decide to not select this variable at all or to select and rename it.\n\n")
    }

    cat(bold("PLEASE BE CAREFUL WITH YOUR CHOICES AS IT IS NOT POSSIBLE TO GO BACK IN THE LOOP!!\n\n"))

    cat("You can press 'Esc' any time to stop the function but it is advised to finish the loop properly! This can be done by either typing 'exit' in the prompt or by looping through all the variables. It makes sure that the executed code is properly written in the log-file and can be copy-pasted into your R-script and adjusted manually at a later time.\n\n")

    if(is.null(dict)) {
      cat("No data dictionary has been provided!\n")
      cat("The dictionary will be downloaded from REDCap with the URL and token you have provided.\n\n")
    } else {
      cat("A data dictionary has been provided.\n")
      cat("Variable names will be read from this dictionary.\n\n")
    }

    if(!is.null(forms)) {
      cat("Form selection has been restricted!\n")
      cat("Only variable names from the following forms will be displayed:\n ")
      cat(paste0(forms, sep="\n"))
      cat("\n\n")
    } else {
      cat("No forms have been provided!\n")
      cat("Variable names of all forms will be displayed.\n\n\n")
    }

    cat("Are you ready to begin? \n 1 = YES\n'esc' = STOP")
    ans <- ""
    while (ans != 1) {
      ans <- readline(prompt="Answer= ")
      if (ans != 1) {
        cat("Please check your answer! \n 1 = YES\n'esc' = STOP")
      }
    }

    cat("\nGreat! Let's begin!\n")
    cat("\n-----------------------------------------------------------------\n\n")
    Sys.sleep(wait)
  }


  # open log-files

  if(log) {
    log_file <- "redcap_import_select_code.txt"
    write.table(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),":\n\nselected_data <- select(import_data"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

    log_table <- "redcap_import_select_overview.csv"
    if (!file.exists(log_table)) {
      write.table("Date,Old Name,New Name\n", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    write.table(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  }

  # read dict, prepare output variables

  rc_vars <- select(dict,c(field_name,field_label,form_name))
  rc_vars$field_label <- strtrim(rc_vars$field_label,50)
  rc_vars$form_name <- strtrim(rc_vars$form_name,10)

  vars_rename <- list()
  imp_vars_out <- character()
  imp_vars_nonewname <- character()
  imp_vars_rename <- character()


  # start loop

  for (i in seq_along(imp_vars)) {
    ans <- ""
    if (any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$")))) {                                          # if variable name found in REDCap dictionary
      if(!suppress_txt) cat(paste("Variable name found in REDCap:", blue(bold(underline(imp_vars[i])))))
      if (!auto_match) {                                                                                            # if auto-import is set to FALSE
        cat("\nShould the variable be kept with this name? \n 1 = YES\n 0 = NO \n 'exit' = stop loop")
        ans <- ""
        while (ans != 1 && ans != 0 && ans != 'exit') {
          ans <- readline(prompt="Answer= ")
          if (ans != 1 && ans != 0 && ans != 'exit') {
            cat("Please check your answer! \n 1 = YES\n 0 = NO \n 'exit' = stop loop")
          }
        }
      }


      if (ans == 'exit') {
        break
      } else if ((ans == '1') || auto_match) {
        vars_rename[[length(vars_rename)+1]] <- imp_vars[i]                                                         # import variable without renaming
        imp_vars_nonewname <- c(imp_vars_nonewname,imp_vars[i])
        rc_vars <- filter(rc_vars,field_name != imp_vars[i])
        if(log){
          write.table(paste0(", ",imp_vars[i]), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
          write.table(paste("",imp_vars[i], sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
        }
        if(!suppress_txt) {
          cat(italic("\n\nVariable will be imported without renaming!\n"))
          cat("\n-----------------------------------------------------------------\n")
        }
        Sys.sleep(wait)

        next
      }
    }

    if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$")))) {                                         # if variable name is not found in REDCap dictionary
      if(!suppress_txt) cat(paste("\nVariable name", underline("NOT"),"found in REDCap:", blue(bold(underline(imp_vars[i])))),"\n\n")
      Sys.sleep(wait)
    }

    if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) | ans == '0') {                            # if variable name is not found in REDCap dictionary or matching var should not be imported with same name
      if(!auto_skip_nomatch) {                                                                                      # if auto-skip is set to FALSE
        print(rc_vars)
        cat(paste("\n\nPlease choose REDCap name from list above for:", blue(bold(underline(imp_vars[i])))),"\n")
        cat("\n Type the name or choose the respective number!\n 'skip' = do NOT select and move to next item \n 'exit' = do Not select and stop loop \n ")

        ans <- ""
        while (!any(grepl(paste0("^",ans,"$"),rc_vars$field_name)) && !any(grepl(paste0("^",ans,"$"),rownames(rc_vars))) && ans != 'exit' && ans != 'skip') {
          ans <- readline(prompt="Answer= ")
          if (!any(grepl(paste0("^",ans,"$"),rc_vars$field_name)) && !any(grepl(paste0("^",ans,"$"),rownames(rc_vars))) && ans !='exit' && ans != 'skip') {
            cat("Variable name not recognized: Please try again!\n 'skip' = do NOT select and move to next item \n 'exit' = do Not select and stop loop \n ")
          }
        }
      }
      if (ans == 'exit') {                                                                       # check answers:
        break                                                                                    # exit
      } else if (ans == 'skip' || auto_skip_nomatch) {
        imp_vars_out <- c(imp_vars_out,imp_vars[i])
        if(!suppress_txt) {
          cat(italic("\nVariable will not be be selected!\n"))
          cat("\n-----------------------------------------------------------------\n")
        }
        Sys.sleep(wait)
        next                                                                                     # skip
      } else if (suppressWarnings(!is.na(as.integer(ans)))) {
        new_name <- rc_vars$field_name[as.integer(ans)]                                          # nbr has been entered
      } else {
        new_name <- ans                                                                          # name has been entered
      }

      vars_rename[[length(vars_rename)+1]] <- imp_vars[i]
      names(vars_rename)[length(vars_rename)] <- new_name
      imp_vars_rename <- c(imp_vars_rename,paste(imp_vars[i],"=",new_name))
      rc_vars <- filter(rc_vars,field_name != new_name)
      if(log) {
        write.table(paste0(", ",new_name," = ",imp_vars[i]), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
        write.table(paste("",imp_vars[i],new_name, sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
      }
      cat(italic(paste0("\nVariable will be selected and renamed:\n\n Old Name = ",imp_vars[i],"\n New Name = ",new_name)))
      cat("\n\n-----------------------------------------------------------------\n")
      Sys.sleep(wait)


    }
  }


  # SELECT AND RENAME VARIABLES
  selected_data <- select(import_data, !!!vars_rename)

  # finalize log-file
  if(log) {
    write.table(")", log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table(paste("\nSUMMARY:\nThe following Variables have been selected without renaming:"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste(imp_vars_nonewname, sep="\n"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table(paste("\nThe following Variables have been selected and renamed:"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste(imp_vars_rename, sep="\n"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table(paste("\nThe following Variables have not been selected:"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste(imp_vars_out, sep="\n"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("\n-----------------------------------------------------------------\n", log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("\n\n", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

  }
  # Return Output
  if(!suppress_txt) cat("\nALL DONE!!!\n\nThanks for using this script!\nIf you encountered any problems while running the script, please let me know!\n\n")
  return(selected_data)



}
