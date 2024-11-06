#'REDCap Select and Rename
#'
#'This function loops through all the variable names of a data set and lets the
#'user compare them with the variable names set up in REDCap. \cr The REDCap
#'data dictionary can either be directly provided or downloaded from the REDCap
#'project by providing an API token and matching URL. \cr For variables with
#'matching names in REDCap, the function can be run so that they will be
#'automatically selected without renaming. If auto-selecting is turned off, the
#'user can decide to not select these variables at all or to select and rename
#'them. \cr For variables without matching names in REDCap, the function can be
#'run so that they will be automatically skipped. If auto-skipping is turned
#'off, the user can decide to select these variables anyway (helpful e.g. when
#'they need to be split for checkbox fields) or to select and rename them. \cr
#'The function returns a data frame with the selected/renamed variables, writes
#'an overview csv-table, and the executed code to a txt-file for copy-pasting
#'and adjusting/reusing.
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
#'@param start_var Define in which column of the import data the loop should
#'  start. Default = 1.
#'@param auto_match If TRUE, variables with matching names will be automatically
#'  selected. If FALSE, the user can decide if the variable shall be imported or
#'  not. Default = TRUE.
#'@param auto_skip_nomatch If TRUE, variables without matching names will be
#'  automatically skipped. If FALSE, the user can decide to select and rename
#'  the variable. Default = FALSE.
#'@param no_match_suggestion For variables without matching names, similar names
#'  in REDCap will be suggested. With this numeric similarity index between 0
#'  (no similarity at all = shows all items) and 1 (identical = shows only
#'  perfect matches) the number of suggestions can be adjusted. Type '0' to turn
#'  off similarity suggestions. Default = 0.5.
#'@param skip_intro If TRUE, the introduction messages will be skipped.
#'  Default = FALSE
#'@param continue If TRUE, a question to continue will be asked before
#'  moving along the loop. Default = TRUE.
#'@param suppress_txt If TRUE, all text output will be suppressed (not
#'  recommended). Default = FALSE.
#'@param log If TRUE, an overview csv-table, and a txt-file are stored in the
#'  working directory. Default = TRUE.
#'@param log_code Name and location of the txt-file containing the executed
#'  code. Default = redcap_import_select_code.txt.
#'@param log_table Name and location of the csv.table containing the tabular
#'  overview. Default = redcap_import_select_overview.csv.
#'@param log_unused IF TRUE, all REDCap variable names that have not been
#'  matched with the data dictionary  will be listed in the end of the
#'  csv-table. Default = TRUE.
#'@param wait Allows you to set the latency time between the steps. Default =
#'  2s.
#'
#'@return Data frame with selected/renamed data. Log-file with executed code.
#'  CSV-table with overview.
#'
#'@export
#'@importFrom stringr str_detect
#'@importFrom crayon bold underline blue italic
#'@importFrom dplyr select filter
#'@importFrom utils write.table
#'@importFrom stringdist stringsim
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
                                 start_var = 1,
                                 auto_match = TRUE,
                                 auto_skip_nomatch = FALSE,
                                 no_match_suggestion = 0.5,
                                 skip_intro = FALSE,
                                 continue = TRUE,
                                 suppress_txt = FALSE,
                                 log = TRUE,
                                 log_code = 'redcap_import_select_code.txt',
                                 log_table = 'redcap_import_select_overview.csv',
                                 log_unused = TRUE,
                                 wait = 2) {

  form_name <- field_name <- field_label <- NULL



  # evaluate inputs ----

  check_data(import_data)

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

  if(length(start_var) != 1) {
    stop("start_var should be a single integer")
  } else if (start_var %% 1 != 0)  {
    stop("start_var should be a single integer")
  }
  if(!is.logical(auto_match)) stop("auto_match should be logical (TRUE/FALSE)")
  if(!is.logical(auto_skip_nomatch)) stop("auto_skip_nomatch should be logical (TRUE/FALSE)")
  if(length(no_match_suggestion) != 1) {
    stop("no_match_suggestion should be a number between 0 and 1")
  } else {
    if(!is.numeric(no_match_suggestion) | no_match_suggestion < 0 | no_match_suggestion > 1) stop("no_match_suggestion should be a number between 0 and 1")
  }
  if(!is.logical(skip_intro)) stop("skip_intro should be logical (TRUE/FALSE)")
  if(!is.logical(continue)) stop("continue should be logical (TRUE/FALSE)")
  if(!is.logical(suppress_txt)) stop("suppress_txt should be logical (TRUE/FALSE)")
  if(!is.logical(log)) stop("log should be logical (TRUE/FALSE)")
  if(!is.character(log_code) || length(log_code) != 1 || !grepl("\\.txt$", log_code)) stop("please provide a valid path for txt-file")
  if(!is.character(log_table) || length(log_table) != 1 || !grepl("\\.csv$", log_table)) stop("please provide a valid path for csv-table")
  if(!is.logical(log_unused)) stop("log_unused should be logical (TRUE/FALSE)")
  if(length(wait) != 1) {
    stop("wait should be a single integer")
  } else if (wait %% 1 != 0) {
    stop("wait should be a single integer")
  }



  # intro ----

  if (!skip_intro) {
    cat("\nHello and welcome!\n\n")
    cat("Let's start with some info about this script and your selections.\n")
    cat("It's best to use fullscreen while working with this script.\n")
    cat("(To turn off this introduction, set 'skip_intro = TRUE')\n\n\n\n")
    Sys.sleep(wait+1)

    cat("This script will loop you through the variable names in the provided data table and compares them with the names in a REDCap data dictionary.\n\n\n")
    cat("Let's have a look at your choices first as provided as function arguments:\n\n")

    if(start_var != 1) {
      cat(paste0("Your script will start at column: ",as.character(start_var),"\n\n"))
    }

    if (auto_match) {
      cat("Auto-selecting of matching variables has been turned on! (To turn it off, set 'auto_match = FALSE')\n")
      cat("If a matching variable name is found in the REDCap dictionary, the variable will be automatically selected without renaming.\n\n")
    } else {
      cat("Auto-selecting of matching variables has been turned off! (To turn it on set 'auto_match = TRUE')\n")
      cat("If a matching variable name is found in the REDCap dictionary, you can decide to not select this variable at all or to select and rename it.\n\n")
    }

    if (auto_skip_nomatch) {
      cat("Auto-skipping of non-matching variables has been turned on! (To turn it off, set 'auto_skip_nomatch = FALSE')\n")
      cat("If a variable has no matching name in the REDCap dictionary, the variable will be automatically skipped.\n\n")
    } else {
      cat("Auto-selecting of non-matching variables has been turned off! (To turn it on set 'auto_skip_nomatch = TRUE')\n")
      cat("If a variable has no matching name in the REDCap dictionary, you can decide to not select this variable at all, select it without renaming (this can be helpful e.g. if you need to split it into multiple variables) or to select and rename it.\n\n")
    }

    if (is.null(dict)) {
      cat("No data dictionary has been provided!\n")
      cat("The dictionary will be downloaded from REDCap with the URL and token you have provided.\n\n")
    } else {
      cat("A data dictionary has been provided.\n")
      cat("Variable names will be read from this dictionary.\n\n")
    }

    if (!is.null(forms)) {
      cat("Form selection has been restricted!\n")
      cat("Only variable names from the following forms will be displayed:\n ")
      cat(paste0(forms, sep="\n"))
      cat("\n\n")
    } else {
      cat("No forms have been provided!\n")
      cat("Variable names of all forms will be displayed.\n\n\n")
    }


    cat(bold("WHEN MOVING ALONG THE LOOP, PLEASE BE CAREFUL WITH YOUR CHOICES AS IT IS NOT POSSIBLE TO GO BACK!!\n\n"))

    cat("You can press 'Esc' any time to stop the function but it is advised to finish the loop properly! This can be done by either typing 'exit' in the prompt or by looping through all the variables. It makes sure that the executed code is properly written in the log-file and can be copy-pasted into your R-script and adjusted manually at a later time.\n\n")



    cat("Are you ready to begin? \n 1 = YES\n'esc' = STOP")
    intro_ans <- ""
    while (intro_ans != 1) {
      intro_ans <- readline(prompt="Answer= ")
      if (intro_ans != 1) {
        cat("Please check your answer! \n 1 = YES\n'esc' = STOP")
      }
    }

    cat("\nGreat! Let's begin!\n")
    cat("\n-----------------------------------------------------------------\n\n")
    Sys.sleep(wait)
  }


  # open log-files ----

  if(log) {
    write.table(paste0("\n\n",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),":\n\nselected_data <- select(",deparse(substitute(import_data))), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("\n\n",format(Sys.time(), "%Y-%m-%d %H:%M:%S")), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(",Variable,Selected,Not Selected,New Name\n", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  }


  # read data & dict, initiate output variable ----

  imp_vars <- colnames(import_data)[start_var:ncol(import_data)]

  rc_vars <- select(dict,c(field_name,field_label,form_name))
  rc_vars$field_label <- strtrim(rc_vars$field_label,50)
  rc_vars$form_name <- strtrim(rc_vars$form_name,10)

  vars_rename <- list()


  # start loop ----

  for (i in seq_along(imp_vars)) {

    # initiate option to go back with a while loop
    # in order to exit the for-loop an additional variable is needed which can be set to TRUE during the loop
    goback = TRUE
    for_break = FALSE

    while (goback) {

      # var that decides what to do in the end
      what_to_do <- ""

      ## variable name found in REDCap dictionary ----
      if (any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$")))) {
        if(!suppress_txt) cat(paste("\nVariable name found in REDCap:", blue(bold(underline(imp_vars[i])))))

        # if auto-import is set to FALSE
        if (!auto_match) {
          cat("\n\nShould the variable be selected with this name?")
          cat("\n 1 = YES")
          cat("\n 0 = NO")
          cat("\n 'exit' = stop loop")
          match_ans <- ""

          ## INPUT ----
          while (match_ans != "1" &&
                 match_ans != "0" &&
                 match_ans != 'exit') {
            match_ans <- readline(prompt="Answer= ")
            if (match_ans != "1" &&
                match_ans != "0" &&
                match_ans != 'exit') {
              cat("Please check your answer!")
              cat("\n 1 = YES")
              cat("\n 0 = NO")
              cat("\n 'exit' = stop loop")
            }
          }
        } else match_ans <- ""

        # exit
        if (match_ans == 'exit') {
          # exit while loop here, break for loop below
          for_break = TRUE
          break
        }

        # select
        if ((match_ans == '1') || auto_match) {
          what_to_do <- "no_rename"
          out_txt <- italic("Variable will be imported without renaming!")
        }


      } # close "if variable found"



      ## variable name is not found in REDCap dictionary ...----
      if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$")))) {
        if(!suppress_txt) cat(paste("\nVariable name", underline("NOT"),"found in REDCap:", blue(bold(underline(imp_vars[i])))),"\n\n")
        Sys.sleep(wait)
      }

      ## ... or matching var should not be imported with same name ----
      if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) | match_ans == '0') {

        # if auto-skip is set to FALSE
        if(!auto_skip_nomatch) {

          print(rc_vars)

          # find similar items in redcap
          sim_item <- stringsim(tolower(imp_vars[i]),tolower(rc_vars$field_name))
          if (any(sim_item > no_match_suggestion) & no_match_suggestion > 0) {
            cat("\n\nSuggestion(s):\n\n")
            print(rc_vars[sim_item > no_match_suggestion,])
          }

          cat(paste("\n\nPlease choose REDCap name from list above for:", blue(bold(underline(imp_vars[i])))),"\n")
          cat("\n Type the name or choose the respective number!")
          cat("\n 'select' = select the variable anyway (without renaming)")
          cat("\n 'skip' = do NOT select and move to next item")
          cat("\n 'exit' = do NOT select and stop loop \n ")
          nomatch_ans <- ""

          ### INPUT ----
          while (!any(grepl(paste0("^",nomatch_ans,"$"),rc_vars$field_name)) &&
                 !any(grepl(paste0("^",nomatch_ans,"$"),rownames(rc_vars))) &&
                 nomatch_ans != 'select' &&
                 nomatch_ans != 'skip' &&
                 nomatch_ans != 'exit') {
            nomatch_ans <- readline(prompt="Answer= ")
            if (!any(grepl(paste0("^",nomatch_ans,"$"),rc_vars$field_name)) &&
                !any(grepl(paste0("^",nomatch_ans,"$"),rownames(rc_vars))) &&
                nomatch_ans != 'select' &&
                nomatch_ans != 'skip' &&
                nomatch_ans != 'exit') {
              cat("Variable name not recognized: Please try again!")
              cat("\n 'select' = select the variable anyway (without renaming)")
              cat("\n 'skip' = do NOT select and move to next item")
              cat("\n 'exit' = do NOT select and stop loop \n ")
            }
          }
        } else nomatch_ans <- ""

        # exit
        if (nomatch_ans == 'exit') {
          # exit while loop here, break for loop below
          for_break = TRUE
          break
        }

        # skip
        if (nomatch_ans == 'skip' || auto_skip_nomatch) {
          what_to_do <- "no_select"
          out_txt <- italic("Variable will not be be selected!")
        }

        # no renaming
        if (nomatch_ans == 'select') {
          what_to_do <- "no_rename"
          out_txt <- italic("\nVariable will be selected without renaming!")
        }

        # renaming
        if (any(grepl(paste0("^",nomatch_ans,"$"),rc_vars$field_name)) |
            any(grepl(paste0("^",nomatch_ans,"$"),rownames(rc_vars)))) {
          what_to_do <- "rename"
          if (suppressWarnings(!is.na(as.integer(nomatch_ans)))) {
            # nbr has been entered
            new_name <- rc_vars$field_name[as.integer(nomatch_ans)]
          } else {
            # name has been entered
            new_name <- nomatch_ans
          }
          out_txt <- italic(paste0("\nVariable will be selected and renamed:\n\n Old Name = ",imp_vars[i],"\n New Name = ",new_name))
        }


      } # close "if variable not found or not selected with same name"


      # display what will happen
      if (!suppress_txt) {
        cat(paste0("\n\n",out_txt))
        cat("\n\n-----------------------------------------------------------------\n\n")
      }
      Sys.sleep(wait)



      ## continue? ----

      if (continue) {
        cat("Continue?")
        cat("\n1 = YES")
        cat("\n0 = NO (repeat same variable)")
        cont_ans <- ""

        ### INPUT ----
        while (cont_ans != '1' &&
               cont_ans != '0') {

          cont_ans <- readline(prompt="Answer= ")

          if (cont_ans != '1' &&
              cont_ans != '0') {

            cat("Please check your answer!")
            cat("\n1 = YES")
            cat("\n0 = NO (repeat same variable)")
            cont_ans <- ""
          }
        }
      } else cont_ans <- ""


      ## WHAT TO DO ----

      if (!continue | cont_ans == "1") {

        # no select
        if (what_to_do == "no_select") {
          write.table(paste("",imp_vars[i],"","x","", sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
        }

        # no rename
        if (what_to_do == "no_rename") {
          vars_rename[[length(vars_rename)+1]] <- imp_vars[i]
          rc_vars <- filter(rc_vars,field_name != imp_vars[i])

          if(log){
            write.table(paste0(", ",imp_vars[i]), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("",imp_vars[i],"x","","-", sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
          }
        }



        # rename
        if (what_to_do == "rename") {
          vars_rename[[length(vars_rename)+1]] <- imp_vars[i]
          names(vars_rename)[length(vars_rename)] <- new_name
          rc_vars <- filter(rc_vars,field_name != new_name)

          if(log) {
            write.table(paste0(", ",new_name," = ",imp_vars[i]), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("",imp_vars[i],"x","",new_name, sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
          }
        }



        goback <- FALSE
      }




    } # close while-loop

    # break for-loop if 'exit' was typed
    if (for_break) break

  } # close for-loop




  # execute code ----
  selected_data <- select(import_data, !!!vars_rename)


  # finalize log-file ----
  if(log) {
    write.table(")\n\n-----------------------------------------------------------------", log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

    if (log_unused) {
      write.table("\n", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(",Unused variables in REDCap:", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table("", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("",rc_vars$field_name, sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    write.table("", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste("-","-","-","-","-", sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

  }


  # Return Output ----
  if(!suppress_txt) {
    cat("\nALL DONE!!!\n\n")
    cat("Thanks for using this script!\n")
    cat("Make sure to check the code and the summary in the log-files!\n")
    cat("If you encountered any problems while running the script, please let me know!\n\n")
  }

  return(selected_data)



}
