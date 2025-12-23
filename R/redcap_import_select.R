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
#'@param input_data Data frame to be imported
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
#'@param batch_size Number of REDCap variables displayed per batch. Default = all variables.
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
#'  off similarity suggestions. If auto-skipping is switched off, this index
#'  can be adjusted while running the script. Default = 0.5.
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
#'@importFrom crayon bold underline blue italic green yellow magenta strip_style red
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




redcap_import_select <- function(input_data,
                                 dict = NULL,
                                 rc_token,
                                 rc_url,
                                 forms = NULL,
                                 batch_size = NULL,
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
  intro_ans <- match_ans <- nomatch_ans <- cont_ans <- ""

  # function for printing to match spacing
  prhelp <- function(label, desc, width = 6) {
    # visible length of the label (ignores ANSI codes)
    vislen <- nchar(strip_style(label), type = "width")
    # compute padding (at least one space)
    spaces <- if (width > vislen) strrep(" ", width - vislen) else " "
    cat("   ", label, spaces, " =  ", desc, "\n", sep = "")
  }


  # evaluate inputs ----

  check_data(input_data)

  if(is.null(dict)) {
    dict_downloaded <- TRUE
    check_token(rc_token)
    check_url(rc_url)
    dict <- redcap_export_meta(rc_token, rc_url)$meta
  } else {
    dict_downloaded <- FALSE
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
  if(!is.null(batch_size)) {
    if (batch_size %% 1 != 0 | batch_size < 1) {
      stop("batch_size should be a single integer greater than 0")
    }
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

    if (dict_downloaded) {
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



    cat("Are you ready to begin?")
    cat(paste0("\nType ",green(bold("y")), " and press Enter to continue."))
    cat(paste0("\nPress ",red(bold("ESC"))," to stop.\n"))
    intro_ans <- ""
    ## INPUT ----
    while (intro_ans != "y") {
      intro_ans <- readline(prompt="Answer= ")
      if (intro_ans != "y") {
        cat("\nPlease check your answer!")
        cat(paste0("\nType ",green(bold("y")), " and press Enter to continue."))
        cat(paste0("\nPress ",red(bold("ESC"))," to stop.\n"))
      }
    }

    cat("\nGreat! Let's begin!\n")
    cat("\n-----------------------------------------------------------------\n\n")
    Sys.sleep(wait)
  }


  # open log-files ----

  if(log) {
    write.table(paste0("\n\n",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),":\n\nselected_data <- select(",deparse(substitute(input_data))), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("\n\n",format(Sys.time(), "%Y-%m-%d %H:%M:%S")), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(",Variable,Selected,Not Selected,New Name\n", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  }


  # read data & dict, initiate output variable ----

  imp_vars <- colnames(input_data)[start_var:ncol(input_data)]

  rc_vars <- select(dict,c(field_name,field_label,form_name))
  rc_vars$field_label <- strtrim(rc_vars$field_label,50)
  rc_vars$form_name <- strtrim(rc_vars$form_name,10)

  if(is.null(batch_size)) {
    batch_size <- nrow(rc_vars)
  }

  vars_rename <- list()


  # start loop ----

  for (i in seq_along(imp_vars)) {

    # initiate option to go back with a while loop
    # in order to exit the for-loop an additional variable is needed which can be set to TRUE during the loop
    goback = TRUE
    for_break = FALSE

    while (goback) {

      # var that decides what to do after the user continues to the next variable
      # like this the same var can be repeated as many times as needed before the final decision is written in the log-file
      what_to_do <- ""
      out_txt <- ""


      ## variable name found in REDCap dictionary ----
      # compares var name to data dictionary field names
      if (any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$")))) {
        if(!suppress_txt) cat(paste0("\nVariable name found in REDCap:", blue(bold(underline(imp_vars[i])))))
        Sys.sleep(wait)

        # options will only be displayed if auto-matching is set to FALSE
        if (!auto_match) {
          cat("\n\nShould the variable be selected with this name?")
          cat("\n\nChoose an option (type and press Enter):\n")
          cat(prhelp(green(bold("y")),"YES, select variable without renaming."))
          cat(prhelp(yellow(bold("n")),"NO, rename the variable."))
          cat(prhelp(yellow("skip"),"do NOT select variable and move to next item"))
          cat(prhelp(red("exit"),"do NOT select variable and stop loop"))
          match_ans <- ""

          ## INPUT ----
          while (match_ans != "y" &&
                 match_ans != "n" &&
                 match_ans != "skip" &&
                 match_ans != 'exit') {
            match_ans <- readline(prompt="Answer= ")
            if (match_ans != "y" &&
                match_ans != "n" &&
                match_ans != "skip" &&
                match_ans != 'exit') {
              cat("\nPlease check your answer!")
              cat("\n\nChoose an option (type and press Enter):\n")
              cat(prhelp(green(bold("y")),"YES, select variable without renaming."))
              cat(prhelp(yellow(bold("n")),"NO, rename the variable."))
              cat(prhelp(yellow("skip"),"do NOT select variable and move to next item"))
              cat(prhelp(red("exit"),"do NOT select variable and stop loop"))
            }
          }
        } else match_ans <- ""

        # select
        if ((match_ans == 'y') || auto_match) {
          what_to_do <- "no_rename"
          out_txt <- italic("Variable will be selected without renaming!")
        }

        # skip
        if (match_ans == 'skip') {
          what_to_do <- "no_select"
          out_txt <- italic("Variable will not be be selected!")
        }

        # exit
        if (match_ans == 'exit') {
          # break the while-loop here
          # for_break will be set to TRUE which will break the variable for-loop below and exit the code
          for_break = TRUE
          break
        }

      } # close "if variable found"



      ## variable name is not found in REDCap dictionary ...----
      # compares var name to data dictionary field names
      if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$")))) {
        if(!suppress_txt) cat(paste0("\nVariable name", underline("NOT"),"found in REDCap:", blue(bold(underline(imp_vars[i])))),"\n\n")
        Sys.sleep(wait)
      }

      ## ... or matching var should not be imported with same name ----
      if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) | match_ans == 'n') {


        # to change the similarity index or adjust batch size, initiate a new while-loop
        # (the two logicals have to be set to TRUE before and in the loop since there are two conditions)
        batch_nbr <- 1
        change_sim <- TRUE
        change_batch <- TRUE
        while (change_sim | change_batch) {
          change_sim <- TRUE
          change_batch <- TRUE

          # options will only be displayed if auto-skip is set to FALSE
          if(!auto_skip_nomatch) {

            cat("\nList of fields in REDCap data dictionary:\n\n")
            Sys.sleep(wait)

            # display batch of REDCap variables
            if (batch_size > nrow(rc_vars)) batch_size <- nrow(rc_vars) # adjust batch size if too high
            if (batch_nbr < 1) batch_nbr <- 1 # adjust batch number if first batch
            if ((batch_nbr-1)*batch_size >= nrow(rc_vars)) batch_nbr <- ceiling(nrow(rc_vars)/batch_size) # adjust batch number if last batch
            if (batch_nbr*batch_size > nrow(rc_vars)) {
              #adjust printing of last batch
              print(rc_vars[((1+(batch_nbr-1)*batch_size):nrow(rc_vars)),])
              } else {
                # printing of batches
                print(rc_vars[(1+(batch_nbr-1)*batch_size):(batch_nbr*batch_size),])
              }
            Sys.sleep(wait)

            # find and display similar items in redcap
            sim_item <- stringsim(tolower(imp_vars[i]),tolower(rc_vars$field_name))
            if (any(sim_item > no_match_suggestion) & no_match_suggestion > 0) {
              cat("\n\nSuggestion(s):\n\n")
              print(rc_vars[sim_item > no_match_suggestion,])
            }
            Sys.sleep(wait)

            cat(paste0("\n\nWhat would you like to do for", blue(bold(underline(imp_vars[i])))),"?")
            cat("\n\nChoose an option (type and press Enter):\n")
            cat(paste0("To ",green("rename")," the variable, type the field name or choose the respective number from the list above!\n"))
            cat(prhelp(yellow("select"),"select the variable anyway (without renaming)",14))
            cat(prhelp(yellow("skip"),"do NOT select variable and move to next item",14))
            cat(prhelp(yellow(bold("n")),"move to next batch",14))
            cat(prhelp(yellow(bold("p")),"move to previous batch",14))
            cat(prhelp(magenta("adjust sim"),"adjust the similarity index to see more/less suggestions",14))
            cat(prhelp(magenta("adjust batch"),"adjust the batch size to see more/less variables",14))
            cat(prhelp(red("exit"),"stop loop",14))
            nomatch_ans <- ""

            ### INPUT ----
            while (!any(grepl(paste0("^",nomatch_ans,"$"),rc_vars$field_name)) &&
                   !any(grepl(paste0("^",nomatch_ans,"$"),rownames(rc_vars))) &&
                   nomatch_ans != 'select' &&
                   nomatch_ans != 'skip' &&
                   nomatch_ans != 'n' &&
                   nomatch_ans != 'p' &&
                   nomatch_ans != 'adjust sim' &&
                   nomatch_ans != 'adjust batch' &&
                   nomatch_ans != 'exit') {
              nomatch_ans <- readline(prompt="Answer= ")

              if (!any(grepl(paste0("^",nomatch_ans,"$"),rc_vars$field_name)) &&
                  !any(grepl(paste0("^",nomatch_ans,"$"),rownames(rc_vars))) &&
                  nomatch_ans != 'select' &&
                  nomatch_ans != 'skip' &&
                  nomatch_ans != 'n' &&
                  nomatch_ans != 'p' &&
                  nomatch_ans != 'adjust sim' &&
                  nomatch_ans != 'adjust batch'&&
                  nomatch_ans != 'exit') {

                cat("\nPlease check your answer!")
                cat("\n\nChoose an option (type and press Enter):\n")
                cat(paste0("To ",green("rename")," the variable, type the field name or choose the respective number from the list above!\n"))
                cat(prhelp(yellow("select"),"select the variable anyway (without renaming)",14))
                cat(prhelp(yellow("skip"),"do NOT select variable and move to next item",14))
                cat(prhelp(yellow(bold("n")),"move to next batch",14))
                cat(prhelp(yellow(bold("p")),"move to previous batch",14))
                cat(prhelp(magenta("adjust sim"),"adjust the similarity index to see more/less suggestions",14))
                cat(prhelp(magenta("adjust batch"),"adjust the batch size to see more/less variables",14))
                cat(prhelp(red("exit"),"stop loop",14))
                nomatch_ans <- ""
              }
            }
          } else nomatch_ans <- ""


          ## change similarity index ----
          if(nomatch_ans == 'adjust sim') {

            new_sim <- NA

            while (!is.numeric(new_sim) |
                   new_sim < 0 |
                   new_sim > 1) {

              cat(paste0("\nTo ",bold("see more")," suggestions, decrease the number."))
              cat(paste0("\nTo ",bold("see less")," suggestions, increase the number."))
              cat(paste0("\nTo ",bold("turn off")," suggestions, enter '0'."))
              cat("\n\nCurrent similarity index is:",no_match_suggestion)
              new_sim <- readline(prompt="Please provide a new similarity index between 0 and 1: ")

              if (suppressWarnings(!is.na(as.numeric(new_sim)))) {
                new_sim <- as.numeric(new_sim)
              }

              if (!is.numeric(new_sim) |
                  new_sim < 0 |
                  new_sim > 1) {
                cat("\nPlease check your answer! Similarity index has to be a number between 0 and 1!\n\n")
                new_sim <- NA
              }
            }

            if(new_sim != no_match_suggestion) {
              no_match_suggestion <- new_sim
              cat(paste0("\nSimilarity index has been adjusted to: ",no_match_suggestion,"\n\nRepeating current variable....\n\n"))
              Sys.sleep(wait)
            }

          } else change_sim <- FALSE


          ## change batch size or move batch ----
          if (nomatch_ans == 'adjust batch') {

            new_batch <- NA

            while (!is.numeric(new_batch) |
                   new_batch < 1 |
                   new_batch %% 1 != 0) {

              cat(paste0("\nTo ",bold("see more")," variables, increase the number."))
              cat(paste0("\nTo ",bold("see less")," variables, decrease the number."))
              cat("\n\nCurrent batch size is:",batch_size)
              new_batch <- readline(prompt="Please provide a new batch size (integer greater than 0): ")

              if (suppressWarnings(!is.na(as.integer(new_batch)))) {
                new_batch <- as.integer(new_batch)
              }

              if (!is.numeric(new_batch) |
                  new_batch < 1 |
                  new_batch %% 1 != 0) {
                cat("\nPlease check your answer! Batch size has to be an integer greater than 0!\n\n")
                new_batch <- NA
              }
            }

            if(new_batch != batch_size) {
              batch_size <- new_batch
              cat(paste0("\nBatch size has been adjusted to: ",batch_size,"\n\nRepeating current variable....\n\n"))
              Sys.sleep(wait)
            }

          } else if (nomatch_ans == 'n') {
            # move to next batch
            batch_nbr <- batch_nbr + 1
            cat(paste0("\nMoving to next batch ",batch_nbr," of ",ceiling(nrow(rc_vars)/batch_size),"....\n\n"))
            Sys.sleep(wait)

          } else if (nomatch_ans == 'p') {
            # move to previous batch
            batch_nbr <- batch_nbr - 1
            cat(paste0("\nMoving to previous batch ",batch_nbr," of ",ceiling(nrow(rc_vars)/batch_size),"....\n\n"))
            Sys.sleep(wait)

          } else change_batch <- FALSE


        } # end  while loop



        # exit
        if (nomatch_ans == 'exit') {
          # break the while-loop here
          # for_break will be set to TRUE which will break the variable for-loop below and exit the code
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
        # (if a correct REDCap field name (or respective row number) has been entered)
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


      ## display what will happen ----
      if (!suppress_txt) {

        if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) & auto_skip_nomatch) {
          cat(paste0("\n\n\n",bold(underline("NOTE:")," Auto-skipping of non-matching variables is active!\n(To turn it off, set 'auto_skip_nomatch = FALSE' or type 'auto_skip off' when prompted to continue.)\n\n\n")))
        } else if (any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) & auto_match) {
          cat(paste0("\n\n\n",bold(underline("NOTE:")," Auto-selecting of matching variables is active!\n(To turn it off, set 'auto_match = FALSE' or type 'auto_match off' when prompted to continue.)\n\n\n")))
        }

        cat(paste0("\n\n",out_txt,"\n\n"))
      }
      Sys.sleep(wait)



      ## continue? ----

      if (continue) {
        cat("\nContinue?")
        cat("\n\nChoose an option (type and press Enter):\n")
        cat(prhelp(green(bold("y")),"YES",15))
        cat(prhelp(yellow(bold("n")),"NO (repeat same variable)",15))

        # show options to switch on/off auto-matching depending on whether matching var has been found
        if (any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) & auto_match) {
          cat(prhelp(magenta("auto_match off"),"turn off auto-selecting for matching variables",15))
        }
        if (any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) & !auto_match){
          cat(prhelp(magenta("auto_match on"),"turn on auto-selecting for matching variables",15))
        }

        # show options to switch on/off auto-skipping depending on whether matching var has not been found
        if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) & auto_skip_nomatch) {
          cat(prhelp(magenta("auto_skip off"),"turn off auto-skipping for non-matching variables",15))
        }
        if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) & !auto_skip_nomatch) {
          cat(prhelp(magenta("auto_skip on"),"turn on auto-skipping for non-matching variables",15))
        }

        cat(prhelp(red("exit"),"stop loop",15))
        cont_ans <- ""

        ### INPUT ----
        while (cont_ans != 'y' &&
               cont_ans != 'n' &&
               cont_ans != 'auto_match on' &&
               cont_ans != 'auto_match off' &&
               cont_ans != 'auto_skip on' &&
               cont_ans != 'auto_skip off' &&
               cont_ans != 'exit') {

          cont_ans <- readline(prompt="Answer= ")

          if (cont_ans != 'y' &&
              cont_ans != 'n' &&
              cont_ans != 'auto_match on' &&
              cont_ans != 'auto_match off' &&
              cont_ans != 'auto_skip on' &&
              cont_ans != 'auto_skip off' &&
              cont_ans != 'exit') {

            cat("\nPlease check your answer!")
            cat("\n\nChoose an option (type and press Enter):\n")
            cat(prhelp(green(bold("y")),"YES",15))
            cat(prhelp(yellow(bold("n")),"NO (repeat same variable)",15))
            if (any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) & auto_match) {
              cat(prhelp(magenta("auto_match off"),"turn off auto-selecting for matching variables",15))
            }
            if (any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) & !auto_match){
              cat(prhelp(magenta("auto_match on"),"turn on auto-selecting for matching variables",15))
            }
            if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) & auto_skip_nomatch) {
              cat(prhelp(magenta("auto_skip off"),"turn off auto-skipping for non-matching variables",15))
            }
            if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) & !auto_skip_nomatch) {
              cat(prhelp(magenta("auto_skip on"),"turn on auto-skipping for non-matching variables",15))
            }
            cat(prhelp(red("exit"),"stop loop",15))
            cont_ans <- ""
          }
        }
      } else cont_ans <- ""

      # exit
      if (cont_ans == 'exit') {
        # exit while loop here, break for loop below
        for_break = TRUE
        break
      }

      ### turn on/off auto selecting ----
      if (cont_ans == 'auto_match on') {
        auto_match <- TRUE
        if(!suppress_txt) cat("\nAuto-selecting of matching variables has been turned on!\n\nRepeating current variable....\n\n")
        Sys.sleep(wait)
      }

      if (cont_ans == 'auto_match off') {
        auto_match <- FALSE
        if(!suppress_txt) cat("\nAuto-selecting of matching variables has been turned off!\n\nRepeating current variable....\n\n")
        Sys.sleep(wait)
      }

      ### turn on/of auto skipping ----
      if (cont_ans == 'auto_skip on') {
        auto_skip_nomatch <- TRUE
        if(!suppress_txt) cat("\nAuto-skipping of non-matching variables has been turned on!\n\nRepeating current variable....\n\n")
        Sys.sleep(wait)
      }

      if (cont_ans == 'auto_skip off') {
        auto_skip_nomatch <- FALSE
        if(!suppress_txt) cat("\nAuto-skipping of non-matching variables has been turned off!\n\nRepeating current variable....\n\n")
        Sys.sleep(wait)
      }


      # WHAT TO DO ----



      if (!continue | cont_ans == "y") {

        ## no select ----
        if (what_to_do == "no_select") {
          if(log) write.table(paste("",imp_vars[i],"","x","", sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
        }

        ## no rename ----
        if (what_to_do == "no_rename") {
          vars_rename[[length(vars_rename)+1]] <- imp_vars[i]
          rc_vars <- filter(rc_vars,field_name != imp_vars[i])

          if(log){
            write.table(paste0(", ",imp_vars[i]), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("",imp_vars[i],"x","","-", sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
          }
        }



        ## rename ----
        if (what_to_do == "rename") {
          vars_rename[[length(vars_rename)+1]] <- imp_vars[i]
          names(vars_rename)[length(vars_rename)] <- new_name
          rc_vars <- filter(rc_vars,field_name != new_name)

          if(log) {
            write.table(paste0(", ",new_name," = ",imp_vars[i]), log_code, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("",imp_vars[i],"x","",new_name, sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
          }
        }


        # end loops ----

        if (!suppress_txt) {
          cat("\n\nMoving to next variable...\n")
          cat("\n-----------------------------------------------------------------\n")
          Sys.sleep(wait)
        }

        # go-back option will be set to false which exits the while-loop
        goback <- FALSE
      }

    } # close while-loop

    # if for_break has been set to TRUE, the code will break the variable for-loop and exit the code
    if (for_break) break

  } # close for-loop







  # execute code ----
  selected_data <- select(input_data, !!!vars_rename)


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
