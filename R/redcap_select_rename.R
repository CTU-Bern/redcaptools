#'REDCap Select and Rename
#'
#'This function loops through all the variable names of a data set and lets the
#'user compare them with the variable names set up in REDCap. An API token is
#'needed to download the variable names from REDCap.Variables with matching
#'names in REDCap can simply be selected without renaming, variables without
#'matching names can be selected and renamed. The function returns a data frame
#'with the selected/renamed variables, writes an overview csv-table, and a short
#'summary together with the executed code to a log-file for copy-pasting and
#'adjusting/reusing.
#'
#'
#'@param import_data Data frame to be imported
#'@param dict Data dictionary (e.g. as
#'  downloaded from REDCap or via \code{redcap_export_meta(rc_token,
#'  rc_url)$meta}). If not supplied, this will be downloaded from the API using
#'  \code{rc_token} and \code{rc_url}.
#'@param rc_token REDCap API token
#'@param rc_url Link to REDCap API.
#'@param forms List of REDCap forms of which variable names will be displayed.
#'  Default = all forms.
#'@param auto if TRUE, variables with matching names will be automatically
#'  selected. If FALSE, the user can decide if the variable shall be imported or
#'  not. Default = TRUE.
#'
#'@return Data frame with selected/renamed data. Log-file with executed code.
#'  CSV-table with overview.
#'@export
#'@importFrom stringr str_detect
#'@importFrom crayon bold underline blue italic
#'@importFrom dplyr select filter
#'
#' @examples
#' data(importdemo)
#' data(meta)
#' redcap_select_rename(importdemo, meta)
#'
#' # if using local data:
#' # token <- "xxxxx"
#' # url <- "xxxxx"
#' # file <- "data.csv"
#' # redcap_recode(file, rc_token = token, rc_url = url)




redcap_select_rename <- function(import_data,
                                 dict = NULL,
                                 rc_token,
                                 rc_url,
                                 forms = NULL,
                                 auto = TRUE) {


  # load data

  imp_vars <- colnames(import_data)

  if(is.null(dict)) dict <- redcap_export_meta(rc_token, rc_url)$meta

  if(!is.null(forms)) dict <- filter(dict, form_name %in% forms)
  rc_vars <- select(dict,c(field_name,field_label))
  rc_vars$field_label <- strtrim(rc_vars$field_label,50)


  # open log-files
  log_file <- "redcap_select_rename_code.txt"
  write.table(paste0(Sys.time(),":\n\nselected_data <- select(import_data"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

  log_table <- "redcap_select_rename_overview.csv"
  if (!file.exists(log_table)) {
    write.table("Date,Old Name,New Name\n", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
  write.table(Sys.time(), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)


  # prepare output variables
  vars_rename <- list()
  imp_vars_out <- character()
  imp_vars_nonewname <- character()
  imp_vars_rename <- character()





  for (i in seq_along(imp_vars)) {
    ans <- ""
    if (any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$")))) {                                    # if variable name found in REDCap dictionnary
      cat(paste("Variable name found in REDCap:", blue(bold(underline(imp_vars[i])))))
      if (!auto) {                                                                                            # if auto-import is set to FALSE
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
      } else if ((ans == '1') || auto) {
        vars_rename[[length(vars_rename)+1]] <- imp_vars[i]                                 # import variable without renaming
        imp_vars_nonewname <- c(imp_vars_nonewname,imp_vars[i])
        rc_vars <- filter(rc_vars,field_name != imp_vars[i])
        write.table(paste0(", ",imp_vars[i]), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
        write.table(paste("",imp_vars[i], sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

        cat(italic("\n\nVariable will be imported without renaming!\n"))
        cat("\n-----------------------------------------------------------------\n")
        Sys.sleep(2)

        next
      }
    }

    if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$")))) {                                  # if variable name is not found in REDCap dictionnairy
      cat(paste("\nVariable name", underline("NOT"),"found in REDCap:", blue(bold(underline(imp_vars[i])))),"\n\n")
      Sys.sleep(1)
    }

    if (!any(str_detect(rc_vars$field_name,paste0("^",imp_vars[i],"$"))) | ans == '0') {           # if variable name is not found in REDCap dictionnairy or should not be imported with same name
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
      if (ans == 'exit') {                                                                    # check answers:
        break                                                                                    # exit
      } else if (ans == 'skip') {
        imp_vars_out <- c(imp_vars_out,imp_vars[i])
        cat(italic("\nVariable will not be be selected!\n"))
        cat("\n-----------------------------------------------------------------\n")
        Sys.sleep(2)
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
      write.table(paste0(", ",new_name," = ",imp_vars[i]), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("",imp_vars[i],new_name, sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

      cat(italic(paste0("\nVariable will be selected and renamed:\n\n Old Name = ",imp_vars[i],"\n New Name = ",new_name)))
      cat("\n\n-----------------------------------------------------------------\n")
      Sys.sleep(2)


    }
  }


  # SELECT AND RENAME VARIABLES
  selected_data <- select(import_data, !!!vars_rename)

  # finalize log-file
  write.table(")", log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

  write.table(paste("\nSUMMARY:\nThe following Variables have been selected without renaming:"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste(imp_vars_nonewname, sep="\n"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

  write.table(paste("\nThe following Variables have been selected and renamed:"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste(imp_vars_rename, sep="\n"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

  write.table(paste("\nThe following Variables have not been selected:"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste(imp_vars_out, sep="\n"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

  write.table("\n-----------------------------------------------------------------\n", log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table("\n\n", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

  # Return Output
  cat("ALL DONE!!!")
  return(selected_data)



}
