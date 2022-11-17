#'REDCap Select and Rename
#'
#'This function loops through all the variable names of a data set and lets the
#'user compare them with the variable names set up in REDCap. An API token is
#'needed to download the variable names from REDCap.Variables with matching
#'names in REDCap can simply be selected without renaming, variables without
#'matching names can be selected and renamed. The function returns a data frame
#'with the selected/renamed variables and writes a summary together with the
#'executed code to a log-file for copy-pasting and adjusting/reusing.
#'
#'
#'@param import_data Data frame to be imported
#'@param rc_token REDCap API token
#'@param url Link to REDCap API. Default: https://redcap.ctu.unibe.ch/api/
#'@param dict Data dictionary (e.g. as downloaded from REDCap or via
#'  \code{redcap_export_meta(rc_token, url)$meta}). If not supplied, this will
#'  be downloaded from the API using \code{rc_token}.
#'@param forms List of REDCap forms of which variable names will be displayed.
#'  Default = all forms.
#'
#'@return Data frame with selected/renamed data. Log-file with executed code.
#'@export
#'@importFrom stringr str_detect
#'@importFrom crayon bold underline
#'@importFrom dplyr select filter
#'
#' @examples
#' token <- "xxxxx"
#' file <- "inst/data.csv"
#' redcap_select(file, token)



redcap_select_rename <- function(import_data,
                                 rc_token,
                                 url = "https://redcap.ctu.unibe.ch/api/",
                                 dict = NULL,
                                 forms = NULL) {


  # load data

  imp_vars <- colnames(import_data)

  if(is.null(dict)) dict <- redcap_export_meta(rc_token, url)$meta

  if(!is.null(forms)) dict <- filter(dict, form_name %in% forms)
  rc_vars <- select(dict,c(field_name,field_label))
  rc_vars$field_label <- strtrim(rc_vars$field_label,50)


  # open log-file
  log_file <- "redcap_select_rename_code.txt"
  write.table(paste0(Sys.time(),":\n\nselected_data <- select(import_data"), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  log_table <- "redcap_select_rename_overview.csv"
  write.table(Sys.time(), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table("Old Name:,New Name:", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

  # prepare output variables
  vars_rename <- list()
  imp_vars_out <- character()
  imp_vars_nonewname <- character()
  imp_vars_rename <- character()





  for (i in seq_along(imp_vars)) {

    ans <- 3
    if (any(str_detect(paste0("^",rc_vars$field_name,"$"),imp_vars[i]))) {                                   # if variable name found in REDCap dictionnary
      cat(paste("Variable name found in REDCap:", bold(underline(imp_vars[i]))))
      cat("\nShould the variable be kept with this name? \n 1 = YES\n 0 = NO \n 'exit' = stop loop")
      ans <- 3
      while (ans != 1 && ans != 0 && ans != 'exit') {
        ans <- readline(prompt="Answer= ")
        if (ans != 1 && ans != 0 && ans != 'exit') {
          cat("Please check your answer! \n 1 = YES\n 0 = NO \n 'exit' = stop loop")
        }
      }

      if (ans == 'exit') {
        break
      }

      if (as.integer(ans) == 1) {
        vars_rename[[length(vars_rename)+1]] <- imp_vars[i]                                 # import variable without renaming
        imp_vars_nonewname <- c(imp_vars_nonewname,imp_vars[i])
        rc_vars <- filter(rc_vars,field_name != imp_vars[i])
        write.table(paste0(", ",imp_vars[i]), log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
        write.table(imp_vars[i], log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

        next
      }
    }

    if (!any(str_detect(paste0("^",rc_vars$field_name,"$"),imp_vars[i]))) {                                  # if variable name is not found in REDCap dictionnairy
      cat(paste("\nVariable name", underline("NOT"),"found in REDCap:", bold(underline(imp_vars[i]))))
    }

    if (!any(str_detect(paste0("^",rc_vars$field_name,"$"),imp_vars[i])) | as.integer(ans) == 0) {           # if variable name is not found in REDCap dictionnairy or should not be imported with same name
      cat("\nShould the variable be imported at all? \n 1 = YES\n 0 = NO \n 'exit' = stop loop")
      ans <- 3
      while (ans != 1 && ans != 0 && ans != 'exit') {
        ans <- readline(prompt="Answer= ")
        if (ans != 1 && ans != 0 && ans != 'exit') {
          cat("Please check your answer! \n 1 = YES\n 0 = NO \n 'exit' = stop loop")
        }
      }

      if (ans == 'exit') {                                                                     # check answers:
        break                                                                                     # exit

      } else if (as.integer(ans) == 0) {                                                          # do not import variable
        imp_vars_out <- c(imp_vars_out,imp_vars[i])
        next

      } else if (as.integer(ans) == 1) {
        print(rc_vars)                                                                            # import with renaming
        cat("Please choose REDCap name from list above for:")
        cat(paste("\n",bold(underline(imp_vars[i]))))
        cat("\n Type the name or choose the respective number!\n 'exit' = do Not select and stop loop \n 'skip' = do NOT select and move to next item")
        ans <- "-999"
        while (!any(grepl(paste0("^",ans,"$"),rc_vars$field_name)) && !any(grepl(paste0("^",ans,"$"),rownames(rc_vars))) && ans != 'exit' && ans != 'skip') {
          ans <- readline(prompt="Answer= ")
          if (!any(grepl(paste0("^",ans,"$"),rc_vars$field_name)) && !any(grepl(paste0("^",ans,"$"),rownames(rc_vars))) && ans !='exit' && ans != 'skip') {
            cat("Variable name not recognized: Please try again!\n 'exit' = do Not select and stop loop \n 'skip' = do NOT select and move to next item")
          }
        }
        if (ans == 'exit') {                                                                    # check answers:
          break                                                                                    # exit
        } else if (ans == 'skip') {
          imp_vars_out <- c(imp_vars_out,imp_vars[i])
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
        write.table(paste(imp_vars[i],new_name, sep = ","), log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
      }
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

  write.table("\n--------------------------------------------------------------------------------------------------\n", log_file, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table("\n", log_table, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

  # Return Output
  cat("ALL DONE!!!")
  return(selected_data)



}
