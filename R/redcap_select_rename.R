#' REDCap Select and Rename
#'
#' @param import_data data to be imported
#' @param rc_token REDCap API token
#' @param url link to REDCap API
#' @param dict data dictionary (e.g. as downloaded from REDCap or via
#'   \code{redcap_export_meta(rc_token, url)$meta}). If not supplied, this will
#'   be downloaded from the API using \code{rc_token}.
#'@param forms list of REDCap forms of which variable names will be displayed. Default = all forms.
#'
#' @return ???
#' @export
#' @importFrom stringr str_detect
#' @importFrom crayon bold underline
#' @importFrom dplyr select
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
  log_file <- file("redcap_select_rename_code.txt")
  code <- "output_file <- select(import_data"
  
  
  # prepare output variables
  vars_rename <- list()
  imp_vars_out <- character()
  imp_vars_nonewname <- character()
  imp_vars_rename <- character()
  
  
  for (i in seq_along(imp_vars)) {
    
    ans <- 3
    if (any(str_detect(rc_vars$field_name,imp_vars[i]))) {                                   # if variable name found in REDCap dictionnary
      cat(paste("Variable name found in REDCap:", bold(underline(imp_vars[i]))))
      cat("\nShould the variable be imported with this name? \n 1 = YES\n 0 = NO")
      ans <- 3
      while (ans != 1 && ans != 0 && ans != 'exit') {
        ans <- readline(prompt="Answer= ")
        if (ans != 1 && ans != 0 && ans != 'exit') {
          cat("Please type only '1' or '0'")
        }
      }
      
      if (ans == 'exit') {
        break
      }
      
      if (as.integer(ans) == 1) {
        vars_rename[[length(vars_rename)+1]] <- imp_vars[i]                                 # import variable without renaming
        imp_vars_nonewname <- c(imp_vars_nonewname,imp_vars[i])
        rc_vars <- filter(rc_vars,field_name != imp_vars[i])
        code[[length(code)+1]] <- imp_vars[i]
        next
      }
    } 
    
    if (!any(str_detect(rc_vars$field_name,imp_vars[i]))) {                                  # if variable name is not found in REDCap dictionnairy
      cat(paste("\nVariable name", underline("NOT"),"found in REDCap:", bold(underline(imp_vars[i]))))
    }
    
    if (!any(str_detect(rc_vars$field_name,imp_vars[i])) | as.integer(ans) == 0) {           # if variable name is not found in REDCap dictionnairy or should not be imported with same name
      cat("\nShould the variable be imported at all? \n 1 = YES\n 0 = NO")
      ans <- 3
      while (ans != 1 && ans != 0 && ans != 'exit') {
        ans <- readline(prompt="Answer= ")
        if (ans != 1 && ans != 0 && ans != 'exit') {
          cat("Please type only '1' or '0'")
        }
      }
      
      if (ans == 'exit') {
        break
      }
      
      if (as.integer(ans) == 0) {                                                             # do not import variable
        imp_vars_out <- c(imp_vars_out,imp_vars[i])
        next
        
      } else {
        print(rc_vars)                                                                        # import it with renaming
        cat("Please choose REDCap name from list above for:")
        cat(paste("\n",bold(underline(imp_vars[i]))))
        cat("\n Type the name or choose the respective number!")
        ans <- "-999"
        while (!any(grepl(paste0("^",ans,"$"),rc_vars$field_name)) && !any(grepl(paste0("^",ans,"$"),rownames(rc_vars))) && ans != 'exit') {
          ans <- readline(prompt="Answer= ")
          if (!any(grepl(paste0("^",ans,"$"),rc_vars$field_name)) && !any(grepl(paste0("^",ans,"$"),rownames(rc_vars))) && ans !='exit') {
            cat("Variable name not recognized: Please try again!")
          }
        }
        if (ans == 'exit') {
          break
        }
        
        if (!is.na(as.integer(ans))) {
          new_name <- rc_vars$field_name[as.integer(ans)]
        } else {
          new_name <- ans
        }
        
        vars_rename[[length(vars_rename)+1]] <- imp_vars[i]
        names(vars_rename)[length(vars_rename)] <- new_name
        imp_vars_rename <- c(imp_vars_rename,paste(imp_vars[i],"=",new_name))
        rc_vars <- filter(rc_vars,field_name != new_name)
        code[[length(code)+1]] <- imp_vars_rename[length(imp_vars_rename)]
        
      }
    }
  }
  
  # select variables from import file
  output_file <- select(import_data, !!!vars_rename)
  
  # close log-file
  full_code <- paste0(paste(code,collapse=",\n"),")")
  writeLines(full_code, log_file)
  close(log_file)
  
  
  # print summary
  cat("ALL DONE!!!")
  cat(paste("\nThe following Variables have been selected: \n"))
  cat(paste("- ",imp_vars_nonewname), sep="\n")
  
  cat(paste("\nThe following Variables have been selected and renamed: \n"))
  cat(paste("-",imp_vars_rename), sep="\n")
  
  cat(paste("\nThe following Variables have not been selected: \n"))
  cat(paste("-",imp_vars_out), sep="\n")
  
  
  return(output_file)
  
  
  
}
