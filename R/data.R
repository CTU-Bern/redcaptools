#' Example import data
#'
#' Simulated import data for the most common REDCap field types and validations
#' to be used with the data dictionary \code{meta} for testing of
#' \code{redcap_select_rename} and \code{redcap_recode}. Item names and coding
#' show mismatches on purpose so that the interactive functions can be tested.
#'
#' @format A data frame with 17 observations and 32 variables.
#'
"importdemo"


#' Example data dictionary
#'
#' Simulated data dictionary for the most common REDCap field types and
#' validations to be used with the sample data \code{importdemo} for testing of
#' \code{redcap_select_rename} and \code{redcap_recode}. Item names and coding
#' show mismatches on purpose so that the interactive functions can be tested.
#'
#' @format A data frame with 32 variable specifications.
#'
"meta"


#' Example meta data
#'
#' Simulated complete meta data for the most common REDCap field types and
#' validations.
#'
#' @format A list of 3 with metadata (data dictionary), instrument (CRF specifications),
#' and project (global project settings).
#'
"proj"
