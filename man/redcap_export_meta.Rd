% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap_export.R
\name{redcap_export_meta}
\alias{redcap_export_meta}
\title{Export most important REDCap metadata tables}
\usage{
redcap_export_meta(
  token,
  url,
  tabs = c("metadata", "event", "formEventMapping", "instrument"),
  ...
)
}
\arguments{
\item{token}{REDcap API token}

\item{url}{address of the API}

\item{tabs}{tables to export}

\item{...}{other parameters passed to the API (see your REDCap API helpfile for options)}
}
\value{
list of dataframes
}
\description{
Export most important REDCap metadata tables
}
\examples{
# token <- "some_really_long_string_provided_by_REDCap"
# redcap_export_meta(token, "https://www.some_redcap_url.com/api/")
}