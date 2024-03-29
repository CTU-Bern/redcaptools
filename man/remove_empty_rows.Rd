% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_empty_rows.R
\name{remove_empty_rows}
\alias{remove_empty_rows}
\title{Analagous to `janitor::remove_empty(..., "rows")`, but allows ignoring specific variables}
\usage{
remove_empty_rows(data, ignore = "^(record_id|redcap)|_complete$")
}
\arguments{
\item{data}{a dataframe}

\item{ignore}{regex identifying variables to ignore}
}
\value{
dataframe
}
\description{
Analagous to `janitor::remove_empty(..., "rows")`, but allows ignoring specific variables
}
\examples{
x <- data.frame(a = c(1:9, NA), b = rep(c("b", NA), 5))
remove_empty_rows(x, "a")
remove_empty_rows(x, FALSE)
}
