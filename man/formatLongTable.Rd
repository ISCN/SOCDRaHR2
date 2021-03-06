% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatLongTable.R
\name{formatLongTable}
\alias{formatLongTable}
\title{Convert to internal data format}
\usage{
formatLongTable(
  data.ls,
  sourceKey,
  targetKey,
  tableOrder = c("study", "site"),
  warnMismatch = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{data.ls}{list of source relational data tables}

\item{sourceKey}{table describing the source relational data tables in \code{data.ls}. Describes what variable is associated with which source header and table and what kind of information is in that column as well as any hard values associated with a given variable}

\item{targetKey}{table describing the target data tables (what table to put the variables in)}

\item{tableOrder}{default order in wihc to process the source tables}

\item{verbose}{boolean flagging lots of output}
}
\value{
a list of data tables in the interal quasi-long format
}
\description{
This function converts a given list of relational data tables to an internal quasi-long format
}
