% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removeZeroYield.R
\name{removeZeroYield}
\alias{removeZeroYield}
\title{This function removes yield that bears the value zero and replace with
missing value.}
\usage{
removeZeroYield(
  data,
  yieldValue,
  yieldObsFlag,
  yieldMethodFlag,
  getSummary = FALSE
)
}
\arguments{
\item{data}{The dataset}

\item{yieldValue}{The column corresponding to the value of yield}

\item{yieldObsFlag}{The column corresponding to the observation status flag
of yield}

\item{yieldMethodFlag}{The column corresponding to the method flag of yield}

\item{getSummary}{logic It is a logic parameter, if it is set to TRUE you can
display on the console messages about how many items have been modified}
}
\value{
A data table where all entries with zero yield are replaced with NA.
}
\description{
By definition, yield can not be zero. When both area harvested and
production are zero, it is a missing value.
}
