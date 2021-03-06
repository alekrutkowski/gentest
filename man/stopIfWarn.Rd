% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/core.R
\name{stopIfWarn}
\alias{stopIfWarn}
\title{Stop if gentest() results include warnings}
\usage{
stopIfWarn(gentest_result)
}
\arguments{
\item{gentest_restult}{Value returned by \code{\link[gentest]{gentest}}.}
}
\value{
If there are any warnings, it stops with an error and
stores the part of the \code{gentest_restult} in global environment
as object \code{WARNING}. If there are no errors, it returns the
\code{gentest_restult} invisibly.
}
\description{
Stop if gentest() results include warnings
}

