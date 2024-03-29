% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{get_rsplit}
\alias{get_rsplit}
\alias{get_rsplit.rset}
\alias{get_rsplit.default}
\title{Retrieve individual rsplits objects from an rset}
\usage{
get_rsplit(x, index, ...)

\method{get_rsplit}{rset}(x, index, ...)

\method{get_rsplit}{default}(x, index, ...)
}
\arguments{
\item{x}{The \code{rset} object to retrieve an rsplit from.}

\item{index}{An integer indicating which rsplit to retrieve: \code{1} for the
rsplit in the first row of the rset, \code{2} for the second, and so on.}

\item{...}{Not currently used.}
}
\value{
The rsplit object in row \code{index} of \code{rset}
}
\description{
Retrieve individual rsplits objects from an rset
}
\examples{
set.seed(123)
(starting_splits <- group_vfold_cv(mtcars, cyl, v = 3))
get_rsplit(starting_splits, 1)

}
