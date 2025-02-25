% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{integral}
\alias{integral}
\title{Take the integral of a vector v}
\usage{
integral(v)
}
\arguments{
\item{v}{A numeric vector}
}
\value{
The integral of \code{v} as a numeric vector.
}
\description{
Take the integral of a vector v by computing the inverse of
the lagged differences (\code{diff} function).
}
