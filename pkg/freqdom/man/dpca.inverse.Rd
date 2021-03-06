% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/dpca.inverse.R
\name{dpca.inverse}
\alias{dpca.inverse}
\title{Retrieve a process from given scores}
\usage{
dpca.inverse(Y, XI)
}
\arguments{
\item{Y}{scores process}

\item{XI}{principal components series}
}
\value{
Retrived process X
}
\description{
For given scores \code{Y} and dynamic principal components \code{XI}
retrive a series from which scores \code{Y} were calculated.
This procedure should be seen as the inverse of \code{\link{dpca.scores}}.
}
\references{
Siegfried Hormann, Lukasz Kidzinski and Marc Hallin
Dynamic Functional Principal Component
Research report, 2012
}
\seealso{
\code{\link{dpca.scores}}, \code{\link{dprcomp}}
}

