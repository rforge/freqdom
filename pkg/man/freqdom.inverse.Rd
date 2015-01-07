\name{freqdom.inverse}
\alias{freqdom.inverse}
\title{Compute an inverse of a given Frequency Domain Operator}
\usage{
freqdom.inverse(S, n = NULL, Kconst = NULL, K = NULL)
}
\arguments{
  \item{S}{first spectral density}

  \item{n}{number of observations used for estimation - the
  precision of inversion is calculated using this
  parameter}

  \item{K}{how many directions should be inverted (as in
  \code{\link{pseudoinverse}})}

  \item{Kconst}{constant for heuristic (as in
  \code{\link{reg.dim.est}})}
}
\value{
Frequency Domain Operator object
}
\description{
For given Frequency Domain Operator \code{S} computes ratio
\code{S^\{-1\}} at each frequency.
}
\seealso{
\code{\link{freqdom.product}}, \code{\link{freqdom.ratio}}
}

