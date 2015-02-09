\name{freqdom.ratio}
\alias{freqdom.ratio}
\title{Compute a ratio of two spectral densities}
\usage{
freqdom.ratio(SC, S, n = NULL, K = NULL, Kconst = NULL)
}
\arguments{
  \item{SC}{first spectral density}

  \item{S}{second spectral density (square)}

  \item{n}{number of observations used for estimation - the
  precision of inversion is calculated using this
  parameter}

  \item{Kconst}{used for heuristing in
  \code{\link{reg.dim.est}}}

  \item{K}{for inversion as in
  \code{\link{freqdom.inverse}}}
}
\value{
Frequency Domain Operator object
}
\description{
For given spectral densities \code{S} and \code{SC}
computes ratio \code{SC \%*\% S^\{-1\}} at each frequency.
}
\examples{
n = 100
X = rar(n)
Y = rar(n)
SYX = spectral.density(X,Y)
SXX = spectral.density(X)
R = freqdom.ratio(SYX,SXX,n)
}
\seealso{
\code{\link{reg.dim.est}}, \code{\link{freqdom.product}},
\code{\link{freqdom.inverse}}
}

