\name{freqdom.kronecker}
\alias{freqdom.kronecker}
\title{Compute a kronecker product of two spectral densities}
\usage{
freqdom.kronecker(S, SC)
}
\arguments{
  \item{S}{first spectral density}

  \item{SC}{second spectral density}
}
\value{
Frequency Domain Operator object
}
\description{
For given spectral densities \code{S} and \code{SC} compute
the Kronecker product \code{S \%x\% SC} at each frequency.
}
\examples{
n = 100
X = rar(n,d=3)
Y = rar(n,d=3)
SX = spectral.density(X)
SY = spectral.density(Y)
R = freqdom.kronecker(SY,SX)
}

