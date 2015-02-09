\name{dprcomp}
\alias{dprcomp}
\title{Compute DPCA filter coefficients}
\usage{
dprcomp(X, lags = -10:10, q = NULL, weights = NULL)
}
\arguments{
  \item{X}{multivariate stationary time series}

  \item{lags}{requested filter coefficients}

  \item{q}{window for spectral density estimation as in
  \code{\link{spectral.density}}}

  \item{weights}{as in \code{\link{spectral.density}}}
}
\value{
principal components series
}
\description{
For a given process \code{X} eigendecompose it's spectral
density and use an inverse fourier transform to get
coefficients of the optimal filter. For details please
refer to Hormann et al paper.
}
\references{
Siegfried Hormann, Lukasz Kidzinski and Marc Hallin Dynamic
Functional Principal Component Research report, 2012
}
\seealso{
\code{\link{dpca.inverse}}, \code{\link{dpca.scores}}
}

