% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/fourier.transform.R
\name{fourier.transform}
\alias{fourier.transform}
\title{Compute fourier transform of given series of operators}
\usage{
fourier.transform(A, freq = NULL)
}
\arguments{
\item{A}{time domain operator series}

\item{freq}{frequencies on which the transfom should be computed}
}
\description{
Compute Fourier transform of a given Time Domain Operator
}
\examples{
X = rar(100,d=2)
C = cov.structure(X)
F = fourier.transform(C) # a simple spectral density estimator
Cinv = invfourier(F)
}
\seealso{
\code{\link{invfourier}}
}

