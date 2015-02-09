\name{freqdom.eigen}
\alias{freqdom.eigen}
\title{Eigendevompose a frequency domain operator at each frequency}
\usage{
freqdom.eigen(S)
}
\arguments{
  \item{S}{frequency domain operator}
}
\value{
Rotated matrix \code{M}
}
\description{
For given frequency domain operator, compute its
eigenvalues and eigendirections such that for close
frequencies eigendirection matrices are close to each other
('quasi-continuity').
}

