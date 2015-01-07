\name{timedom.norms}
\alias{timedom.norms}
\title{Compute operator norms of elements of a filter}
\usage{
timedom.norms(A, type = "2")
}
\arguments{
  \item{A}{the series of operators}

  \item{type}{matrix norm to be used as in
  \link[pkg:base]{norm}}
}
\value{
List with \code{lags} and \code{norms} vetors
}
\description{
For given series of operators \eqn{(A_k)_{k \in D}} where
\eqn{D = }\code{A$lags} Computes operator norms for each
element and returns a series \eqn{\|A_k\|}
}

