\name{plot.freqdom}
\alias{plot.freqdom}
\title{Plot a frequency domain operator}
\usage{
\method{plot}{freqdom}(SD, x = NULL, y = NULL)
}
\arguments{
  \item{SD}{Frequency domain object}

  \item{x}{vector of horizontal coefficients}

  \item{y}{vector of vertical coefficients}
}
\description{
Plot chosen coefficients of frequenct domain operator.
Draws all pairs \eqn{ \{ (i,j) | i \in x, j\in y }.
}

