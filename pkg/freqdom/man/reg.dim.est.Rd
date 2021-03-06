% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/reg.dim.est.R
\name{reg.dim.est}
\alias{reg.dim.est}
\title{Estimate the optimal dimension in linear regression problem}
\usage{
reg.dim.est(eigenvalues, n, Kconst = 1)
}
\arguments{
\item{eigenvalues}{vector of eigenvalues}

\item{n}{used for estimation}

\item{Kconst}{parameter for fitting the convergence rate to 1/(Kconst*n^{1/2})}
}
\value{
number of 'safe' eigendirections
}
\description{
Consider a linear regression problem for a multivariate stationary time series X_t:
\deqn{Y_t = P X_t + \varepsilon_t.}
Estimator based on formula
\deqn{EY_0X_0 (EX_0^2)^{-1} = P }
is fragile on the eigendirections of \eqn{EX_0^2} with small eigenvalues.
It is therefore desired to truncate the inversion at a level where eigenvalues are
estimated consistently.
Procedure \code{dim.est} suggest such level by taking only the eigenvalues which are greater
and equal than \eqn{1/\sqrt{n}K_{const}}.
It is designed for \eqn{\sqrt{n}} consistent matrix estimator and can serve as one of heuristics
for matrix inverion problems.
}
\examples{
n = 100
X = rar(n)
Y = rar(n)
CV = lagged.cov(X,Y)
E = eigen(CV)
K = reg.dim.est(E$values, n)
}
\references{
Siegfried Hormann and Lukasz Kidzinski
A note on estimation in Hilbertian linear models
Research report, 2012
}
\seealso{
\code{\link{reg.est}}, \code{\link{pseudoinverse}}
}

