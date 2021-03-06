% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_unifm.R
\name{gen_unifm}
\alias{gen_unifm}
\title{Generate Uniform random numbers}
\usage{
gen_unifm(min = 0, max = 1, n, seed = 1)
}
\arguments{
\item{min}{The minimum value of random numbers}

\item{max}{The maxinum value of random numbers}

\item{n}{The number of random numbers to gernate}

\item{seed}{The seed value of random number generation}
}
\value{
A vector of uniform random numbers bounded by the min and max
}
\description{
The function \code{gen_unifm} generates a vector of uniform random numbers
}
\examples{
gen_unifm(0, 1, 10, 2020)
}
