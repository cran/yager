% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grnn.parpred.R
\name{grnn.parpred}
\alias{grnn.parpred}
\title{Calculate predicted values of GRNN by using parallelism}
\usage{
grnn.parpred(net, x)
}
\arguments{
\item{net}{The GRNN object generated by grnn.fit()}

\item{x}{The matrix of input predictors}
}
\value{
A vector of predicted values
}
\description{
The function \code{grnn.parpred} calculates a vector of GRNN predicted values based on an input matrix
}
\examples{
data(iris, package = "datasets")
Y <- ifelse(iris[, 5] == "setosa", 1, 0)
X <- scale(iris[, 1:4])
gnet <- grnn.fit(x = X, y = Y)
grnn.parpred(gnet, X[seq(5), ])
}
\seealso{
\code{\link{grnn.predict}}
}
