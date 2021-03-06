% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grnn.margin.R
\name{grnn.margin}
\alias{grnn.margin}
\title{Derive the marginal effect of a predictor used in a GRNN}
\usage{
grnn.margin(net, i, plot = TRUE)
}
\arguments{
\item{net}{The GRNN object generated by grnn.fit()}

\item{i}{The ith predictor in the GRNN}

\item{plot}{TRUE or FALSE to plot the marginal effect}
}
\value{
A plot of the marginal effect or a dataframe of the marginal effect
}
\description{
The function \code{grnn.margin} derives the marginal effect of a predictor used in a GRNN
by assuming mean values for the rest predictors
}
\examples{
data(iris, package = "datasets")
Y <- ifelse(iris[, 5] == "setosa", 1, 0)
X <- scale(iris[, 1:4])
gnet <- grnn.fit(x = X, y = Y)
grnn.margin(gnet, 1, plot = FALSE)
}
\seealso{
\code{\link{grnn.partial}}
}
