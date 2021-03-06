#' Calculate predicted values of GRNN by using parallelism
#'
#' The function \code{grnn.parpred} calculates a vector of GRNN predicted values based on an input matrix
#'
#' @param net  The GRNN object generated by grnn.fit() 
#' @param x    The matrix of input predictors 
#'
#' @return A vector of predicted values
#'
#' @seealso \code{\link{grnn.predict}}
#'
#' @examples
#' data(iris, package = "datasets")
#' Y <- ifelse(iris[, 5] == "setosa", 1, 0)
#' X <- scale(iris[, 1:4])
#' gnet <- grnn.fit(x = X, y = Y)
#' grnn.parpred(gnet, X[seq(5), ])

grnn.parpred <- function(net, x) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  if (is.matrix(x) == F) stop("x needs to be a matrix.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  if (ncol(x) != ncol(net$x)) stop("x dimension is not consistent with grnn.", call. = F)

  cls <- parallel::makeCluster(min(floor(nrow(x) / 3), parallel::detectCores() - 1), type = "PSOCK")
  obj <- c("net", "x", "grnn.predone", "grnn.predict")
  parallel::clusterExport(cls, obj, envir = environment())
  spx <- parallel::parLapplyLB(cls, parallel::clusterSplit(cls, seq(nrow(x))),
                               function(c_) x[c_, ])
  rst <- parallel::parLapplyLB(cls, spx, function(x_) grnn.predict(net, x_))
  parallel::stopCluster(cls)
  return(Reduce(c, rst))
}
