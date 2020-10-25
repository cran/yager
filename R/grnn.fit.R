#' Create a general regression neural network
#'
#' The function \code{grnn.fit} creates a general regression neural network (GRNN)
#'
#' @param x     The matrix of predictors
#' @param y     The vector of response variable
#' @param w     The vector of weights with default = 1 for each record
#' @param sigma The scalar of smoothing parameter
#'
#' @return A general regression neural network object
#'
#' @references
#' Donald Specht. (1991). A General Regression Neural Network.
#'
#' @examples
#' data(iris, package = "datasets")
#' Y <- ifelse(iris[, 5] == "setosa", 1, 0)
#' X <- scale(iris[, 1:4])
#' gnet <- grnn.fit(x = X, y = Y)

grnn.fit <- function(x, y, sigma = 1, w = rep(1, length(y))) {
  ### CHECK X MATRIX ###
  if (is.matrix(x) == F) stop("x needs to be a matrix.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  ### CHECK Y VECTOR ###
  if (is.vector(y) == F) stop("y needs to be a vector.", call. = F)
  if (anyNA(y) == T) stop("NA found in y.", call. = F)
  if (length(y) != nrow(x)) stop("x and y need to share the same length.", call. = F)
  ### CHECK W VECTOR ###
  if (is.vector(w) == F) stop("w needs to be a vector.", call. = F)
  if (anyNA(w) == T) stop("NA found in w.", call. = F)
  if (length(w) != nrow(x)) stop("x and w need to share the same length.", call. = F)
  ### CHECK SIGMA ###
  if (sigma <= 0) stop("sigma needs to be positive", call. = F)

  gn <- structure(list(), class = "General Regression Neural Net")
  gn$x <- x
  gn$y <- y
  gn$w <- w
  gn$sigma <- sigma
  return(gn)
}
