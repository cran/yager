#' Generate a list of index for the n-fold cross-validation
#'
#' The function \code{folds} generates a list of index for the n-fold cross-validation
#'
#' @param idx  A vector of index list
#' @param n    The number of n folds
#' @param seed The seed value to generate random n-fold index
#'
#' @return A list of n-fold index
#'
#' @examples
#' folds(seq(10), n = 3, seed = 2020)

folds <- function(idx, n, seed = 1) {
  g <- with(set.seed(seed), sample(idx, length(idx))) %% n + 1
  r <- split(idx, g)
  names(r) <- paste('Fold', seq(n), sep = '')
  return(r)
}
