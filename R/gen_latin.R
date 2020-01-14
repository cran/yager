#' Generate random numbers of latin hypercube sampling
#'
#' The function \code{gen_latin} generates a vector of random numbers by latin hypercube sampling
#'
#' @param min  The minimum value of random numbers
#' @param max  The maxinum value of random numbers
#' @param n    The number of random numbers to gernate
#' @param seed The seed value of random number generation
#'
#' @return A vector of random numbers bounded by the min and max
#'
#' @examples
#' gen_latin(0, 1, 10, 2020)

gen_latin <- function(min = 0, max = 1, n, seed = 1) {
  set.seed(seed)
  return(round(min + (max - min) * c(lhs::randomLHS(n, k = 1)), 8))
}
