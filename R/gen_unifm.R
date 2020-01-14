#' Generate Uniform random numbers
#'
#' The function \code{gen_unifm} generates a vector of uniform random numbers
#'
#' @param min  The minimum value of random numbers
#' @param max  The maxinum value of random numbers
#' @param n    The number of random numbers to gernate
#' @param seed The seed value of random number generation
#'
#' @return A vector of uniform random numbers bounded by the min and max
#'
#' @examples
#' gen_unifm(0, 1, 10, 2020)

gen_unifm <- function(min = 0, max = 1, n, seed = 1) {
  set.seed(seed)
  return(round(min + (max - min) * runif(n), 8))
}
