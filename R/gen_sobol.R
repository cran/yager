#' Generate sobol sequence
#'
#' The function \code{gen_sobol} generates a vector of scrambled sobol sequence
#'
#' @param min  The minimum value of random numbers
#' @param max  The maxinum value of random numbers
#' @param n    The number of random numbers to gernate
#' @param seed The seed value of random number generation
#'
#' @return A vector of sobol sequence bounded by the min and max
#'
#' @examples
#' gen_sobol(0, 1, 10, 2020)

gen_sobol <- function(min = 0, max = 1, n, seed = 1) {
  return(round(min + (max - min) * randtoolbox::sobol(n, dim = 1, scrambling = 3, seed = seed), 8))
}
