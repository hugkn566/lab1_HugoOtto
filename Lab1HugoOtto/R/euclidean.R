#' Compute the euclidean algorithm for two numbers 
#'  
#' @param (a,b) Numeric scalar or integer.
#' @details This function computes euclidean algorithm to find the greatest common divisor between two integers. 
#' @return The greatest common divisor of \code{a} and \code{b}. 
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm (\href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Wikipedia}).
#' @examples  
#' euclidean(123612, 13892347912) 
#' euclidean(100, 1000) 

euclidean <- function(a, b) {
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}




