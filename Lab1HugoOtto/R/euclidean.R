#' Compute the euclidean algorithm for two numbers 
#'  
#' @param c(a,b) Numeric scalar or integer. 
#' @return The greatest common divisor of \code{a} and \code{b}. 
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




