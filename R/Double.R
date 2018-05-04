#' Double
#'
#' Compute the Double value of a vector
#'
#' The Double is calculated by multiplying each value by 2.
#'
#' @param number A numeric vector
#'
#' @return A vector containing the Double values.
#' @export
#'
#' @examples
#' Double(runif(3))
Double <- function(number) {
  return(2 * number)
}
