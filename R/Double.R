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
  # Input check
  if (!is.numeric(number))
    stop("Double requires a numeric object")
  return(2L * number)
}



#' FuzzyDouble
#'
#' Fuzzy double of a numeric object.
#'
#' Doubles an object with a random noise: a Gaussian error drawn
#' by \code{\link{rnorm}}.
#'
#' @param x A numeric object
#' @param mean The mean noise. Default is 0.
#' @param sd The standard deviation of the noise. Default is 1.
#'
#' @return a \code{FuzzyDouble} object which is a data.frame with
#' columns \code{x} for the input and \code{y} for the output.
#'
#' @seealso \code{\link{plot.FuzzyDouble}},
#' \code{\link{autoplot.FuzzyDouble}}
#' @export
FuzzyDouble <- function(x, mean = 0, sd = 1) {
  # Double x and add normal error
  y <- 2 * x + stats::rnorm(n = length(x), mean = mean,
                            sd = sd)
  # Make a data.frame
  fuzzydouble <- data.frame(x = x, y = y)
  # Make it a FuzzyMultiple object
  class(fuzzydouble) <- c("FuzzyDouble", class(fuzzydouble))
  return(fuzzydouble)
}


#' Plot FuzzyDouble
#'
#' Plot a FuzzyDouble object
#'
#' @param x The \code{\link{FuzzyDouble}} object
#' @param xlab The X-axis label
#' @param ylab The Y-axis label
#' @param ... Extra parameters passed to \code{\link{plot}}
#' @param LineCol The color of the line representing $y=2x$
#'
#' @importFrom graphics plot
#' @method plot FuzzyDouble
#' @export
#'
#' @examples
#' plot(FuzzyDouble(1:10))
plot.FuzzyDouble <- function(x, xlab = "x", ylab = "Double",
                             ..., LineCol = "red") {
  # xy standard plot
  graphics::plot(x$x, x$y, xlab = xlab, ylab = ylab,
                 ...)
  # Add the regression line
  graphics::lines(x$x, 2 * x$x, col = LineCol)
}
