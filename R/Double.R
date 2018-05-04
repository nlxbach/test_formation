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


#' Plot FuzzyDouble
#'
#' Plot a FuzzyDouble object with ggplot2
#'
#' @inheritParams plot.FuzzyDouble
#' @param object The \code{\link{FuzzyDouble}} object
#' @param ... Extra parameters passed to \code{\link{autoplot}}
#'
#' @return A \code{\link{ggplot}} object.
#'
#' @importFrom ggplot2 autoplot
#' @method autoplot FuzzyDouble
#' @export
#'
#' @examples
#' autoplot(FuzzyDouble(1:10))
autoplot.FuzzyDouble <- function(object, xlab = "x",
                                 ylab = "Double", ..., LineCol = "red") {
  # ggplot
  thePlot <- ggplot2::ggplot(data = object, ggplot2::aes_(x = ~x,y = ~y)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::geom_line(ggplot2::aes_(y = ~2 * x), colour = LineCol)
  return(thePlot)
}
