#' Invert item scoring
#'
#' A function to invert the scoring of a variable (e.g. a likert item)
#'
#' @param x A vector to be inverted
#' @param min The minimum possible score for this vector
#' @param max The maximum possible score for this vector
#'
#' @return A vector of numbers
#' @export
#'
#' @examples
#' x <- 1:7
#' y <- -4:4
#' z <- 3:21
#' invertItem(x, 1, 7)
#' invertItem(y, -4, 4)
#' invertItem(z, 3, 21)
invertItem <- function(x, min, max) {
  if(!is.numeric(x) | !is.numeric(min) | !is.numeric(max)) stop("Inputs are not numeric")
  ifelse(max<min, stop("Maximum value is less than minimum value"), "")
  ifelse(x>max | x < min, stop("Value is out of expected range"), "")

  reb <- (min-1)
  (max - reb + 1) - (x - reb) + reb
}

