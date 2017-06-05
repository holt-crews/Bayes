#' Calculate the probability of y given x using Bayes Theorem.
#'
#' @param x The probability of the event x (values must be between 0 and 1).
#' @param y The probability of the event y (values must be between 0 and 1).
#' @param cond_xy The probability of the conditional event, x given that y occurs (values must be between 0 and 1). If x and y are independent, leave this parameter empty.
#' @return The probability of the conditional event, y given that x occurs.
#' @examples
#' bayes_theor(.4, .5, .3)
#' bayes_theor(.2, .8, .7)
#' @export
bayes_theor = function(x, y, cond_xy = (x*y)/y){
  if (x < 0 || x > 1) stop("Invalid probability for x!", call. = FALSE)
  if (y < 0 || y > 1) stop("Invalid probabilty for y!", call. = FALSE)
  if (cond_xy < 0 || cond_xy > 1) stop("Invalid probability for cond_xy!", call. = FALSE)
  return((cond_xy*y)/x)
}

