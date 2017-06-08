#' Calculate the probability of y given x using Bayes Theorem.
#' Returns Pie Chart to Display the Conditional Probability
#'
#' @param x The probability of the event x (values must be between 0 and 1).
#' @param y The probability of the event y (values must be between 0 and 1).
#' @param cond_xy The probability of the conditional event, x given that y occurs (values must be between 0 and 1).
#' @return  The probability of the conditional event, y given that x occurs.
#'          Plots a pie chart of the probability of the conditional event occuring vs not occuring.
#' @examples
#' bayes_theor(.4, .5, .3)
#' bayes_theor(.2, .8, .7)
#' @export


bayes_theor_pie = function(x, y, cond_xy = (x*y)/y){
  if (x < 0 || x > 1) stop("Invalid probability for x!", call. = FALSE)
  if (y < 0 || y > 1) stop("Invalid probabilty for y!", call. = FALSE)
  if (cond_xy < 0 || cond_xy > 1) stop("Invalid probability for cond_xy!", call. = FALSE)
  cond_yx=(cond_xy*y)/x
  (cond_yx=(cond_xy*y)/x)
  print(cond_yx)
  slices <- c(cond_yx, 1-cond_yx)
  lbls = c("Event Occuring", "Not Occuring")
  pie(slices, labels = lbls, main = "Probability of Y occuring given X")

}
