#' Calculate the probability of each value of x in a partition of an event given an event y. Assuming x and y are independent.
#'
#' @param x A vector representing a partition of an event with each value representing a different possiblity (each value must be between 0 and 1 and all must add up to 1).
#' @param y The probability of the event y (values must be between 0 and 1).
#' @return A vector of the conditional probabilities of each value of x given y. The indices correspond to the indices of the vector x.
#' @examples
#' bayes_theor_extended(c(.2, .4, .4), .3)
#' @export
bayes_theor_extended = function(x, y){
  if (x < 0 || x > 1) stop("Invalid probability for x!", call. = FALSE)
  if (y < 0 || y > 1) stop("Invalid probabilty for y!", call. = FALSE)
  if(sum(x) != 1) stop("The values of x do not add up to 1", call. = FALSE)
  sum = 0
  for(i in x){
    sum = sum + (i * i * y)
  }

  prob = c(0)
  count = 1

  for(i in x){
    prob[count] = (y * i * i) / sum
    count = count + 1
  }
  return(prob)
}
