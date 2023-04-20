#' Compute Dirchlet Probability of Last Item in a Sequence
#' @description
#' The probability of observing the last (jth) element in a sequence.
#'
#' \eqn{p(x^{j} = k | X^{j-1}, \alpha) = \frac{n_{k}^{j-1}+1}{j-1+K}}
#'
#' @param x The sequence passed as a Factor
#'
#' @return Probability of seeing the last item in the sequence given the preceding elements
#' @export
#'
#' @examples
#' dir_prob(factor(c(1), levels = 1:3))
dir_prob <- function(x) {
  stopifnot("Input must be factor" = class(x) == "factor")
  stopifnot("Length must be non-0" = length(x) > 0)

  # we are interested in calculating p for the last element
  obs <- x[length(x)]
  # we use only past values
  t <- table(utils::head(x, length(x) - 1))
  # nK is the sum of the prior: alpha = [a_1, ... a_n]
  # where a_1 ... a_n all start equal to 1
  nK <- length(levels(x))

  # The denominator updated the sum of alpha without having to update alpha directly
  pr <- (t[[obs]] + 1) / (length(x) - 1 + nK)
  return(pr)
}

sdir_prob <- function(x) {
  sapply(1:length(x), \(i) dir_prob(x[1:i]))
}

#' Compute Dirchlet Probability of Last Item in a Sequence
#' @description
#' A general case of@seealso [dir_prob()]
#' @param x The sequence
#' @param n Number of element from the end of the sequence to consider (0 or 1)
#' @param k Which element to calculate the probability for
#'
#' @return Probability of seeing item `k` in the sequence given the preceding elements up to `n` elements from the end
#' @export
#'
#' @examples
#' dir_prob(factor(c(1), levels = 1:3)) == dir_prob1(factor(c(1), levels = 1:3))
dir_prob1 <- function(x, n=1, k=-1) {
  if (k == -1){
    obs <- x[length(x)]
  }else{
    obs <- k
  }
  t <- table(utils::head(x, length(x) - n))
  pr <- (t[[obs]] + 1) / (length(x) - n + length(levels(x)))
  return(pr)
}
