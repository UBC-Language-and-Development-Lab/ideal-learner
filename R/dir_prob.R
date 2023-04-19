#' Compute Dirchlet Probability of Last Item in a Sequence
#' @description
#' The probability of observing the last (jth) element in a sequence.
#'
#' \eqn{p(x^{j} = k | X^{j-1}, \alpha) = \frac{n_{k}^{j-1}+1}{j-1+K}}
#'
#' @param x The sequence passed as a Factor
#' @param alpha smoothing factor applied to counts
#'
#' @return Probability of seeing the last item in the sequence given the preceding elements
#' @export
#'
#' @examples
#' dir_prob(factor(x, levels = 1:3))
dir_prob <- function(x) {
  obs <- x[length(x)]
  t <- table(x)
  pr <- t[[obs]] / (sum(t) - 1 + length(levels(x)))
  return(pr)
}
