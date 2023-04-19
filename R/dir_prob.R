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


#' Compute Dirchlet Probability of Last Item in a Sequence
#' @description
#' A general case of@seealso [dir_prob()]
#' @param x The sequence
#' @param n Number of element from the end of the sequence to consider (0 or 1)
#' @param k Which element to calculate the probability for
#'
#' @return
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
  t <- table(x[1:length(x)-n])
  pr <- (t[[obs]]+1) / (length(x) - n + length(levels(x)))
  return(pr)
}
