#' Compute Learning Progress
#' @description
#' Learning progress is quantified in terms of Kullback-Leibler Divergence
#' (or information gain), \eqn{D_{KL}}:
#'
#' \eqn{D_{KL}=(p^j|p^{j-1}) \sum_{k=1}^{K}p(x^j=k|X^j,\alpha)\log_{2}\frac{p(x^j=k|X^j,\alpha)}{p(x^j=k|X^{j-1},\alpha)}}
#'
#' \eqn{D_{KL}} is the divergence between a weighted average of prediction error
#' at trial j and a weighted average of prediction error at trial j − 1, and
#' hence, it is a suitable way to model learning progress (Poli et al, 2020)
#'
#' @seealso [dir_prob()],[dir_prob1()]
#'
#' @seealso
#' @param x a `factor` representing an observed sequence.
#'
#' @return Estimate of learning progress
#' @export
#'
#' @examples
#' learning_progress(factor(c(1), levels = 1:3))
learning_progress <- function(x) {
  nK <- length(levels(x))

  sum(sapply(
    1:nK, \(k) dkl(x, k)
  ))
}

dkl <- function(x, k) {
  dir_prob1(x, 0, k) * log2(dir_prob1(x, 0, k) / dir_prob1(x, 1, k))
}

slearning_progress <- function(x) {
  sapply(1:length(x), \(i) learning_progress(x[1:i]))
}
