#' Calculate predictability in terms of negative entropy
#'
#' @description
#' Predictability is quantified it terms of negative entropy
#'
#' \eqn{-H(p^{j}) = \sum_{k=1}^{K}p(x^j = k|X^j , \alpha)log_{2}p(x^j = k|X^j , \alpha)}
#'
#' Note that, different from surprise, here, predictability is estimated considering also the event j, and not just up to j − 1
#'
#' \eqn{p(x^j=k)} values are calculated in @seealso [dir_prob1()]
#'
#' @param x
#'
#' @return Predictability score
#' @export
#'
#' @examples
#' predictability(factor(c(1), levels = 1:3))
predictability <- function(x){
  -sum(sapply(1:3, \(k) dir_prob1(x, 0, k) * log2(dir_prob1(x, 0, k))))
}