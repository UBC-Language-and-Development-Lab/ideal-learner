#' Calculate predictability in terms of negative entropy
#'
#' @description
#' Predictability is quantified it terms of negative entropy
#'
#' \eqn{-H(p^{j}) = \sum_{k=1}^{K}p(x^j = k|X^j, \alpha)\log_{2}p(x^j = k|X^j, \alpha)}
#'
#' Note that, different from surprise, here, predictability is estimated considering also the event j, and not just up to j − 1
#'
#' \eqn{p(x^j=k)} values are calculated in [prob_target()]
#'
#' @param x The sequence of type Factor with specified levels
#'
#' @return Predictability score
#' @export
#'
#' @examples
#' predictability(factor(c(1), levels = 1:3))
predictability <- function(x) {
  nK <- length(levels(x))
  sum(sapply(
    1:nK,
    (\(k) prob_target1(x, 0, k) * log2(prob_target1(x, 0, k)))
  ))
}

spredictability <- function(x) {
  sapply(1:length(x), \(i) predictability(x[1:i]))
}
