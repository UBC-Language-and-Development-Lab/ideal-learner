#' Compute the surprise of observing the last event in a sequence
#'
#' @description
#' Following the formula for Shannon information:
#'
#' \eqn{I(x^{j} = k) = -log_{2}p(x^{j} = k | X^{j-1}, \alpha)}
#'
#' Note that the probability \eqn{p(x^{j} = k | X^{j-1}, \alpha)} is the definition
#' for Dirichlet probability seen in
#' @seealso [prob_target()]
#'
#' @param x The sequence passed as a Factor
#'
#' @return Information in bits
#' @export
#'
#' @examples
#' surprise(factor(c(1), levels = 1:3))
surprise <- function(x) {
  -log2(prob_target(x))
}

ssurprise <- function(x) {
  sapply(1:length(x), \(i) surprise(x[1:i]))
}
