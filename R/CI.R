#' @title Confidence interval
#' 
#' @description
#' This wrapper function computes the standard error or the confidence interval
#' of the values in \code{x}. If \code{na.rm} is \code{TRUE}, missing values are
#' removed before the computation proceeds.
#'
#' @param x a numeric vector or an \R object which is coercible to one by
#' \code{as.vector(x, "numeric")}.
#' @param level the confidence level required.
#' @param na.rm logical. Should missing values be removed?
#' 
#' @seealso
#' \code{\link[stats]{sd}} for the standard deviation.
#' 
#' @examples
#' se(1:5)
#' CI(1:5)
#' 
#' @importFrom stats qnorm sd
#' @export
se <- function(x, na.rm = FALSE) {
  sd(x, na.rm = na.rm) / sqrt(length(x))
}

#' @export
CI <- function(x, level = 0.95, na.rm = FALSE) {
  qnorm(1 - ((1 - level) / 2)) * se(x, na.rm = na.rm)
}
