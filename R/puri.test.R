#' @title Sen-Puri test
#' 
#' @description
#' Performs an analysis of variance (ANOVA) on ranks according to
#' Sen and Puri (1969), also known as Scheirer-Ray-Hare-Test (Scheirer et al.,
#' 1976)
#' 
#' @usage
#' puri.test(formula, data, ...)
#'
#' @param formula a formula specifying the model.
#' @param data data frame in which the variables specified in the formula will be found
#' @param \dots further arguments to be passed to \code{lm}, such as \code{subset} or
#' \code{na.action}
#' 
#' @seealso
#' \code{\link[stats]{lm}}, \code{\link[stats]{aov}}
#' 
#' @examples
#' data(npk)
#' puri.test(yield ~ N + P + K, data = npk)
#'
#' @references
#' Puri, M.L., Sen, P.K., 1969. A Class of Rank Order Tests for a General Linear
#' Hypothesis. The Annals of Mathematical Statistics 40, 1325–1343.
#' 
#' Scheirer, C.J., Ray, W.S., Hare, N., 1976. The Analysis of Ranked Data
#' Derived from Completely Randomized Factorial Designs. Biometrics 32,
#' 429–434. doi:10.2307/2529511
#' 
#' @aliases scheirer.test
#' 
#' @importFrom stats pchisq lm anova as.formula
#' @export
puri.test <- scheirer.test <- function(formula, data, ...) {
  mf <- model.frame(formula, data)
  mf[,1] <- rank(mf[,1])

  # Perform model
  model <- lm(mf, ...)
  pt <- anova(model)
  
  # Adjust output
  pt$`F value` <- pt$`Pr(>F)` <- NULL
  pt$`H value` <- pt$`Sum Sq` / (sum(pt$`Sum Sq`) / sum(pt$Df))
  pt$`Pr(>H)` <- 1 - pchisq(pt$`H value`, pt$Df)
  
  return(pt)
}
