#' @title Sen–Puri test
#' 
#' @description
#' Performs an analysis of variance (ANOVA) on ranks according to Sen and Puri.
#' 
#' @usage
#' puri.test(formula, data = NULL, ...)
#'
#' @param formula a formula specifying the model.
#' @param data data frame in which the variables specified in the formula will be found
#' 
#' @seealso
#' \code{\link[stats]{lm}}, \code{\link[stats]{aov}}
#' 
#' @examples
#' puri.test(yield ~ N + P + K, data = npk)
#'
#' @references
#' Puri ML, Sen PK. 1969. A Class of Rank Order Tests for a General Linear Hypothesis.
#' The Annals of Mathematical Statistics. 40:1325–1343.
#' 
puri.test <- function(formula, data, ...) {
  # Rank response variable
  ranked <- as.formula(paste("rank(",as.character(formula)[2],") ~ ",
                             as.character(formula)[3], sep=""))
  # Perform model
  model <- lm(ranked, ...)
  AOV <- anova(model)
  # Show diagnostics
  par(mfrow=c(2,2))
  plot(model)
  par(mfrow=c(1,1))
  # Adjust output
  MS <- AOV[,1:3]
  MS[,4] <- MS[,2]/(sum(MS[,2])/sum(MS[,1]))
  MS[,5] <- 1-pchisq(MS[,4],MS[,1])
  colnames(MS)[4:5] <- c("H","Pr(>H)")
  return(MS)
}
