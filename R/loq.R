#' @title Limit of quantification (LOQ)
#' 
#' @description
#' Estimates the LOQ based on a given calibration function. The LOQ is defined as the lowest
#' quantity of a substance that can be quantified / distinguished from another sample
#' with confidence.
#' 
#' @usage
#' loq(object, conc, alpha = 0.05, beta = 0.5)
#'
#' @param object a univariate model object of class \code{lm} with a model formula 
#' \code{Signal ~ Conc} or \code{Signal ~ Conc - 1} containing the linear regression of the
#' analyte concentrations with respect to the signal intensity recorded
#' @param conc the column expression specifying the respective concentrations
#' @param alpha the error tolerance for the quantification limit (critical value)
#' @param beta the error tolerance for a false negative
#'  
#' @examples
#' \dontrun{
#' data(icp)
#' Cal <- lm(Signal ~ Conc, data = icp)
#' loq(Cal, Conc)
#' }
#'
#' @references
#' Currie, LA. 1997. Nomenclature in evaluation of analytical methods including detection
#' and quantification capabilities (IUPAC Recommendations 1995). Analytica Chimica Acta 391:105-126.
#' 
loq <- function(object, conc, alpha = 0.05, beta = 0.5) {
  if (missing(conc) | missing(object)) stop("Linear model or input data missing")
  if (class(object) != "lm") stop("Input object needs to be of class 'lm'")
  
  name <- deparse(substitute(conc))
  conc <- object$model[,name]
  
  n <- length(unique(conc))
  m <- round(length(conc)/n)
  s <- summary(object)$sigma/summary(object)[[4]][name,"Estimate"]
  t <- qt(1-alpha/2, n-summary(object)$df[1])
  Q <- sum((conc - mean(conc))^2)/m
  
  k <- 1/beta  
  a <- 2*(k*s*t)^2*n*m*mean(conc)
  b <- (k*s*t)^2*(Q*n+Q*m+m*n*mean(conc)^2)
  c <- n*m*(Q-(k*s*t)^2)
  
  if (a+4*b*c < 0) {
    return(NA)
    warning("LOQ calculation failed. NAs generated")
  } else {
    return((-a+sqrt(a+4*b*c))/(2*c))
  }
}
