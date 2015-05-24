#' @title Limit of detection (LOD)
#' 
#' @description
#' Estimates the LOD based on the calibration function. The LOD is defined as the lowest
#' quantity of a substance that can be distinguished from the absence of that substance
#' (blank value) within a stated confidence level.
#' 
#' @usage
#' lod(object, conc, alpha = 0.05)
#'
#' @param object a univariate model object of class \code{lm} with a model formula 
#' \code{Signal ~ Conc} or \code{Signal ~ Conc - 1} containing the linear regression of the
#' analyte concentrations with respect to the signal intensity recorded
#' @param conc the column expression specifying the respective concentrations
#' @param alpha the error tolerance for the detection limit (critical value)
#'  
#' @examples
#' \dontrun{
#' data(icp)
#' Cal <- lm(Signal ~ Conc, data = icp)
#' lod(Cal, Conc)
#' }
#'
#' @references
#' Currie, LA. 1997. Nomenclature in evaluation of analytical methods including detection
#' and quantification capabilities (IUPAC Recommendations 1995). Analytica Chimica Acta 391:105-126.
#' 
lod <- function(object, conc, alpha = 0.05) {
  if (missing(conc) | missing(object)) stop("Linear model or input data missing")
  if (class(object) != "lm") stop("Input object needs to be of class 'lm'")
  
  name <- deparse(substitute(conc))
  conc <- object$model[,name]
  
  n <- length(unique(conc))
  m <- round(length(conc)/n)
  s <- summary(object)$sigma/summary(object)[[4]][name,"Estimate"]
  t <- qt(1-alpha/2, n-summary(object)$df[1])
  Q <- sum((conc - mean(conc))^2)/m
  
  return(s*t*sqrt((1/n)+(1/m)+((mean(conc))^2/Q)))
}
