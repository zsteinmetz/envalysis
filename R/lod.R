#' @title Limit of detection (LOD)
#' 
#' @description
#' Estimates the LOD based on the calibration function. The LOD is defined as the lowest
#' quantity of a substance that can be distinguished from the absence of that substance
#' (blank value) within a stated confidence level.
#' 
#' @usage
#' lod(object, alpha = 0.05)
#'
#' @param object a univariate model object of class \code{lm} with a model formula 
#' \code{Signal ~ Concentration} or \code{Signal ~ Concentraion - 1} containing
#' the linear regression of the analyte concentrations with respect to the signal
#' intensity recorded
#' @param alpha the error tolerance for the detection limit (critical value)
#'  
#' @examples
#' data(icp)
#' Cal <- lm(Signal ~ Conc, data = icp)
#' lod(Cal)
#'
#' @references
#' Currie, LA. 1997. Nomenclature in evaluation of analytical methods including detection
#' and quantification capabilities (IUPAC Recommendations 1995). Analytica Chimica Acta 391:105-126.
#' 
#' @seealso
#' \code{\link{loq}}
#' 
lod <- function(object, alpha = 0.05) {
  if (missing(object) | class(object) != "lm") 
    stop("Input object needs to be of class 'lm'")
  
  conc <- object$model[[2]]
  
  n <- length(table(conc))
  m <- unique(table(conc))
  
  if (length(m) != 1) stop("Measurement replicates of unequal size")
  
  s <- summary(object)$sigma/coef(object)[2]
  t <- 2*qt(1-alpha, n-summary(object)$df[1])
  Q <- sum((conc - mean(conc))^2)/m
  
  return(s*t*sqrt((1/n)+(1/m)+((mean(conc))^2/Q)))
}
