#' @title Limit of quantification (LOQ)
#' 
#' @description
#' Estimates the LOQ based on a given calibration function. The LOQ is defined as the lowest
#' quantity of a substance that can be quantified / distinguished from another sample
#' with confidence.
#' 
#' @usage
#' loq(object, alpha = 0.01, beta = 1/3)
#'
#' @param object a univariate model object of class \code{lm} with a model formula 
#' \code{Signal ~ Concentration} or \code{Signal ~ Concentration - 1} containing
#' the linear regression of the analyte concentrations with respect to the signal
#' intensity recorded
#' @param alpha the error tolerance for the quantification limit (critical value)
#' @param beta the error tolerance for a false negative
#'  
#' @examples
#' data(icp)
#' Cal <- lm(Signal ~ Conc, data = icp)
#' loq(Cal)
#'
#' @references
#' Currie, LA. 1997. Nomenclature in evaluation of analytical methods including detection
#' and quantification capabilities (IUPAC Recommendations 1995). Analytica Chimica Acta 391:105-126.
#' 
#' @seealso
#' \code{\link{lod}}
#' 
#' @importFrom stats qt coef
#' @export
loq <- function(object, alpha = 0.01, beta = 1/3) {
  if (missing(object) | class(object) != "lm") 
    stop("Input object needs to be of class 'lm'")
  
  conc <- object$model[[2]]
  
  n <- length(table(conc))
  m <- unique(table(conc))
  k <- 1/beta
  
  if (length(m) != 1) stop("Measurement replicates of unequal size")
  
  s <- summary(object)$sigma/coef(object)[2]
  t <- -qt(alpha, n-summary(object)$df[1])
  Q <- sum((conc - mean(conc))^2)/m
  # TODO: Iterative process
  return(k*s*t*sqrt(1/m+1/n+(k*lod(object, alpha)-mean(conc))^2/Q))
}
