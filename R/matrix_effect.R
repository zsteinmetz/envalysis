#' @family calibration
#' 
#' @title Assess matrix effects and matrix-matched calibrations
#' 
#' @description
#' Calculate the matrix effect by comparing the slope of a solvent-based
#' calibration curve with one or more matrix-matched calibration. The matrix
#' effect is expressed as signal suppression/enhancement ratio.
#' 
#' @param object an object of class '\code{\link{calibration}}'
#' obtained from analyzing standard solutions of different concentration
#' (solvent calibration data).
#' @param \dots additional objects of the same type obtained from
#' matrix-matched calibration data.
#' 
#' @return
#' The magnitude of a matrix effect is estimated by subtracting the slope of a
#' matrix-matched calibration from that of the solvent-based calibration. The
#' difference is divided by the slope of the solvent-based calibration.
#' 
#' @details
#' Matrix effects or signal suppression/enhancement ratios should be evaluated
#' during analytical method development to avoid over- or underestimation of
#' sample concentrations. In addition, signal suppression/enhancement ratios may
#' help to justify the validity of a regular solvent calibration as opposed to
#' matrix-matched calibrations. This may be the case if matrix effects or
#' signal suppression/enhancement ratios are close to measurement repeatability.
#' 
#' @author
#' Julius Albert, Zacharias Steinmetz
#' 
#' @examples
#' data(din32645)
#' din <- calibration(Area ~ Conc, data = din32645)
#' 
#' m32645 <- din32645
#' m32645$Area <- din32645$Area * 1.5
#' matrix <- calibration(Area ~ Conc, data = m32645)
#' 
#' matrix_effect(din, matrix)
#' 
#' @export
matrix_effect <- function(object, ...)
  UseMethod("matrix_effect")

#' @export
matrix_effect.default <- function(object, ...) {
  stop("object needs to be of class 'calibration'")
}

#' @rdname matrix_effect
#' 
#' @export
matrix_effect.calibration <- function(object, ...) {
  fo <- list(...)
  
  arg1 <- match.call()[2L]
  arg2 <- match.call(expand.dots = FALSE)$...
  
  if(!all(sapply(fo, class) == "calibration"))
    stop("object needs to be of class 'calibration'")
  
  res <- unlist(sapply(fo, .slopedev, object))
  names(res) <- paste(arg1, arg2, sep = " - ")
  
  return(res)
}

# Auxiliary function for matrix_effect()
.slopedev <- function(matrix, std) {
  (matrix$model$coefficients[2] - std$model$coefficients[2]) / 
    std$model$coefficients[2]
}
