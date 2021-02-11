#' @family calibration
#' 
#' @title Assess matrix effects and matrix-matched calibrations
#' 
#' @param object an object of class \code{\link[envalysis]{calibration}}
#' obtained by analyzing standard solutions.
#' @param \dots additional objects of the same type obtained from
#' matrix matched calibrations.
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
  
  if (!all(sapply(fo, class) == "calibration"))
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
