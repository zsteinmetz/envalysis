#' @family calibration
#' @rdname weighted-calibration
#' 
#' @title Tools for weighted calibrations
#' 
#' @param object an object of class \code{\link[envalysis]{calibration}}.
#' @param \dots additional objects of the same type.
#' 
#' @examples
#' data(din32645)
#' din <- calibration(Area ~ Conc, data = din32645)
#' 
#' relerr(din)
#' 
#' @export
relerr <- function(object)
  UseMethod("relerr")

#' @export
relerr.default <- function(object) {
  stop("object needs to be of class 'calibration'")
}

#' @rdname weighted-calibration
#' 
#' @export
relerr.calibration <- function(object) {
  model <- object$model
  
  (((model$model[,1] - object$model$coefficients[1]) /
      object$model$coefficients[2]) - model$model[,2]) / model$model[,2]
}

#' @rdname weighted-calibration
#' 
#' @examples
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

#' @rdname weighted-calibration
#' 
#' @export
matrix_effect.calibration <- function(object, ...) {
  others <- list(...)
  
  obj_name <- match.call()[2L]
  arg_names <- match.call(expand.dots = FALSE)$...
  
  if (!all(sapply(others, class) == "calibration"))
    stop("object needs to be of class 'calibration'")
  
  res <- unlist(sapply(others, .slopedev, object))
  names(res) <- paste(obj_name, arg_names, sep = " - ")
  
  return(res)
}

# Auxiliary function for matrix_effect()
.slopedev <- function(matrix, std) {
  (matrix$model$coefficients[2] - std$model$coefficients[2]) / 
    std$model$coefficients[2]
}
