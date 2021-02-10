#' @family calibration
#' @rdname weighted-calibration
#' 
#' @title Tools for weighted calibrations
#'
#' @description
#' 
#' ff
#' 
#' @param object an object of class \code{\link[envalysis]{calibration}}.
#' @param \dots currently not used.
#' 
#' @examples
#' data(din32645)
#' din <- calibration(Area ~ Conc, data = din32645)
#' 
#' relerr(din)
#' 
#' @export
relerr <- function(object, ...)
  UseMethod("relerr")

#' @export
relerr.default <- function(object, ...) {
  stop("object needs to be of class 'calibration'")
}

#' @rdname weighted-calibration
#' 
#' @export
relerr.calibration <- function(object, ...) {
  model <- object$model
  
  (((model$model[,1] - object$model$coefficients[1]) /
      object$model$coefficients[2]) - model$model[,2]) / model$model[,2]
}
