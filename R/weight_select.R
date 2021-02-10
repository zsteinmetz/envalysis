#' @family calibration
#' 
#' @title Tools for weighted calibrations
#' 
#' @param object an object of class \code{\link[envalysis]{calibration}}.
#' @param weights a list of weights.
#' 
#' @examples
#' data(din32645)
#' din <- calibration(Area ~ Conc, data = din32645)
#' 
#' @export
weight_select <- function(object, weights)
  UseMethod("weight_select")

#' @export
weight_select.default <- function(object, weights) {
  stop("object needs to be of class 'calibration'")
}

#' @rdname weight_select
#' 
#' @export
weight_select.calibration <- function(object, weights) {
  model <- object$model
  
  # TODO
}

#' @examples
#' relerr(din)
#' 
#' @rdname weight_select
#' @export
relerr <- function(object)
  UseMethod("relerr")

#' @export
relerr.default <- function(object) {
  stop("object needs to be of class 'calibration'")
}

#' @rdname weight_select
#' 
#' @export
relerr.calibration <- function(object) {
  model <- object$model
  
  (((model$model[,1] - object$model$coefficients[1]) /
      object$model$coefficients[2]) - model$model[,2]) / model$model[,2]
}
