#' @family calibration
#' 
#' @title Tools for weighted calibrations
#' 
#' @param object an object of class \code{\link[envalysis]{calibration}}.
#' @param add_weights a list of weights.
#' @param \dots further arguments passed to \code{\link[envalysis]{calibration}}.
#' 
#' @examples
#' data(din32645)
#' din <- calibration(Area ~ Conc, data = din32645)
#' 
#' @export
weight_select <- function(object, add_weights, ...)
  UseMethod("weight_select")

#' @export
weight_select.default <- function(object, add_weights, ...) {
  stop("object needs to be of class 'calibration'")
}

#' @rdname weight_select
#' 
#' @export
weight_select.calibration <- function(object, add_weights = NULL, ...) {
  model <- object$model
  arg <- as.character(match.call()[2L])
  
  stdw <- apply(expand.grid("1/", all.vars(model$formula), "^", c(0.5, 1, 2)),
                1, paste, collapse = "")
  allw <- c(stdw, add_weights)
  
  nw <- calibration(model$formula, object$data, weights = NULL, ...)
  w <- lapply(allw, function(x) {calibration(model$formula, object$data,
                                             weights = x, ...)})
  
  m <- c(list(object), list(nw), w)
  names(m) <- c(arg, "NULL", allw)
  
  srelerr <- unlist(lapply(m, function(x) {sum(abs(x$relerr))}))
  Rsq <- unlist(lapply(m, function(x) {x$adj.r.squared}))
  int <- unlist(lapply(m, function(x) {x$model$coef[1]}))
  sl <- unlist(lapply(m, function(x) {x$model$coef[2]}))
  sum <- data.frame(names(m), srelerr, Rsq, int, sl)
  names(sum) <- c("Weights", "Sum relative error", "Adjusted R-squared",
                  "Intercept", "Slope")
  
  sum <- sum[order(sum$`Sum relative error`),]
  print(sum[,1:3], row.names = F)
  
  list(models = m, summary = sum, best = sum[1,1])
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
