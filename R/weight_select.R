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
  objname <- as.character(match.call()[2L])
  
  stdweights <- apply(expand.grid("1/", all.vars(model$formula), "^",
                                  c(0.5, 1, 2)), 1, paste, collapse = "")
  allweights <- c(stdweights, add_weights)
  
  nullweight <- calibration(model$formula, object$data, weights = NULL, ...)
  weighted <- lapply(allweights, function(x) {calibration(model$formula,
                                                          object$data, weights = x, ...)})
  
  models <- c(list(object), list(nullweight), weighted)
  names(models) <- c(objname, "NULL", allweights)
  
  sumrelerr <- unlist(lapply(models, function(x) {sum(abs(x$relerr))}))
  Rsq <- unlist(lapply(models, function(x) {x$adj.r.squared}))
  pr <- data.frame(names(models), sumrelerr, Rsq)
  names(pr) <- c("Weights", "Sum relative error", "Adjusted R-squared")
  
  po <- pr[order(pr$`Sum relative error`),]
  print(po, row.names = F)
  
  out <- list(models = models, best = po[1,1])
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
