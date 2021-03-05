#' @family calibration
#' 
#' @title Tools for weighted calibrations
#' 
#' @description 
#' Selecting optimum model weights by comparing sum relative errors
#' (\code{relerr()}) of weighted \code{\link{calibration}} models as
#' suggested by Almeida et al. (2002).
#' 
#' @param object an object of class '\code{\link{calibration}}'.
#' @param add_weights a list of weights to be added to the default weights to be
#' checked. These are \code{1/concentration^0.5}, \code{1/concentration^1},
#' \code{1/concentration^2}, \code{1/signal^0.5}, \code{1/signal^1}, and
#' \code{1/signal^2}.
#' @param \dots further arguments passed to \code{\link{calibration}()}.
#' 
#' @return
#' \code{weight_select} produces a matrix with differently weighted
#' \code{calibration} models ordered by sum relative errors.
#' \code{relerr} compares the nominal concentrations with those predicted by
#' the \code{\link{calibration}} model.
#' 
#' @details
#' If calibration data is not homoscedastic, a weighted least squares linear
#' calibration model may be applied to counteract the influence of high
#' concentrations on the regression model. This, in turn, typically improves the
#' accuracy at the lower end of the calibration curve (Almeida et al., 2002).
#' \code{weight_select} uses sum relative errors (\code{relerr}) to
#' find the best weight as suggested by Almeida et al. (2002). Predefined
#' weights include \code{1/concentration^0.5}, \code{1/concentration^1},
#' \code{1/concentration^2}, \code{1/signal^0.5}, \code{1/signal^1}, and
#' \code{1/signal^2} (see \code{\link[envalysis]{calibration}} for details).
#' 
#' @author
#' Julius Albert, Kilian Kenngott, Zacharias Steinmetz
#' 
#' @examples
#' data(din32645)
#' din <- calibration(Area ~ Conc, data = din32645)
#' 
#' weight_select(din)
#' 
#' @references
#' Almeida, A. M. D., Castel-Branco, M. M., & Falcao, A. C. (2002). Linear
#' regression for calibration lines revisited: weighting schemes for
#' bioanalytical methods. \emph{Journal of Chromatography B}, \bold{774}(2),
#' 215-222. \doi{10.1016/S1570-0232(02)00244-1}.
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
  
  nw <- calibration(model$formula, object$data, weights = NULL,
                    check_assumptions = F, ...)
  w <- lapply(allw, function(x) {calibration(model$formula, object$data,
                                             weights = x,
                                             check_assumptions = F,...)})
  
  m <- c(list(object), list(nw), w)
  names(m) <- c(arg, "NULL", allw)
  
  mmat <- matrix(
    c(unlist(lapply(m, function(x) {sum(abs(x$relerr))})),
      unlist(lapply(m, function(x) {x$adj.r.squared}))
      ), ncol = 2)
  colnames(mmat) <- c("Sum relative error", "Adj. R-squared")
  rownames(mmat) <- names(m)
  
  mmat[order(mmat[,1]),]
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
