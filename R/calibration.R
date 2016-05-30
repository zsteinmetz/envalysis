#' @family calibration
#' 
#' @title Analytical calibration functions
#'
#' @description
#' Defines a \code{calibration} object for the calculation of concentrations
#' from measurement signals including estimatations for the limit of detection
#' (LOD) and limit of quantification (LOQ) in accordance with DIN
#' 32645:2008-11.
#' 
#' The LOD is defined as the lowest quantity of a substance that can be
#' distinguished from the absence of that substance (blank value) within a
#' given confidence level (alpha). The LOQ is defined as the lowest quantity of
#' a substance that can be quantified/distinguished from another sample given
#' with respect to a defined confidence level (k).
#'
#' @param formula model formula providing the recorded signal intensities with
#' respect to the nominal analyte concentrations in the form of
#' \code{signal ~ concentration} or \code{signal ~ concentraion - 1}
#' @param data an optional data frame containing the variables in the model
#' @param model model class to be used for fitting; currently, only \code{lm}
#' is supported
#' @param \dots further arguments passed to the model environment
#' 
#' @return
#' \code{calibration} returns an object of \code{\link[base]{class}}
#' "calibration". \code{summary} calls the model summary together with the
#' respective LOD and LOQ.
#' 
#' @examples
#' data(din32645)
#' din <- calibration(Area ~ Conc, data = din32645)
#' summary(din)
#'
#' @references
#' DIN 32645:2008-11, 2008. Chemical analysis - Decision limit, detection limit
#' and determination limit under repeatability conditions - Terms, methods,
#' evaluation (Technical standard). Deutsches Institut für Normung, Berlin.
#' 
#' Currie, L.A., 1999. Nomenclature in evaluation of analytical methods
#' including detection and quantification capabilities:
#' (IUPAC Recommendations 1995). Analytica Chimica Acta 391, 105–126.
#' 
#' Massart, D.L., Vandeginste, B.G., Buydens, L.M.C., Lewi, P.J.,
#' Smeyers-Verbeke, J., 1997. Handbook of chemometrics and qualimetrics:
#' Part A. Elsevier Science Inc.
#'
#' @seealso
#' \code{\link{icp}}, \code{\link{din32645}}
#' 
#' @importFrom stats qt coef
#' @export
calibration <- function(formula, data, model = "lm", ...) {
  # Collate names
  xname <- paste(formula[3])
  yname <- paste(formula[2])

  # Reorganize data
  newdata <- data[data[xname] != 0, c(xname, yname)]
  blanks <- data[data[xname] == 0, yname]

  obj <- do.call(model, list(formula = formula, data = newdata, ...))
  obj$call <- formula
  
  class(obj) <- c(class(obj), "calibration")
  obj$calibration$blanks <- blanks
  obj$calibration$lod <- lod(obj)
  obj$calibration$loq <- loq(obj)
  return(obj)
}

#' @family calibration
#' @rdname calibration
#' 
#' @export
summary.calibration <- function(object, ...) {
  print(summary(object, ...))
  cat(paste0("Limit of detection (LOD): ", signif(object$calibration$lod, 3), '\n'))
  cat(paste0("Limit of quantification (LOQ): ", signif(object$calibration$loq, 3), '\n\n'))
}

#' @family calibration
#' @rdname calibration
#' @param object a univariate model object of class \code{calibration} or \code{lm}
#' with a model formula as shown above
#' @param alpha error tolerance for the detection limit (critical value)

#' @examples
#' lod(din)
#' 
#' @export
lod <- function(object, alpha = 0.01) UseMethod("lod")

#' @export
lod.default <- lod.lm <- function(object, alpha = 0.01) {
  stop("Object needs to be of class 'lm' or 'calibration'")
}

#' @export
lod.calibration <- lod.lm <- function(object, alpha = 0.01) {
  conc <- object$model[[2]]
  n <- length(table(conc))
  m <- unique(table(conc))

  if (length(m) != 1) warning("Measurement replicates of unequal size. ",
                              "LOD estimation might be incorrect.")

  t <- -qt(alpha, n - summary(object)$df[1])
  b <- coef(object)[2]

  if (length(object$calibration$blanks) > 0) {
    # Direct method (LOD from blanks)
    sl <- sd(object$calibration$blanks) / b
    return(sl * t * sqrt(1/n + 1/m))
  } else {
    # Indirect method (LOD from calibration curve)
    message('No blanks provided. LOD is estimated from the calibration line.')
    sx0 <- summary(object)$sigma / b
    Qx <- sum((conc - mean(conc))^2) / m
    return(sx0 * t * sqrt(1/n + 1/m + (mean(conc))^2/Qx))
  }
}

#' @family calibration
#' @rdname calibration
#' 
#' @param k relative uncertainty for the limit of quantification (1/beta)
#' 
#' @examples
#' loq(din)
#' 
#' @export
loq <- function(object, alpha = 0.01, k = 3) UseMethod("loq")

#' @export
loq.calibration <- function(object, alpha = 0.01, k = 3) {
  conc <- object$model[[2]]
  n <- length(table(conc))
  m <- unique(table(conc))
  
  if (length(m) != 1) warning("Measurement replicates of unequal size. ",
                              "LOD estimation might be incorrect.")
  
  t <- -qt(alpha, n - summary(object)$df[1])
  b <- coef(object)[2]
  lod <- object$calibration$lod
  
  if (length(object$calibration$blanks) > 0) {
    # Direct method (LOD from blanks)
    sl <- sd(object$calibration$blanks) / b
    return(k * sl * t * sqrt(1/n + 1/m))
  } else {
    # Indirect method (LOD from calibration curve)
    message('No blanks provided. LOQ is estimated from the calibration line.')
    sx0 <- summary(object)$sigma / b
    Qx <- sum((conc - mean(conc))^2) / m
    return(k * sx0 * t * sqrt(1/n + 1/m + (k * lod - mean(conc))^2 / Qx))
  }
}

# TODO: Inverse predict