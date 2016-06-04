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
#' "calibration". \code{print} calls the function parameters together with the
#' respective LOD and LOQ. \code{summary} may be used to retrieve the model
#' parameters to be found as a list item called "model".
#' 
#' @examples
#' data(din32645)
#' din <- calibration(Area ~ Conc, data = din32645)
#' din
#' plot(din, interval = "confidence")
#' summary(din$model)
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

  model <- do.call(model, list(formula = formula, data = newdata, ...))
  model$call <- formula
  
  cal <- structure(list(), class = "calibration")
  cal$model <- model
  
  cal$adj.r.squared <- summary(model)$adj.r.squared
  cal$blanks <- blanks
  cal$lod <- lod(cal)
  cal$loq <- loq(cal)
  return(cal)
}

#' @family calibration
#' @rdname calibration
#' 
#' @export
print.calibration <- function(x, ...) {
  print(x$model, ...)
  
  cat(paste0("Adjusted R-squared:  ", signif(x$adj.r.squared, 3), "\n"))
  cat("\n")
  cat("Blanks:\n")
  print(x$blanks)
  cat(paste0("LOD:  ", signif(x$lod, 3), ',\t'))
  cat(paste0("LOQ:  ", signif(x$loq, 3), '\n\n'))
}

#' @family calibration
#' @rdname calibration
#' 
#' @param interval Type of interval calculation (can be abbreviated); see
#' \code{\link[stats]{predict}} for details
#' @param level tolerance/confidence level; see \code{\link[stats]{predict}}
#' for details
#' 
#' @export
plot.calibration <- function(x, interval = NULL, level = 0.95, ...) {
  model <- x$model
  
  conc <- model$model[[2]]
  new <- data.frame(conc = seq(min(conc), max(conc), length.out = 100 * length(conc)))
  names(new) <- names(model$model)[2]
  pred <- data.frame(new, predict(x$model, new, interval = interval, level = level))
  
  plot(model$call, data = model$model, ...)
  lines(pred[, 2] ~ pred[, 1])
  
  tryCatch(
    {
    lines(pred[, 3] ~ pred[, 1], lty = 2)
    lines(pred[, 4] ~ pred[, 1], lty = 2)
    }, error = function(e) invisible()
  )
}

#' @family calibration
#' @rdname calibration
#' @param x an object of class \code{calibration} with a model formula
#' as shown above
#' @param alpha error tolerance for the detection limit (critical value)
#'
#' @examples
#' lod(din)
#' 
#' @export
lod <- function(x, alpha = 0.01) UseMethod("lod")

#' @export
lod.default <- function(x, alpha = 0.01) {
  stop("Object needs to be of class 'calibration'")
}

#' @export
lod.calibration <- function(x, alpha = 0.01) {
  model <- x$model
  
  conc <- model$model[[2]]
  n <- length(table(conc))
  m <- unique(table(conc))

  if (length(m) != 1) warning("Measurement replicates of unequal size. ",
                              "LOD estimation might be incorrect.")

  t <- -qt(alpha, n - model$rank)
  b <- coef(model)[2]

  if (length(x$blanks) > 0) {
    # Direct method (LOD from blanks)
    sl <- sd(x$blanks) / b
    return(sl * t * sqrt(1/n + 1/m))
  } else {
    # Indirect method (LOD from calibration curve)
    message('No blanks provided. LOD is estimated from the calibration line.')
    sx0 <- summary(model)$sigma / b
    Qx <- sum((conc - mean(conc))^2) / m
    return(sx0 * t * sqrt(1/n + 1/m + (mean(conc))^2 / Qx))
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
loq <- function(x, alpha = 0.01, k = 3) UseMethod("loq")

#' @export
loq.calibration <- function(x, alpha = 0.01, k = 3) {
  model <- x$model
  
  conc <- model$model[[2]]
  n <- length(table(conc))
  m <- unique(table(conc))
  
  if (length(m) != 1) warning("Measurement replicates of unequal size. ",
                              "LOD estimation might be incorrect.")
  
  t <- -qt(alpha, n - model$rank)
  b <- coef(model)[2]
  lod <- x$lod
  
  if (length(x$blanks) > 0) {
    # Direct method (LOD from blanks)
    sl <- sd(x$blanks) / b
    return(k * sl * t * sqrt(1/n + 1/m))
  } else {
    # Indirect method (LOD from calibration curve)
    message('No blanks provided. LOQ is estimated from the calibration line.')
    sx0 <- summary(model)$sigma / b
    Qx <- sum((conc - mean(conc))^2) / m
    return(k * sx0 * t * sqrt(1/n + 1/m + (k * lod - mean(conc))^2 / Qx))
  }
}

# TODO: Inverse predict
# Confidence intervals for LOD/LOQ
# Check alpha/beta, also to be passed from calibration to lod
