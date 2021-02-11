#' @family calibration
#' @rdname calibration
#' 
#' @title Analytical calibration functions
#'
#' @description
#' Defines a \code{calibration} object for the calculation of concentrations
#' from measurement signals including estimations for the limit of detection
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
#' \code{signal ~ concentration} or \code{signal ~ concentraion - 1}; model
#' formulas are currently restricted to those forms, however, the possibility
#' to use \code{log} or \code{sqrt} transformed data will be implemented in the
#' future.
#' @param data an optional data frame containing the variables in the model.
#' @param weights an optional character string containing one or more model
#' variables, for example, in the form of "1/\code{concentration}^0.5" or
#' "1/\code{signal}" which is internally converted to a numeric vector and
#' passed to the fitting process of the selected model.
#' @param model model class to be used for fitting; currently,
#' \code{\link[stats]{lm}} and \code{\link[MASS]{rlm}} are supported.
#' @param check_assumptions automatically check for normality and
#' homoscedasticity of model residuals using and \code{\link[stats]{shapiro.test}}
#' \code{\link[lmtest]{bptest}}) respectively.
#' @param \dots further arguments passed to the submethod, namely the
#' respective model environment such as \code{lm}), \code{plot}, or
#' \code{print}.
#' 
#' @details
#' If the \code{data} supplied to \code{calibration} contain more than one blank
#' value, i.e. measurements with a nominal concentration of zero, the LOD and
#' LOQ are calculated from the deviation of the blank samples. This method is
#' called "blank method" according to DIN 32645:2008-11 and supposed to be more
#' accurate than the so-called "calibration method" which will be used for the
#' estimation of LOD and LOQ when \code{data} does not contain zero
#' concentration measurements.
#' 
#' @return
#' \code{calibration} returns an object of \code{\link[base]{class}}
#' "calibration". \code{print} calls the function parameters together with the
#' respective LOD and LOQ. \code{plot} plots the respective calibration curve
#' together with the measurement values. \code{summary} may be used to retrieve
#' the model parameters to be found as a list item called "model".
#' 
#' @examples
#' data(din32645)
#' din <- calibration(Area ~ Conc, data = din32645)
#' din
#' plot(din)
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
#' @importFrom stats qt coef qchisq model.frame shapiro.test
#' @importFrom graphics plot lines
#' @importFrom lmtest bptest
#' @export
calibration <- function(formula, data = NULL, weights = NULL, model = "lm",
                        check_assumptions = FALSE, ...) {
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]), 
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  
  # Collate data
  mf <- model.frame(formula, data)
  newdata <- mf[mf[2L] != 0, ]
  blanks <- mf[mf[2L] == 0, 1]
  
  if (!is.null(weights) & length(weights) == 1 & is.character(weights)) {
    newweights <- with(newdata, eval(parse(text = weights)))
  } else {
    if (is.null(weights) | length(weights) == nrow(newdata)) {
      newweights <- weights
    } else {
      stop("'weights' needs to be a single character value or a numeric vector ",
           "of the same length as non-zero concentration data (without blanks)")
    }
  }

  model <- do.call(model, list(formula = formula, data = newdata, weights = newweights, ...))
  model$call <- match.call(expand.dots = F)
  model$formula <- formula
  
  cal <- structure(list(), class = "calibration")
  cal$model <- model
  
  cal$adj.r.squared <- summary(model)$adj.r.squared
  if (is.null(cal$adj.r.squared)) cal$adj.r.squared <- NA
  
  cal$data <- data
  cal$blanks <- blanks
  cal$lod <- lod(cal)
  cal$loq <- loq(cal)
  cal$relerr <- relerr(cal)
  
  return(cal)
  
  if (is.null(weights) & check_assumptions) {
    swt <- shapiro.test(model$residuals)
    bpt <- bptest(model)
    
    cat("Check for normality of residuals\n")
    print(swt)
    cat("Check for homoscedasticity of residuals\n")
    print(bpt)
    
    if (swt$p.value < 0.05 || bpt$p.value < 0.05)
      warning("model assumptions may not be met; double check graphically and ",
              "consider using a weighted model instead")
  }
}

#' @rdname calibration
#' 
#' @export
print.calibration <- function(x, ...) {
  print(x$model, ...)
  
  cat(paste0("Adjusted R-squared:  ", signif(x$adj.r.squared, 4), "\n"))
  cat(paste0("Sum relative error:  ", signif(sum(abs(x$relerr)), 4), "\n"))
  cat("\n")
  cat("Blanks:\n")
  print(x$blanks)
  cat("\n")
  print(signif(rbind(x$lod, x$loq), 3))
}

#' @rdname calibration
#'
#' @export
summary.calibration <- function(object, ...) {
  summary(object$model, ...)
}

#' @rdname calibration
#' 
#' @param interval Type of interval calculation (can be abbreviated); see
#' \code{\link[stats]{predict}} for details.
#' @param level tolerance/confidence level; see \code{\link[stats]{predict}}
#' and \code{\link[stats]{confint}} for details.
#' 
#' @export
plot.calibration <- function(x, interval = "conf", level = 0.95, ...) {
  model <- x$model
  
  conc <- model$model[,2]
  new <- data.frame(conc = seq(min(conc), max(conc), length.out = 100 * length(conc)))
  names(new) <- all.vars(model$formula)[2]
  pred <- data.frame(new, predict(x$model, new, interval = interval, level = level))
  
  plot(model$formula, data = model$model, ...)
  lines(pred[, 2] ~ pred[, 1])
  
  tryCatch(
    {
    lines(pred[, 3] ~ pred[, 1], lty = 2)
    lines(pred[, 4] ~ pred[, 1], lty = 2)
    }, error = function(e) invisible()
  )
}

#' @rdname calibration
#' 
#' @param x,object an object of class \code{calibration} with a model formula
#' as shown above.
#' @param alpha error tolerance for the detection limit (critical value).
#'
#' @examples
#' lod(din)
#' 
#' @export
lod <- function(object, ...) {
  UseMethod("lod")
}

#' @export
lod.default <- function(object, ...) {
  stop("object needs to be of class 'calibration'")
}

#' @rdname calibration
#' 
#' @export
lod.calibration <- function(object, alpha = 0.01, level = 0.05, ...) {
  model <- object$model
  
  conc <- model$model[,2]
  n <- length(conc)
  m <- mean(table(conc))
  digs <- max(nchar(gsub("(.*\\.)|([0]*$)", "", as.character(conc)))) + 1

  if (m != round(m)) warning("measurement replicates of unequal size; ",
                             "LOD estimation might be incorrect")
  if (n <= model$rank) stop("data points less than degrees of freedom")

  b <- coef(model)[2]

  if (length(object$blanks) > 1) {
    # Direct method (LOD from blanks)
    sl <- sd(object$blanks) / b
    val <- sl * -qt(alpha, n - 1) * sqrt(1/n + 1/m)
  } else {
    # Indirect method (LOD from calibration curve)
    if (length(object$blanks) == 1) {
      message("only one blank value supplied; LOD is estimated from the calibration curve")
    } else {
      message("no blanks provided; LOD is estimated from the calibration curve") 
    }
    sx0 <- summary(model)$sigma / b
    Qx <- sum((conc - mean(conc))^2) / m
    val <- sx0 * -qt(alpha, n - model$rank) * sqrt(1/n + 1/m + (mean(conc))^2 / Qx)
  }
  res <- c(val, .conf(n, level) * val)
  matrix(round(res, digs), nrow = 1, dimnames = list("LOD", names(res)))
}

#' @rdname calibration
#' 
#' @param k relative uncertainty for the limit of quantification (1/beta).
#' @param maxiter a positive integer specifying the maximum number of iterations
#' to calculate the LOQ.
#' 
#' @examples
#' loq(din)
#' 
#' @export
loq <- function(object, ...)
  UseMethod("loq")

#' @export
loq.default <- function(object, ...) {
  stop("object needs to be of class 'calibration'")
}

#' @rdname calibration
#' 
#' @export
loq.calibration <- function(object, alpha = 0.01, k = 3, level = 0.05,
                            maxiter = 10, ...) {
  model <- object$model
  
  conc <- model$model[,2]
  n <- length(conc)
  m <- mean(table(conc))
  digs <- max(nchar(gsub("(.*\\.)|([0]*$)", "", as.character(conc)))) + 1
  
  if (m != round(m)) warning("measurement replicates of unequal size; ",
                             "LOQ estimation might be incorrect")
  if (n <= model$rank) stop("data points less than degrees of freedom")
  
  b <- coef(model)[2]
  
  sx0 <- summary(model)$sigma / b
  Qx <- sum((conc - mean(conc))^2) / m
  
  val <- k * object$lod[1]
  for (i in 1:maxiter) {
    prval <- val
    val <- k * sx0 * -qt(alpha/2, n - model$rank) *
      sqrt(1/n + 1/m + (val - mean(conc))^2 / Qx)
    if (round(prval, digs) == round(val, digs)) break
  }
  
  res <- c(val, .conf(n, level) * val)
  matrix(round(res, digs), nrow = 1, dimnames = list("LOQ", names(res)))
}

# Auxiliary function for confidence intervals of LOD/LOQ
.conf <- function(n, level = 0.05) {
  kappa <- sqrt((n - 1) / qchisq(c(1 - level/2, level/2), n - 1))
  names(kappa) <- c('lwr', 'upr')
  return(kappa)
}

# TODO: Inverse predict?
# TODO: Add "Erfassungsgrenze"
# TODO: Check alpha/beta, also to be passed from calibration to lod
