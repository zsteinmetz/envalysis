#' @family calibration
#'
#' @title Analytical calibration functions
#'
#' @description
#' Defines a '\code{calibration}' object for the calculation of concentrations
#' from measurement signals including estimations for the limit of detection
#' (LOD) and limit of quantification (LOQ) in accordance with DIN 32645 (2008).
#'
#' @param formula model formula providing the recorded signal intensities with
#' respect to the nominal/specified analyte concentrations in the form of
#' \code{signal ~ concentration} or \code{signal ~ concentration - 1}; model
#' formulas are currently restricted to those forms.
#' @param data an optional data frame containing the variables in the model.
#' @param blanks a vector of numeric blank values overriding those automatically
#' retrieved from calibration data.
#' @param weights an optional character string containing one or more model
#' variables, for example, in the form of "\code{1/concentration^0.5}" or
#' "\code{1/signal}" which is internally converted to a numeric vector and
#' passed to the fitting process of the selected model; see also
#' \code{\link{weight_select}()}
#' @param model model class to be used for fitting; currently,
#' \code{\link[stats]{lm}()} and \code{\link[MASS]{rlm}()} are supported.
#' @param check_assumptions automatically check for normality and
#' homoscedasticity of model residuals using \code{\link[stats]{shapiro.test}()}
#' and \code{\link[lmtest]{bptest}()}, respectively; only executed if
#' \code{weights == NULL}.
#' @param interval type of interval plotted (can be abbreviated); see
#' \code{\link[stats]{predict}()} for details.
#' @param level tolerance/confidence level; see \code{\link[stats]{predict}()}
#' and \code{\link[stats]{confint}()} for details.
#' @param which character vector indicating the parameters to export; defaults 
#' to \code{c("coef", "adj.r.squared", "lod", "loq", "blanks")}.
#' @param x,object an object of class '\code{calibration}' with a model formula
#' as shown above.
#' @param y numeric; the value to inverse predict.
#' @param alpha numeric; error tolerance for the detection limit (critical
#' value).
#' @param k numeric; relative uncertainty for the limit of quantification
#' (\code{1/beta}).
#' @param maxiter a positive integer specifying the maximum number of iterations
#' to calculate the LOQ.
#' @param below_lod value to be assigned if inverse prediction is below LOD;
#' defaults to \code{"NULL"} which keeps predicted values untouched. Other
#' options may be \code{NA} or \code{0}.
#' @param newdata a data frame in which to look for variables with which to
#' predict. If \code{NULL}, values are guessed; \code{\link[stats]{predict.lm}()}
#' for details.
#' @param method character indicating the method used for inverse prediction;
#' defaults to \code{"analytic"}.
#' @param \dots further arguments passed to submethods; for
#' instance, the respective model environment such as \code{\link[stats]{lm}()},
#' \code{\link[base]{print}()}, or \code{\link[graphics]{plot}()}.
#' 
#' @details
#' The LOD is defined as the lowest quantity of a substance that can be
#' distinguished from the absence of that substance (blank value) within a given
#' confidence level (\code{alpha}). The LOQ is defined as the lowest quantity of
#' a substance that can be quantified/distinguished from another sample given
#' with respect to a defined confidence level (\code{k}).
#'
#' If the \code{data} supplied to \code{calibration} contain more than one blank
#' value, namely measurements with a nominal/specified concentration of or close
#' to zero, the LOD and LOQ are calculated from the deviation of the blank
#' samples. This method is called "blank method" according to DIN 32645 (2008)
#' and supposed to be more accurate than the so-called "calibration method"
#' which will be used for the estimation of LOD and LOQ when \code{data} does
#' not contain zero concentration measurements.
#' 
#' @return
#' \code{calibration} returns an object of \code{\link[base]{class}}
#' '\code{calibration}'.
#' 
#' @author 
#' Zacharias Steinmetz
#' 
#' @examples
#' data(din32645)
#' din <- calibration(Area ~ Conc, data = din32645)
#'
#' @references
#' Almeida, A.M.D., Castel-Branco, M.M., & Falcao, A.C. (2002). Linear
#' regression for calibration lines revisited: weighting schemes for
#' bioanalytical methods. \emph{Journal of Chromatography B}, \bold{774}(2),
#' 215-222. \doi{10.1016/S1570-0232(02)00244-1}.
#' 
#' Currie, L.A. (1999). Nomenclature in evaluation of analytical methods
#' including detection and quantification capabilities: (IUPAC Recommendations
#' 1995). \emph{Analytica Chimica Acta} \bold{391}, 105-126.
#' 
#' DIN 32645 (2008). \emph{Chemical analysis - Decision limit, detection limit
#' and determination limit under repeatability conditions - Terms, methods,
#' evaluation}. Technical standard. Deutsches Institut für Normung, Berlin.
#' 
#' Massart, D.L., Vandeginste, B.G., Buydens, L.M.C., Lewi, P.J., &
#' Smeyers-Verbeke, J. (1997). \emph{Handbook of chemometrics and qualimetrics:
#' Part A}. Elsevier Science Inc.
#'
#' @importFrom stats qt coef qchisq model.frame shapiro.test
#' @importFrom graphics plot lines
#' @importFrom lmtest bptest
#' @export
calibration <- function(formula, data = NULL, blanks = NULL, weights = NULL,
                        model = "lm", check_assumptions = TRUE, ...) {
  if(missing(formula) || (length(formula) != 3L) || 
     (length(attr(terms(formula[-2L]), "term.labels")) != 1L))
    stop("'formula' missing or incorrect", call. = F)
  
  mf <- model.frame(formula, data)
  newdata <- mf[mf[2L] != 0, ]
  if(is.null(blanks)) blanks <- mf[mf[2L] == 0, 1]
  
  if(!is.null(weights) & length(weights) == 1 & is.character(weights)) {
    newweights <- with(newdata, eval(parse(text = weights)))
  } else {
    if(is.null(weights) | length(weights) == nrow(newdata)) {
      newweights <- weights
    } else {
      stop("'weights' needs to be a single character value or a numeric vector ",
           "of the same length as non-zero concentration data (without blanks)",
           call. = F)
    }
  }

  model <- do.call(model, list(formula = formula, data = newdata,
                               weights = newweights, ...))
  model$call <- match.call(expand.dots = F)
  model$formula <- formula
  
  cal <- structure(list(), class = "calibration")
  cal$model <- model
  
  cal$adj.r.squared <- summary(model)$adj.r.squared
  if(is.null(cal$adj.r.squared)) cal$adj.r.squared <- NA
  
  cal$data <- data
  cal$blanks <- blanks
  cal$lod <- lod(cal)
  cal$loq <- loq(cal)
  cal$relerr <- relerr(cal)
  
  if(is.null(weights) & check_assumptions) {
    cal$shapiro.test <- shapiro.test(model$residuals)
    cal$shapiro.test$data.name <- paste0("residuals(", deparse(model$call), ")")
    cal$bptest <- bptest(model)
    cal$bptest$data.name <- deparse(formula)
    
    if(cal$shapiro.test$p.value < 0.05 || cal$bptest$p.value < 0.05)
      warning("Model assumptions may not be met; double check graphically and ",
              "consider using a weighted model instead", call. = F)
  }
  
  return(cal)
}

#' @rdname calibration
#' 
#' @return
#' \code{print()} calls the function parameters together with the respective LOD
#' and LOQ.
#' \code{summary()} may be used to retrieve the summary of the underlying model.
#' \code{plot()} plots the respective calibration curve together with the
#' measurement values.
#' 
#' @examples
#' print(din)
#' summary(din)
#' plot(din)
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
  if(!is.null(x$shapiro.test) & !is.null(x$bptest)) {
    cat("\n")
    cat("Check for normality of residuals:\n")
    print(x$shapiro.test)
    cat("Check for homoscedasticity of residuals:\n")
    print(x$bptest)
  }
}

#' @rdname calibration
#' 
#' @export
summary.calibration <- function(object, ...) {
  summary(object$model, ...)
}

#' @rdname calibration
#' 
#' @export
plot.calibration <- function(x, interval = "conf", level = 0.95, ...) {
  pred <- predict(x, interval = interval, level = level)
  
  plot(x$model$formula, data = x$model$model, ...)
  lines(pred[, 2] ~ pred[, 1])
  
  tryCatch({
    lines(pred[, 3] ~ pred[, 1], lty = 2)
    lines(pred[, 4] ~ pred[, 1], lty = 2)
  }, error = function(e) invisible()
  )
}

#' @rdname calibration
#' 
#' @return
#' \code{as.list()} returns a named list.
#' 
#' @examples
#' as.list(din)
#' 
#' @export
as.list.calibration <- function(x, which = c("coef", "adj.r.squared", "lod",
                                             "loq", "blanks"), ...) {
  res <- list()
  
  if("coef" %in% which) res <- c(res, as.list(x$model$coefficients))
  if("adj.r.squared" %in% which) res <- c(res, adj.r.squared = x$adj.r.squared)
  if("lod" %in% which) res <- c(res, lod = x$lod[1])
  if("loq" %in% which) res <- c(res, loq = x$loq[1])
  if("blanks" %in% which) res <- c(res, blank_mean = mean(x$blanks),
                                   blank_sd = sd(x$blanks))
  
  return(res)
}

#' @rdname calibration
#' 
#' @return
#' \code{lod()} and \code{loq()} return a named vector with the LOD and LOQ
#' together with lower and upper confidence limits.
#'
#' @examples
#' lod(din)
#' loq(din)
#' 
#' @export
lod <- function(x, ...) {
  UseMethod("lod")
}

#' @rdname calibration
#'
#' @export
lod.default <- function(x,  ...) {
  stop("object 'x' needs to be of class 'calibration'")
}

#' @rdname calibration
#' 
#' @export
lod.calibration <- function(x, blanks = NULL, alpha = 0.01, level = 0.05, ...) {
  model <- x$model
  
  conc <- model$model[,2]
  n <- length(conc)
  m <- mean(table(conc))
  digs <- max(nchar(gsub("(.*\\.)|([0]*$)", "", as.character(conc)))) + 1

  if(m != round(m)) warning("Measurement replicates of unequal size; ",
                            "LOD estimation might be incorrect", call. = F)
  if(n <= model$rank) stop("data points less than degrees of freedom", call. = F)

  b <- coef(model)[2]

  if(is.null(blanks)) blanks <- x$blanks
  
  if(length(blanks) > 1) {
    # Direct method (LOD from blanks)
    sl <- sd(blanks) / b
    val <- sl * -qt(alpha, n - 1) * sqrt(1/n + 1/m)
  } else {
    # Indirect method (LOD from calibration curve)
    message("number of blank values <= 1; LOD is estimated from the ",
            "calibration curve")

        sx0 <- summary(model)$sigma / b
    Qx <- sum((conc - mean(conc))^2) / m
    val <- sx0 * -qt(alpha, n - model$rank) * sqrt(1/n + 1/m + (mean(conc))^2 /
                                                     Qx)
  }
  res <- c(val, .conf(n, level) * val)
  
  return(matrix(round(res, digs), nrow = 1, dimnames = list("LOD", names(res))))
}

#' @rdname calibration
#' 
#' @export
loq <- function(x, ...) {
  UseMethod("loq")
}

#' @rdname calibration
#' 
#' @export
loq.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'calibration'")
}

#' @rdname calibration
#' 
#' @export
loq.calibration <- function(x, blanks = NULL, alpha = 0.01, k = 3, level = 0.05,
                            maxiter = 10, ...) {
  model <- x$model
  
  conc <- model$model[,2]
  n <- length(conc)
  m <- mean(table(conc))
  digs <- max(nchar(gsub("(.*\\.)|([0]*$)", "", as.character(conc)))) + 1
  
  if(m != round(m)) warning("Measurement replicates of unequal size; ",
                            "LOQ estimation might be incorrect", call. = F)
  if(n <= model$rank) stop("data points less than degrees of freedom",
                           call. = F)
  
  b <- coef(model)[2]
  
  sx0 <- summary(model)$sigma / b
  Qx <- sum((conc - mean(conc))^2) / m
  
  if(is.null(blanks)) blanks <- x$blanks
  
  val <- k * lod(x, blanks, alpha, level)[1]
  for(i in 1:maxiter) {
    prval <- val
    val <- k * sx0 * -qt(alpha/2, n - model$rank) *
      sqrt(1/n + 1/m + (val - mean(conc))^2 / Qx)
    if(round(prval, digs) == round(val, digs)) break
  }
  
  res <- c(val, .conf(n, level) * val)
  matrix(round(res, digs), nrow = 1, dimnames = list("LOQ", names(res)))
}

#' @rdname calibration
#' 
#' @return
#' \code{predict()} returns a \code{data.frame} of predictions.
#' 
#' @examples
#' predict(din)
#' 
#' @export
predict.calibration <- function(object, newdata = NULL, interval = "conf", ...) {
  model <- object$model
  conc <- model$model[,2]
  
  if(is.null(newdata)) {
    newdata <- data.frame(conc = seq(min(conc), max(conc),
                                     length.out = 100 * length(conc)))
    names(newdata) <- all.vars(model$formula)[2]
  }
    
  data.frame(newdata, predict(object$model, newdata, interval = interval, ...))
}

#' @rdname calibration
#' 
#' @return
#' \code{inv_predict()} predicts/calculates analyte concentrations from signal
#' intensities.
#' 
#' @examples
#' inv_predict(din, 5000)
#' 
#' @seealso
#' \code{\link[investr]{invest}()} for alternative inverse prediction methods;
#' 
#' @export
inv_predict <- function(x, ...) {
  UseMethod("inv_predict")
}

#' @rdname calibration
#' 
#' @export
inv_predict.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'calibration'")
}

#' @rdname calibration
#' 
#' @export
inv_predict.calibration <- function(x, y, below_lod = NULL,
                                    method = "analytic", ...) {
  mf <- x$model$model

  if(method == "analytic") {
    if(!inherits(x$model, "lm"))
      stop("calibration model needs to be of type 'lm'; try method = 'invest' ",
           "instead (requires investr)", call. = F)
    xprd <- (y - coef(x$model)[[1]]) / coef(x$model)[[2]]
  } else if(method == "invest") {
    xprd <- sapply(y, function(y) {
      do.call(method, list(x$model, y, ...))$estimate |> 
        tryCatch(error = function(e) NA)
    })
  } else {
    xprd <- do.call(method, list(x$model, y, ...))
  }
  
  nm <- names(mf)[2L]
  if(any(xprd > max(mf[2L]), na.rm = T) || any(xprd < min(mf[2L]), na.rm = T) ||
     any(is.na(xprd))) {
    warning(paste0("'", nm, "' out of calibration range"), call. = F)
  } else if(any(xprd < x$lod[[1]], na.rm = T)) {
    warning(paste0("'", nm, "' below LOD"), call. = F)
  }
  
  if(!is.null(below_lod)) xprd[xprd < x$lod[[1]]] <- below_lod
  
  return(xprd)
}

# Auxiliary function for confidence intervals of LOD/LOQ
.conf <- function(n, level = 0.05) {
  kappa <- sqrt((n - 1) / qchisq(c(1 - level/2, level/2), n - 1))
  names(kappa) <- c('lwr', 'upr')
  return(kappa)
}

# Avoid "object not found" errors when testing
.datatable.aware <- TRUE
