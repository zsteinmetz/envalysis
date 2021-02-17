#' @title envalysis reimplementation of mselect
#' 
#' @description
#' This function should behave just like \code{\link[drc]{drm}} from \code{drc},
#' with the main difference that model objects are passed through the
#' function instead of requiring the data to be present in \code{.GlobalEnv}. If
#' you have trouble with this function, you can use \code{drc::mselect}
#' instead.
#' 
#' @usage
#' mselect(object, fctList = NULL, nested = FALSE, 
#' sorted = c("IC", "Res var", "Lack of fit", "no"), linreg = FALSE, icfct = AIC)
#' 
#' @param object an object of class \code{drc}.
#' @param fctList a list of dose-response functions to be compared.
#' @param nested logical; \code{TRUE} results in F tests between adjacent models
#' (in \code{fctList}; only sensible for nested models.
#' @param sorted character string determining according to which criterion the
#' model fits are ranked.
#' @param linreg logical indicating whether or not additionally polynomial
#' regression models (linear, quadratic, and cubic models) should be fitted
#' (they could be useful for a kind of informal lack-of-test consideration for
#' the models specified,  capturing unexpected departures).
#' @param icfct function for supplying the information criterion to be used.
#' \code{\link{AIC}} and \code{\link{BIC}} are two options.
#' 
#' @details
#' For Akaike's information criterion and the residual standard error: the
#' smaller the better and for lack-of-fit test (against a one-way ANOVA model): 
#' the larger (the p-value) the better. Note that the residual standard error is
#' only available for continuous dose-response data.
#' 
#' Log likelihood values cannot be used for comparison unless the models are
#' nested.
#' 
#' @return
#' A matrix with one row for each model and one column for each criterion.
#' 
#' @author
#' Christian Ritz
#' 
#' @examples
#' library(drc)
#' 
#' ryegrass.m1 <- drm(rootl ~ conc, data = ryegrass, fct = LL.4())
#' mselect(ryegrass.m1, list(LL.3(), LL.5(), W1.3(), W1.4(), W2.4(), baro5()))
#' 
#' @importFrom stats AIC logLik anova lm
#' @import drc
#' @export
mselect <- function(object, fctList = NULL, nested = FALSE,
                    sorted = c("IC", "Res var", "Lack of fit", "no"),
                    linreg = FALSE, icfct = AIC) {
  sorted <- match.arg(sorted)
  if (!is.logical(nested)) {
    stop("'nested' argument takes only the values: FALSE, TRUE")
  }
  contData <- identical(object$type, "continuous")
  nestedInd <- 3 + contData + nested
  mc <- match.call()
  lenFL <- length(fctList)
  retMat <- matrix(0, lenFL + 1, 3 + contData + nested)
  retMat[1, 1] <- logLik(object)
  retMat[1, 2] <- icfct(object)
  retMat[1, 3] <- modelFit(object)[2, 5]
  if (contData) {
    tryRV <- try(summary(object)$resVar, silent = TRUE)
    if (!inherits("tryRV", "try-error")) {
      retMat[1, 4] <- tryRV
    }
    else {
      retMat[1, 4] <- NA
    }
  }
  if (nested) {
    retMat[1, nestedInd] <- NA
  }
  fctList2 <- rep("", lenFL + 1)
  fctList2[1] <- object$fct$name
  if (!is.null(fctList)) {
    prevObj <- object
    for (i in 1:lenFL) {
      tempObj <- try(drm(object$call$formula, data = object$data,
                         fct = fctList[[i]]), 
                     silent = TRUE)
      fctList2[i + 1] <- fctList[[i]]$name
      if (!inherits(tempObj, "try-error")) {
        retMat[i + 1, 1] <- logLik(tempObj)
        retMat[i + 1, 2] <- icfct(tempObj)
        retMat[i + 1, 3] <- modelFit(tempObj)[2, 5]
        if (contData) {
          tryRV2 <- try(summary(tempObj)$resVar, silent = TRUE)
          if (!inherits("tryRV2", "try-error")) {
            retMat[i + 1, 4] <- tryRV2
          }
          else {
            retMat[i + 1, 4] <- NA
          }
        }
        if (nested) {
          retMat[i + 1, nestedInd] <- 
            anova(prevObj, tempObj, details = FALSE)[2, 5]
        }
      }
      else {
        retMat[i + 1, ] <- NA
      }
      prevObj <- tempObj
    }
  }
  rownames(retMat) <- as.vector(unlist(fctList2))
  cnames <- c("logLik", "IC", "Lack of fit")
  if (contData) {
    cnames <- c(cnames, "Res var")
  }
  if (nested) {
    cnames <- c(cnames, "Nested F test")
  }
  colnames(retMat) <- cnames
  if (linreg) {
    drcData <- as.data.frame(object$data[, c(2, 1)])
    names(drcData) <- c("yVec", "xVec")
    linFitList <- list(lm(yVec ~ xVec, data = drcData),
                       lm(yVec ~ xVec + I(xVec * xVec), data = drcData),
                       lm(yVec ~ xVec + I(xVec * xVec) + I(xVec * xVec * xVec),
                          data = drcData))
    linModMat <- matrix(unlist(lapply(linFitList, function(listObj) {
      c(logLik(listObj), icfct(listObj), NA, (summary(listObj)$sigma)^2)
    })), 3, 4, byrow = TRUE)
    rownames(linModMat) <- c("Lin", "Quad", "Cubic")
    colnames(linModMat) <- cnames[1:4]
    if (nested) {
      retMat <- retMat[, 1:4]
    }
    retMat <- rbind(retMat, linModMat)
  }
  if (sorted != "no") {
    return(retMat[order(retMat[, sorted]), ])
  }
  else {
    return(retMat)
  }
}
