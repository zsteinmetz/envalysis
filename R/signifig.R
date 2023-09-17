#' @title Print significant figures
#' 
#' @description
#' This function reports the significant figures of a given \code{mean} together
#' with its respective \code{error} term (for instance confidence interval or
#' standard deviation).
#'
#' @param mean a numeric vector or data frame object containing the averaged
#' values.
#' @param error a numeric vector or data frame object containing the respective
#' error terms.
#' @param data a data frame containing the specified columns. If empty,
#' \code{mean} and \code{error} need to be given as numeric vectors.
#' @param style a string specifying the output style to be used. The default
#' style \code{"pm"} reports the results as "3 ± 6", while \code{"par"} results
#' in outputs like "0.26 (0.02)". "siunitx" returns "0.26 (2)" which might be
#' used together with xtable for automated LaTeX table outputs.
#' @param na.digit an integer controlling to which significant digit the mean
#' value should be rounded if the error is zero or no error data was provided.
#' @param \dots arguments passed to \code{prettyNum}().
#' 
#' @examples
#' signifig(mean = c(0.28, 5, -31.6, 2.6, 2, NA, 27.1),
#'          error = c(0.688, 0.8, 11.6, 9.6, NA, 1.6, 0))
#' 
#' @author 
#' Zacharias Steinmetz
#' 
#' @references
#' Taylor, J.R. (1997). \emph{Error analysis: the study of uncertainties in physical
#' measurements}. University Science Books, Sausalito, CA.
#' 
#' @export
signifig <- function(mean, error, data, style = "pm", na.digit = 2, ...) {
  if (!missing(data)) {
    mean <- data[, deparse(substitute(mean))]
    error <- data[, deparse(substitute(error))]
  }
  if (length(mean) != length(error))
    stop("mean and error terms of unequal size", call. = F)
  if (any(error[!is.na(error)] < 0))
    stop("error term contains one or more negative values", call. = F)
  if (!style %in% c("pm", "par", "siunitx")) {
    warning("Style unknown, use 'pm' instead", call. = F)
    style <- "pm"
  }
  
  e <- signif(error, 1)
  eo <- prettyNum(e, ...)
  
  m <- mean
  
  if (any(e >= 1 & !is.na(e))) {
    m[e >= 1 & !is.na(e)] <- sprintf(
      round(mean[e >= 1 & !is.na(e)],
            -nchar(eo[e >= 1 & !is.na(e)]) + 1),
      fmt = "%1.0f")
  }
  if (any(e < 1 & !is.na(e))) {
    m[e < 1 & !is.na(e)] <- sprintf(
      round(mean[e < 1 & !is.na(e)],
            nchar(eo[e < 1 & !is.na(e)]) - 2),
      fmt = paste0("%1.", nchar(eo[e < 1 & !is.na(e)]) - 2, "f"))
  }
  if (any(is.na(e) | e == 0)) {
    m[is.na(e) | e == 0] <- round(mean[is.na(e) | e == 0], na.digit) 
  }
  
  mo <- prettyNum(m, ...)

  if (style == "pm") out <- paste(mo, "\u00b1", eo)
  if (style == "par") out <- paste0(mo," (", eo,")")
  if (style == "siunitx") {
    out <- paste0(suppressWarnings(as.numeric(m)),
                  "(", gsub("0.", "", e),")")
  } 

  return(out)
}
