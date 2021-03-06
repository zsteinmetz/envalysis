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
#' @param signif.na an integer controlling to which significant digit the mean
#' value should be rounded when no error data was given.
#' @param style a string specifying the output style to be used. The default
#' style \code{"pm"} reports the results as "3 ± 6", while \code{"par"} results
#' in outputs like "0.26 (0.02)". "siunitx" returns "0.26 (2)" which might be
#' used together with xtable for automated LaTeX table outputs.
#' 
#' @examples
#' signifig(mean = c(0.28,5), error = c(0.688, 8))
#' 
#' @author 
#' Zacharias Steinmetz
#' 
#' @references
#' Taylor, J.R. (1997). \emph{Error analysis: the study of uncertainties in physical
#' measurements}. University Science Books, Sausalito, CA.
#' 
#' @export
signifig <- function(mean, error, data, signif.na = 2, style = "pm") {
  if (!missing(data)) {
    mean <- data[, deparse(substitute(mean))]
    error <- data[, deparse(substitute(error))]
  }
  
  if (length(mean) != length(error)) stop("mean and error term of unequal size")
  if (!style %in% c("pm", "par", "siunitx")) {
    warning("style unknown, use 'pm' instead")
    style <- "pm"
  }
  
  e <- signif(error, 1)
  m <- ifelse(is.na(e) | e == 0, signif(mean, signif.na),
              ifelse(e >= 1, round(mean, -nchar(as.character(e)) + 1),
                     format(round(mean, nchar(as.character(e)) - 2),
                            nsmall = nchar(as.character(e)) - 2))
              )
  l <- as.numeric(substr(e, nchar(e), nchar(e)))

  if (style == "pm") output <- paste(m, "\u00b1", e)
  if (style == "par") output <- paste0(m," (", e,")")
  if (style == "siunitx") output <- paste0(m,"(", l,")")

  return(output)
}
