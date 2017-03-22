#' @title Print significant figures
#' 
#' @description
#' This function reports the significant figures of a given \code{mean} together
#' with its respective \code{error} term (e.g. confidence interval or standard
#' deviation).
#'
#' @param mean a numeric vector or data frame object containing the averaged
#' values
#' @param error a numeric vector or data frame object containing the respective
#' error terms
#' @param data a data frame containing the specified columns. If empty,
#' \code{mean}
#' and \code{error} need to be given as numeric vectors
#' @param signif.na an integer controlling to which significant digit the mean
#' value should be rounded when no error data was given
#' @param style a string specifying the output style to be used. The default
#' style \code{"pm"} reports the results as "3 ± 6", while \code{"par"} results
#' in outputs like "0.26 (0.02)". "siunitx" returns "0.26 (2)" which might be
#' used together with xtable for automated LaTeX table outputs.
#' 
#' @examples
#' signifig(mean = c(0.28,5), error = c(0.688, 8))
#' 
#' @references
#' Taylor, J.R., 1997. Error analysis: the study of uncertainties in physical
#' measurements. University Science Books, Sausalito, CA.
#' 
#' @export
signifig <- function(mean, error, data, signif.na = 2, style = "pm") {
  if (!missing(data)) {
    mean <- data[, deparse(substitute(mean))]
    error <- data[, deparse(substitute(error))]
  }
  
  if (length(mean) != length(error)) stop("Mean and error term of unequal size")
  if (!style %in% c("pm", "par", "siunitx")) {
    warning("Style unknown, use 'pm' instead")
    style <- "pm"
  }
  
  # TODO: Replace for loop by lapply
  output <- c()
  for (i in 1:length(mean)) {
    e <- signif(error[i], 1)
    if (is.na(e) | e == 0) {
      m <- signif(mean[i], signif.na)
    } else {
      if (e >= 1) {
        m <- round(mean[i], -nchar(as.character(e))+1)
        l <- e
      } else {
        n <- nchar(as.character(e))-2
        m <- format(round(mean[i], n), nsmall = n)
        l <- as.numeric(substr(e, nchar(e), nchar(e)))
      }
    }
    if (style == "pm") output <- c(output, paste(m, "\u00b1", e))
    if (style == "par") output <- c(output, paste0(m," (", e,")"))
    if (style == "siunitx") output <- c(output, paste0(m,"(", l,")"))
  }
  return(output)
}
