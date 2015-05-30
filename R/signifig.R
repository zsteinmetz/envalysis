#' @title Print significant figures
#' 
#' @description
#' This function reports the significant figures of a given \code{mean} together
#' with its respective \code{error} term (e.g. confidence interval or standard deviation).
#' 
#' @usage
#' signifig(mean, error, data, method = "pm")
#'
#' @param mean a numeric vector or data frame object containing the averaged values
#' @param error a numeric vector or data frame object containing the respective error terms
#' @param data a data frame containing the specified columns. If empty, \code{mean}
#' and \code{error} need to be given as numeric vectors
#' @param method a string specifying the output method to be used. The default method
#' \code{"pm"} reports the results as "3 ± 6", while \code{"par"} results in outputs
#' like "401 (89)".
#' 
#' @examples
#' signifig(mean = c(0.28,5), error = c(0.688, 8), method = "par")
#' 
#' @references
#' Taylor JR. 1997. Error analysis: the study of uncertainties in physical measurements.
#' University Science Books, Sausalito, CA.
#' 
signifig <- function(mean, error, data, method = "pm") {
  if (!missing(data)) {
    mean <- data[, deparse(substitute(mean))]
    error <- data[, deparse(substitute(error))]
  }
  if (length(mean) != length(error)) stop("Mean and Error term of unequal length")
  if (!method %in% c("pm", "par")) {
    stop("Method unknown, use 'pm' instead")
    mode <- "pm"
  }
  
  output <- c()
  for (i in 1:length(mean)) {
    e <- signif(error[i], 1)
    if (e >= 1) { m <- signif(mean[i], 1)
    } else { m <- round(mean[i], nchar(as.character(e))-2) }
    if (method == "pm") output <- c(output, paste(m,"\u00b1", e))
    if (method == "par") output <- c(output, paste(m," (", e,")", sep=""))
  }
  return(output)
}
