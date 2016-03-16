#' @title Convert frequencies to raw data
#' 
#' @description
#' Convert a data frame or matrix containing summarized frequencies information back
#' to raw data.
#' 
#' @usage
#' make.raw(data, var.name, freq.name)
#'
#' @param data a data frame or matrix containing the frequency information
#' @param var.name a character string specifying the column with the respective variable
#' @param freq.name a character string specifying the column with the respective frequency information
#'
#' @examples
#' data <- data.frame(var = c(10.2,15,5), freq=c(7,6,10))
#' make.raw(data, "var", "freq")
#' 
#' @seealso
#' \code{\link[graphics]{hist}}
#' 
#' @export
make.raw <- function(data, var.name, freq.name) {
  if (missing(data)) {
    stop("Input data missing, with no default")
  }
  if (missing(var.name) | missing(freq.name)) {
    stop("Column names containing the variables and/or frequencies must be specified")
  }
  if (!class(data) %in% c("data.frame","matrix")) {
    stop("Input data needs to be of class 'data.frame' or 'matrix'")
  }
  else
  {
    if (class(data) == "matrix") {
      data <- data.frame(data)
    }
  }
  if (!var.name %in% names(data) | !freq.name %in% names(data)) {
    stop("The specified variables/frequency names are no columns of the provided data frame")
  }
  
  out <- data.frame()
  for (i in 1:nrow(data)) {
    out <- rbind(out, data.frame(data[i,setdiff(names(data),c(freq.name,var.name))],
                                 var=rep(data[i,var.name], data[i,freq.name]),
                                 row.names=NULL),
                 row.names=NULL)
  }
  names(out)[names(out)=="var"] <- var.name
  return(out)
}
