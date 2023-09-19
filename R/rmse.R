#' @title Root mean square error
#' 
#' @description
#' This function computes the root mean square error (RMSE) between a vector of
#' observed values \code{x} and simulated values \code{y}. \code{rel = FALSE}
#' returns the absolute RMSE, \code{rel = TRUE} the relative one. If
#' \code{na.rm} is \code{TRUE}, missing values are omitted before the
#' computation proceeds.
#'
#' @param x a numeric vector containing observed values.
#' @param y a numeric vector containing simulated values.
#' @param rel logical. If \code{TRUE}, the relative RMSE is calculated, if
#' \code{FALSE} the absolute RMSE is returned.
#' @param na.rm logical. Should missing values be removed?
#' 
#' @author 
#' Zacharias Steinmetz
#' 
#' @examples
#' rmse(c(0.12,0.59,NA), c(0.15,0.63,1.2))
#'
#' @importFrom stats na.omit
#' @export
rmse <- function(x, y, rel = F, na.rm = T)
{
  data <- data.frame(x = x, y = y)
  if(na.rm) data <- na.omit(data)
  
  abs <- sqrt(mean((data$x - data$y)^2))

  if(rel) {
    return(abs / mean(data$x))
  } else {
    return(abs)
  }
}
