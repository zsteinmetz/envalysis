#' @title Root mean square error
#' 
#' @description
#' This function computes the root mean square error (RMSE) of the two vectors
#' \code{obs} and \code{sim}. \code{rel = FALSE} returns the absolute RMSE,
#' \code{rel = TRUE} the relative one. If \code{na.rm} is \code{TRUE}, missing
#' values are omitted before the computation proceeds.
#'
#' @param obs a numeric vector containing observed values.
#' @param sim a numeric vector containing simulated values.
#' @param rel logical. If TRUE, the relative RMSE is calculated, if FALSE the
#' absolute RMSE is returned.
#' @param na.rm logical. Should missing values be removed?
#' 
#' @author 
#' Zacharias Steinmetz
#' 
#' @examples
#' rmse(c(0.12,0.59,NA), c(0.15,0.63,1.2))
#'
#' @importFrom  stats na.omit
#' @export
rmse <- function(obs, sim, rel = F, na.rm = T)
{
  data <- data.frame(obs = obs, sim = sim)
  if (na.rm) data <- na.omit(data)
  
  abs <- sqrt(mean((data$obs - data$sim)^2))

  if (rel) {
    return(abs / mean(data$obs))
  } else {
    return(abs)
  }
}
