#' @title Categorize water drop penetration times
#' 
#' @description
#' This wrapper function categorizes water drop penetration times (WDPT) [s] according to
#' the scale proposed by Bisdom et al. (1993).
#' 
#' @usage
#' bisdom(wdpt)
#'
#' @param wdpt a numeric vector containing WDPT measurement data in seconds
#' 
#' @seealso
#' \code{\link[base]{findInterval}} for the generic function.
#' 
#' @examples
#' bisdom(c(2,6,20,NA,3,385))
#' 
#' @references
#' Bisdom E, Dekker L, Schoute J. 1993. Water Repellency of Sieve Fractions from
#' Sandy Soils and Relationships with Organic Material and Soil Structure. Geoderma.
#' 56:105-118.
#' 
#' @export
bisdom <- function(wdpt) {
  return(findInterval(wdpt, c(0,5,60,600,3600)))
}
