#' @title Categorize water drop penetration times
#' 
#' @description
#' This wrapper function categorizes water drop penetration times (WDPT) in
#' seconds according to the scale proposed by Bisdom et al. (1993).
#' 
#' @param wdpt a numeric vector containing WDPT measurement data in seconds.
#' @param \dots arguments passed to \code{\link[base]{findInterval}()}.
#' 
#' @seealso
#' \code{\link[base]{findInterval}()} for the generic function.
#' 
#' @author 
#' Zacharias Steinmetz
#' 
#' @examples
#' bisdom(c(2,6,20,NA,3,385))
#' 
#' @references
#' Bisdom, E., Dekker, L., & Schoute, J. (1993). Water Repellency of Sieve 
#' Fractions from Sandy Soils and Relationships with Organic Material and Soil 
#' Structure. \emph{Geoderma} \bold{56}, 105-118. DOI:
#' \href{https://doi.org/10.1016/0016-7061(93)90103-R}{10.1016/0016-7061(93)90103-R}
#' 
#' @export
bisdom <- function(wdpt, ...) {
  findInterval(wdpt, c(0,5,60,600,3600), ...)
}
