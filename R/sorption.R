#' @title Sorption isotherms
#' 
#' @description
#' This function returns the concentration of a substance sorbed to a surface boundary
#' after an equilibrium has established at constant temperature given the concentration(s)
#' \code{conc} of the dissolved substance.
#' 
#' @usage
#' sorption(conc, par, type = "freundlich")
#'
#' @param conc a numeric vector containing the concentration(s) of the dissolved substance
#' @param par a numeric vector specifying the function parameters, see examples for
#' details and correct order
#' @param type a character string indicating the type of sorption isotherm to be used:
#' \code{"linear"} for the linear type,
#' \code{"freundlich"} for the Freundlich isotherm,
#' \code{"langmuir"} for the Langmuir isotherm,
#' \code{"BET"} for the BET model according to Brunauer, Emmet, and Teller
#' \code{"redlich"} for the Redlich-Peterson isotherm,
#' 
#' @examples
#' sorption(conc = 1:5, Kd = 2.5, type = "linear")
#' sorption(conc = 1:5, c(K = 4, n = 0.6), type = "freundlich")
#' sorption(conc = 1:5, c(KL = 2, qmax = 10), type = "langmuir")
#' sorption(conc = 1:5, c(K = 50, qmax = 10, Csat = 10), type = "BET")
#' sorption(conc = 1:5, c(A = 30, B = 0.8), type = "redlich")
#' 
#' @references
#' Atkins PW. 2001. Physical chemistry. Oxford Univ. Press, Oxford.
#' 
sorption <- function(conc, par, type = "freundlich") {
  length.check <- function(par, len) {
    if (class(par) != "numeric" | length(par) != len) stop(paste("par requires a numeric vector of length =", len))
  }
  
  if (type %in% c("linear", "lin")) {
    length.check(par, 1)
    eqn <- conc * par[1]
  }
  if (type %in% c("freundlich", "f")) {
    length.check(par, 2)
    eqn <- par[1]*(conc^par[2])
  }
  if (type %in% c("langmuir", "lang")) {
    length.check(par, 2)
    eqn <- (par[1]*par[2]*conc)/(1+par[1]*conc)
  }
  if (type %in% c("redlich", "r")) {
    length.check(par, 2)
    eqn <- (conc * par[1]) / (1+conc^par[2])
  }
  if (type %in% c("BET", "bet")) {
    length.check(par, 3)
    eqn <- (par[1]*par[2]*conc)/((par[3] - conc)*(1+(((par[1]-1)*conc)/par[3])))
  }
  return(eqn)
}
