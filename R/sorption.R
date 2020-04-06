#' @title Sorption isotherms
#' 
#' @description
#' This function returns the concentration of a substance sorbed to a surface boundary
#' after an equilibrium has established at constant temperature given the concentration(s)
#' \code{conc} of the dissolved substance.
#' 
#' @param conc a numeric vector containing the concentration(s) of the dissolved
#' substance.
#' @param par a numeric vector specifying the function parameters, see examples
#' for details and correct order.
#' @param type a character string indicating the type of sorption isotherm to be used:
#' \code{"linear"} for the linear type,
#' \code{"freundlich"} for the Freundlich isotherm,
#' \code{"langmuir"} for the Langmuir isotherm,
#' \code{"BET"} for the BET model according to Brunauer, Emmet, and Teller
#' \code{"redlich"} for the Redlich-Peterson isotherm.
#' 
#' @examples
#' sorption(conc = 1:5, c(Kd = 2.5), type = "linear")
#' sorption(conc = 1:5, c(K = 4, n = 0.6), type = "freundlich")
#' sorption(conc = 1:5, c(KL = 2, qmax = 10), type = "langmuir")
#' sorption(conc = 1:5, c(K = 50, qmax = 10, Csat = 10), type = "BET")
#' sorption(conc = 1:5, c(A = 30, B = 0.8), type = "redlich")
#' 
#' @references
#' Atkins, P.W., 2001. Physical chemistry, 6. ed., reprint. ed. Oxford Univ.
#' Press, Oxford.
#' 
#' @export
sorption <- function(conc, par, type = "freundlich") {
  len <- switch(type,
                linear = 1,
                freundlich = 2,
                langmuir = 2,
                redlich = 2,
                BET = 3,
                {stop("Sorption isotherm (type) unknown")})
  if (class(par) != "numeric" | length(par) != len) stop(paste("par requires a numeric vector of length =", len))
  
  eqn <- switch(type, 
         linear = conc*par[1],
         freundlich = par[1]*(conc^par[2]),
         langmuir = (par[1]*par[2]*conc)/(1+par[1]*conc),
         redlich = (conc * par[1]) / (1+conc^par[2]),
         BET = (par[1]*par[2]*conc)/((par[3] - conc)*(1+(((par[1]-1)*conc)/par[3])))
  )
  return(eqn)
}
