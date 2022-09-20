#' @title Sorption isotherms
#' 
#' @description
#' This function returns the concentration of a substance sorbed to a surface boundary
#' after an equilibrium has established at constant temperature given the concentration(s)
#' \code{conc} of the dissolved substance.
#' 
#' @param conc a numeric vector containing the concentration(s) of the dissolved
#' substance.
#' @param pars a numeric vector specifying the function parameters, see examples
#' for details and correct order.
#' @param type a character string indicating the type of sorption isotherm to be used:
#' \code{"linear"} for the linear type,
#' \code{"freundlich"} for the Freundlich isotherm,
#' \code{"langmuir"} for the Langmuir isotherm,
#' \code{"BET"} for the BET model according to Brunauer, Emmet, and Teller
#' \code{"redlich"} for the Redlich-Peterson isotherm.
#' 
#' @author
#' Zacharias Steinmetz
#' 
#' @examples
#' sorption(conc = 1:5, pars = c(Kd = 2.5), type = "linear")
#' sorption(conc = 1:5, pars = c(K = 4, n = 0.6), type = "freundlich")
#' sorption(conc = 1:5, pars = c(KL = 2, qmax = 10), type = "langmuir")
#' sorption(conc = 1:5, pars = c(K = 50, qmax = 10, Csat = 10), type = "BET")
#' sorption(conc = 1:5, pars = c(A = 30, B = 0.8), type = "redlich")
#' 
#' @references
#' Atkins, P.W. (2001). \emph{Physical chemistry}, Oxford University Press, Oxford.
#' 
#' @export
sorption <- function(conc, pars, type = "freundlich") {
  len <- switch(type,
                linear = 1,
                freundlich = 2,
                langmuir = 2,
                redlich = 2,
                BET = 3,
                { stop("Sorption isotherm (type) unknown") })
  if (!inherits(pars, "numeric") | length(pars) != len)
    stop(paste("pars requires a numeric vector of length =", len))
  
  eqn <- switch(type, 
         linear = conc * pars[1],
         freundlich = pars[1] * (conc^pars[2]),
         langmuir = (pars[1] * pars[2] * conc) / (1 + pars[1] * conc),
         redlich = (conc * pars[1]) / (1 + conc^pars[2]),
         BET = (pars[1] * pars[2] * conc) / ((pars[3] - conc) * 
                 (1 + (((pars[1] - 1) * conc) / pars[3])))
  )
  return(eqn)
}
