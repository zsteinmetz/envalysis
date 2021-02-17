#' @family texture
#' 
#' @title ASTM soil texture analysis
#' 
#' @description
#' Calculates the particle size distribution and both DIN and USDA texture
#' classes from a series of hydrometer readings according to
#' ASTM D422-63 (2007). 
#' 
#' @param formula an object of class '\code{\link[stats]{formula}}' of the form
#' \code{reading ~ blank + time + temp}.
#' @param data a data frame containing the variables in \code{formula}.
#' @param reading a numeric vector of data values providing the hydrometer
#' readings at the bottom of the meniscus.
#' @param blank a numeric vector containing the blank readings taken in 5 g/L
#' sodium hexametaphosphate solution (composite correction).
#' @param time a numeric vector containing the time passed since the beginning
#' of the measurement in minutes.
#' @param temp an integer vector containing the measured temperature.
#' @param conc the concentration of the soil solution, default is 50 g/L as
#' proposed in the ASTM guideline.
#' @param Gs specific gravity of the suspension.
#' @param hydrometer a character string specifying the hydrometer used; accepted
#' values are \code{"auto"} for auto-detection (default), \code{"151H"}, and
#' \code{"152H"}.
#' @param model string is passed to \code{\link[drc]{drm}()}, "auto" chooses the
#' best fitting model automatically.
#' @param plot logical; if \code{TRUE} the particle size distribution is plotted.
#' 
#' @return
#' \code{texture} returns an object of \code{\link[base]{class}} '\code{texture}.
#' The functions \code{print}() and \code{plot}() are available to retrieve the
#' soil texture classes and the particle size distribution, respectively.
#' 
#' An object of class '\code{texture}' is a list containing the following
#' components:
#' 
#' \tabular{ll}{
#' \code{meta} \tab Measurement meta data\cr
#' \code{distribution} \tab data frame providing the particle size
#' distribution\cr
#' \code{model} \tab information on the fitted \code{\link[drc]{drm}} model\cr
#' \code{din} \tab Main DIN texture classes\cr
#' \code{usda} \tab Main USDA texture classes\cr
#' }
#' 
#' @author
#' Zacharias Steinmetz
#' 
#' @examples
#' data(clayloam)
#' 
#' ## Default method
#' attach(clayloam)
#' texture(reading, blank, time, temperature)
#' detach(clayloam)
#' 
#' ## Formula interface
#' texture(reading ~ blank + time + temperature, clayloam)
#'
#' @references
#' ASTM D422-63 (2007). \emph{Standard Test Method for Particle-Size Analysis
#' of Soils}. Technical standard. ASTM International, West Conshohocken, PA.
#' Available from \url{http://www.astm.org/Standards/D422.htm}
#' 
#' @importFrom stats terms predict na.omit
#' @importFrom graphics plot
#' @import drc
#' @export
texture <- function(reading, ...) {
  UseMethod("texture")
}

#' @rdname texture
#' 
#' @export
texture.formula <- function(formula, data = NULL, ...) {
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]), 
                                                                  "term.labels")) != 3L))
    stop("'formula' missing or incorrect")
  
  mf <- model.frame(formula, data)
  lst <- as.list(mf)
  names(lst) <- c("reading", "blank", "time", "temp")

  do.call("texture", c(lst, list(...)))
}

#' @rdname texture
#' 
#' @export
texture.default <- function(reading, blank, time, temp, conc = 50, Gs = 2.65,
                            hydrometer = "auto", model = "auto", plot = F, ...) {
  # Data preparation
  Gs <- round(Gs, 2)
  temp <- as.integer(round(temp))
  
  # Error handling
  if (length(Gs) != 1L) stop("'Gs' must be a single number")
  if (any(is.na(c(reading - blank, time, temp)))) warning("input data contains NAs")
  
  if (hydrometer == "auto") {
    if (all(reading == round(reading), na.rm = T) & all(reading %in% c(0:60, NA))) {
      hydrometer <- "152H"
    } else {
      if (all(reading != round(reading), na.rm = T) & all(reading %in% c(1:1.038, NA))) {
        hydrometer <- "151H"
      } else {
        stop("automatic detection failed; specify the hydrometer used")
      }
    }
  }
  if (hydrometer == "152H") {
    int <- 10.5; sl <- 0.164
    alpha <- 1.53 - 0.2 * Gs
    }
  if (hydrometer == "151H") {
    int <- 275.0161; sl <- 264.5161
    alpha <- 1
    }
  
  # Calculation according to ASTM
  temp_chr <- as.character(temp)
  temp_chr[is.na(temp_chr)] <- "NA"
  corr_val <- (reading - blank) + d422_ct[as.character(temp)]
  eff_depth <- (int - sl * corr_val) + 1 / 2 * (14 - (67 / 27.8))
  diam <- d422_k[temp_chr, as.character(Gs)] * sqrt(eff_depth / time)
  perc_pass <- corr_val * alpha / conc

  distr <- data.frame(particle.size = diam, perc.passing = perc_pass)

  # Fit DRC
  if (model == "auto") {
    init <- drm(perc.passing ~ particle.size, data = distr, fct = LL.2())
    fctList <- list(LL.2(), LL.3(), LL.3u(), LL.4(), LL.5(), W1.2(), W1.3(),
                    W1.4(), W2.2(), W2.3(), W2.4(), BC.4(), BC.5(), LL2.2(),
                    LL2.3(), LL2.3u(), LL2.4(), LL2.5(), MM.2(), MM.3())
    opt <- envalysis::mselect(init, fctList)
    fit <- drm(perc.passing ~ particle.size, data = distr,
               fct = eval(call(row.names(opt)[1])))
  } else {
    fit <- drm(perc.passing ~ particle.size, data = distr,
               fct = eval(call(model)))
  }
  
  din <- .textureclass(c(0.002, 0.063), fit)
  usda <- .textureclass(c(0.002, 0.05), fit)             
  
  out <- list(meta = c(Hydrometer = hydrometer, Gs = Gs, Conc = conc),
              distribution = distr, model = fit, din = din, usda = usda)
  class(out) <- c("texture", class(out))
  
  if (plot) plot(out)
  return(out)
}

#' @rdname texture
#' 
#' @param x an object of class '\code{texture}'.
#' @param \dots further arguments to be passed to \code{texture}() (currently not
#' used), \code{print}(), or \code{plot}().
#' 
#' @export
print.texture <- function(x, ...) {
  cat(paste0("Soil particle estimation according to ASTM D422-63\n",
             "Hydrometer model: ", x$meta[1], "\n",
             "Specific gravity (Gs) = ", x$meta[2], "   Soil extract: ",
             x$meta[3], " g/L\n\n",
             "Particle size distribution:\n"))
  names(x$distribution) <- c("Particle size", "Percent passing")
  print(x$distribution, digits = 3, row.names = F)
  cat("\n")
  cat(paste0("Fitted with ", x$model$fct$text, " (", x$model$fct$name,")\n"))
  cat(paste0("\n",
             "Soil texture classes (DIN 4022):\n"))
  print(x$din, digits = 3)
  cat(paste0("\n",
             "Soil texture classes (USDA):\n"))
  print(x$usda, digits = 3)
}

#' @rdname texture
#'
#' @export
plot.texture <- function(x, ...) plot(x$model, log = "x", type = "all", ...)

# Auxiliary function for retrieving main texture classes
.textureclass <- function(psize, object) {
  bounds <- predict(object, newdata = data.frame(particle.size = psize), se.fit = T)
  if (bounds[1, 1] < 0) bounds[1, 1] <- 0
  if (bounds[2, 1] > 1) bounds[2, 1] <- 1
  
  matrix(
    c(bounds[1, 1], bounds[2, 1] - bounds[1, 1], 1 - bounds[2, 1], bounds[1, 2],
      bounds[2, 2] + bounds[1, 2], bounds[2, 2]), ncol = 3, byrow = T,
    dimnames = list(c("Estimate", "Std. Error"),
                    c("Clay", "Silt", "Sand"))
  )
}
