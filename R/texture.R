#' @family texture
#' @title ASTM soil texture analysis
#' 
#' @description
#' Calculates the particle size distribution and both DIN and USDA texture
#' classes from a series of hydrometer readings according to ASTM D422-63(2007)e2. 
#' 
#' @usage
#' texture(time, reading, blank, temp, data, conc = 50, Gs = 2.65,
#' hydrometer = "auto", plot = F)
#'
#' @param time a numeric vector or data frame object containing the time passed
#' since the beginning of the measurement in minutes
#' @param reading a numeric vector or data frame object providing the actual hydrometer
#' readings at the bottom of the meniscus
#' @param blank a numeric vector or data frame object containing the blank readings
#' taken in 5 g L-1 sodium hexametaphosphate solution (composite correction)
#' @param temp a numeric vector or data frame object containing the measured
#' temperature
#' @param data a data frame containing the specified columns. If empty, data
#' need to be given as numeric vectors
#' @param conc the concentration of the soil solution, default = 50 g L-1 as
#' proposed in the ASTM guideline
#' @param Gs specific gravity of the suspension, default = 2.65
#' @param hydrometer a character sting specifying the hydrometer used. Accepted
#' values are \code{"auto"} for auto-detection (default), \code{"151H"}, and \code{"152H"}
#' @param plot logical; if TRUE the particle size distribution is plotted
#' 
#' @return
#' \code{texture} returns an object of class "texture". The functions
#' \code{print} and \code{plot} are available to retrieve the soil texture classes
#' and the particle size distribution, respectively.
#' 
#' An object of class "texture" is a list containing the following components:
#' 
#' 
#' @examples
#' data(clayloam)
#' texture(Time, Reading, Blank, Temperature, data = clayloam)
#'
#' @references
#' ASTM D422-63(2007)e2. 2007. Standard Test Method for Particle-Size Analysis
#' of Soils. ASTM International, West Conshohocken, PA. Available from
#' http://www.astm.org/Standards/D422.htm
#' 
texture <- function(time, reading, blank, temp, data, conc = 50, Gs = 2.65,
                    hydrometer = "auto", plot = F) {
  # Data preparation
  data <- na.omit(data)
  if (!missing(data)) {
    time <- data[, deparse(substitute(time))]
    temp <- data[, deparse(substitute(temp))]
    reading <- data[, deparse(substitute(reading))]
    blank <- data[, deparse(substitute(blank))]
  }
  Gs <- round(Gs, 2)
  temp <- round(temp)
  data(list = c("d422_k", "d422_ct"))
  
  # Error handling
  if (hydrometer == "auto") {
    if (class(reading) == "integer" & all(reading %in% 0:60)) {
      hydrometer <- '152H'
    } else {
      if (class(reading) == "numeric" & all(reading %in% 1:1.038)) {
        hydrometer <- '151H'
      } else {
        stop('Automatic detection failed. Specify the hydrometer used.')
      }
    }
  }
  if (hydrometer == '152H') {
    int <- 10.5; sl <- 0.164
    alpha <- 1.53 - 0.2 * Gs}
  if (hydrometer == '151H') {
    int <- 275.0161; sl <- 264.5161
    alpha <- 1}
  
  # Calculation according to ASTM
  corr_val <- (reading - blank) + ct[as.character(temp)]
  eff_depth <- (int - sl * corr_val) + 1 / 2 * (14 - (67 / 27.8))
  diam <- k[as.character(temp),as.character(Gs)] * sqrt(eff_depth / time)
  perc_pass <- corr_val*alpha / conc

  dist <-  data.frame("particle.size" = diam, "perc.passing" = perc_pass)
  
  # Fit NLS model
  fit <- nls(perc.passing ~ SSlogis(particle.size, Asym, xmid, scal),
             data = dist)
  
  # Retrieve main texture classes
  tex_classes <- function(x, obj) {
    bounds <- sigmoid(x, obj)
    
    matrix(
      c(bounds["estimate", 1], bounds["estimate", 2] - bounds["estimate", 1],
        1 - bounds["estimate", 2], bounds["st.error", 1], bounds["st.error", 2] +
          bounds["st.error", 1], bounds["st.error", 2]
      ), ncol = 3, byrow = T,
      dimnames = list(c("Estimate", "Std. Error"),
                      c('Clay', 'Silt', 'Sand'))
    )
  }
  
  din <- tex_classes(c(0.002, 0.063), fit)
  usda <- tex_classes(c(0.002, 0.05), fit)             
  
  out <- list(dist = dist, din = din, usda = usda, model = fit,
              info = c(Hydrometer = hydrometer, Gs = Gs, Conc = conc))
  class(out) <- c(class(out), 'texture')
  if (plot) plot(out)
  return(out)
}

#' @family texture
#' @rdname texture
print.texture <- function(x, ...) {
  cat(paste0('Soil particle estimation according to ASTM D422-63\n',
             'Hydrometer model: ', x$info[1], '\n',
             'Specific gravity (Gs) = ', x$info[2], '   Soil extract: ',
              x$info[3], ' g/L\n\n',
             'Particle size distribution:\n'))
  names(x$dist) <- c('Particle size', 'Percent passing')
  print(x$dist, digits = 3, row.names = F)
  cat(paste0('\n',
             'Soil texture classes (DIN):\n'))
  print(x$din, digits = 3)
  cat(paste0('\n',
             'Soil texture classes (USDA):\n'))
  print(x$usda, digits = 3)
}

#' @family texture
#' @rdname texture
plot.texture <- function(x, main = 'Particle size distribution',
                         xlab = "Particle size", ylab = "Percent passing", ...) {
  plot(perc.passing ~ particle.size, data = x$dist,
       main = main, xlab = xlab, ylab = ylab, log = "x", ...)
  curve(sigmoid(x, x$model)[1,], add = T)
}

#' @family texture
#' @rdname texture
sigmoid <- function(x, obj) {
  model_est <- summary(obj)$coefficients[,1]
  model_err <- summary(obj)$coefficients[,2]
  estimate <- SSlogis(x, model_est[1], model_est[2], model_est[3])
  lwr <- SSlogis(x, model_est[1]+model_err[1], model_est[2]+model_err[2],
                 model_est[3]+model_err[3])
  upr <- SSlogis(x, model_est[1]-model_err[1], model_est[2]-model_err[2],
                 model_est[3]-model_err[3])
  rbind(estimate, st.error = abs(upr - lwr)/2)
}
