texture <- function(time, temp, reading, blank, data, conc = 50, Gs = 2.65,
                    hydrometer = "auto", plot = F) {
  data <- na.omit(data)
  if (!missing(data)) {
    time <- data[, deparse(substitute(time))]
    temp <- data[, deparse(substitute(temp))]
    reading <- data[, deparse(substitute(reading))]
    blank <- data[, deparse(substitute(blank))]
  }
  Gs <- round(Gs, 2)
  temp <- round(temp)
  
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
    
  corr_val <- (reading - blank) + ct[as.character(temp)]
  eff_depth <- (int - sl * corr_val) + 1 / 2 * (14 - (67 / 27.8))
  diam <- k[as.character(temp),as.character(Gs)] * sqrt(eff_depth / time)
  perc_pass <- corr_val*alpha / conc

  dist <-  data.frame("particle.size" = diam, "perc.passing" = perc_pass)
  
  # Fit NLS model
  fit <- nls(perc.passing ~ SSlogis(particle.size, Asym, xmid, scal),
             data = dist)
  fun <- function(x) SSlogis(x, coef(fit)[1], coef(fit)[2], coef(fit)[3])
  if (plot) {
    plot(perc.passing ~ particle.size, data = dist,
         main = 'Particle size distribution',
         xlab = "Particle size", ylab = "Percent passing", log = "x")
    curve(fun, add = T)
  }
  bounds <- fun(c(0.002, 0.063, 0.05))
  din <- c(bounds[1], bounds[2] - bounds[1], 1 - bounds[2])
  usda <- c(bounds[1], bounds[3] - bounds[1], 1 - bounds[3])
  names(din) <- names(usda) <- c('Clay', 'Silt', 'Sand')
  
  out <- list(dist = dist, din = din, usda = usda, model = fit,
              info = c(Hydrometer = hydrometer, Gs = Gs, Conc = conc))
  class(out) <- 'texture'
  return(out)
}

print.texture <- function(obj, ...) {
  cat(paste0('Soil particle estimation according to ASTM D422-63\n',
             'Hydrometer model: ', obj$info[1], '\n',
             'Specific gravity (Gs) = ', obj$info[2], '   Soil extract: ',
              obj$info[3], ' g/L\n\n',
             'Particle size distribution:\n'))
  names(obj$dist) <- c('Particle size', 'Percent passing')
  print(obj$dist, digits = 3, row.names = F)
  cat(paste0('\n',
             'Soil texture classes:\n'))
  print(rbind(" DIN" = obj$din, " USDA" = obj$usda), digits = 3)
}

plot.texture <- function(obj, main = 'Particle size distribution',
                         xlab = "Particle size", ylab = "Percent passing", ...) {
  plot(perc.passing ~ particle.size, data = obj$dist,
       main = main, xlab = xlab, ylab = ylab, log = "x")
  fun <- function(x) SSlogis(x, coef(obj$model)[1], coef(obj$model)[2],
                             coef(obj$model)[3])
  curve(fun, add = T)
}
