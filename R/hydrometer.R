texture <- function(time, temp, reading, blank, data, conc = 50, Gs = 2.65,
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
    bounds <- sig(x, obj)
    
    matrix(
      c(
        bounds["estimate", 1],
        bounds["estimate", 2] - bounds["estimate", 1],
        1 - bounds["estimate", 2],
        bounds["st.error", 1],
        bounds["st.error", 2] +
          bounds["st.error", 1],
        bounds["st.error", 2]
      ),
      ncol = 3,
      byrow = T,
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

print.texture <- function(obj, ...) {
  cat(paste0('Soil particle estimation according to ASTM D422-63\n',
             'Hydrometer model: ', obj$info[1], '\n',
             'Specific gravity (Gs) = ', obj$info[2], '   Soil extract: ',
              obj$info[3], ' g/L\n\n',
             'Particle size distribution:\n'))
  names(obj$dist) <- c('Particle size', 'Percent passing')
  print(obj$dist, digits = 3, row.names = F)
  cat(paste0('\n',
             'Soil texture classes (DIN):\n'))
  print(obj$din, digits = 3)
  cat(paste0('\n',
             'Soil texture classes (USDA):\n'))
  print(obj$usda, digits = 3)
}

plot.texture <- function(obj, main = 'Particle size distribution',
                         xlab = "Particle size", ylab = "Percent passing", ...) {
  plot(perc.passing ~ particle.size, data = obj$dist,
       main = main, xlab = xlab, ylab = ylab, log = "x")
  curve(sig(x, obj$model)[1,], add = T)
}

# Sigmoid helper function
sig <- function(x, obj) {
  model_est <- summary(obj)$coefficients[,1]
  model_err <- summary(obj)$coefficients[,2]
  estimate <- SSlogis(x, model_est[1], model_est[2], model_est[3])
  lwr <- SSlogis(x, model_est[1]+model_err[1], model_est[2]+model_err[2], model_est[3]+model_err[3])
  upr <- SSlogis(x, model_est[1]-model_err[1], model_est[2]-model_err[2], model_est[3]-model_err[3])
  rbind(estimate, st.error = abs(upr - lwr)/2)
}
