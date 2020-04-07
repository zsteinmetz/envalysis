## ----setup, include=FALSE, echo=FALSE-----------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE, 
  message = FALSE,
  comment = "# >"
)

## ----texture------------------------------------------------------------------
# Load envalysis
library(envalysis)

# Load and look at sample data
data(clayloam)
clayloam

# Calculate the particle size distribution
tex <- texture(reading ~ blank + time + temperature, clayloam, plot = T)
tex

## ----classify-----------------------------------------------------------------
# Load soiltexture
library(soiltexture)

# Prepare data
germansoil <- data.frame(t(tex$din["Estimate",] * 100))
names(germansoil) <- toupper(names(germansoil))

ussoil <- data.frame(t(tex$usda["Estimate",] * 100))
names(ussoil) <- toupper(names(ussoil))

# Get texture class, for example, in accordance with the German
# "Bodenartendiagramm" (DE.BK94.TT)
TT.points.in.classes(germansoil, class.sys = "DE.BK94.TT")

# Get USDA texture class (USDA.TT)
TT.points.in.classes(ussoil, class.sys = "USDA.TT")

