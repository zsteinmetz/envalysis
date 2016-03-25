## ----setup, include=FALSE, echo=FALSE------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE, 
  message = FALSE,
  comment = "# >"
)

## ----texture-------------------------------------------------------------
# Install envalysis
#if (!'devtools' %in% installed.packages()[,'Package']) install.packages('devtools')
#devtools::install_github('zsteinmetz/envalysis')
require(envalysis)

# Load and look at sample data
data(clayloam)
clayloam

# Calculate the particle size distribution
texture(Time, Reading, Blank, Temperature, data = clayloam, plot = T)

