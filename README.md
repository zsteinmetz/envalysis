envalysis
=========

[![CRAN version](https://www.r-pkg.org/badges/version/envalysis)](https://CRAN.R-project.org/package=envalysis) 
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![R-CMD-check](https://github.com/zsteinmetz/envalysis/workflows/R-CMD-check/badge.svg)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/envalysis)](https://CRAN.R-project.org/package=envalysis)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1240304.svg)](https://doi.org/10.5281/zenodo.1240304)

**envalysis** is an R package containing miscellaneous functions for data
analyses in environmental chemistry and ecotoxicology. Provides, for example,
`calibration()` to calculate calibration curves and corresponding limits of
detection (LODs) and quantification (LOQs) according to German DIN
32645:2008-11. `texture()` makes it easy to estimate soil particle size
distributions from hydrometer measurements (ASTM D422-63(2007)e2).
Some functions of the package require **ggplot2** or **drc**.

## Functions
Currently, the following functions are available:

### Data manipulation and analysis

* Confidence intervals `CI()` and standard errors `se()`
* Root mean square errors `rmse()`
* Limit of detection (LOD) `lod()` and limit of quantification (LOQ) `loq()` as
part of the `calibration` class to produce linear calibration curves according
to German DIN 32645:2008-11
* Various sorption isotherms `sorption()`
* Determine particle size distributions and soil texture classes (DIN/USDA)
measured with a soil hydrometer in accordance with ASTM D422-63(2007)e2 using
`texture()`; see
[vignette](https://htmlpreview.github.io/?https://github.com/zsteinmetz/envalysis/blob/master/vignettes/texture.html)
for details

### Data presentation

* Categorize water drop penetration times according to Bisdom et al. (1993)
`bisdom()`
* Report significant figures, namely round means and erros to the least
significant digit, using `signifig()`
* Clean, black-and-white ggplot2 theme for scientific publications
`theme_publish()`; a preview is available
[here](https://htmlpreview.github.io/?https://github.com/zsteinmetz/envalysis/blob/master/vignettes/theme_publish.html)

## Installation
**envalysis** is available on CRAN and GitHub.

### Install from CRAN (stable version)

```r
install.packages("envalysis")
```

### Install from GitHub (development version)

To install the package, paste the following code into your R console
(requires **devtools**):

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("zsteinmetz/envalysis")
library(envalysis)
```
