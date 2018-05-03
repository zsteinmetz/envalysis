envalysis
=========
[![CRAN version](http://www.r-pkg.org/badges/version/envalysis)](https://CRAN.R-project.org/package=envalysis) 
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis-CI Build Status](https://travis-ci.org/zsteinmetz/envalysis.svg?branch=master)](https://travis-ci.org/zsteinmetz/envalysis)
[![License](http://img.shields.io/:license-GPL--3-blue.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html) 
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/envalysis)](https://CRAN.R-project.org/package=envalysis)

`envalysis` is an R package containing miscellaneous functions for data analyses
in environmental chemistry and ecotoxicology. Provides, e.g., `calibration()` to
calculate calibration curves and corresponding limits of detection (LODs) and
quantification (LOQs) according to German DIN 32645:2008-11. `texture()` makes
it easy to estimate soil particle size distributions from hydrometer
measurements (ASTM D422-63(2007)e2).
Some functions of the package require `ggplot2` or `drc`.

## Functions
Currently, the following functions are available:

### Data manipulation and analysis

* Confidence intervals `CI()`
* Root mean square errors `rmse()`
* Limit of detection (LOD) `lod()` and limit of quantification (LOQ) `loq()` as
part of the `calibration` class to produce linear calibration curves according
to German DIN 32645:2008-11
* Various sorption isotherms `sorption()`
* Determine particle size distributions and soil texture classes (DIN/USDA) measured with a soil hydrometer in accordance with ASTM D422-63(2007)e2 using `texture()`; see [vignette](./vignettes/texture.md) for details

### Data presentation

* Categorize water drop penetration times according to Bisdom et al. (1993) `bisdom()`
* Report significant figures, i.e. round means and erros to the least significant digit, using `signifig()`
* Clean, black-and-white ggplot2 theme for scientific publications `theme_publish()`; a preview is available [here](./vignettes/theme_publish.md)

## Installation
`envalysis` is available on CRAN and GitHub.

### Install from CRAN (stable version)

```r
install.packages("envalysis")
```

### Install from GitHub (development version)

To install the package, paste the following code into your R console
(requires `devtools`):

```r
if (!'devtools' %in% installed.packages()[,'Package']) install.packages('devtools')
devtools::install_github('zsteinmetz/envalysis')
library(envalysis)
```
