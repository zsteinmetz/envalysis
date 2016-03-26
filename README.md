envalysis
=========
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis-CI Build Status](https://travis-ci.org/zsteinmetz/envalysis.svg?branch=master)](https://travis-ci.org/zsteinmetz/envalysis)
[![License](http://img.shields.io/:license-GPL--3-blue.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)

`envalysis` is an R package containing miscellaneous functions for data analyses in environmental chemistry and ecotoxicology. Some functions of the package require `ggplot2` or `drc`.

## Functions
Currently, the following functions are available:

### Data manipulation and analysis

* Confidence intervals `CI()`
* Root mean square errors `rmse()`
* Limit of detection (LOD) `lod()`
* Limit of quantification (LOQ) `loq()`
* Various sorption isotherms `sorption()`
* Convert frequency data back to raw data `make.raw()`
* ANOVA on ranks according to Sen and Puri (also known as Scheirer-Ray-Hare-Test) `puri.test()`
* Determine particle size distributions and soil texture classes (DIN/USDA) measured with a soil hydrometer in accordance with ASTM D422-63(2007)e2 using `texture()`

### Data presentation

* Categorize water drop penetration times according to Bisdom et al. (1993) `bisdom()`
* Report significant figures, i.e. round means and erros to the least significant digit, using `signifig()`
* Clean, black-and-white ggplot2 theme for scientific publications `theme_publish()`. A preview is available [here](./vignettes/theme_publish.png).

## Installation
`envalysis` is available on github. To install the package, paste the following code into your R console (requires `devtools`):

```r
if (!'devtools' %in% installed.packages()[,'Package']) install.packages('devtools')
devtools::install_github('zsteinmetz/envalysis')
require(envalysis)
```
