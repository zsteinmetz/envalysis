envalysis
=========
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis-CI Build Status](https://travis-ci.org/zsteinmetz/envalysis.svg?branch=master)](https://travis-ci.org/zsteinmetz/envalysis)
[![License](http://img.shields.io/:license-GPL--3-blue.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)

`envalysis` is a R package containing miscellaneous functions for data analyses in environmental chemistry and ecotoxicology.

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
* Calculate particle size distributions and texture classes obtained by a soil hydrometer according to ASTM D422-63(2007)e2 with `texture()`

### Data presentation

* Categorize water drop penetration times according to Bisdom et al. (1993) `bisdom()`
* Report significant figures `signifig()`
* Clean, black-and-white ggplot2 theme for scientific publications `theme_publish()`. A demo file is available [here](./demo/theme_publish.md).

## Installation
`envalysis` is available on github. To install the package paste the following code into your R console:

```r
if (!'devtools' %in% installed.packages()[,'Package']) install.packages('devtools')
devtools::install_github('zsteinmetz/envalysis')
require(envalysis)
```
