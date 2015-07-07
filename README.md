envalysis
=========
[![Travis-CI Build Status](https://travis-ci.org/zsteinmetz/envalysis.svg?branch=master)](https://travis-ci.org/zsteinmetz/envalysis)

`envalysis` is a R package containing miscellaneous functions for data analyses in environmental chemistry and ecotoxicology.

## Functions
Currently, the following functions are available:

### Data manipulation and analysis

* Confidence intervals `CI()`
* Limit of detection (LOD) `lod()`
* Limit of quantification (LOQ) `loq()`
* Various sorption isotherms `sorption()`
* Convert frequency data back to raw data `make.raw()`
* ANOVA on ranks according to Sen and Puri `puri.test()`

### Data presentation

* Categorize water drop penetration times according to Bisdom et al. (1993) `bisdom()`
* Report significant figures `signifig()`
* ggplot2 theme for scientific publications (dependent on grid) `theme_publish()`

## Installation
`envalysis` is available on github. To install the package paste the following code into your R console:

```r
if (!'devtools' %in% installed.packages()[,'Package']) install.packages('devtools')
require(devtools)
install_github('zsteinmetz/envalysis')
require(envalysis)
```
