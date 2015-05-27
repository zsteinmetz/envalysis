envalysis
============

`envalysis` is a R package containing miscellaneous functions for data analyses in environmental chemistry and ecotoxicology.

## Functions
Currently, the following functions are available:

* Confidence intervals `CI()`
* Reporting significant figures `signifig()`
* Limit of detection (LOD) `lod()`
* Limit of quantification (LOQ) `loq()`
* Various sorption isotherms `sorption()`
* Categorize water drop penetration times according to Bisdom et al. (1993) `bisdom()`
* Convert frequency data back to raw data `make.raw()`
* ANOVA on ranks according to Sen and Puri `puri.test()`

## Installation
`envalysis` is available on github. To install the package paste the following code into your R console:

```r
if (!'devtools' %in% installed.packages()[,'Package']) install.packages('devtools')
require(devtools)
install_github('zsteinmetz/envalysis')
require(envalysis)
```
