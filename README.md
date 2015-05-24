envalysis
============

`envalysis` is a R package containing miscellaneous functions for data analyses in environmental chemistry and ecotoxicology.

## Functions
Currently, the following functions are available:

* Confidence intervals `CI()`
* Sen–Puri test `puri.test()`
* Reporting significant figures `signifig()`

## Installation
`envalysis` is currently only available on github. To install `envalysis` use:

```r
if (!'devtools' %in% installed.packages()[,'Package']) install.packages('devtools')
require(devtools)
install_github('zsteinmetz/envalysis')
require(envalysis)
```