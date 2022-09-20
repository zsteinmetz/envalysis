# envalysis

Miscellaneous Functions for Environmental Analyses

[![CRAN version](https://www.r-pkg.org/badges/version/envalysis)](https://CRAN.R-project.org/package=envalysis) 
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/zsteinmetz/envalysis/workflows/R-CMD-check/badge.svg)](https://github.com/zsteinmetz/envalysis/actions)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/envalysis)](https://CRAN.R-project.org/package=envalysis)
[![DOI](https://img.shields.io/badge/shortDOI-10/ft9p-blue.svg)](https://doi.org/ft9p)

**envalysis** is an R package containing miscellaneous functions for data
analysis in environmental chemistry and ecotoxicology. Provides, for example,
`calibration()` to calculate calibration curves and corresponding limits of
detection (LODs) and limits of quantification (LOQs) according to German DIN
32645 (2008). `texture()` makes it easy to estimate soil particle size
distributions from hydrometer measurements (ASTM D422-63, 2007).
Some functions of the package require **ggplot2** or **drc**.

## Functions

Currently, the following functions are available:

### Data manipulation and analysis

* Calculating limits of detection `lod()` and limits of quantification `loq()`
  as part of the `calibration` class to produce linear calibration curves
  in accordance with German DIN 32645 (2008)
* Finding optimum weights for weighted calibrations using `weight_select()`
* Estimating matrix effects (signal suppression/enhancement) with `matrix_effect()`
* Determine particle size distributions and soil texture classes (DIN/USDA)
  measured with a soil hydrometer in accordance with ASTM D422-63 (2007) using
  `texture()`; see
  [vignette](https://htmlpreview.github.io/?https://github.com/zsteinmetz/envalysis/blob/master/vignettes/texture.html)
  for details
* Confidence intervals `CI()`, standard errors `se()`, and root mean square
  errors `rmse()`
* Various sorption isotherms `sorption()`

### Data presentation

* Categorize water drop penetration times according to Bisdom et al. (1993)
  with `bisdom()`
* Report significant figures, namely round means and errors to the least
  significant digit, using `signifig()`
* Clean, black-and-white ggplot2 theme for scientific publications
  `theme_publish()`; a preview is available
  [here](https://htmlpreview.github.io/?https://github.com/zsteinmetz/envalysis/blob/master/vignettes/theme_publish.html)

## Installation

**envalysis** is available on CRAN and GitHub.

### Install from CRAN (stable version)

You can install the released version of **envalysis** from
[CRAN](https://CRAN.R-project.org) with:

```r
install.packages("envalysis")
```

### Install from GitHub (development version)

To install the development version of this package, paste the following code
into your R console (requires **devtools**):

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("zsteinmetz/envalysis")
```
