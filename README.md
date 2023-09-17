# envalysis

Miscellaneous Functions for Environmental Analyses

<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/envalysis)](https://CRAN.R-project.org/package=envalysis) 
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/zsteinmetz/envalysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zsteinmetz/envalysis/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/zsteinmetz/envalysis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/zsteinmetz/envalysis?branch=main)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/envalysis)](https://CRAN.R-project.org/package=envalysis)
[![DOI](https://img.shields.io/badge/shortDOI-10/ft9p-blue.svg)](https://doi.org/10.5281/ZENODO.1240304)
<!-- badges: end -->

**envalysis** is an R package containing miscellaneous functions for data
analysis in environmental chemistry and ecotoxicology. Provides, for example,
`calibration()` to calculate calibration curves and corresponding limits of
detection (LODs) and limits of quantification (LOQs) according to German DIN
32645 (2008). `texture()` makes it easy to estimate soil particle size
distributions from hydrometer measurements (ASTM D422-63, 2007). Some functions
of the package require **ggplot2** or **drc**.

## Functions

The following functions are available:

### Data manipulation and analysis

* Calculating limits of detection `lod()` and limits of quantification `loq()`
  as part of the `'calibration'` class to produce linear calibration curves
  in accordance with German DIN 32645 (2008); ; see
  [vignette](https://zsteinmetz.de/envalysis/articles/calibration.html) for
  details
* Calculating concentrations from calibration curves using `inv_predict()`
* Finding optimum weights for weighted calibrations using `weight_select()`
* Estimating matrix effects (signal suppression/enhancement) with
  `matrix_effect()`
* Determining particle size distributions and soil texture classes (DIN/USDA)
  measured with a soil hydrometer in accordance with ASTM D422-63 (2007) using
  `texture()`; see
  [vignette](https://zsteinmetz.de/envalysis/articles/texture.html) for details
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
  [here](https://zsteinmetz.de/envalysis/articles/theme_publish.html)

## Installation

**envalysis** is available on CRAN and GitHub.

### Install from CRAN (stable version)

You can install the released version of **envalysis** from
[CRAN](https://cran.r-project.org/package=envalysis) with:

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
