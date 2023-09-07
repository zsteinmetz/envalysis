# envalysis 0.6.0

## New Features

- Inverse predict concentrations from calibration curves using an `invest()`
  wrapper
- `as.list()` method for class '`calibration`'
- pkgdown documentation

## Minor Improvements

- Code coverage
- GitHub Actions for macOS
- Don't export `mselect()` fork anymore; use drc::`mselect()` instead


# envalysis 0.5.5

## Minor Improvements

- The `check_assumptions` argument in `calibration()` is now less verbose;
  test results may be retrieved by calling `print()`
- Replaced `size` argument in ggplot2::`element_rect()` and
  ggplot2::`element_line()` with `linewidth`
- Update SOP for particle size estimations using `texture()`
- Changed maintainer email address
- Corrected typos


# envalysis 0.5.4

## Bug Fixes

- skip tests for ggplot2 v3.4.0 due to deprecation warnings; replace `size`
  argument in ggplot2::`element_rect()` with `linewidth` later


# envalysis 0.5.3

## Bug Fixes

- Fixed moved URLs


# envalysis 0.5.2

## Minor Improvements

- Update GitHub Actions
- Tidy news file

## Bug Fixes

- Fix 'invalid nsmall argument' error when using `signifig()` with certain value
  combinations


# envalysis 0.5.1

## New Features

- Finding optimum weights for weighted calibrations using `weight_select()`
- Calculating matrix effects (signal suppression/enhancement) with
  `matrix_effect()`
- `calibration()` now checks for model assumptions

## Minor Improvements

- Additional "blanks" parameter introduced to `calibration()`, `lod()`, and
  `loq()`
- Snapshot testing
- Improved and more consistent documentation


# envalysis 0.4.2

## Minor Improvements

- Move to testthat 3rd edition

## Bug Fixes

- Fix regression when using weights in `calibration()`


# envalysis 0.4.1

## New Features

- First preparations for weights support in `calibration()`

## Minor Improvements

- Rename master branch to main

## Bug Fixes

- Update `testthat::expect_equal()` calls to keep compatibility with R 4.1.0


# envalysis 0.4.0

## Minor Improvements

- `texture()` now takes data as formula
- tibble support for `texture()`
- `loq()` iterates only until significant digits won't change anymore

## Bug Fixes

- Force percentage bounds for `texture()` to 0 and 100
- Increased margins for `theme_publish()`


# envalysis 0.3.3

## Minor Improvements

- First CRAN release
- Better package description


# envalysis 0.3.2

## Bug Fixes

- Reimplementation of drc's `mselect()` for `texture()` to get rid of global
  variables


# envalysis 0.3.1

## Minor Improvements

- `loq()` now uses iterations instead of estimating the value from `lod()`

## Bug Fixes

- Better handling of unbalanced designs in `calibration()`

## Defunct Functions

- `make.raw()`, use `rep()` instead ;-)


# envalysis 0.3.0

## New Features

- Starting with testthat

## Minor Improvements

- `signifig()` supports 'siunitx' LaTeX output
- Better data handling in `calibration()`
- Updated man pages

## Bug Fixes

- `theme_publish()` updated to work with current ggplot2 versions
- `signifig()` can handle zeros better

## Defunct Functions

- `puri.test()`, use lmer on ranks (lme4) with Type II-ANOVA (car) instead


# envalysis 0.2.2

## Bug Fixes

- Temporary fix to make `mselect()` work
- TODO: Get rid of assignment to .GlobalEnv


# envalysis 0.2.1

## Minor Improvements

- Switch to drc package for texture curve fitting


# envalysis 0.2.0

## New Features

- `texture` class for automatic determination of particle size distribution
  using a hydrometer in accordance with ASTM D422-63(2007)e2

## Minor Improvements

- updated `theme_publish()`
- demo file added


# envalysis 0.1.0

## Initial Feature Set

- Confidence intervals CI()
- Root mean square errors `rmse()`
- Limit of detection (LOD) `lod()`
- Limit of quantification (LOQ) `loq()`
- Various sorption isotherms `sorption()`
- Convert frequency data back to raw data `make.raw()`
- ANOVA on ranks according to Sen and Puri (also known as
  Scheirer-Ray-Hare-Test) `puri.test()`
- Categorize water drop penetration times according to Bisdom et al. (1993)
  `bisdom()`
- Report significant figures, i.e. round means and erros to the least
  significant digit, using `signifig()`
- Clean, black-and-white ggplot2 theme for scientific publications
  `theme_publish()`
