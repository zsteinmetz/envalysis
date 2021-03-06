# Helper function for expect_snapshot_file()
save_png <- function(code, width = 600, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code
  
  path
}

data(din32645)
din <- calibration(Area ~ Conc, data = din32645)

data(neitzel2003)
neitzel <- calibration(Meas ~ Conc, data = neitzel2003)

test_that("print(), summary(), and plot() work", {
  expect_output(print(din))
  expect_output(print(summary(din)))
  expect_silent(plot(din))
})

test_that("Snapshot output consistent", {
  expect_snapshot_output(print(din))
  expect_snapshot_output(print(neitzel))
  skip_on_ci()
  expect_snapshot_file(save_png(plot(din)), "plot.png")
})

test_that("Correct R squared computed correctly", {
  expect_equal(round(din$adj.r.squared, 3), 0.983)
})

test_that("LOD calculation computed correctly", {
  expect_equal(round(lod(din)[1], 3), 0.053)
  expect_equal(round(lod(neitzel)[1], 3), 0.009)
})

test_that("LOQ calculation computed correctly", {
  expect_equal(round(loq(din)[1], 3), 0.212)
  expect_equal(round(loq(neitzel, k = 3, alpha = 0.05)[1], 3), 0.060)
  expect_equal(round(loq(neitzel, k = 2, alpha = 0.05)[1], 3), 0.041)
  expect_equal(round(loq(neitzel, k = 3, alpha = 0.01)[1], 3), 0.086)
  expect_equal(round(loq(neitzel, k = 2, alpha = 0.01)[1], 3), 0.059)
})

cal <- calibration(Area ~ Conc, data = din32645[din32645$Conc != 0,])
lm <- lm(Area ~ Conc, data = din32645[din32645$Conc != 0,])

test_that("calibration() and lm() give equal results for non-zero concentrations", {
  expect_equal(coef(cal$model), coef(lm))
})

test_that("Difference between blank method and estimation from calibration curve", {
  expect_message(alt <- calibration(Area ~ Conc, data = din32645[din32645$Conc != 0, ]))
  expect_false(identical(din, alt))
  expect_equal(loq(din), loq(alt))
})

test_that("Unbalanced design gives warning", {
  suppressWarnings(
    ublcd <- calibration(Area ~ Conc, data = rbind(din32645, din32645[15,]),
                       check_assumptions = F)
    )
  expect_warning(lod(ublcd))
  expect_warning(loq(ublcd))
})

w1 <- calibration(Area ~ Conc, data = din32645, weights = "1/Area^2")
w2 <- calibration(Area ~ Conc, data = din32645,
                  weights = 1/din32645[din32645$Conc != 0, ]$Area^2)
wlm <- lm(Area ~ Conc, data = din32645[din32645$Conc != 0,],
          weights = 1/din32645[din32645$Conc != 0, ]$Area^2)

test_that("Weights work correctly", {
  expect_error(calibration(Area ~ Conc, data = din32645, weights = 1/din32645$Area^2))
  
  expect_equal(coef(w1$model), coef(w2$model))
  expect_equal(coef(w2$model), coef(wlm))
  expect_false(isTRUE(all.equal(coef(cal$model), coef(w1$model))))
})

