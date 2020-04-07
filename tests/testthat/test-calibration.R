context("Calibration")

data(din32645)
din <- calibration(Area ~ Conc, data = din32645)

data(neitzel2003)
neitzel <- calibration(Meas ~ Conc, data = neitzel2003)

test_that("Correct R squared", {
  expect_equal(round(din$adj.r.squared, 3), 0.983)
})

test_that("LOD calculation", {
  expect_equal(round(lod(din)[1], 3), 0.053)
  expect_equal(round(lod(neitzel)[1], 3), 0.009)
})

test_that("LOQ calculations", {
  expect_equal(round(loq(din)[1], 3), 0.212)
  expect_equal(round(loq(neitzel, k = 3, alpha = 0.05)[1], 3), 0.060)
  expect_equal(round(loq(neitzel, k = 2, alpha = 0.05)[1], 3), 0.041)
  expect_equal(round(loq(neitzel, k = 3, alpha = 0.01)[1], 3), 0.086)
  expect_equal(round(loq(neitzel, k = 2, alpha = 0.01)[1], 3), 0.059)
})

test_that("Blank method vs. estimation from calibration curve", {
  expect_message(
    alt <- calibration(Area ~ Conc, data = din32645[din32645$Conc != 0, ])
  )
  expect_false(identical(din, alt))
  expect_equal(loq(din), loq(alt))
})

test_that("Unbalanced design gives warning", {
  expect_warning(
    calibration(Area ~ Conc, data = rbind(din32645, din32645[15,]))
  )
})

