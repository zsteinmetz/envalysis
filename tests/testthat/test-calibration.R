context("Calibration")

data(din32645)
din <- calibration(Area ~ Conc, data = din32645)

test_that("R squared", {
  expect_equal(round(din$adj.r.squared, 3), 0.983)
})

test_that("LOD/LOQ", {
  expect_equal(round(lod(din)[1], 3), 0.053)
  expect_equal(round(loq(din)[1], 3), 0.212)
})

test_that("Blank method vs. estimation", {
  expect_message(
    alt <- calibration(Area ~ Conc, data = din32645[din32645$Conc != 0, ])
  )
  expect_false(identical(din, alt))
  expect_equal(loq(din), loq(alt))
})

test_that("Unbalanced design", {
  expect_warning(
    calibration(Area ~ Conc, data = rbind(din32645, din32645[15,]))
  )
})
