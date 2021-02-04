test_that("Bisdom scaled WDPTs work as expected", {
  expect_equal(bisdom(c(2,6,20,3,385)), c(1, 2, 2, 1, 3))
  expect_warning(bisdom("a string"))
  expect_true(is.na(bisdom(NA)))
  
  expect_identical(bisdom(3600), bisdom(4000))
})

test_that("Confidence intervals work as expected", {
  expect_equal(round(CI(1:5), 2), 1.39)
  expect_warning(CI(NA))
})

test_that("Root mean square errors (RMSE) work as expected", {
  expect_equal(round(rmse(c(0.12,0.59,NA), c(0.15,0.63,1.2)), 4), 0.0354)
  expect_true(is.na(rmse(NA, NA)))
})
