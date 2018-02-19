context("Texture")

data(clayloam)
tex <- texture(Time, Reading, Blank, Temperature, data = clayloam)

test_that("Function type", {
  expect_equal(tex$model$fct$name, "LL2.3u")
})

test_that("DIN 4022", {
  din <- tex$din[1,]
  expect_equivalent(sum(din), 1)
  expect_equivalent(round(din, 3), c(0.318, 0.476, 0.207))
})

test_that("USDA", {
  usda <- tex$usda[1,]
  expect_equivalent(sum(usda), 1)
  expect_equivalent(round(usda, 3), c(0.318, 0.429, 0.254))
})
