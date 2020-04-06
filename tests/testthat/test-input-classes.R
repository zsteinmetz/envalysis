context("Input class compatibility")

library(tibble)
library(data.table)

data(din32645)
test_that("calibration()", {
  expect_silent(calibration(Area ~ Conc, data = as_tibble(din32645)))
  expect_silent(calibration(Area ~ Conc, data = data.table(din32645)))
})

data(clayloam)
test_that("texture()", {
  expect_silent(texture(reading ~ blank + time + temperature, as_tibble(clayloam),
                        model = "W1.2"))
  expect_silent(texture(reading ~ blank + time + temperature, data.frame(clayloam),
                        model = "W1.2"))
})

