context("Texture")

data(clayloam)
tex <- texture(clayloam$reading, clayloam$blank, clayloam$time, clayloam$temperature)
f <- texture(reading ~ blank + time + temperature, clayloam)

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

test_that("Methods giving equal results", {
  expect_equal(tex, f, check.environment = F)
})

fct <- "W1.2"
dc <- 100
tex12 <- texture(clayloam$reading, clayloam$blank, clayloam$time, clayloam$temperature,
                 model = fct, conc = dc)
f12 <- texture(reading ~ blank + time + temperature, clayloam,
               model = fct, conc = dc)

test_that("Optional arguments are passed correctly", {
  expect_identical(tex12$model$fct$name, fct)
  expect_identical(f12$model$fct$name, fct)
  expect_identical(tex12$meta[3], c(Conc = as.character(dc)))
  expect_identical(f12$meta[3], c(Conc = as.character(dc)))
})
