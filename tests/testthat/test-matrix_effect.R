data(din32645)
din <- calibration(Area ~ Conc, data = din32645)

mlp <- runif(10, -2, 2)
me <- sapply(mlp, function(x) {
  conc <- din32645$Conc
  area <- din32645$Area * x
  mm <- calibration(area ~ conc, check_assumptions = F)
  matrix_effect(din, mm)
})

test_that("matrix_effect() handles input errors correctly", {
  matrix_effect(1:10) |> expect_error()
  matrix_effect(din, 1:10) |> expect_error()
  
  matrix_effect(din, din) |> expect_silent()
})

test_that("matrix_effect() is calculated correctly", {
  matrix_effect(din, din) |> expect_equal(0, ignore_attr = T)
  expect_equal(me - mlp + 1, rep(0, 10), ignore_attr = T)
})
