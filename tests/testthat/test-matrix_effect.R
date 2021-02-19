mlp <- runif(10, -2, 2)

data(din32645)
din <- calibration(Area ~ Conc, data = din32645)

mm <- sapply(mlp, function(x){
  conc <- din32645$Conc
  area <- din32645$Area * x
  mm <- calibration(area ~ conc, check_assumptions = F)
  matrix_effect(din, mm)
  })


test_that("Matrix effect/SSE computed correctly", {
  expect_equal(mm - mlp + 1, rep(0, 10), ignore_attr = T)
})
