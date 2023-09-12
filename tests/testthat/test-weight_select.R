data(din32645)
din <- calibration(Area ~ Conc, data = din32645)

test_that("relerr() and weight_select() handle input errors correctly", {
  relerr(1) |> expect_error()
  weight_select(1) |> expect_error()
})

test_that("relerr() computes correctly", {
  expect_equal(round(sum(abs(relerr(din))), 4), 0.7978)
})

test_that("weight_select() produces consistent output", {
  expect_silent(ws <- weight_select(din))
  expect_snapshot_output(ws)
  expect_equal(round(ws[1,], 4), c(0.6109, 0.9841), ignore_attr = T)
  expect_equal(rownames(ws)[1], "1/Conc^2.0")
})
