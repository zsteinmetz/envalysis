test_that("bisdom() works as expected", {
  bisdom(c(2,6,20,3,385)) |> expect_equal(c(1, 2, 2, 1, 3))
  bisdom("a string") |> expect_warning()
  bisdom(NA) |> is.na() |> expect_true()
  
  expect_identical(bisdom(3600), bisdom(4000))
})

test_that("CI() works as expected", {
  CI(1:5) |> round(2) |> expect_equal(1.39)
  CI(NA) |> expect_warning()
})

test_that("rmse() works as expected", {
  rmse(c(0.12,0.59,NA), c(0.15,0.63,1.2)) |> round(4) |> expect_equal(0.0354)
  rmse(c(0.12,0.59,NA), c(0.15,0.63,1.2), rel = T) |> round(4) |>
    expect_equal(0.0996)
  rmse(NA, NA) |> is.na() |> expect_true()
})
