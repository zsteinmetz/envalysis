test_that("Correct output", {
  expect_equal(signifig(mean = c(0.28,5), error = c(0.688, 8)),
               c("0.3 ± 0.7", "5 ± 8"))
  expect_equal(signifig(mean = c(0.28,5), error = c(0.688, 8), style = "siunitx"),
               c("0.3(7)", "5(8)"))
})

test_that("Correct NA handling", {
  expect_true(grepl("NA", signifig(NA, 2)))
  expect_true(grepl("NA", signifig(0.83, NA)))
})

test_that("Correct error handling", {
  expect_warning(signifig(1.36, 0.68, style = "xy"))
  expect_error(signifig(1:3, c(0.3, 0.6)))
})
