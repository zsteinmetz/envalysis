test_that("Correct output", {
  expect_equal(signifig(mean = c(-31.6, 2.6), error = c(11.6, 9.6)),
               c("-30 ± 10", "0 ± 10"))
  expect_equal(signifig(mean = c(0.28, 5), error = c(0.688, 0.8)),
               c("0.3 ± 0.7", "5.0 ± 0.8"))
  expect_equal(signifig(mean = c(0.28, 5), error = c(0.688, 0.8), style = "par"),
               c("0.3 (0.7)", "5.0 (0.8)"))
  expect_equal(signifig(mean = c(0.28, 5), error = c(0.688, 0.8), style = "siunitx"),
               c("0.3(7)", "5(8)"))
})

test_that("Arguments passed to prettyNum()", {
  a <- c(0.009559871, 0.005288050)
  b <- c(0.0010160651, 0.0006998495)
  expect_equal(signifig(a, b, scientific = F), c("0.010 ± 0.001", "0.0053 ± 0.0007"))
})

test_that("Correct NA handling", {
  expect_true(grepl("NA", signifig(NA, 1.6)))
  expect_true(grepl("NA", signifig(0.83, NA)))
  expect_equal(signifig(0.83, NA, na.digit = 1), "0.8 ± NA")
  expect_equal(signifig(27.1, 0), "27.1 ± 0")
  expect_equal(signifig(27.1, 0, na.digit = 0), "27 ± 0")
})

test_that("Correct error handling", {
  expect_warning(signifig(1.36, 0.68, style = "xy"))
  expect_error(signifig(1:3, c(0.3, 0.6)))
})
