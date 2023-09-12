df <- data.frame(mean = c(-31.6, 2.6), error = c(11.6, 9.6))

test_that("signifig() handles input errors correctly", {
  signifig(1:3, c(0.3, 0.6)) |> expect_error()
  signifig(c(-31.6, 2.6), c(-11.6, 9.6)) |> expect_error()
  signifig(1.36, 0.68, style = "xy") |> expect_warning()
})

test_that("signifig() produces correct output", {
  signifig(mean, error, df) |>
    expect_equal(signifig(c(-31.6, 2.6), c(11.6, 9.6)))
  
  signifig(c(-31.6, 2.6), c(11.6, 9.6)) |> expect_equal(c("-30 ± 10", "0 ± 10"))
  signifig(c(0.28, 5), c(0.688, 0.8)) |> 
    expect_equal(c("0.3 ± 0.7", "5.0 ± 0.8"))
  signifig(c(0.28, 5), c(0.688, 0.8), style = "par") |> 
    expect_equal(c("0.3 (0.7)", "5.0 (0.8)"))
  signifig(mean = c(0.28, 5), error = c(0.688, 0.8), style = "siunitx") |> 
    expect_equal(c("0.3(7)", "5(8)"))
})

test_that("signifig() handles NAs correctly", {
  grepl("NA", signifig(NA, 1.6)) |> expect_true()
  grepl("NA", signifig(0.83, NA)) |> expect_true()
  
  signifig(0.83, NA, na.digit = 1) |> expect_equal("0.8 ± NA")
  signifig(27.1, 0) |> expect_equal("27.1 ± 0")
  signifig(27.1, 0, na.digit = 0) |> expect_equal("27 ± 0")
})

test_that("signifig() arguments are passed to prettyNum()", {
  a <- c(0.009559871, 0.005288050)
  b <- c(0.0010160651, 0.0006998495)
  signifig(a, b, scientific = F) |> 
    expect_equal(c("0.010 ± 0.001", "0.0053 ± 0.0007"))
})
