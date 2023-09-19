test_that("sorption() handles input errors correctly", {
  sorption(1:5, c(K = 4)) |> expect_error()
  sorption(1:5, c(Kd = 2.5), type = "xy") |> expect_error()
})

test_that("sorption() output is correct", {
  sorption(1:5, c(Kd = 2.5), type = "linear") |> 
    expect_equal(c(2.5, 5.0, 7.5, 10.0, 12.5))
  
  sorption(1:5, c(K = 4, n = 0.6), type = "freundlich") |> round(2) |> 
    expect_equal(c(4.00, 6.06, 7.73, 9.19, 10.51))
  
  sorption(1:5, c(KL = 2, qmax = 10), type = "langmuir") |> round(2) |> 
    expect_equal(c(6.67, 8.00, 8.57, 8.89, 9.09))
  
  sorption(1:5, c(K = 50, qmax = 10, Csat = 10), type = "BET") |>
    round(2) |> 
    expect_equal(c(9.42, 11.57, 13.65, 16.18, 19.61))
  
  sorption(1:5, c(A = 30, B = 0.8), type = "redlich") |> round(2) |> 
    expect_equal(c(15.00, 21.89, 26.41, 29.77, 32.44))
})
