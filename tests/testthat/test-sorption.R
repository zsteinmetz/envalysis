context("Sorption isotherms")

test_that("Correct output", {
  expect_equal(
    sorption(conc = 1:5, c(Kd = 2.5), type = "linear"),
    c(2.5, 5.0, 7.5, 10.0, 12.5)
  )
  expect_equal(
     round(sorption(conc = 1:5, c(K = 4, n = 0.6), type = "freundlich"), 2),
     c(4.00, 6.06, 7.73, 9.19, 10.51)
  )
  expect_equal(
    round(sorption(conc = 1:5, c(KL = 2, qmax = 10), type = "langmuir"), 2),
    c(6.67, 8.00, 8.57, 8.89, 9.09)
  )
  expect_equal(
    round(sorption(conc = 1:5, c(K = 50, qmax = 10, Csat = 10), type = "BET"), 2),
    c(9.42, 11.57, 13.65, 16.18, 19.61)
  )
  expect_equal(
    round(sorption(conc = 1:5, c(A = 30, B = 0.8), type = "redlich"), 2),
    c(15.00, 21.89, 26.41, 29.77, 32.44)
  )
})

test_that("Error handling", {
  expect_error(sorption(conc = 1:5, c(K = 4)))
})

  
