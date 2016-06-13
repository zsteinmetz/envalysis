context("Helper functions")

test_that("Bisdom scaled WDPTs work as expected", {
  expect_that(bisdom(c(2,6,20,3,385)), equals(c(1, 2, 2, 1, 3)))
  expect_that(bisdom("a string"), gives_warning())
  expect_true(is.na(bisdom(NA)))
  
  bis1 <- bisdom(3600)
  bis2 <- bisdom(4000)
  expect_identical(bis1, bis2)
})

test_that("Confidence intervals give proper results", {
  expect_that(round(CI(1:5), 2), equals(1.39))
  expect_true(is.na(CI(NA)))
})

test_that("Convert frequencies to raw data", {
  df <- data.frame(var = c("a", "b", "c"), freq = as.integer(c(7,6,10)))
  raws <- make.raw(df, "var", "freq")
  tab <- table(raws)
  
  test <- data.frame(tab)
  names(test) <- c("var", "freq")
  expect_identical(df, test)
})
