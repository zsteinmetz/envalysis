library(tibble)
library(data.table)

data.table::setDTthreads(2)

data(din32645)
test_that("calibration() handles data.tables and tibbles well", {
  expect_silent(calibration(Area ~ Conc, data = as_tibble(din32645),
                            check_assumptions = F))
  expect_silent(calibration(Area ~ Conc, data = data.table(din32645),
                            check_assumptions = F))
})

data(clayloam)
test_that("texture() handles data.tables and tibbles well", {
  expect_silent(texture(reading ~ blank + time + temperature, as_tibble(clayloam),
                        model = "W1.2"))
  expect_silent(texture(reading ~ blank + time + temperature, data.frame(clayloam),
                        model = "W1.2"))
})
