# Helper function for expect_snapshot_file()
save_png <- function(code, width = 600, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code
  
  path
}

data(clayloam)
tex <- texture(clayloam$reading, clayloam$blank, clayloam$time, clayloam$temperature)
f <- texture(reading ~ blank + time + temperature, clayloam)

test_that("print() and plot() work", {
  expect_output(print(tex))
  expect_silent(plot(tex))
})

test_that("Snapshot output consistent", {
  expect_snapshot_output(print(tex))
  skip_on_ci()
  expect_snapshot_file(save_png(plot(tex)), "plot.png")
})

test_that("Correct function type", {
  expect_equal(tex$model$fct$name, "LL2.3u")
})

test_that("DIN computed correctly", {
  din <- tex$din[1,]
  expect_equal(sum(din), 1, ignore_attr = T)
  expect_equal(round(din, 3), c(0.318, 0.476, 0.207), ignore_attr = T)
})

test_that("USDA computed correctly", {
  usda <- tex$usda[1,]
  expect_equal(sum(usda), 1, ignore_attr = T)
  expect_equal(round(usda, 3), c(0.318, 0.429, 0.254), ignore_attr = T)
})

test_that("Different input methods giving equal results", {
  expect_equal(tex$din, f$din)
})

fct <- "W1.2"
dc <- 100
tex12 <- texture(clayloam$reading, clayloam$blank, clayloam$time, clayloam$temperature,
                 model = fct, conc = dc)
f12 <- texture(reading ~ blank + time + temperature, clayloam,
               model = fct, conc = dc)

test_that("Optional arguments are passed correctly", {
  expect_identical(tex12$model$fct$name, fct)
  expect_identical(f12$model$fct$name, fct)
  expect_identical(tex12$meta[3], c(Conc = as.character(dc)))
  expect_identical(f12$meta[3], c(Conc = as.character(dc)))
})
