# Helper function for expect_snapshot_file()
save_png <- function(code, width = 600, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code
  
  return(path)
}

data(clayloam)

form <- texture(reading ~ blank + time + temperature, data = clayloam)
vec <- texture(clayloam$reading, clayloam$blank, clayloam$time,
               clayloam$temperature, plot = T)
h151 <- texture(I(reading / 1000 + 0.999) ~ I(blank / 1000 + 0.999) +
                  time + temperature, data = clayloam, model = "LL.4")

test_that("texture() handles input errors correctly", {
  texture(reading ~ blank, data = clayloam) |> expect_error()
  texture(1:200) |> expect_error()
  
  texture(reading ~ blank + time + temperature, data = clayloam, Gs = 1:2) |> 
    expect_error()
  
  texture(I(reading / 1000 + 1) ~ blank + time + temperature, data = clayloam) |> 
    expect_error()
  
  texture(reading ~ blank + time + temperature, data = clayloam) |> 
    expect_warning()
  
  texture(reading ~ blank + time + temperature, data = clayloam,
          model = "LL2.3u") |> 
    expect_silent()
})

test_that("texture() produces correct output", {
  expect_s3_class(form, "texture")
  expect_s3_class(vec, "texture")
  expect_s3_class(h151, "texture")
  
  expect_equal(form$distribution, vec$distribution)
  expect_equal(form$din, vec$din)
  expect_equal(form$usda, vec$usda)
  
  expect_equal(form$model$fct$name, "LL2.3u")
  
  din <- form$din[1,]
  sum(din) |> expect_equal(1, ignore_attr = T)
  round(din, 3) |> expect_equal(c(0.318, 0.476, 0.207), ignore_attr = T)
  
  usda <- form$usda[1,]
  sum(usda) |> expect_equal(1, ignore_attr = T)
  round(usda, 3) |> expect_equal(c(0.318, 0.429, 0.254), ignore_attr = T)
})

test_that("print() and plot() work", {
  print(form) |> expect_output()
  plot(form) |> expect_silent()
})

test_that("print() and plot() produce consistent output", {
  print(form) |> expect_snapshot_output()
  
  skip_on_ci()
  plot(form) |> save_png() |> expect_snapshot_file("plot.png")
})

test_that("optional arguments passed correctly to texture()", {
  fct <- "W1.2"
  dc <- 100
  vec <- texture(clayloam$reading, clayloam$blank, clayloam$time,
                   clayloam$temperature, model = fct, conc = dc) |> 
    expect_silent()
  form <- texture(reading ~ blank + time + temperature, clayloam, model = fct,
                  conc = dc) |> 
    expect_silent()
  
  expect_identical(form$model$fct$name, fct)
  expect_identical(vec$model$fct$name, fct)
  expect_identical(form$meta[3], c(Conc = as.character(dc)))
  expect_identical(vec$meta[3], c(Conc = as.character(dc)))
})

test_that("as_tridata() handles input errors correctly", {
  as_tridata(1:200) |> expect_error()
  as_tridata(form) |> expect_error()
  as_tridata(form, "abc") |> expect_error()
})

test_that("as_tridata() works as expected", {
  din <- as_tridata(form, "din") |> expect_silent()
  
  expect_s3_class(din, "data.frame")
  expect_length(din, 3)
})
