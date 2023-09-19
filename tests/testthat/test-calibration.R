library(investr)

# Helper function for expect_snapshot_file()
save_png <- function(code, width = 600, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code
  
  return(path)
}

data(din32645)

din <- calibration(Area ~ Conc, data = din32645)
din_woa <- calibration(Area ~ Conc, data = din32645, check_assumptions = F)

data(neitzel2003)
neitzel <- calibration(Meas ~ Conc, data = neitzel2003)

test_that("calibration() handles input errors correctly", {
  calibration( ~ Conc, data = din32645) |> expect_error()
  
  calibration(Area ~ Conc, data = rbind(din32645, din32645[15,]),
              check_assumptions = F) |> 
    expect_warning() |> 
    expect_warning() |> 
    expect_warning()
  
  calibration(Area ~ Conc, data = din32645[din32645$Conc != 0,]) |> 
    expect_message() |> expect_message()
  calibration(Area ~ Conc, data = din32645) |> expect_silent()
})

test_that("calibration() produces correct output", {
  expect_s3_class(din, "calibration")
  
  round(din$lod[1], 3) |> expect_equal(0.053)
  round(din$loq[1], 3) |> expect_equal(0.212)
  
  round(din$adj.r.squared, 3) |> expect_equal(0.983)
  round(din$relerr, 3) |> expect_contains(c(0.199, 0.067, -0.036))
  expect_contains(din$blanks, c(2003, 1943))
})

alt <- calibration(Area ~ Conc, data = din32645[din32645$Conc != 0,])
lm <- lm(Area ~ Conc, data = din32645[din32645$Conc != 0,])

test_that("calibration() gives correct result for non-zero concentrations", {
  expect_false(identical(din, alt))
  expect_equal(loq(din), loq(alt))
  expect_equal(coef(alt$model), coef(lm))
})

test_that("weights work correctly", {
  w1 <- calibration(Area ~ Conc, data = din32645, weights = "1/Area^2") |> 
    expect_silent()
  w2 <- calibration(Area ~ Conc, data = din32645,
                    weights = 1/din32645[din32645$Conc != 0, ]$Area^2) |> 
    expect_silent()
  wlm <- lm(Area ~ Conc, data = din32645[din32645$Conc != 0,],
            weights = 1/din32645[din32645$Conc != 0, ]$Area^2)
  
  expect_error(calibration(Area ~ Conc, data = din32645,
                           weights = 1/din32645$Area^2))
  
  expect_equal(coef(w1$model), coef(w2$model))
  expect_equal(coef(w2$model), coef(wlm))
  expect_false(isTRUE(all.equal(coef(alt$model), coef(w1$model))))
})

test_that("print(), summary(), and plot() work", {
  print(din) |> expect_output()
  print(summary(din)) |> expect_output()
  plot(din) |> expect_silent()
})

test_that("snapshot output consistent", {
  print(din) |> expect_snapshot_output()
  print(din_woa) |> expect_snapshot_output()
  print(neitzel) |> expect_snapshot_output()
  
  skip_on_ci()
  plot(din) |> save_png() |> expect_snapshot_file("plot.png")
})

test_that("lod() and loq() handle input errors correctly", {
  lod(1) |> expect_error()
  loq(2) |> expect_error()
  
  lod(din) |> expect_silent()
  loq(din) |> expect_silent()
})

test_that("lod() and loq() are calculated correctly", {
  expect_equal(din$lod, lod(din))
  expect_equal(din$loq, loq(din))
  
  lod(neitzel)[1] |> round(3) |> expect_equal(0.009)

  loq(neitzel, k = 3, alpha = 0.05)[1] |> round(3) |> expect_equal(0.060)
  loq(neitzel, k = 2, alpha = 0.05)[1] |> round(3) |> expect_equal(0.041)
  loq(neitzel, k = 3, alpha = 0.01)[1] |> round(3) |> expect_equal(0.086)
  loq(neitzel, k = 2, alpha = 0.01)[1] |> round(3) |> expect_equal(0.059)
})

test_that("inv_predict() works as expected", {
  inv_predict(1) |> expect_error()
  inv_predict(din) |> expect_error()

  ipa <- inv_predict(din, c(4986, 6210)) |> expect_silent()
  inv_predict(din, c(1000, 6020)) |> expect_warning()
  inv_predict(din, c(4900, 8500)) |> expect_warning()
  inv_predict(din, c(1000, 6020), method = "invest") |> expect_warning()
  inv_predict(din, c(4900, 8500), method = "invest") |> expect_warning()
  
  round(ipa, 3) |> expect_equal(c(0.259, 0.386))
  inv_predict(din, c(4986, 6210), method = "invest") |> expect_equal(ipa)
})

test_that("as.list() works as expected", {
  lst <- as.list(din) |> expect_silent()
  
  inherits(lst, "list") |> expect_true()
  
  names(lst) |> expect_contains(c("Conc", "lod", "loq"))
  expect_equal(lst$lod, din$lod[,1])
  expect_equal(lst$loq, din$loq[,1])
})

test_that("batch calibration works as expected", {
  data("phenolics")
  
  tyrosol_1 <- subset(phenolics$seq, Compound == "Tyrosol" & Batch == 1)
  cal_1 <- calibration(Area ~ `Spec Conc`,
                       data = subset(tyrosol_1, Type == "Standard")) |> 
    expect_silent()
  expect_warning(
    tyrosol_1$`Calc Conc` <- inv_predict(cal_1, tyrosol_1$Area, below_lod = 0)
  )
  
  dt <- lapply(phenolics, as.data.table)
  dt$cal <- dt$seq[Type == "Standard", calibration(Area ~ `Spec Conc`) |> 
                     as.list(c("coef", "adj.r.squared", "lod", "loq")),
                   by = .(Compound, Batch)] |> expect_silent()
  cal_dt <- dt$cal[Compound == "Tyrosol" & Batch == 1]
  expect_equal(cal_dt$adj.r.squared, cal_1$adj.r.squared)
  expect_equal(cal_dt$lod, cal_1$lod[[1]])
  expect_equal(cal_dt$loq, cal_1$loq[[1]])
  
  dt$seq[, `Calc Conc` := calibration(Area ~ `Spec Conc`,
                                      .SD[Type == "Standard"]) |> 
           inv_predict(Area, below_lod = 0),
         by = .(Compound, Batch)] |>
    expect_warning() |> expect_warning() |> expect_warning() |>
    expect_warning() |> expect_warning() |> expect_warning()
  
  seq_dt <- dt$seq[Compound == "Tyrosol" & Batch == 1]
  expect_equal(seq_dt$`Calc Conc`, tyrosol_1$`Calc Conc`)
})
