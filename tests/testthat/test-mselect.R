library(drc)
ryegrass_mod <- drm(rootl ~ conc, data = ryegrass, fct = LL.4())

test_that("mselect() fork gives the same output as the original", {
  expect_equal(
    .mselect(ryegrass_mod, list(LL.3(), W2.4(), baro5())),
    mselect(ryegrass_mod, list(LL.3(), W2.4(), baro5()))
  )
})
