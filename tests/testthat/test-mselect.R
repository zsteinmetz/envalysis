library(drc)
ryegrass_mod <- drm(rootl ~ conc, data = ryegrass, fct = LL.4())

test_that("Both functions give the same output", {
  expect_equal(
    envalysis::mselect(ryegrass_mod, list(LL.3(), W2.4(), baro5())),
    drc::mselect(ryegrass_mod, list(LL.3(), W2.4(), baro5()))
  )
})
