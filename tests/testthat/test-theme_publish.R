library(ggplot2)

p <- ggplot(mtcars) +
  geom_point(aes(x = wt, y = mpg,
                 colour = factor(gear))) + facet_wrap( ~ am)

test_that("Plotting works without warnings", {
  expect_silent(print(p))
  expect_silent(print(p + theme_publish()))
})
