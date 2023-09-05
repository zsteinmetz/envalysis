## ----setup, include=FALSE, echo=FALSE-----------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE, 
  message = FALSE,
  comment = "# >"
)

## ----packages-----------------------------------------------------------------
library(envalysis)
library(ggplot2)

## ----default_theme------------------------------------------------------------
data("mtcars")

p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
     color = factor(gear))) + facet_wrap( ~ am)
p

## ----theme_publish------------------------------------------------------------
p + theme_publish()

