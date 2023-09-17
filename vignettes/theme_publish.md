---
title: "ggplot2 theme for scientific publications"
author: "Zacharias Steinmetz"
date: "2023-09-17"
output:
  html_document:
    keep_md: yes
    fig_width: 8
    fig_height: 5
vignette: >
  %\VignetteIndexEntry{ggplot2 theme for scientific publications}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Packages

First, load **ggplot2** alongside **envalysis**.


```r
library(envalysis)
library(ggplot2)
```

## Default theme

The following code chunk plots the **ggplot2** `mtcars` sample data set with the
default **ggplot2** theme.


```r
data("mtcars")

p <- ggplot(mtcars) +
  geom_point(aes(x = wt, y = mpg, color = factor(gear))) +
  labs(
    title = "Fuel economy declines as weight increases",
    subtitle = "(1973-74)",
    caption = "Data from the 1974 Motor Trend US magazine.",
    x = "Weight (1000 lbs)",
    y = "Fuel economy (mpg)",
    colour = "Gears"
  ) +
  facet_wrap( ~ am, labeller = labeller(am = c("0" = "Automatic",
                                               "1" = "Manual")))
p
```

<img src="/home/zacharias/Dokumente/RPTU/Seafile/Research/Code/envalysis/vignettes/theme_publish_files/figure-html/default_theme-1.png" style="display: block; margin: auto;" />

## `theme_publish()`

Adding `theme_publish()` applies the custom theme.


```r
p + theme_publish()
```

<img src="/home/zacharias/Dokumente/RPTU/Seafile/Research/Code/envalysis/vignettes/theme_publish_files/figure-html/theme_publish-1.png" style="display: block; margin: auto;" />

`theme_publish()` allows for changing the base font face, font size, and
line widths. More arguments may be passed to **ggplot2**'s `theme_bw()`


```r
p + theme_publish(base_size = 16, base_family = "Times", base_linewidth = 0.7)
```

<img src="/home/zacharias/Dokumente/RPTU/Seafile/Research/Code/envalysis/vignettes/theme_publish_files/figure-html/theme_arguments-1.png" style="display: block; margin: auto;" />
