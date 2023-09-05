---
title: "theme_publish() showcase"
author: "Zacharias Steinmetz"
date: "2023-09-05"
output:
  html_document:
    keep_md: yes
    fig_height: 5
    fig_width: 8
vignette: >
  %\VignetteIndexEntry{theme_publish demo}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
  \usepackage[utf8]{inputenc}
---



First, we require **ggplot2**.


```r
library(envalysis)
library(ggplot2)
```

We use the `mtcars` sample data set for plotting with the default **ggplot2**
theme.


```r
data("mtcars")

p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
     color = factor(gear))) + facet_wrap( ~ am)
p
```

![](/home/zacharias/Dokumente/RPTU/Seafile/Research/Code/envalysis/vignettes/theme_publish_files/figure-html/default_theme-1.png)<!-- -->

Adding `theme_publish()` applies the custom theme.


```r
p + theme_publish()
```

![](/home/zacharias/Dokumente/RPTU/Seafile/Research/Code/envalysis/vignettes/theme_publish_files/figure-html/theme_publish-1.png)<!-- -->
