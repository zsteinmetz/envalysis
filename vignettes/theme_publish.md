---
title: "theme_publish() demo"
author: "Zacharias Steinmetz"
date: "2018-02-19"
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



## Sample code


```r
require(envalysis)
require(ggplot2)
p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
     colour=factor(gear))) + facet_wrap( ~ am)
p
```

![](theme_publish_files/figure-html/theme_publish-1.png)<!-- -->

```r
p + theme_publish() + ggsave('theme_publish.png', dpi = 96)
```

![](theme_publish_files/figure-html/theme_publish-2.png)<!-- -->
