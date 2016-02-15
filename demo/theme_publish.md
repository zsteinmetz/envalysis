# theme_publish demo

Sample code:

```r
require(ggplot2)
p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
     colour=factor(gear))) + facet_wrap( ~ am)
p
p + theme_publish() + ggsave('theme_publish.png', dpi = 96)
```

Output:

![theme_publish](./theme_publish.png)
