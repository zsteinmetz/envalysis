#' @title ggplot2 theme for scientific publications
#'
#' @description
#' Themes set the general aspect of the plot such as the color of the
#' background, gridlines, the size and colour of fonts. This particular theme is
#' based on the classic dark-on-light ggplot2 \code{theme_bw} and has been used
#' for scientific publications,
#'
#' @usage
#' theme_publish(base_size = 12, base_family = "")
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @examples
#' require(ggplot2)
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'      colour=factor(gear))) + facet_wrap( ~ am)
#' p
#' p + theme_publish()
#'
#' @seealso
#' \code{\link[ggplot2]{ggtheme}}
#'
#' @export
theme_publish <- function(base_size = 12, base_family = '') {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      axis.line = element_blank(),
      axis.text = element_text(colour = 'black', size = 0.875 * base_size,
                               lineheight = 0.9),
      axis.text.x = element_text(margin = margin(2.5,2.5,5,2.5,'pt')),
      axis.text.y = element_text(margin = margin(2.5,2.5,2.5,5,'pt')),
      axis.ticks = element_line(colour = 'black', size = 0.25),
      axis.title = element_text(colour = 'black', size = base_size),
      axis.ticks.length = unit(0.25, 'lines'),

      legend.key = element_blank(),
      legend.key.size = unit(base_size, 'pt'),
      legend.key.width = unit(1.5 * base_size, 'pt'),
      legend.text = element_text(size = base_size),
      legend.title = element_text(size = base_size, face = 'bold'),
      legend.position = 'bottom',
      legend.box = 'horizontal',

      panel.background = element_rect(fill = NA, colour = "black", size = 0.125),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.margin = unit(0.5, 'lines'),

      strip.background = element_rect(colour = 'black', fill = NA, size = 0.125),
      strip.text = element_text(colour = 'black', size = base_size),
      strip.text.x = element_text(margin = margin(4,0,4,0)),
      strip.text.y = element_text(margin = margin(0,4,0,4), angle = -90)
    )
}
