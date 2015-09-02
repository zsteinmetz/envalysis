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
#' require(grid)
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'      colour=factor(gear))) + facet_wrap( ~ am)
#' p
#' p + theme_publish()
#' 
#' @seealso
#' \code{\link[ggplot2]{ggtheme}}
#' 
theme_publish <- function(base_size = 12, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    ggplot2::theme(
      axis.line =         element_blank(),
      axis.text.x =       element_text(size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
      axis.text.y =       element_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1),
      axis.ticks =        element_line(colour = "black", size = 0.2),
      axis.title.x =      element_text(size = base_size, vjust = -0.2),
      axis.title.y =      element_text(size = base_size, angle = 90, vjust = 1),
      axis.ticks.length = grid::unit(0.25, "lines"),
      axis.ticks.margin = grid::unit(0.2, "lines"),
      legend.key.size =   grid::unit(1.2, "lines"),
      legend.text =       element_text(size = base_size * 0.8, family = "sans"),
      legend.title =      element_text(size = base_size * 0.8, face = "bold", hjust = 0),
      legend.position =   "right",
      
      panel.background =  element_rect(fill = "white"), 
      panel.margin =      grid::unit(0.4, "lines"),
      
      strip.background =  element_rect(fill="white"), 
      strip.text.x =      element_text(size = base_size * 0.8),
      strip.text.y =      element_text(size = base_size * 0.8, angle = -90),
      
      plot.title =        element_text(size = base_size * 1.2),
      plot.margin =       grid::unit(c(1, 1, 0.5, 0.5), "lines")
    )
}
