#' @title ggplot2 theme for scientific publications
#'
#' @description
#' Themes set the general aspect of the plot such as the color of the
#' background, grid lines, the size and color of fonts. This particular theme is
#' based on the classic dark-on-light ggplot2 \code{\link[ggplot2]{theme_bw}}
#' and has been used for scientific publications.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param base_line_size base line size for, e.g. for ticks and axes
#' @param \dots further arguments to be passed to \code{\link[ggplot2]{theme_bw}}
#'
#' @author 
#' Zacharias Steinmetz
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
#' \code{\link[ggplot2]{theme_bw}}
#'
#' @import ggplot2
#' @export
theme_publish <- function(base_size = 12, base_family = "",
                          base_line_size = 0.25, ...) {
  half_line <- base_size / 2
  small_rel <- 0.8
  small_size <- small_rel * base_size
  
  # TODO: replace size with linewidth in `element_rect()`
  theme_bw(base_size = base_size, base_family = base_family, ...) %+replace%
    theme(
      rect = element_rect(fill = "transparent", colour = NA, color = NA,
                          size = 0, linetype = 0),
      text = element_text(family = base_family, face = "plain",
                          colour = "black", size = base_size, hjust = 0.5,
                          vjust = 0.5, angle = 0, lineheight = 0.9,
                          margin = ggplot2::margin(), debug = F),
      
      axis.text = element_text(size = small_size),
      axis.text.x = element_text(margin = ggplot2::margin(t = small_size/4),
                                 vjust = 1),
      axis.text.y = element_text(margin = ggplot2::margin(r = small_size/4), 
                                 hjust = 1),
      axis.title.x = element_text(margin = ggplot2::margin(t = small_size,
                                                           b = small_size)),
      axis.title.y = element_text(angle = 90,
                                  margin = ggplot2::margin(r = small_size,
                                                           l = small_size/4)),
      axis.ticks = element_line(colour = "black", size = base_line_size),
      axis.ticks.length = unit(0.25, 'lines'),
      
      axis.line = element_line(colour = "black", size = base_line_size),
      axis.line.x = element_line(colour = "black", size = base_line_size), 
      axis.line.y = element_line(colour = "black", size = base_line_size), 
      
      legend.spacing = unit(base_size/4, "pt"),
      legend.key = element_blank(),
      legend.key.size = unit(1 * base_size, "pt"),
      legend.key.width = unit(1.5 * base_size, 'pt'),
      legend.text = element_text(size = rel(small_rel)),
      legend.title = element_text(size = rel(small_rel), face = 'bold'),
      legend.position = 'bottom',
      legend.box = 'horizontal',
      
      panel.spacing = unit(1, "lines"),
      panel.background = element_blank(),
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      strip.text = element_text(size = base_size),
      strip.background = element_rect(fill = NA, colour = "black",
                                      size = 0.125),
      strip.text.x = element_text(face = 'bold', hjust = 0,
                                  margin = ggplot2::margin(b = small_size/2,
                                                           t = small_size/4)),
      strip.text.y = element_text(angle = -90, face = 'bold',
                                  margin = ggplot2::margin(l = small_size/2,
                                                           r = small_size/4)),
      
      plot.margin = unit(c(5,5,0,0), "pt"),
      plot.background = element_blank(),
      plot.title = element_text(face = "bold", size = 1.2 * base_size, 
                                margin = ggplot2::margin(b = half_line),
                                hjust = 0)
    )
}
