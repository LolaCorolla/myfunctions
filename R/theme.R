#' Custom theme for the thesis/publications
#'
#' @return A ggplot2 theme object
#' @export
#' @examples
#' library(tidyverse)
#' library(ggprism)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   thesis.theme()
#'
# Theme ----
thesis.theme <- function() {
  ggprism::theme_prism() +
    ggplot2::theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      # makes grid lines blank
      panel.background = element_rect(fill = "transparent",
                                      colour = NA),
      # plot background transparent
      plot.margin = margin(0.8, 1, 0.4, 0.4, "cm"),
      # margin of plot within image
      axis.title = element_text(
        # face = "bold",
        size = 20,
        colour = "black"),
      axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
      axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
      # distance between axis and axis title
      axis.line = element_line(linewidth = 1),
      axis.ticks = element_line(linewidth = 1,
                                colour = "black"),
      # thickness of axis lines and ticks
      axis.ticks.length = unit(6, "pt"),
      prism.ticks.length = unit(4, "pt"),
      # length of minor and major axis ticks
      axis.text = element_text(
        # face = "bold",
        colour = "black",
        size = 20),
      legend.position.inside = c(0.98, 0.96),
      # places box within the plot (sideways, up and down)
      legend.justification = c("right", "top"),
      # general position of legend box
      legend.box.just = "centre",
      legend.margin = margin(6, 6, 6, 6),
      legend.box.background = element_rect(
        colour = "black",
        linewidth = 1),
      # gives black outline to legend box
      legend.key = element_blank(),
      # makes background of legend key clear
      legend.title = element_text(
        #face = "bold",
        size = 13,
        colour = "black",
        hjust = 0.5),
      legend.text = element_text(
        margin = margin(t = 0.2, unit = 'cm'),
        size = 10,
        vjust = 1.5)
      # spacing of legend text inside legend box
    )
}
