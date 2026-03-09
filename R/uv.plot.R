#' Title: Function for plotting uv-vis curves*
#'
#' For plotting uv-vis data.
#'
#' @usage
#' It takes a data frame pre-arranged in wavelength and absorbance columns and plots it. It has legend and saving options.
#'
#' @param df A data frame.
#' @param legend Logical vector indicating the inclusion of a legend in the plot.
#' @param legend_items A vector string with the legend elements. The legend must of equal length as the number of columns.
#' @param save Logical element for optional saving. Default is T.
#' @param savepath A string indicating path for saving.
#' @param smooth A logical vector indicating whether the curve should be smoothed, default is T. It uses \code{smoothen.uvvis} from this package to fit a cubic smoothing spline.
#'
#' @return A plot.
#'
#' @examples
#'library(tidyverse)
#'library(myfunctions)
#'
#'
# uv.plot function -------
#' @export
uv.plot <- function(df, legend = T, legend_items = 0, save = T, savepath, smooth = T){

  if (length(legend_items) != ncol(df)-1 && legend_items != 0) {
    stop("Elements in legend do not match the number of plot elements. Make sure the legend matches the column names or do not add a legend argument.")
  } else if (length(legend_items) == ncol(df)-1) {
    colnames(df)[-1] <- legend_items
  }

  if (smooth == F) {
    df_graph <- pivot_longer(df, -1)
  } else {
    df_graph <- df %>%
      smoothen.uvvis() %>%
      pivot_longer(-1)
  }

  plt <- ggplot(df_graph,
                aes(x = nm,
                    y = value,
                    colour = name)) +
    geom_line(
      linewidth = 1,
      show.legend = legend
    ) +
    labs(
      y = "Absorbance",
      x = "Wavelength (nm)"
    ) +
    thesis.theme() +
    theme(
      legend.title= element_blank()
    )

  if (save == T) {
    ggsave(savepath, width = 4.5, height = 3, scale = 2)
  }

  plt

}
