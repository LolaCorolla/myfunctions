#' Title: Function to automatically make a heatmap
#'
#' \code{confocal.heatmap} It automatically makes a heatmap from a dataframe.
#'
#' @usage
#'If replicas present, `confocal.heatmap()` is used best in conjunction with \code{\link{match.pat}} so that
#'it can determine the replicas within the file vector and add the indices accordingly.
#'If no replicas, then it can be used as a stand alone.
#'
#' @param selected_files A string vector with the files selected for mapping
#' @param replicas A logical value indicating whether the vector has replicas that should be averages.
#' If T, the function will take the average from the files indicates to by replicas by `index = `.
#' If F, then no average will be calculated and plots will be generated for each file in the `selected_files` vector.
#' @param index A numeric vector e.g. `c(1,2)` indicating the location of the replicas within the `selected_files` vector.
#'
#' @return A plot saved within `02_fig_output`.
#'
#' @examples
#' @export
#' @examples
#' library(tidyverse)
#' library(ggprism)
#' example_data <- data.frame(x = c(1, 2, 3), y = c(3, 67, 90))
#' get.rsquared(example_data, "x", "y")
#'
# function to organise data and make heat map
#' @export
confocal.heatmap <- function(selected_files,
                             replicas = F, index = 1,
                             points = 100, lines = 10,
                             height = 500, width = 1000,
                             manual = F) {

  # are there replicas in the files?
  if (replicas == T) {
    # read files into long data frame
    df <- map(selected_files[index], read.table, sep = "\t") %>%
      bind_rows(.id = "ID")
    # columns = replica
    df1 <- data.frame(matrix(df$V1, 1014, 2))
    # make everything type numeric
    df1 <- df1[15:nrow(df1), ] %>%
      mutate(across(everything(), as.numeric))
    # mean row wise
    df2 <- apply(df1, 1, mean)

    # name of plot
    plot_name_index <- index[1]

  } else {
    df <- read.table(selected_files[index], sep = "\t")
    # make everything type numeric
    df2 <- as.numeric(df[15:nrow(df), ])

    # name of plot
    plot_name_index <- index
  }
  # organise into points and lines
  points = 100
  lines = 10
  df3 <- data.frame(matrix(df2, points, lines)) %>%
    pivot_longer(cols = c(1:all_of(lines))) %>%
    mutate(y = rep(0:(lines - 1), points),
           x = rep(seq(lines, lines * points, by = lines),
                   each = lines))

  # heatmap
  height = 500 # (um) dimensions of scanning area in confocal
  width = 1000 # um

  ggplot(df3,
         aes(y = y,
             x = x,
             fill = value)) +
    geom_tile(position = "identity") +
    scale_y_continuous(
      expand = c(0,-0.5),
      limits = c(-1, 10),
      breaks = seq(-0.5, 10, by = 2),
      labels = seq(0, height, by = 100)
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(5, 1005, by = 100),
      labels = seq(0, width, by = 100),
    ) +
    scale_fill_gradient2(
      low = "black",
      mid = "black",
      high = "red",
      midpoint = 500,
    ) +
    labs(x = "x (\U03bcm)",
         y = "y (\U03bcm)",
         fill = "Raman\nIntensity") +
    theme_bw() +
    theme(
      plot.margin = margin(0.2, 0.9, 0.2, 0.2, "cm"),
      legend.position.inside = c(1, 0.5),
      axis.ticks.length = unit(-0.25, "cm"),
      axis.ticks = element_line(colour = "white"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 13)
    )

  # extract name of sample
  new_name1 <- str_split_i(selected_files[plot_name_index],
                           "/| ", 3)
  # replace spec with 561 nm
  new_name2 <- str_replace(new_name1, "spec", "561nm")

  if (manual == T) {
    new_name2 <- paste0(new_name2, "_manual")
  }

  # make new name
  new_name_plot <- paste0("02_fig_output/",
                          folder_with_files,
                          "/",
                          new_name2,
                          ".png")
  # save plot
  ggsave(new_name_plot,
         width = 7.5,
         height = 5)
}
