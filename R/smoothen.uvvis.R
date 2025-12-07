#' Title: Function to smoothen curves for UV-vis data
#'
#' \code{smoothen.uvvis} uses the smooth function \code{\link{smooth.spline}} to fit a cubic smoothing spline.
#' It then applies it to all the columns of a data frame, and provides a second data frame with the smoothen values.
#' Can also provide a pivoted data frame for plotting various curves.
#'
#' @usage
#'x and y are strings with the respective column names.
#'
#' @param df A data frame.
#' @param spar Numerical value indicating amount of smoothing. See \code{\link{smooth.spline}}. Default is 0.5.
#' @param xIndex Numerical value indicating location of the x-axis column. Default is first column.
#' @param pivotLonger Logical value indicating whether the smoothen data frame should be pivoted for plotting. Default is F.
#'
#' @return A data frame with the smoothen curves.
#'
#' @examples
#' library(tidyverse)
#' # load example data
#' df <- read.csv("eg_rough_curve.csv")
#'
#' # smoothen curves
#' smoothen_data <- smoothen.uvvis(df, xIndex = 1, pivotLonger = T)
#'
#' # plotting smoothen data
#' ggplot(smoothen_data,
#'         aes(x = XYDATA,
#'             y = value,
#'             colour = name)) +
#' geom_line() +
#' theme_bw()
#'
#' smoothen_data <- df[1:2]
#' smoothen_data$smooth <- smoothen.uvvis(df[1:2], pivotLonger = F)[[2]]
#' smoothen_data <- pivot_longer(smoothen_data, cols = !1)
#'
#' # plotting side by side
#' ggplot(smoothen_data,
#'         aes(x = XYDATA,
#'             y = value,
#'             colour = name)) +
#' geom_line(show.legend = F) +
#' labs(x = "Wavelength (cm)", y = "Absorbance") +
#' facet_wrap(~name) +
#' theme_bw()

# function -------
#' @export
smoothen.uvvis <- function(df,
                           spar = 0.5,
                           xIndex = 1,
                           pivotLonger = F) {

  sm_list <- map(df, smooth.spline, spar = 0.5)
  # smoothen curves (cols) and put in a list

  df_sm <- map(1:ncol(df),
               function(x) sm_list[[colnames(df)[x]]][["y"]]) %>%
    as.data.frame()
  # extract smoothed data from the list above for each col of the df

  colnames(df_sm) <- colnames(df)
  # change col names to original

  df_sm[xIndex] <- df[xIndex]
  # change first column (x axis) to original because it got smoothed out too

  if (pivotLonger == T) {
    df_sm <- pivot_longer(df_sm, cols = !all_of(xIndex))
  } else {df_sm}
  # pivot longer for plotting of many curves
}
