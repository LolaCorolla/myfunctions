#' Title: Function for 0 to 1 curve normalisation*
#'
#' \code{zero.norm} 0 to 1 curve normalisation.
#'
#' @usage
#' It takes a data frame and an specified column (numeric position) and normalises according to a range of values in the x-axis.
#' For example, if there are multiple peaks in a spectra, then specifying the range for normalisation can exclude undesired peaks.
#'
#' @param df A data frame.
#' @param col A numeric vector indicating the position of the column in the data frame.
#' @param min_x Start of x-axis range, default = 300.
#' @param max_x End of x-axis range, default = 700.
#'
#' @return A numeric vector with normalised values.
#'
#' @examples
#'
#'
#'
# zero.norm function -------
#' @export
zero.norm <- function(df, col, min_x = 300, max_x = 700){
  max_y <- max(df[,col][df[,1] %in% min_x:max_x])
  min_y <- min(df[,col][df[,1] %in% min_x:max_x])

  (df[, col] - min_y) / (max_y - min_y)
}
