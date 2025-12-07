#' Title: Function to turn p-values into stars*
#'
#' \code{pval.stars} converts the numeric values of a vector into specified sequences of symbols to signify p-value significance.
#'
#' @usage
#'It only takes a single value, so vectorising needs to be used here if \code{pval.stars} is to be applied
#'to a column or a vector with multiple elements.
#'
#' @param p A numeric vector.
#' @param symbol A string ("symbol") indicating the symbol to that will indicate significance.Default is `"*"`.
#'
#' @return It generates a vector of symbols (strings)
#'
#' @examples
#' (example_data <- data.frame(p.value = seq(0, 0.1, 0.02)))
#' example_data$stars <- sapply(example_data$p.value, pval.stars)
#' example_data
#'
#'
# r squared function -------
#' @export
pval.stars <- function(p, symbol =  "*") {
  sequences <- sapply(1:4,
                      \(x) paste0(rep(symbol, x), collapse = ""))

  if (p < 0.0001) sequences[4] #0.0001
  else if (p < 0.001) sequences[3] #0.001
  else if (p < 0.01) sequences[2] #0.01
  else if (p < 0.05) sequences[1] #0.05
  else "ns" #1
}
