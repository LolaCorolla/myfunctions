#' Title: Multi-element string vector
#'
#' \code{msc} makes each element within a vector into a separate string.
#'
#' @usage
#' msc(...)
#'
#' @param ... elements to be turned into character type.
#'
#' @return A string vector.
#'
#' @examples
#' msc(element1, element2, element3)
#'
#' # for combining numbers and letters
#' paste0(msc(44,45), "nm")
#'
# function -------
#' @export
msc <- function(...) {
  args <- as.list(substitute(list(...)))[-1]
  sapply(args, function(x) {
    if (is.name(x) | is.numeric(x)) {
      as.character(x)
    } else if (is.call(x)) {
      paste(deparse(x), collapse = "")
    } else if (is.character(x)) {
      x
    } else if (is.symbol(x) && grepl("^`.*`$", deparse(x))) {
      eval(parse(text = deparse(x)))  # Evaluate backtick-wrapped names
    } else {
      warning("Unexpected input detected in msc() function.")
      as.character(deparse(x))
    }
  })
}
