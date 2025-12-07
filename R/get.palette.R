#' Get a named palette
#'
#' Just a list of palettes I've used in the thesis.
#'
#' @usage get.palette("palette_name")
#' Make sure the name is a string.
#'
#' @param pal_blind different colours from colour-blind friendly palette
#' @param greens Dark to light greens
#' @param yellows Dark to light yellows
#' @param series_two Black and red
#' @param GPT Palette from chatGPT
#' @param pastel Pastel colours
#'
#' @export
#' @examples
#' library(tidyverse)
#' library(ggprism)
#' example_data <- data.frame(x = c(1:8),
#'                            y = seq(10, 80, by = 10),
#'                            z = factor(c(1:8)))
#' ggplot(example_data, aes(x, y, colour = z)) +
#'   geom_point() +
#'   scale_color_manual(values = get.palette("pal_blind")) +
#'   thesis.theme()
#' get.palette("pal_blind")
get.palette <- function(name = "pal_blind") {
  palettes <- list(
    pal_blind = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7"),
    greens = c("#042812","#015134","#056e12" ,"#007961","#09aa3b", "#02b72e", "#6bb92d", "#c9e9ae"),
    greens3 = c("#02b72e", "#056e12", "#007961"),
    greens5 = c("#042812","#015134", "#09aa3b", "#6bb92d", "#c9e9ae"),
    yellows = c("#e7a000", "#f76015", "#a41415"),
    series_two = c("black", "#a41415"),
    GPT = c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51"),
    pastel = c("#A8DADC", "#F1FAEE", "#457B9D")
  )
  palettes[[name]]
}
