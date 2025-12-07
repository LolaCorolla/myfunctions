#' Title: Find matching file names
#'
#' \code{match.pat} A function customized for my file system where I have 2 replicas for each sample.
#' The file names for this will only differ by the replica number e.g. A1, A2, B1, B2. But, sometimes more
#' complicated than that. So, this function finds the matching samples and groups it by replica numbers.
#'
#' @usage
#'match.pat(pattern_list)
#'
#' @param patterns A string vector
#'
#' @return A list is generated with the following components:
#' \item{indices }{the position of the matching elements within the vector}
#' \item{patterns }{the matching elements}
#'
#' @examples
#' example_data <-c("C3T2_spec", "C1T1_spec", "C1T2_spec", "C3T1_spec")
#' match.pat(example_data)

# Function to find matching patterns differing only by '1' and '2'
#' @export
match.pat <- function(patterns) {
  matching_pairs <- list()

  patterns <- str_split_i(patterns, "/| ", 3)

  for (i in 1:length(patterns)) {
    for (j in (i+1):length(patterns)) {
      pattern1 <- patterns[i]
      pattern2 <- patterns[j]

      # Replace '1' with 'X' and '2' with 'X' to check for equivalence
      generalized1 <- gsub("1", "X", pattern1)
      generalized2 <- gsub("1|2", "X", pattern2)

      if (generalized1 %in% generalized2) {
        matching_pairs <- append(matching_pairs,
                                 list(list(indices = c(i, j),
                                           patterns = c(pattern1, pattern2))))
      }
    }
  }

  return(matching_pairs)
}
