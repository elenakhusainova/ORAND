#' Auxiliary function minePatterns (using FP-growth algorithm)
#'
#' The function uses fp-growth algorithm from rCBA package to mine all frequent
#' patterns from the given data. The user can provide minimum pattern support -
#' the proportion of the data that have such pattern.
#' The function returns the patterns in the form of a list
#' indexed by the pattern lengths.
#'
#' @usage .mine_patterns(data, support = 0.05, max_length = 3)
#'
#' @param data a data frame containing the variables in the model
#' @param support the minimum support of the pattern
#' @param max_length the maximum length of the output patterns
#'
#' @return a list of all patterns in the given data, indexed by lengths
#' @keywords internal
#' 

.mine_patterns <- function(data, support = 0.05, max_length = 3) { # TODO: let user provide support

  # Change the data format to "transactions":
  data <- data.frame(sapply(data, as.factor))
  data_trans <- methods::as(data, "transactions")
  data_trans <- methods::as(data_trans, "list")

  # Mine the patterns:
  patterns <- fpgrowth(data_trans,
                       supp = -support * length(data_trans),
                       zmax = max_length)

  # Get the patterns (rules) from the above:
  patterns <- sapply(1:length(patterns),
                     FUN = function(i) {
                       patterns[[i]][[1]]
                     })

  if (max(lengths(patterns)) == 0) stop("All patterns are of length zero!\n")

  return(patterns)
}
