#' Auxiliary function screenPatterns (using information gain)
#'
#' The function screens for the available patterns to pick best ones based
#' on the information gain
#'
#' @usage .screen_patterns(data, classes, pattern_pool, num_pat_max = 2000)
#'
#' @param data a data frame containing the variables in the model
#' @param classes a vector of classes (0 or 1) for the data.
#' @param pattern_pool the set of all possible patterns for \code{pattern_set} mined from the data
#' @param num_pat_max how many patterns to keep
#' @return a list of all patterns in the given data, indexed by lengths
#' @keywords internal
#' 

.screen_patterns <- function(data, classes, pattern_pool, num_pat_max = 2000) {

  entropies <- entropies(data, classes, pattern_pool)

  threshold <- ifelse(num_pat_max < length(entropies),
                      sort(entropies)[num_pat_max],
                      max(entropies))
  patterns <- pattern_pool[entropies <= threshold]
  pattern_pool <- lapply(1:max(lengths(patterns)),
                         FUN = function(l) {
                           unname(patterns[lengths(patterns) == l])
                         })

  return(pattern_pool)
}
