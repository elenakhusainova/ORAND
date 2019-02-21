#' Auxiliary function to change the pattern set \code{pattern_set}
#'
#' '.cover_more()' returns one of the neighbors of \code{pattern_set} that
#' results in more positively classified observations
#'
#' @usage .cover_more(pattern_set, method, p, data, classes, prior, params, pattern_pool)
#'
#' @param pattern_set the pattern set
#' @param method method used for finding the minima: either "pattern" or "literal"
#' @param p the probability of returning a random neighbor (as opposed to the one
#'     with the minimum score function)
#' @param data a data frame containing the variables for the model.
#'   No continuous data is allowed.
#' @param classes a vector of classes (0 or 1) for the data.
#' @param prior prior: "BetaBinomial" or "Poisson"
#' @param params list of parameters for the model based on the choice of prior
#' @param pattern_pool the set of all possible patterns for \code{pattern_set} mined from the data
#' @return a new pattern set
#' @keywords internal
#' 

.cover_more <- function(pattern_set, method, p, data,
                        classes, prior, params, pattern_pool) {

  ########################################################################
  # --------------------- method == "pattern" -------------------------- #
  ########################################################################
  if (method == "pattern") {

    ##### Pick one random neighbor of pattern_set:
    added <- FALSE
    iter <- 0 # Just to make double sure the loop stops eventually
    while (!added & iter < 10000) {
      rand_pattern_set <- pattern_set
      class(rand_pattern_set) <- "boa"

      # Pick a random pattern from the pattern pool:
      new_pattern <- sample(sample(pattern_pool,
                                   1,
                                   prob = lengths(pattern_pool))[[1]], 1)

      ##### Add the pattern to the set if it's not already there:
      if (length(rand_pattern_set) < length(new_pattern[[1]])) {
        rand_pattern_set[[length(new_pattern[[1]])]] <- new_pattern
        added <- TRUE
      }
      else if (!new_pattern %in% rand_pattern_set[[length(new_pattern[[1]])]]) {
        rand_pattern_set[[length(new_pattern[[1]])]] <-
          c(rand_pattern_set[[length(new_pattern[[1]])]], new_pattern)
        added <- TRUE
      }
      iter <- iter + 1
    }
    if (iter == "10000") cat("Ooops, check the while-loop in cover_more!\n")

    # With probability p return this random neighbor:
    if (runif(1) < p) return(rand_pattern_set)

    ##### Otherwise find the optimal neighbor (the one that minimizes the score
    ##### function):
    else {
      min_score <- .score(data, classes, rand_pattern_set,
                          prior, params, pattern_pool)
      min_pattern_set <- rand_pattern_set
      # For each pattern length in the pattern pool:
      sapply(1:length(pattern_pool), function(l) {
        if (length(pattern_pool[[l]]) > 0) {
          # For each pattern of this length try to add it to the pattern set
          # and compare the scores:
          sapply(1:length(pattern_pool[[l]]), function(j) {
            temp <- pattern_pool[[l]][j]
            temp_pattern_set <- pattern_set
            if (length(temp_pattern_set) < length(temp[[1]])) {
              temp_pattern_set[[length(temp[[1]])]] <- temp
              new_score <- .score(data, classes, temp_pattern_set,
                                  prior, params, pattern_pool)
              if (new_score < min_score) {
                min_pattern_set <<- temp_pattern_set
                min_score <<- new_score
              }
            } else if (!temp %in% temp_pattern_set[[length(temp[[1]])]]) {
              temp_pattern_set[[length(temp[[1]])]] <-
                c(temp_pattern_set[[length(temp[[1]])]], temp)
              new_score <- .score(data, classes, temp_pattern_set,
                                  prior, params, pattern_pool)
              if (new_score < min_score) {
                min_pattern_set <<- temp_pattern_set
                min_score <<- new_score
              }
            }
          })
        }
      })
      return(min_pattern_set)
    }
  }

  ########################################################################
  # --------------------- method == "literal" -------------------------- #
  ########################################################################
  else {

    ##### With probability 0.5 we increase the coverage by removing a
    ##### literal from a pattern:
    if (runif(1) < 0.5) {

      # Check the input:
      if (sum(lengths(pattern_set)) == 0) {
        warning("Warning: The pattern set in empty, cannot remove a literal!\n")
        return(pattern_set)
      }

      # Pick a random pattern of the pattern set from which a literal will
      # be removed:
      rand_pattern_set <- pattern_set
      l <- sample(1:length(pattern_set), 1, prob = lengths(pattern_set))
      j <- sample(1:length(pattern_set[[l]]), 1)

      # Delete this pattern from this length:
      this_pattern <- rand_pattern_set[[l]][[j]]
      rand_pattern_set[[l]][[j]] <- NULL

      ##### Add this pattern to where it belongs:
      if (l > 1) {
        # Remove a literal:
        this_literal <- sample(1:l, 1)
        new_pattern <- list(this_pattern[-this_literal])
        if (!new_pattern %in% rand_pattern_set[[l - 1]]) {
          rand_pattern_set[[l - 1]] <- c(rand_pattern_set[[l - 1]], new_pattern)
        }
      }

      # With probability p return this random neighbor:
      if (runif(1) < p) return(rand_pattern_set)

      # Otherwise find the optimal neighbor (the one that minimizes the score
      # function):
      else {
        min_score <- .score(data, classes, rand_pattern_set,
                            prior, params, pattern_pool)
        min_pattern_set <- rand_pattern_set
        # For each length of a pattern in the pattern set:
        sapply(1:length(pattern_set), function(l) {
          # For each pattern of this length if there are any:
          if (length(pattern_set[[l]]) > 0) {
            sapply(1:length(pattern_set[[l]]), function(j) {
              ##### For each literal in this pattern delete it, compare
              ##### scores and pick the best one:
              sapply(1:length(pattern_set[[l]][[j]]), function(i) {
                temp_pattern_set <- pattern_set
                this_pattern <- temp_pattern_set[[l]][[j]]
                new_pattern <- list(this_pattern[-i])
                temp_pattern_set[[l]][[j]] <- NULL
                if (l > 1) {
                  if (!new_pattern %in% temp_pattern_set[[l - 1]]) {
                    temp_pattern_set[[l - 1]] <- c(temp_pattern_set[[l - 1]],
                                                   new_pattern)
                  }
                }
                new_score <- .score(data, classes, temp_pattern_set,
                                    prior, params, pattern_pool)
                if (new_score < min_score) {
                  min_pattern_set <<- temp_pattern_set
                  min_score <<- new_score
                }
              })
            })
          }
        })
        return(min_pattern_set)
      }
    }

    ##### With probability 0.5 we increase the coverage by adding
    ##### a new one literal pattern:
    else {

      ##### Pick one random neighbor of pattern_set:
      rand_pattern_set <- pattern_set
      var <- sample(names(data), 1)
      new_literal <-
        list(paste0(var, "=", c(sample(unique(as.character(data[, var])), 1))))
      if (!new_literal %in% rand_pattern_set[[1]]) {
        rand_pattern_set[[1]] <- c(rand_pattern_set[[1]], list(new_literal))
      }

      # With probability p return this random neighbor:
      if (runif(1) < p) return(rand_pattern_set)

      ##### Otherwise find the optimal neighbor (the one that
      ##### minimizes the score function):
      else {
        min_score <- .score(data, classes, rand_pattern_set,
                            prior, params, pattern_pool)
        min_pattern_set <- rand_pattern_set
        # For each variable:
        sapply(names(data), function(var) {
          ##### For each possible value of that variable create a literal,
          ##### add it to the set, compute the scores and pick the best:
          sapply(as.character(unique(as.character(data[, var]))),
                 function(value) {
                   new_literal <- paste0(var, "=", c(value))
                   temp_pattern_set <- pattern_set
                   temp_pattern_set[[1]] <-
                     c(temp_pattern_set[[1]], list(new_literal))
                   new_score <- .score(data, classes, temp_pattern_set,
                                       prior, params, pattern_pool)
                   if (new_score < min_score) {
                     min_pattern_set <<- temp_pattern_set
                     min_score <<- new_score
                   }
                 })
        })
        return(min_pattern_set)
      }
    }
  }

  stop("Error: shouldn't ever be here!\n")
  return(-1)
}
