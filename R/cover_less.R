#' Auxiliary function to change the pattern set
#'
#' \code{.cover_less()} returns one of the neighbors of \code{pattern_set} that
#' results in less positively classified observations. How neighbor is defined
#' depends on the parameter \code{method}
#'
#' @usage .cover_less(pattern_set, method, p, data, classes, prior, params, 
#'         pattern_pool)
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

.cover_less <- function(pattern_set, method, p, data, classes,
                        prior, params, pattern_pool) {

  ########################################################################
  # --------------------- method == "pattern" -------------------------- #
  ########################################################################
  if (method == "pattern") {

    ##### Pick one random neighbor (i.e. differs in one pattern) of pattern_set:
    rand_pattern_set <- pattern_set
    # Select one pattern from the pattern set at random:
    l <- sample(1:length(rand_pattern_set), 1, prob = lengths(rand_pattern_set))
    j <- sample(1:length(rand_pattern_set[[l]]), 1)
    # Delete this pattern:
    rand_pattern_set[[l]][[j]] <- NULL

    # With probability p return this random neighbor:
    if (runif(1) < p) return(rand_pattern_set)
    
    # Otherwise find the optimal neighbor (the one that minimizes the score
    # function):
    else {
      # The score of the random neighbor:
      min_score <-
        .score(data, classes, rand_pattern_set, prior, params, pattern_pool)

      ##### Find (greedily) the neighbor with the least score:
      min_pattern_set <- rand_pattern_set
      sapply(1:length(pattern_set), function(l) { # For each pattern length
        if (length(pattern_set[[l]]) > 0) { # if we have patterns of that length
          sapply(1:length(pattern_set[[l]]), function(j) { # for each pattern
            temp_pattern_set <- pattern_set
            temp_pattern_set[[l]][[j]] <- NULL
            new_score <-
              .score(data, classes, temp_pattern_set,
                     prior, params, pattern_pool)
            if (new_score < min_score) {
              min_pattern_set <<- temp_pattern_set
              min_score <<- new_score
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
    ##### With probability 0.5 we decrease the coverage by adding a literal to
    ##### a random pattern in the pattern set:
    if (runif(1) < 0.5) {

      ##### Pick one random neighbor (i.e. differs in one literal) of
      ##### pattern_set (If there are patterns to which a literal can
      ##### be added):
      if (sum(lengths(pattern_set)[-ncol(data)]) > 0) {
        added <- FALSE
        iter <- 0 # Just to make double sure the loop stops eventually
        while (!added & iter < 10000) {
          rand_pattern_set <- pattern_set

          # Select a random pattern to which we can add a literal:
          l <- sample(setdiff(1:length(pattern_set), ncol(data)),
                      1,
                      prob = lengths(pattern_set)[-ncol(data)])
          j <- sample(1:length(pattern_set[[l]]), 1)

          # Sample a literal to add:
          var <- sample(
            setdiff(names(data),
                    gsub("(.*)=(.*)", "\\1", rand_pattern_set[[l]][[j]])),
            1)
          new_literal <- paste0(var, "=",
                                c(sample(unique(as.character(data[, var])), 1)))

          # Store the pattern with added literal and delete it from the list
          # (its length is different now)
          this_pattern <- rand_pattern_set[[l]][[j]]
          new_pattern <- list(c(this_pattern, new_literal))
          rand_pattern_set[[l]][[j]] <- NULL

          # Add the pattern to the list with the patterns of this length
          # if it's not there already:
          if (length(rand_pattern_set) >= l + 1) {
            if (length(rand_pattern_set[[l + 1]]) > 0) {
              if (!new_pattern %in% rand_pattern_set[[l + 1]]) {
                rand_pattern_set[[l + 1]] <-
                  c(rand_pattern_set[[l + 1]], new_pattern)
                added <- TRUE
              }
            }
          } else {
            rand_pattern_set[[l + 1]] <- new_pattern
            added <- TRUE
          }
          iter <- iter + 1
        }
        if (iter == "10000") cat("Ooops, check the while-loop in cover_less!\n")

        # With probability p return this random neighbor:
        if (runif(1) < p) return(rand_pattern_set)

        ##### Otherwise find the optimal neighbor
        ##### (the one that minimizes the score function):
        else {
          min_score <- .score(data, classes, rand_pattern_set,
                              prior, params, pattern_pool)
          min_pattern_set <- rand_pattern_set
          # For each length less that maximum possible:
          sapply(setdiff(1:length(pattern_set), ncol(data)), function(l) {
            # If there are patterns of this length:
            if (length(pattern_set[[l]]) > 0) {
              ##### For each pattern of this length: add a new literal,
              ##### put the new pattern to the patterns of this size if it's
              ##### not there yet, calculate the score, find the best.
              sapply(1:length(pattern_set[[l]]), function(j) {
                sapply(setdiff(names(data),
                               gsub("(.*)=(.*)", "\\1", pattern_set[[l]][[j]])),
                       function(var) {
                         new_literal <-
                           paste0(var, "=",
                                  c(sample(unique(as.character(data[, var])),
                                           1)))
                         temp_pattern_set <- pattern_set
                         this_pattern <- temp_pattern_set[[l]][[j]]
                         new_pattern <- list(c(this_pattern, new_literal))

                         if (length(temp_pattern_set) >= l + 1) {
                           if (length(temp_pattern_set[[l + 1]]) > 0) {
                             if (!new_pattern %in% rand_pattern_set[[l + 1]]) {
                               temp_pattern_set[[l + 1]] <-
                                 c(temp_pattern_set[[l + 1]], new_pattern)
                               temp_pattern_set[[l]][[j]] <- NULL
                             }
                           } else {
                             temp_pattern_set[[l + 1]] <- new_pattern
                             temp_pattern_set[[l]][[j]] <- NULL
                           }
                         } else {
                           temp_pattern_set[[l + 1]] <- new_pattern
                           temp_pattern_set[[l]][[j]] <- NULL
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
      } else {
        warning("Warning: cannot add a literal to a pattern as there no
                uncompleted patterns! Return the same pattern set at this
                iteratrion step.\n")
        return(pattern_set)
      }
    }

    ##### With probability 0.5 we decrease the coverage by removing a pattern:
    else {

      ##### Pick one random neighbor of pattern_set:
      rand_pattern_set <- pattern_set
      l <- sample(1:length(pattern_set), 1, prob = lengths(pattern_set))
      j <- sample(1:length(pattern_set[[l]]), 1)
      rand_pattern_set[[l]][[j]] <- NULL

      # With probability p return this random neighbor:
      if (runif(1) < p) return(rand_pattern_set)

      ##### Otherwise find the optimal neighbor (the one that minimizes
      ##### the score function):
      else {
        min_score <-
          .score(data, classes, rand_pattern_set, prior, params, pattern_pool)
        min_pattern_set <- rand_pattern_set
        # For each length of a pattern in the pattern_set:
        sapply(1:length(pattern_set), function(l) {
          # If there are patterns of this length:
          if (length(pattern_set[[l]]) > 0) {

            ##### Delete patterns one at a time and pick the best:
            sapply(1:length(pattern_set[[l]]), function(j) {
              temp_pattern_set <- pattern_set
              temp_pattern_set[[l]][[j]] <- NULL
              new_score <- .score(data, classes, temp_pattern_set,
                                  prior, params, pattern_pool)
              if (new_score < min_score) {
                min_pattern_set <<- temp_pattern_set
                min_score <<- new_score
              }
            })
          }
        })
        return(min_pattern_set)
      }
    }
  }

  stop("Error: shouldn't ever be here!\n")
  return(-1)
}
