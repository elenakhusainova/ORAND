#' Calculates the score function
#'
#' '.score()' is an auxiliary function to calculate the score function
#' (defined as negative logarithm of the likelihood) for the given set of patterns
#'
#' @usage .score(data, classes, pattern_set, prior, params, pattern_pool)
#'
#' @param data a data frame containing the variables for the model.
#'   No continuous data is allowed.
#' @param classes a vector of classes (0 or 1) for the data.
#' @param pattern_set the pattern set
#' @param prior prior: "BetaBinomial" or "Poisson"
#' @param params list of parameters for the model based on the choice of prior
#' @param pattern_pool the set of all possible patterns for \code{pattern_set} mined from the data
#' @return score
#' @keywords internal
#' 

.score <- function(data, classes, pattern_set, prior, params, pattern_pool) {

  ##### Get true positive, true negative, false positive and false
  ##### negative counts:
  pred <- predict(pattern_set, data)
  tp <- sum(classes == 1 & pred == 1)
  tn <- sum(classes == 0 & pred == 0)
  fp <- sum(classes == 0 & pred == 1)
  fn <- sum(classes == 1 & pred == 0)

  ##### Calculate the (log)probability of the counts given the prior:
  log_p1 <- -lbeta(params$alpha_plus, params$beta_plus) +
    lbeta(tp + params$alpha_plus, fp + params$beta_plus)
  log_p2 <- -lbeta(params$alpha_minus, params$beta_minus) +
    lbeta(tn + params$alpha_minus, fn + params$beta_minus)

  ##### Get the auxiliary vector of maximum number of patterns of each length
  ##### the data can provide (using the fact that data is binary):
  max_num_pat <- sapply(1:ncol(data),
                        FUN = function(i) {2 * choose(ncol(data), i)})

  ##### Calculate the (log)probability of the set pattern_set given the prior:
  if (prior == "BetaBinomial") {

    pat_lens <- lengths(pattern_set)
    if (params$max_length > length(pat_lens)) {
      pat_lens <- c(pat_lens, rep(0, params$max_length - length(pat_lens)))
    }
    log_p3 <- sum(
      sapply(1:params$max_length, function(l) {
        log_p <- -lbeta(params$alpha[l], params$beta[l]) +
          lbeta(pat_lens[l] + params$alpha[l],
                max_num_pat[l] - pat_lens[l] +
                  params$beta[l])
        log_p
      }))
  }
  else if (prior == "Poisson") {
    p3 <- ifelse(sum(lengths(pattern_set)) == sum(max_num_pat),
                 ppois(sum(lengths(pattern_set)),
                       params$lambda_m, lower.tail = FALSE),
                 dpois(sum(lengths(pattern_set)), params$lambda_m))
    log_p3 <- ifelse(p3 == 0, -1000, log(p3))
    log_p3 <- log_p3 + sum(
      sapply(1:length(pattern_set),
             function(l) {
               out <- 0
               if (length(pattern_set[[l]]) > 0) {
                 if (l == 1) p <- ppois(1, params$lambda_l)
                 else if (l == ncol(data))
                   p <- ppois(l, params$lambda_l, lower.tail = FALSE)
                 else p <- dpois(l, params$lambda_l)
                 log_p <- ifelse(p == 0, -1000, log(p))

                 log_p <- log_p - log(choose(ncol(data), l))
                 log_p <- log_p * length(pattern_set[[l]])

                 log_p <- log_p + sum(
                   sapply(1:length(pattern_set[[l]]),
                          function(i) {
                            sum(
                              sapply(gsub("(.*)=(.*)", "\\1",
                                          pattern_set[[l]][[i]]),
                                     function(var) {
                                       temp <- length(unique(data[, var]))
                                       -ifelse(temp == 0, -1000, log(temp))
                                     }))
                          }))
                 out <- log_p
               }
               out
             }))

  }

  # Calculate and return the score (which is -log(likelihood)):
  score <- - log_p1 - log_p2 - log_p3
  return(score)
}
