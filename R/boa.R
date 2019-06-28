#' Implementing the BOA algorithm with R
#'
#' @param data a data frame containing the variables for the model. No continuous data is allowed.
#' @param classes a vector of classes (0 or 1) for the data.
#' @param prior prior: "BetaBinomial" or "Poisson"
#' @param params list of parameters for the model based on the choice of prior
#' @param method method used for finding the minima: either "pattern" or "literal"
#' @param iter_max maximum number of iterations
#' @param cool_rate cooling rate
#' @param p prob of selecting a random neighbor
#' @param metric metric to use for impurity measure of a split. Possible values: "cond.entropy", "Gini"
#' @return vector of classes
#'
#' @examples
#' params <- list(max_length = 3, alpha = rep(10, 3), beta = rep(10, 3),
#'          alpha_plus = 500, alpha_minus = 500, beta_plus = 1, beta_minus = 1)
#' df <- TicTacToe[, -28]
#' dfClasses <- TicTacToe$outcome
#' b <- boa(data = df, classes = dfClasses, prior = "BetaBinomial", 
#'          method = "pattern", metric = "cond.entropy", params, iter_max = 50, 
#'          cool_rate = 1000, p = 0.1)
#' b
#' @export

boa <- function(data, classes, prior = "BetaBinomial", method = "pattern",
                metric = "cond.entropy", params, iter_max = 100, cool_rate = 10, 
                p = 0.1) {
  
  ########################################################################
  # ----------------------- Checking the input ------------------------- #
  ########################################################################
  
  if (!class(data) == "data.frame" || 
      grepl("=", names(data)) ||
      any(is.na(data))) {
    stop("Error in boa(): argument 'data' should be a data frame without '=' in 
         the variable names and contain no missing values\n")
  }
  
  if (!length(classes) == nrow(data) || 
      any(is.na(as.numeric(classes))) ||
      !all(as.numeric(classes) %in% c(0,1))) {
    stop("Error in boa(): argument 'classes' should have the same number of 
         observations (classified as 0 or 1) as 'data' and contain no missing 
         values\n")
  }
  
  if (!prior %in% c("BetaBinomial", "Poisson")) {
    stop("Error in boa(): argument 'prior' should be either 'BetaBinomial'
         or 'Poisson'\n")
  }
  
  if (!method %in% c("pattern", "literal")) {
    stop("Error in boa(): argument 'method' should be either 'pattern'
         or 'literal'\n")
  }
  
  if (!class(iter_max) == "numeric" ||
      !length(iter_max) == 1 ||
      iter_max < 1) {
    stop("Error in boa(): argument 'iter_max' should be a positive integer\n")
  }
  
  if (!class(cool_rate) == "numeric" ||
      !length(cool_rate) == 1 ||
      cool_rate < 0) {
    stop("Error in boa(): argument 'cool_rate' should be a positive number\n")
  }
  
  if (!class(p) == "numeric" ||
      !length(p) == 1 ||
      p < 0 || p > 1) {
    stop("Error in boa(): argument 'p' should be a number between 0 and 1\n")
  }
  
  if (!class(params) == "list") 
    stop("Error in boa(): argument 'params' should be a list\n")
  if (prior == "BetaBinomial") {
    if (!all(names(params) == c("max_length", "alpha", "beta", "alpha_plus",
                                "alpha_minus", "beta_plus", "beta_minus")))
      stop("Error in boa(): check the names of argument 'params'\n")
    if (!class(params$max_length) == "numeric" ||
        !length(params$max_length) == 1 ||
        params$max_length < 1 || params$max_length > ncol(data) ||
        
        !class(params$alpha) == "numeric" ||
        !length(params$alpha) == params$max_length || any(params$alpha <= 0) ||
        
        !class(params$beta) == "numeric" ||
        !length(params$beta) == params$max_length || any(params$beta <= 0) ||
        
        !class(params$alpha_plus) == "numeric" ||
        !length(params$alpha_plus) == 1 || params$alpha_plus <= 0 ||
        
        !class(params$alpha_minus) == "numeric" ||
        !length(params$alpha_minus) == 1 || params$alpha_minus <= 0 ||
        
        !class(params$beta_plus) == "numeric" ||
        !length(params$beta_plus) == 1 || params$beta_plus <= 0 ||
        
        !class(params$beta_minus) == "numeric" ||
        !length(params$beta_minus) == 1 || params$beta_minus <= 0)
    stop("Error in boa(): check the argument 'params'\n")
  } else {
    if (!all(names(params) == c("lambda_m", "lambda_l", "alpha_plus",
                                "alpha_minus", "beta_plus", "beta_minus")))
      stop("Error in boa(): check the names of argument 'params'\n") 
    if (!class(params$lambda_m) == "numeric" ||
        !length(params$lambda_m) == 1 || params$lambda_m < 0 ||
        
        !class(params$lambda_l) == "numeric" ||
        !length(params$lambda_l) == 1 || params$lambda_l < 0 ||
        
        !class(params$alpha_plus) == "numeric" ||
        !length(params$alpha_plus) == 1 || params$alpha_plus <= 0 ||
        
        !class(params$alpha_minus) == "numeric" ||
        !length(params$alpha_minus) == 1 || params$alpha_minus <= 0 ||
        
        !class(params$beta_plus) == "numeric" ||
        !length(params$beta_plus) == 1 || params$beta_plus <= 0 ||
        
        !class(params$beta_minus) == "numeric" ||
        !length(params$beta_minus) == 1 || params$beta_minus <= 0)
    stop("Error in boa(): check the argument 'params'\n")
  }
  
  ########################################################################
  # ----------------- Initializing pattern_set ------------------------- #
  ########################################################################
  # pattern_set is a list of lists of array of strings: Each string is a
  # literal and an array of strings is a pattern.
  # The arrays are combined in the lists based on their lengths.
  if (prior == "BetaBinomial") {
    
    # Mining and screening the patterns: we mine all patterns from the 
    # positively classified observations with lengths less than max_length and
    # with small conditional entropy:
    start.time <- Sys.time()
    pattern_pool <- .mine_patterns(data[classes == 1, ], 
                                   max_length = params$max_length)
    end.time <- Sys.time()
    cat("The patterns are mined! Time taken:", 
        round(difftime(end.time, start.time, units = "min"), 4), "min \n")
    
    start.time <- Sys.time()
    pattern_pool <- .screen_patterns(data, classes, pattern_pool, 
                                     num_pat_max = 2000, 
                                     metric = metric) # TODO: Let the user provide num_pat_max
    end.time <- Sys.time()
    cat("The patterns are screened! Time taken:", 
        round(difftime(end.time, start.time, units = "min"), 4), "min \n")
    
    # Vector of probabilities for a pattern of each length in the pattern_pool
    # to be included in the pattern set:
    prob_length <- rbeta(params$max_length, params$alpha, params$beta)
    # Sample how many patterns of certain length will be included in the
    # pattern_set:
    sizes <- sapply(1:min(params$max_length, length(pattern_pool)), 
                    function(l) {rbinom(1, length(pattern_pool[[l]]), 
                                        prob_length[l])})
    # Get the pattern_set:
    pattern_set <- lapply(1:min(params$max_length, length(pattern_pool)), 
                          function(l) {sample(pattern_pool[[l]], sizes[l])})
  }
  
  else if (prior == "Poisson") {
    cat("Prior 'Poisson' is not tested yet\n")
    
    # Total number of patterns in pattern_set:
    num_pat <- rpois(1, params$lambda_m)
    # Lengths of patterns in pattern_set:
    len_pat <-
      tabulate(pmax(1, pmin(rpois(num_pat, params$lambda_l), ncol(data))))
    # Get the pattern_set:
    pattern_set <- lapply(1:length(len_pat), function(l) {
      if (len_pat[l] == 0) return(list())
      lapply(1:len_pat[l], function(i) {
        vars <- sample(names(data), l)
        out <- sapply(vars, function(var) {
          paste0(var, "=", sample(unique(as.character(data[, var])), 1))
        })
        out
      })
    })
  }
  
  ########################################################################
  # ---------------- MAP (maximizing a posteriori) --------------------- #
  ########################################################################
  
  ##### Prepare for the while loop:
  iter <- 0
  this <- sample(1:length(pattern_pool), 1, prob = lengths(pattern_pool))
  pattern_set <- list()
  pattern_set[[this]] <- sample(pattern_pool[[this]], 1)
  curr_pattern_set <- pattern_set
  # For keeping track of the best pattern_set we seen:
  min_pattern_set <- pattern_set
  class(curr_pattern_set) <- "boa" # I need it so that predict.boa works
  class(min_pattern_set) <- "boa"
  # Find the missclassified by the current pattern set observations:
  misclassified <- which(predict(curr_pattern_set, data) != classes)
  
  ##### Iterations:
  while (iter < iter_max & length(misclassified) > 0) {
    
    # Nice output:
    pr <- round(iter*20 / iter_max) # Progress
    smr <- 
      paste0("ME: ", round(length(misclassified) / length(classes), 3),
             " Score: ", round(.score(data, classes, curr_pattern_set,
                                      prior, params, pattern_pool), 3)) # Summary of iteration
    cat("\rProgress: |", paste0(rep("=", pr)),
        paste0(rep(" ", 20 - pr)), "|   Iteration ", ifelse(iter < 10, " ",""),
        iter, "/", iter_max, "  ", smr, sep = "")
    
    # Find an example of missclasified data:
    miss_ex <- classes[sample(misclassified, 1)]
    
    # Based on whether the example is FN or FP run different scenarios:
    if (miss_ex) {
      new_pattern_set <- .cover_more(curr_pattern_set, method, p, data, classes,
                                     prior, params, pattern_pool)
    } else {
      new_pattern_set <- .cover_less(curr_pattern_set, method, p, data, classes,
                                     prior, params, pattern_pool)
    }
    
    # If the new pattern set is better than the best we've seen so far
    # keep it:
    if (.score(data, classes, min_pattern_set, prior, params, pattern_pool) >
        .score(data, classes, new_pattern_set, prior, params, pattern_pool)) {
      min_pattern_set <- new_pattern_set
    }
    # Define the probability of moving to the newly created patten set
    # (we don't always go there because we don't want to be stuck in the local
    # minima):
    alpha <- min(1, exp(- (.score(data, classes, new_pattern_set,
                                  prior, params, pattern_pool) -
                             .score(data, classes, curr_pattern_set,
                                    prior, params, pattern_pool))
                        * log(1 + iter) / cool_rate))
    if (runif(1) <= alpha) curr_pattern_set <- new_pattern_set
    
    iter <- iter + 1
    misclassified <- which(predict(curr_pattern_set, data) != classes)
  }
  
  if (length(misclassified) == 0) {
    cat("\nFound the perfect solution, stopping now!\n")
  }
  
  return(min_pattern_set)
}
