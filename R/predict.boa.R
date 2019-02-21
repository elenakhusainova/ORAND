#' Prediction function
#'
#' @method predict boa
#' @param object pattern set
#' @param data data frame containing the variables for the model.
#'   No continuous data is allowed.
#' @param ... additional arguments for specific methods.
#' @return a vector of predictions
#' @export

predict.boa <- function(object, data, ...) {

  if (!inherits(object, "boa"))
    warning("The pattern set is not a boa class!\n")

  pred <- rep(0, nrow(data))
  sapply(1:length(object), function(l) {
    if (length(object[[l]]) == 0) return(NULL)
    sapply(1:length(object[[l]]), function(i) {
      vars <- gsub("(.*)=(.*)", "\\1", (object[[l]][[i]]))
      vals <- gsub("(.*)=(.*)", "\\2", (object[[l]][[i]]))
      pred[Reduce("&",
                  lapply(1:length(vars),
                         function(j) {
                           data[, vars[j]] == vals[j]
                         }
                  ))] <<- 1
    })
  })

  return(pred)
}
