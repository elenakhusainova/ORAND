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
  if (sum(lengths(object)) == 0) return(pred)
  temp <- unlist(object, recursive = F)
  temp <- unlist(lapply(temp, FUN = function(x){paste(paste0("data$", x), 
                                                      collapse = " & ")}))
  cmd <- paste0("(", paste0(temp, collapse = ") | ("), ")")
  cmd <- gsub("=", "==", cmd)
  pred <- as.numeric(eval(parse(text=cmd)))
  return(pred)
}
