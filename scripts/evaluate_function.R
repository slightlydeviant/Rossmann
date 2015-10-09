# evaluate_function.R

evaluate <- function(truth, fitted){
  # Calculate the Root Mean Squared Percentage Error
  stopifnot(length(truth) == length(fitted))
  len <- length(fitted)
  rmspe <- sqrt(len * sum(((truth - fitted) / truth)^2))
  return(rmspe)
}