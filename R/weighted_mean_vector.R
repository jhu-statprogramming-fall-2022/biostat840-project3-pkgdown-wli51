#' Calculates the weighted mean of a variable number of elements
#'
#' Calculates the weighted mean of a variable number of elements, input can be multiple vectors
#'
#' @details This function a variable number of parameters plus one named parameter weight, a vector that must be of the length of the number of other parameters, and calcualtes the weighted average based on the given weight vector
#'
#' @param ... numeric, variable length input of element to compute weighted mean with, can be vectors but must all be of the same length
#'
#' @param weights numeric, vector with length equivalent to the number of other parameters, will be used as weights for the mean calculation, must be positive but need not sum up to 1
#'
#' @return numeric, weighted mean, if input is in vector form then a vector will be returned
#'
#' @export
#'
#' @examples
#' weighted_mean_vector(weights=c(0.8,0.2), 1, 10)
#' weighted_mean_vector(weights=c(0.8,0.2, 2), 1, 10, 100)
#' weighted_mean_vector(weights=c(0.8,0.2), c(1,2,3), c(10, 20, 30))
#'
weighted_mean_vector <- function(..., weights) {

  if (any(is.na(weights)) || any(!is.numeric(weights)) ) {
    stop("weight must be numeric")
  }

  l = list(...)

  mat = do.call(rbind, l)
  weights = c(weights)

  out = rep(NA, dim(mat)[2])

  if (length(weights) == dim(mat)[1] & is.numeric(mat)) {
    out = apply(t(do.call(rbind, l)*weights), sum, MARGIN=1)/sum(weights)
  }

  out
}

