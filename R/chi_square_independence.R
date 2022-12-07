#' Performs the Chi-square test of independence
#'
#' Performs the Chi-square test of independence on a input R by C matrix
#'
#' @details This function takes a input 2 dimensional matrix (without margins), computes the table margins and the chi-square statistic, and finally calculates the p-value
#'
#' @param dat numeric, matrix of data, at least 2 by 2
#'
#' @return numeric, a combined vector of length 2, the first element is the uncorrected chi-square statistic, the second is the p-value of the test
#'
#' @importFrom stats pchisq
#'
#' @export
#'
#' @examples
#' chi_square_independence(dat= matrix(c(5,15,25,3), ncol=2,nrow=2) )
#'
chi_square_independence <- function(dat) {

  if (any(is.na(dat)) || any(!is.numeric(dat)) ) {
    stop("dat must be numeric")
  }

  mat = data.matrix(dat)

  ncol = dim(mat)[2]
  nrow = dim(mat)[1]

  if (ncol <= 1 || nrow <= 1) {
    stop("dat must be at least 2 by 2")
  }

  col_margin = apply(mat, sum, MARGIN=2)
  row_margin = apply(mat, sum, MARGIN=1)

  expected_mat = as.vector(row_margin)%*%t(as.vector(col_margin))/sum(row_margin)

  chi_sq = sum(c( (mat - expected_mat)^2/expected_mat ))

  c(chi_sq, pchisq(q=chi_sq, df=(ncol-1)*(nrow-1), lower.tail=FALSE))

}
