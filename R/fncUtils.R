#' Extract index from matrix.
#'
#' @details
#'
#' If matrix has column names, then oredered factor is returned.
#'
#' @examples
#' u <- matrix(1:10, ncol = 2)
#' extractIndexFromMatrix(u)
#' colnames(u) <- c("fn1", "fn2")
#' extractIndexFromMatrix(u)
#'
#' data("katowice.pollination")
#' extractIndexFromMatrix(katowice.pollination)
#'
extractIndexFromMatrix <- function(x) {

  if (is.null(colnames(x))) {
    return(1:ncol(x))
  }

  idx <- colnames(x)
  factor(idx, levels = idx, ordered = TRUE)
}
