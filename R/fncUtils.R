#' Extract index from matrix.
#' @noRd
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
#' data("katowice.airpollution")
#' extractIndexFromMatrix(katowice.airpollution)
#'
extractIndexFromMatrix <- function(x) {

  if (is.null(colnames(x))) {
    return(seq_len(ncol(x)))
  }

  idx <- colnames(x)
  factor(idx, levels = idx, ordered = TRUE)
}
