.depTheme <- function() {
  return(ggplot2::theme(axis.title.x = ggplot2::element_text(face = "bold", vjust = 0, size = 16),
               axis.title.y = ggplot2::element_text(face = "bold", angle = 90,
                                           vjust = 0.2, size = 16),
               axis.text.x = ggplot2::element_text(size = 14),
               axis.text.y = ggplot2::element_text(size = 14),
               title = ggplot2::element_text(face = "bold", vjust = 1, size = 18)))
}

# Okabe-Ito blue and vermillion: the first stays the blue every DD plot has
# been drawn in, and the pair is distinguishable under the common forms of
# colour blindness. More than two samples falls back to ggplot2's own scale.
.ddPlotColors <- function(n) {
  base <- c("#0072B2", "#D55E00")

  if (n <= length(base)) {
    return(base[seq_len(n)])
  }

  grDevices::hcl(h = seq(15, 375, length.out = n + 1)[seq_len(n)],
                 c = 100, l = 65)
}

.testNorm <- function(d = 2) {
  MASS::mvrnorm(100, rep(1, d), diag(d))
}

.addAlpha <- function(col, alpha = 1) {
  apply(sapply(col, col2rgb) / 255, 2, function(x) {
    rgb(x[1], x[2], x[3], alpha = alpha)
  })
}

# Shared input contract for the depth entry points.
#
# depth(), depthEuclid(), depthMah(), depthProjection(), depthTukey() and
# depthLP() all opened with the same fourteen lines: X defaults to u, data
# frames become matrices, a bare vector is one column of data or one
# observation. Six copies meant the dimension check that belongs at the end of
# it only ever landed in one of them, and the omission is expensive: the
# Armadillo kernels behind Mahalanobis, Projection and LP subtract u's rows
# from X's inside a `#pragma omp parallel for`, so the exception a mismatch
# throws escapes a parallel region, calls std::terminate(), and takes the whole
# R session down with SIGABRT instead of raising a catchable R error.
#
# X may be missing in the caller - missing() sees that through the call - and
# the error is reported against the caller so users see depthMah(...), not this
# helper.
.coerceDepthInput <- function(u, X) {
  if (missing(X)) {
    X <- u
  }

  if (is.data.frame(u)) {
    u <- as.matrix(u)
  }
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }
  if (is.vector(X)) {
    X <- matrix(X, ncol = 1)
  }
  if (is.vector(u)) {
    u <- matrix(u, ncol = ncol(X))
  }

  if (ncol(u) != ncol(X)) {
    stop(simpleError(
      gettextf("'u' has %d column(s) but 'X' has %d; the dimensions must match",
               ncol(u), ncol(X)),
      call = sys.call(-1)
    ))
  }

  list(u = u, X = X)
}
