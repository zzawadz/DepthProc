.depTheme <- function() {
  return(theme(axis.title.x = element_text(face = "bold", vjust = 0, size = 16),
               axis.title.y = element_text(face = "bold", angle = 90,
                                           vjust = 0.2, size = 16),
               axis.text.x = element_text(size = 14),
               axis.text.y = element_text(size = 14),
               title = element_text(face = "bold", vjust = 1, size = 18)))
}

.extractDepthParams <- function(u, X, ...) {
  tmp <- list(...)
  params <- c("method", "ndir", "name", "la", "lb", "pdim", "depth1", "depth2",
              "beta", "threads", "mean", "cov", "exact")
  def_param <- list(method = "Projection", ndir = 1000, name = "X", la = 1,
                    lb = 1, pdim = 2, depth1 = "Projection",
                    depth2 = "Projection", beta = 0.5, threads = -1,
                    mean = "NULL", cov = "NULL", exact = TRUE)

  fastIfElse <- function(name, tmp, def) {
    x <- ifelse(is.null(tmp[[name]]), def[[name]], tmp[[name]])

    if (name %in% c("mean", "cov")) {
      return(tmp[[name]])
    }

    return(x)
  }

  tmp <- sapply(params, fastIfElse, tmp, def_param, simplify = FALSE)
  tmp <- c(list(u = u, X = X), tmp)

  return(tmp)
}

.removeDepthParams <- function(...) {
  tmp <- list(...)
  params <- c("method", "ndir", "name", "la", "lb", "pdim", "depth1", "depth2",
              "beta", "threads", "mean", "mean", "cov", "exact")
  names <- names(tmp)
  tmp <- sapply(names, function(x) {
    ifelse(x %in% params, NA, tmp[x])
  })
  tmp[!sapply(tmp, is.na)]
}

.testNorm <- function(d = 2) {
  mvrnorm(100, rep(1, d), diag(d))
}

.addAlpha <- function(col, alpha = 1) {
  apply(sapply(col, col2rgb) / 255, 2, function(x) {
    rgb(x[1], x[2], x[3], alpha = alpha)
  })
}
