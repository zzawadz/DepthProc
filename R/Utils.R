.depTheme <- function() {
  return(theme(axis.title.x = element_text(face = "bold", vjust = 0, size = 16),
               axis.title.y = element_text(face = "bold", angle = 90,
                                           vjust = 0.2, size = 16),
               axis.text.x = element_text(size = 14),
               axis.text.y = element_text(size = 14),
               title = element_text(face = "bold", vjust = 1, size = 18)))
}

.testNorm <- function(d = 2) {
  mvrnorm(100, rep(1, d), diag(d))
}

.addAlpha <- function(col, alpha = 1) {
  apply(sapply(col, col2rgb) / 255, 2, function(x) {
    rgb(x[1], x[2], x[3], alpha = alpha)
  })
}
