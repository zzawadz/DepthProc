# The method a depth computation uses, and the arguments that go with it, are
# passed around this package as a free-form list() -- depth_params, or
# depth_params1/depth_params2, or dep1d_params -- merged with the computed u and
# X by c() and handed to do.call(depth, ...). Nine files do that. Nothing
# validated the list at any layer, so the method name was a string buried in an
# untyped bag: a typo in it, or an argument name that no depth function has,
# reached do.call() unremarked and either picked the wrong method or was
# silently dropped.
#
# depthSpec() is the constructor for that list, and .depthParams() is the
# check every entry point runs on whatever it was handed. Plain lists still
# work -- they are the documented interface and are all over user code -- they
# are just validated now.

# The methods depth() dispatches on. depth()'s own error message is built from
# this, so the two cannot disagree about what is valid.
.depthMethodNames <- c("Mahalanobis", "Euclidean", "Projection", "Tukey",
                       "LP", "Local", "MBD", "FM")

# The arguments a method accepts, read off the implementing function rather
# than listed by hand, so this cannot drift from the functions it describes.
# MBD and FM return NULL: they go through fncDepth(), which forwards `...` on
# to fncDepthFM(), so the reachable set is not a single formals() list.
.depthMethodFormals <- function(method) {
  fun <- switch(
    method,
    Mahalanobis = depthMah,
    Euclidean = depthEuclid,
    Projection = depthProjection,
    Tukey = depthTukey,
    LP = depthLP,
    Local = depthLocal,
    NULL
  )

  if (is.null(fun)) {
    return(NULL)
  }

  setdiff(names(formals(fun)), c("u", "X"))
}

.checkDepthMethodName <- function(method, arg) {
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop(gettextf(
      "the 'method' element of %s must be a single character value, not %s",
      arg, sQuote(class(method)[1L])
    ))
  }
  if (!(method %in% .depthMethodNames)) {
    stop(gettextf(
      "unknown depth method %s in %s; must be one of %s",
      sQuote(method), arg,
      paste(sQuote(.depthMethodNames), collapse = ", ")
    ))
  }

  method
}

# Validates a depth parameter list. `arg` names the argument in the caller so
# the message says which of depth_params, depth_params1, ... was wrong.
.validateDepthParams <- function(params, arg, strict = FALSE) {
  if (length(params) == 0L) {
    return(invisible(TRUE))
  }

  nms <- names(params)

  if (is.null(nms) || any(!nzchar(nms))) {
    stop(gettextf(
      paste("every element of %s must be named; element %d is not. A bare",
            "string is the commonest case -- write %s rather than %s"),
      arg, which(is.null(nms) | !nzchar(nms))[1L],
      sQuote('list(method = "Tukey")'), sQuote('list("Tukey")')
    ))
  }
  if (anyDuplicated(nms)) {
    stop(gettextf("%s names %s more than once",
                  arg, sQuote(nms[anyDuplicated(nms)])))
  }

  # u and X are supplied by whichever function is doing the computing, and are
  # merged with this list by c(); a copy here becomes a duplicate argument.
  reserved <- intersect(c("u", "X"), nms)
  if (length(reserved) > 0L) {
    stop(gettextf(
      paste("%s must not contain %s: the data is supplied by the function",
            "being called, not by the parameter list"),
      arg, paste(sQuote(reserved), collapse = " or ")
    ))
  }

  method <- "Projection"
  if ("method" %in% nms) {
    method <- .checkDepthMethodName(params$method, arg)
  }

  if (strict) {
    allowed <- .depthMethodFormals(method)

    if (!is.null(allowed)) {
      unknown <- setdiff(setdiff(nms, "method"), allowed)

      if (length(unknown) > 0L) {
        stop(gettextf(
          "%s is not an argument of %s depth; it takes %s",
          paste(sQuote(unknown), collapse = ", "), sQuote(method),
          if (length(allowed) == 0L) {
            "no further arguments"
          } else {
            paste(sQuote(allowed), collapse = ", ")
          }
        ))
      }
    }
  }

  invisible(TRUE)
}

# The one place a depth_params argument is turned into something safe to splice
# into a do.call(). Accepts a depthSpec, a plain list, or NULL, and always
# returns a plain list.
.depthParams <- function(depth_params, arg = "depth_params") {
  if (is.null(depth_params)) {
    return(list())
  }
  if (!is.list(depth_params)) {
    stop(gettextf(
      paste("%s must be a list of arguments for depth(), or a depthSpec(),",
            "not %s; did you mean %s?"),
      arg, sQuote(class(depth_params)[1L]),
      if (is.character(depth_params) && length(depth_params) == 1L) {
        sQuote(sprintf('list(method = "%s")', depth_params))
      } else {
        sQuote("list(method = ...)")
      }
    ))
  }

  .validateDepthParams(depth_params, arg, strict = inherits(depth_params,
                                                            "depthSpec"))

  unclass(depth_params)
}

#' @title Arguments for a depth method
#'
#' @description
#'
#' Builds the list of arguments that the higher-level functions of this package
#' --- \code{\link{scaleCurve}}, \code{\link{asymmetryCurve}},
#' \code{\link{depthContour}}, \code{\link{depthPersp}},
#' \code{\link{depthMedian}}, \code{\link{ddPlot}} and the rest --- forward to
#' \code{\link{depth}} as their \code{depth_params} argument.
#'
#' @param method character, the depth method. One of "Projection" (the
#'   default), "Mahalanobis", "Euclidean", "Tukey", "LP" or "Local" for
#'   multivariate data, or "MBD" or "FM" for functional data.
#' @param ... further arguments for that method, for example \code{ndir} for
#'   "Projection", or \code{exact} for "Tukey".
#'
#' @details
#'
#' A plain \code{list(method = "Tukey", exact = TRUE)} is still accepted
#' everywhere and means the same thing. What \code{depthSpec()} adds is that
#' the method name and the argument names are checked immediately, against the
#' formal arguments of the function that implements the method, rather than
#' travelling through several layers as an unexamined list --- a misspelled
#' method used to select a different depth or a different renderer, and a
#' misspelled argument used to be dropped without a word.
#'
#' The checks on argument names are not applied to "MBD" and "FM", whose
#' arguments are forwarded through \code{\link{fncDepth}} to
#' \code{\link{fncDepthFM}} and so are not a single list of formals.
#'
#' @return A list of the given arguments, with class \code{depthSpec}.
#'
#' @seealso \code{\link{depth}}
#'
#' @examples
#' x <- MASS::mvrnorm(100, c(0, 0), diag(2))
#'
#' DepthProc::scaleCurve(x, depth_params = DepthProc::depthSpec("Mahalanobis"))
#'
#' # a misspelled argument is reported instead of being dropped
#' try(DepthProc::depthSpec("Projection", ndirs = 100))
#'
#' # as is a misspelled method
#' try(DepthProc::depthSpec("tukey"))
#'
#' @export
#'
depthSpec <- function(method = "Projection", ...) {
  spec <- c(list(method = method), list(...))

  .validateDepthParams(spec, "depthSpec()", strict = TRUE)

  structure(spec, class = c("depthSpec", "list"))
}

#' @export
print.depthSpec <- function(x, ...) {
  cat("Depth specification:", x$method, "\n")

  rest <- x[setdiff(names(x), "method")]
  if (length(rest) > 0L) {
    utils::str(unclass(rest), no.list = TRUE, give.attr = FALSE)
  }

  invisible(x)
}
