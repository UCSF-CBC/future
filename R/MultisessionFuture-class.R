#' @inheritParams ClusterFuture-class
#' @inheritParams Future-class
#'
#' @return
#' `MultisessionFuture()` returns an object of class `MultisessionFuture`,
#' which inherits from `ClusterFuture`.
#' 
#' @section Usage:
#' To use 'multisession' futures, use `plan(multisession, ...)`, cf. [multisession].
#'
#' @export
#' @rdname ClusterFuture-class
MultisessionFuture <- function(expr = NULL, substitute = TRUE, envir = parent.frame(), workers = NULL, ...) {
  if (substitute) expr <- substitute(expr)

  future <- ClusterFuture(expr = expr, substitute = FALSE, envir = envir, workers = workers, ...)
  future <- structure(future, class = c("MultisessionFuture", class(future)))
  future
}
