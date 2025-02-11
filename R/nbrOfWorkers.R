#' Get the number of workers available
#'
#' @param evaluator A future evaluator function.
#' If NULL (default), the current evaluator as returned
#' by [plan()] is used.
#'
#' @return
#' `nbrOfWorkers()` returns a positive number in \eqn{{1, 2, 3, ...}}, which
#' for some future backends may also be `+Inf`.
#'
#' @example incl/nbrOfWorkers.R
#'
#' @export
nbrOfWorkers <- function(evaluator = NULL) {
  UseMethod("nbrOfWorkers")
}


#' @export
nbrOfWorkers.cluster <- function(evaluator) {
  assert_no_positional_args_but_first()
  
  expr <- formals(evaluator)$workers
  workers <- eval(expr, enclos = baseenv())
  if (is.function(workers)) workers <- workers()
  if (is.character(workers)) {
    stop_if_not(!anyNA(workers))
    workers <- length(workers)
  } else if (is.numeric(workers)) {
  } else if (inherits(workers, "cluster")) {
    workers <- length(workers)
  } else {
    stopf("Unsupported type of 'workers' for evaluator of class %s: %s", commaq(class(evaluator)), class(workers)[1])
  }
  stop_if_not(length(workers) == 1L, !is.na(workers), workers >= 1L, is.finite(workers))

  workers
}

#' @export
nbrOfWorkers.uniprocess <- function(evaluator) {
  assert_no_positional_args_but_first()
  
  1L
}


#' @export
nbrOfWorkers.multiprocess <- function(evaluator) {
  assert_no_positional_args_but_first()
  
  expr <- formals(evaluator)$workers
  workers <- eval(expr, enclos = baseenv())
  if (is.function(workers)) workers <- workers()
  if (is.numeric(workers)) {
  } else {
    stopf("Unsupported type of 'workers' for evaluator of class %s: %s", commaq(class(evaluator)), class(workers)[1])
  }
  stop_if_not(length(workers) == 1L, !is.na(workers), workers >= 1L, is.finite(workers))

  workers
}

#' @export
nbrOfWorkers.future <- function(evaluator) {
  assert_no_positional_args_but_first()
  
  expr <- formals(evaluator)$workers
  workers <- eval(expr, enclos = baseenv())
  if (is.function(workers)) workers <- workers()
  if (is.numeric(workers)) {
  } else if (is.null(workers)) {
    workers <- Inf
  } else {
    stopf("Unsupported type of 'workers' for evaluator of class %s: %s", commaq(class(evaluator)), class(workers)[1])
  }
  stop_if_not(length(workers) == 1L, !is.na(workers), workers >= 1L)

  workers
}

#' @export
nbrOfWorkers.NULL <- function(evaluator) {
  assert_no_positional_args_but_first()
  
  nbrOfWorkers(plan("next"))
}



#' @param background If TRUE, only workers that can process a future in the
#' background are considered.  If FALSE, also workers running in the main \R
#' process are considered, e.g. when using the 'sequential' backend.
#'
#' @param \dots Not used; reserved for future use.
#'
#' @return
#' `nbrOfFreeWorkers()` returns a non-negative number in
#' \eqn{{0, 1, 2, 3, ...}} which is less than or equal to `nbrOfWorkers()`.
#'
#' @rdname nbrOfWorkers
#' @export
nbrOfFreeWorkers <- function(evaluator = NULL, background = FALSE, ...) {
  UseMethod("nbrOfFreeWorkers")
}


#' @export
nbrOfFreeWorkers.cluster <- function(evaluator, background = FALSE, ...) {
  assert_no_positional_args_but_first()
  
  workers <- nbrOfWorkers(evaluator)
  
  ## Create a dummy, lazy future based on the future strategy ("evaluator")
  f <- evaluator(NULL, lazy = TRUE)

  ## Special case
  if (inherits(f, "SequentialFuture")) {
    return(if (isTRUE(background)) 0L else 1L)
  }
  
  name <- attr(f$workers, "name", exact = TRUE)
  stop_if_not(is.character(name), length(name) == 1L)
  reg <- sprintf("workers-%s", name)
  ## Number of unresolved cluster futures
  usedNodes <- length(FutureRegistry(reg, action = "list", earlySignal = FALSE))
  
  workers <- workers - usedNodes
  stop_if_not(length(workers) == 1L, !is.na(workers), workers >= 0L, is.finite(workers))
  
  workers
}


#' @export
nbrOfFreeWorkers.uniprocess <- function(evaluator, background = FALSE, ...) {
  assert_no_positional_args_but_first()

  if (isTRUE(background)) 0L else 1L
}

#' @export
nbrOfFreeWorkers.multicore <- function(evaluator, background = FALSE, ...) {
  assert_no_positional_args_but_first()
  
  workers <- nbrOfWorkers(evaluator)
  
  workers <- workers - usedCores()
  stop_if_not(length(workers) == 1L, !is.na(workers), workers >= 0L, is.finite(workers))

  workers
}

#' @export
nbrOfFreeWorkers.multiprocess <- function(evaluator, background = FALSE, ...) {
  stopf("nbrOfFreeWorkers() is not implemented for this type of future backend (please contacts the maintainer of that backend): %s", commaq(class(evaluator)))
}

#' @export
nbrOfFreeWorkers.future <- function(evaluator, background = FALSE, ...) {
  assert_no_positional_args_but_first()

  workers <- nbrOfWorkers(evaluator)
  if (is.infinite(workers)) return(workers)

  stopf("nbrOfFreeWorkers() is not implemented for this type of future backend (please contacts the maintainer of that backend): %s", commaq(class(evaluator)))
}


#' @export
nbrOfFreeWorkers.NULL <- function(evaluator, background = FALSE, ...) {
  assert_no_positional_args_but_first()
  
  nbrOfFreeWorkers(plan("next"), background = background, ...)
}


#' @export
nbrOfFreeWorkers.logical <- function(evaluator, background = FALSE, ...) {
  assert_no_positional_args_but_first()
  if (missing(background)) {
    stop("Arguments 'background' of nbrOfFreeWorkers() must be named, if used")
  }
  nbrOfFreeWorkers(NULL, background = force(background), ...)
}
