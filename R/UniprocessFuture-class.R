#' An uniprocess future is a future whose value will be resolved synchronously in the current process
#'
#' @inheritParams Future-class
#' 
#' @param \dots Additional named elements passed to [Future()].
#'
#' @return
#' `UniprocessFuture()` returns an object of class `UniprocessFuture`.
#'
#' @export
#' @name UniprocessFuture-class
#' @keywords internal
UniprocessFuture <- function(expr = NULL, substitute = TRUE, envir = parent.frame(), ...) {
  if (substitute) expr <- substitute(expr)

  future <- Future(expr = expr, substitute = FALSE, envir = envir, asynchronous = FALSE, ...)
  future <- structure(future, class = c("UniprocessFuture", class(future)))
  future
}


#' @export
run.UniprocessFuture <- function(future, ...) {
  debug <- getOption("future.debug", FALSE)
  
  if (future$state != 'created') {
    label <- future$label
    if (is.null(label)) label <- "<none>"
    stop(FutureError(sprintf("A future ('%s') can only be launched once.", label), future = future))
  }

  ## Assert that the process that created the future is
  ## also the one that evaluates/resolves/queries it.
  assertOwner(future)

  expr <- getExpression(future)
  envir <- future$envir

  ## Run future
  future$state <- 'running'
  future$result <- eval(expr, envir = envir, enclos = baseenv())
  future$state <- 'finished'

  if (debug) mdebugf("%s started (and completed)", class(future)[1])

  ## Always signal immediateCondition:s and as soon as possible.
  ## They will always be signaled if they exist.
  signalImmediateConditions(future)

  ## Signal conditions early, iff specified for the given future
  signalEarly(future, collect = FALSE)

  invisible(future)
}


#' @export
result.UniprocessFuture <- function(future, ...) {
  ## Has the result already been collected?
  result <- future$result
  if (!is.null(result)) {
    if (inherits(result, "FutureError")) stop(result)
    return(result)
  }
  
  if (future$state == "created") {
    ## Make sure that run() does not signal errors
    earlySignal <- future$earlySignal
    future$earlySignal <- FALSE
    run(future)
    future$earlySignal <- earlySignal
  }

  result <- future$result
  if (inherits(result, "FutureResult")) return(result)

  ex <- UnexpectedFutureResultError(future)
  future$result <- ex
  stop(ex)
}


#' @export
resolved.UniprocessFuture <- function(x, ...) {
  if (x$lazy) {
    ## resolved() for lazy uniprocess futures must force result()
    ## such that the future gets resolved.  The reason for this
    ## is so that polling is always possible, e.g.
    ## while(!resolved(f)) Sys.sleep(5);
    result(x)
  }
  NextMethod()
}

#' @export
getExpression.UniprocessFuture <- function(future, immediateConditions = TRUE, ...) {
  ## Assert that no arguments but the first is passed by position
  assert_no_positional_args_but_first()
  NextMethod(immediateConditions = immediateConditions)
}



#' @return
#' `SequentialFuture()` returns an object of class `SequentialProcess`,
#' which inherits from `UniprocessFuture`.
#'
#' @section Usage:
#' To use 'sequential' futures, use `plan(sequential)`, cf. [sequential].
#'
#' @rdname UniprocessFuture-class
#' @export
SequentialFuture <- function(expr = NULL, envir = parent.frame(), substitute = TRUE, lazy = FALSE, globals = TRUE, ...) {
  if (substitute) expr <- substitute(expr)
  f <- UniprocessFuture(expr = expr, envir = envir, substitute = FALSE, lazy = lazy, globals = globals, ...)
  structure(f, class = c("SequentialFuture", class(f)))
}
