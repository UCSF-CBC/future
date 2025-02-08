#' @param FUN A \link[base]{function} to be evaluated.
#'
#' @param args A \link[base]{list} of arguments passed to function `FUN`.
#'
#' @return
#' `f <- futureCall(FUN, args)` creates a [Future] `f` that calls function `FUN` with arguments `args`, where the value of the future is retrieved using `x <- value(f)`.
#'
#' @example incl/futureCall.R
#'
#' @details
#' The `futureCall()` function works analogously to
#' \code{\link[base]{do.call}()}, which calls a function with a set of
#' arguments.  The difference is that `do.call()` returns the value of
#' the call whereas `futureCall()` returns a future.
#'
#' @rdname future
#' @export
futureCall <- function(FUN, args = list(), envir = parent.frame(), lazy = FALSE, seed = FALSE, globals = TRUE, packages = NULL, stdout = TRUE, conditions = "condition", earlySignal = FALSE, label = NULL, gc = FALSE, ...) {
  stop_if_not(is.function(FUN))
  stop_if_not(is.list(args))

  debug <- getOption("future.debug", FALSE)  
  if (debug) mdebug("futureCall() ...")
  
  ## NOTE TO SELF: We'd ideally have an 'envir' argument also for
  ## futureCall(), cf. future().  However, it's not yet clear to me how
  ## to do this, because we need to have globalsOf() to search for globals
  ## from the current environment in order to identify the globals of 
  ## arguments 'FUN' and 'args', cf. future.apply::future_lapply().
  ## /HB 2018-03-06
  globalEnv <- environment()
#  envir <- new.env(parent = envir)

  expr <- quote(do.call(what = FUN, args = args))
  
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 1. Global variables
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.logical(globals)) {
    ## Gather all globals?
    if (globals) {
      if (debug) mdebug("Finding globals ...")

#      expr <- do.call(call, args = c(list("FUN"), list(...)))
      gp <- getGlobalsAndPackages(expr, envir = globalEnv, tweak = tweakExpression, globals = TRUE)
      globals <- gp$globals
      packages <- unique(c(packages, gp$packages))
      gp <- NULL
      
      if (debug) {
        mdebugf(" - globals found: [%d] %s", length(globals), hpaste(sQuote(names(globals))))
        mdebugf(" - needed namespaces: [%d] %s", length(packages), hpaste(sQuote(packages)))
        mdebug("Finding globals ... DONE")
      }
    } else {
      ## globals = FALSE
      globals <- c("FUN", "args")
      globals <- globalsByName(globals, envir = globalEnv, mustExist = FALSE)
    }
  } else if (is.character(globals)) {
    globals <- unique(c(globals, "FUN", "args"))
    globals <- globalsByName(globals, envir = globalEnv, mustExist = FALSE)
  } else if (is.list(globals)) {
    names <- names(globals)
    if (length(globals) > 0 && is.null(names)) {
      stop(FutureError("Invalid argument 'globals'. All globals must be named."))
    }
  } else {
    stop(FutureError("Invalid argument 'globals': ", mode(globals)))
  }
  globals <- as.FutureGlobals(globals)
  stop_if_not(inherits(globals, "FutureGlobals"))

  ## Make sure to clean out globals not found
  globals <- cleanup(globals, drop = "missing")

  names <- names(globals)
  if (!is.element("FUN", names)) globals$FUN <- FUN
  if (!is.element("args", names)) globals$args <- args

  names <- setdiff(names(globals), c("FUN", "args"))
  if (length(names) > 0) {
    env <- environment(globals$FUN)
    env <- new.env(parent = env)
    environment(globals$FUN) <- env
    for (name in names) {
      env[[name]] <- globals[[name]]
    }
  }
  
  if (debug) {
    mdebug("Globals to be used in the future:")
    mstr(globals)
  }

  f <- future(expr, substitute = FALSE, envir = envir, lazy = lazy, seed = seed, globals = globals, packages = packages, stdout = stdout, conditions = conditions, earlySignal = earlySignal, label = label, gc = gc, ...)

  if (debug) mdebug("futureCall() ... DONE")
  
  f
}
