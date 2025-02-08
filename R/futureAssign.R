#' @param x the name of a future variable, which will hold the value
#'        of the future expression (as a promise).
#'
#' @param assign.env The \link[base]{environment} to which the variable
#' should be assigned.
#'
#' @return
#' `x %<-% value` (a future assignment) and
#' `futureAssign("x", value)` create a [Future] that evaluates
#' expression `expr` and binds its value (as a
#' \link[base:delayedAssign]{promise}) to
#' a variable `x`.  The value of the future is automatically retrieved
#' when the assigned variable (promise) is queried.
#' The future itself is returned invisibly, e.g.
#' `f <- futureAssign("x", expr)` and `f <- (x %<-% expr)`.
#' Alternatively, the future of a future variable `x` can be retrieved
#' without blocking using \code{f <- \link{futureOf}(x)}.
#' Both the future and the variable (promise) are assigned to environment
#' `assign.env` where the name of the future is `.future_<name>`.
#'
#' @rdname future
#' @export
futureAssign <- function(x, value, envir = parent.frame(), substitute = TRUE, lazy = FALSE, seed = FALSE, globals = TRUE, packages = NULL, stdout = TRUE, conditions = "condition", earlySignal = FALSE, label = NULL, gc = FALSE, ..., assign.env = envir) {
  stop_if_not(is.character(x), !is.na(x), nzchar(x))
  if (substitute) value <- substitute(value)
  
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## (1) Arguments passed to future()
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  future.args <- list(value, envir = envir, lazy = lazy, seed = seed, globals = globals, packages = packages, stdout = stdout, conditions = conditions, earlySignal = earlySignal, label = label, gc = gc, ...)
  ## Any arguments set via disposible option?
  args <- getOption("future.disposable", NULL)
  if (!is.null(args)) {
    for (name in names(args)) future.args[name] <- args[name]
    on.exit(options(future.disposable = NULL))
  }


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## (2) Create future
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Name of "future" saved in parallel with the "promise"
  future_name <- sprintf(".future_%s", x)
  if (exists(future_name, envir = assign.env)) {
    msg <- sprintf("A future with name %s already exists in environment %s: %s", sQuote(future_name), sQuote(environmentName(assign.env)), hpaste(ls(envir = assign.env, all.names = TRUE)))
##    warning(msg)
  }

  ## Evaluate expression/value as a "future" and assign its value to
  ## a variable as a "promise".
  ## NOTE: We make sure to pass 'envir' in order for globals to
  ## be located properly.
  future <- do.call(future::future, args = future.args, envir = envir)

  ## Assign future to assignment environment
  future_without_gc <- future
  future_without_gc$.gcenv <- NULL
  assign(future_name, future_without_gc, envir = assign.env)


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## (3) Create promise holding the future's value
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Here value may throw an error causing the assign value to be a
  ## "delayed" error, which will be thrown each time the variable is
  ## retrieved.
  env <- new.env()
  env$job <- future
  delayedAssign(x, local({
    value <- value(future)
    ## Remove internal future variable
    rm(list = future_name, envir = assign.env)
    value
  }), eval.env = env, assign.env = assign.env)

  invisible(future)
}
