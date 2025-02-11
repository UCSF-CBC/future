source("incl/start.R")
library("listenv")

for (cores in 1:availCores) {
  message(sprintf("Testing with %d cores ...", cores))
  options(mc.cores = cores)

  message("*** Global argument '...' ...")

  sum_fcns <- list()

  sum_fcns$A <- function(x, ...) {
    message("Arguments '...' exists: ", exists("...", inherits = TRUE))
    y %<-% { sum(x, ...) }
    y
  }


  sum_fcns$B <- function(x, ...) {
    sumt <- function(x) {
      message("Arguments '...' exists: ", exists("...", inherits = TRUE))
      y %<-% { sum(x, ...) }
      y
    }
    sumt(x)
  }

  sum_fcns$C <- function(x, y) {
    message("Arguments '...' exists: ", exists("...", inherits = TRUE))
    y %<-% { sum(x, y) }
    y
  }

  sum_fcns$D <- function(x, y) {
    message("Arguments '...' exists: ", exists("...", inherits = TRUE))
    y %<-% { sum(x, y, ...) }
    y
  }

  ## Issue/PR #400: Emulate how '...' may be used by the 'rlang' package
  sum_fcns$E <- function(...) {
    message("Arguments '...' exists: ", exists("...", inherits = TRUE))

    ## Grab '...' into a Globals object
    globals <- globals::globalsByName("...", envir=environment())

    ## Evaluate an expression with '...' in an environment that does not
    ## have an '...' object - hence the parent.frame().  This will produce
    ## an error unless we pass 'globals' which contains '...'
    f <- future({
      fcn <- function() sum(...)
      fcn()
    }, envir = parent.frame(), globals = globals)
    y <- value(f)
    y
  }

  ## Issue: https://github.com/futureverse/globals/issues/72
  sum_fcns$F <- function(x, y) {
    message("Using '...' in a formula")

    fcn <- function(x, y) {
      z = ~ list(...)
      sum(x, y)
    }
    
    f <- future(fcn(x, y))
    y <- value(f)
    y
  }


  for (strategy in supportedStrategies(cores)) {
    message(sprintf("- plan('%s') ...", strategy))
    plan(strategy, substitute = FALSE)

    for (name in names(sum_fcns)) {
      message(sprintf("** Sum function '%s' with plan('%s') ...", name, strategy))
      sum_fcn <- sum_fcns[[name]]
      print(sum_fcn)
      y <- try(sum_fcn(1:2, 3))
      print(y)
      if (name %in% c("D")) {
        stopifnot(inherits(y, "try-error"))
      } else {
        stopifnot(y == 6)
      }
    }
  }

  message(sprintf("Testing with %d cores ... DONE", cores))
} ## for (cores ...)

message("*** Global argument '...' ... DONE")

source("incl/end.R")
