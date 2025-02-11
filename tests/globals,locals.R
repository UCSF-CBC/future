source("incl/start.R")
library("listenv")
oopts <- c(oopts, options(
  future.debug = FALSE
))

message("*** Globals inside local() environments ...")

for (strategy in supportedStrategies()) {
  message(sprintf("- plan('%s') ...", strategy))
  plan(strategy)

  message("- Missing global variable")
  g <- function() a
  h <- function() {
    a <- 1
    g()
  }
  res <- tryCatch({
    f <- future(h())
    v <- value(f)
  }, error = identity)
  stopifnot(inherits(res, "error"))

  message("- Missing global variable (v2)")
  g <- function() a
  h <- function(g) {
    a <- 1
    g()
  }
  res <- tryCatch({
    f <- future(h(g))
    v <- value(f)
  }, error = identity)
  stopifnot(inherits(res, "error"))

  message("- Missing global variable (v3)")
  g <- function() a
  res <- tryCatch({
    f <- future(local({
      a <- 1
      g()
    }))
    v <- value(f)
  }, error = identity)
  stopifnot(inherits(res, "error"))

  message("- Non-missing global variable")
  a <- 2
  g <- function() a
  f <- future(local({
    a <- 1
    g()
  }), lazy = TRUE)
  rm(list = "a")

  if (isTRUE(as.logical(Sys.getenv("R_CHECK_IDEAL")))) {
    v <- value(f)
    stopifnot(v == 2)
  } else {
    v <- value(f)
    stopifnot(v == 2)
  }


  message("- Non-missing global variable (inside local())")
  ## FIXME: This works and fails in non-expected ways /HB 2022-05-26
  local({
    a <- 2
    g <- function() a
    f <- future(local({
      a <- 1
      g()
    }), lazy = TRUE)
    rm(list = "a")

    truth <- 2
    message("truth: ", truth)
    
    res <- tryCatch({ v <- value(f) }, error = identity)
    print(res)
    stopifnot(identical(v, truth))
  })


  message("- Name clashing of globals across local() environments")

  ## Closures with local globals of the same name
  g <- local({ a <- 2; function() a })
  h <- local({ a <- 1; function() a })
  truth <- g() + h()
  message("truth: ", truth)

  ## FIXME: This works and fails in non-expected ways /HB 2022-05-26
  ## With:
  ##
  ##   f <- future(g() + h())
  ##
  ## there is a risk that global 'a' of g() overwrites global 'a' of h(),
  ## resulting in h() == 2, rather than h() == 1. Vice versa, with:
  ##
  ##   f <- future(h() + g())
  ##
  ## 'a' of h() might overwride 'a' of g() so that g() == 1.
  ## This has to be fixed.
  ##
  ## See also https://github.com/futureverse/future/issues/608
  
  f <- future(g() + h())
  v <- tryCatch(value(f), error = identity)

  utils::str(list(strategy = strategy, v = v))
  
  if (isTRUE(as.logical(Sys.getenv("R_CHECK_IDEAL")))) {
    stopifnot(identical(v, truth))
  } else if (isTRUE(getOption("future.globals.keepWhere", FALSE))) {
    message("future.globals.keepWhere=TRUE")
    stopifnot(identical(v, truth))
  } else {
    message("future.globals.keepWhere=FALSE")
    if (isTRUE(getOption("future.globals.globalsOf.locals", TRUE))) {
      message("future.globals.globalsOf.locals=TRUE")
      stopifnot(identical(v, 4))    ## <= SERIOUS BUG!
    } else {
      message("future.globals.globalsOf.locals=FALSE")
      stopifnot(inherits(v, "error"))
    }
  }
} ## for (strategy ...)

message("*** Globals inside local() environments ... DONE")

source("incl/end.R")
