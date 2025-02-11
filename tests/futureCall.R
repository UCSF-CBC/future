source("incl/start.R")

options(future.debug = FALSE)
message("*** futureCall() ...")

message("- futureCall() - sequential + lazy ...")

f1 <- future(do.call(rnorm, args = list(n = 100)), seed = NULL, lazy = TRUE)
f2 <- futureCall(rnorm, args = list(n = 100), seed = NULL, lazy = TRUE)

set.seed(42L)
v0 <- rnorm(n = 100)
str(list(v0 = v0))

set.seed(42L)
v1 <- value(f1)
str(list(v1 = v1))

set.seed(42L)
v2 <- value(f2)
str(list(v2 = v2))

## Because we use lazy futures and set the
## random seed just before they are resolved
stopifnot(all.equal(v1, v0))
stopifnot(all.equal(v1, v2))

message("- futureCall() - sequential + lazy ... DONE")

message("- futureCall() - mix of strategies, cores, lazy and globals ...")

a <- 3
args <- list(x = 42, y = 12)
v0 <- do.call(function(x, y) a * (x - y), args = args)

for (cores in 1:availCores) {
  message(sprintf("Testing with %d cores ...", cores))
  options(mc.cores = cores)
  strategies <- supportedStrategies(cores)

  for (strategy in strategies) {
    message(sprintf("- plan('%s') ...", strategy))
    plan(strategy)

    for (lazy in c(FALSE, TRUE)) {
      for (globals in c(FALSE, TRUE)) {
        message("- futureCall() #1")
        a <- 3
        args <- list(x = 42, y = 12)
        f <- futureCall(function(x, y) a * (x - y), args = args,
                        globals = globals, lazy = lazy)
        rm(list = c("a", "args"))
        print(f)
        res1 <- tryCatch({
          v1 <- value(f)
        }, error = identity)
        stopifnot(!inherits(res1, "FutureError"))
        utils::str(list(strategy = strategy, globals = globals, lazy = lazy, v0 = v0, res1 = res1))
        if (isTRUE(as.logical(Sys.getenv("R_CHECK_IDEAL")))) {
          message("R_CHECK_IDEAL=TRUE")
          if (globals) {
            stopifnot(all.equal(v1, v0))
          } else {
            stopifnot(inherits(res1, "error"))
          }
        } else {
          if (!inherits(res1, "error")) {
            stopifnot(all.equal(v1, v0))
          } else {
            stopifnot(!globals)
          }
        }

        message("- futureCall() #2")
        a <- 3
        args <- list(x = 42, y = 12)
        f <- futureCall(function(x, y) a * (x - y), args = args,
                        globals = "a", lazy = lazy)
        rm(list = c("a", "args"))
        print(f)
        res2 <- tryCatch({
          v2 <- value(f)
        }, error = identity)
        stopifnot(!inherits(res2, "FutureError"))
        utils::str(list(strategy = strategy, globals = globals, lazy = lazy, v0 = v0, res2 = res2))
        if (isTRUE(as.logical(Sys.getenv("R_CHECK_IDEAL")))) {
          message("R_CHECK_IDEAL=TRUE")
          stopifnot(all.equal(v2, v0))
        } else {
          if (!inherits(res2, "error")) {
            stopifnot(all.equal(v2, v0))
          } else {
            stopifnot(!globals)
          }
        }
        
        message("- futureCall() #3")
        args <- list(x = 42, y = 12)
        f <- futureCall(function(x, y) a * (x - y), args = args,
                        globals = list(a = 3), lazy = lazy)
        rm(list = "args")
        print(f)
        res3 <- tryCatch({
          v3 <- value(f)
        }, error = identity)
        stopifnot(!inherits(res3, "FutureError"))
        utils::str(list(strategy = strategy, globals = globals, lazy = lazy, v0 = v0, res3 = res3))
        if (isTRUE(as.logical(Sys.getenv("R_CHECK_IDEAL")))) {
          message("R_CHECK_IDEAL=TRUE")
          stopifnot(all.equal(v3, v0))
        } else {
          if (!inherits(res3, "error")) {
            stopifnot(all.equal(v3, v0))
          } else {
            stopifnot(!globals)
          }
        }

        message("- futureCall() #4")
        ## FIXME: This works and fails in non-expected ways /HB 2022-05-26
        local({
          a <- 2
          g <- function() a
          f <- futureCall(g, globals = globals, lazy = lazy)
          rm(list = "a")

          truth <- 2
          message("truth: ", truth)

          v4 <- tryCatch(value(f), error = identity)
          print(v4)

          utils::str(list(strategy = strategy, globals = globals, lazy = lazy, v4 = v4))

          if (isTRUE(as.logical(Sys.getenv("R_CHECK_IDEAL")))) {
            message("R_CHECK_IDEAL=TRUE")
            if (globals) {
              stopifnot(identical(v4, truth))
            } else {
              stopifnot(inherits(v4, "error"))
            }
          } else if (isTRUE(getOption("future.globals.keepWhere", FALSE))) {
            message("future.globals.keepWhere=TRUE")
            if (isTRUE(getOption("future.globals.globalsOf.locals", TRUE))) {
              message("future.globals.globalsOf.locals=TRUE")
              if (globals) {
                stopifnot(identical(v4, truth))
              } else {
                if (lazy) {
                  stopifnot(inherits(v4, "error"))
                } else {
                  stopifnot(identical(v4, truth))
                }
              }
            } else {
              message("future.globals.globalsOf.locals=FALSE")
              if (lazy) {
                stopifnot(inherits(v4, "error"))
              } else {
                stopifnot(identical(v4, truth))
              }
            }
          } else {
            message("future.globals.keepWhere=FALSE")
            if (isTRUE(getOption("future.globals.globalsOf.locals", TRUE))) {
              message("future.globals.globalsOf.locals=TRUE")
              if (globals) {
                stopifnot(identical(v4, truth))
              } else {
                stopifnot(inherits(v4, "error"))
              }
            } else {
              message("future.globals.globalsOf.locals=FALSE")
              stopifnot(inherits(v4, "error"))
            }
          }
        })

        rm(list = c("v1", "v2", "v3", "v4"))
      }
    }
  }
  message(sprintf("Testing with %d cores ... DONE", cores))
}

message("- futureCall() - mix of strategies, cores, lazy and globals ... DONE")

message("- futureCall() - bug fixes")

plan(sequential)
plan(cluster, workers = 1L)

fcn <- function() a
v <- tryCatch(local({
  abc <- 42
  f <- futureCall(fcn, args = list(), globals = "abc")
  value(f)
}), error = identity)

## Bug #262: the above used to return NULL
stopifnot(!is.null(v))

## Bug: Now, it instead fails, because it cannot find 'abc'
print(v)
stopifnot(inherits(v, "error"))

message("*** futureCall() ... DONE")

source("incl/end.R")
