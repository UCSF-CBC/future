source("incl/start.R")
library("listenv")
oopts <- c(oopts, options(
  future.globals.resolve = TRUE,
  future.globals.onMissing = "error"
))

message("*** Tricky use cases related to globals ...")

for (cores in 1:availCores) {
  message(sprintf("Testing with %d cores ...", cores))
  options(mc.cores = cores)

  message("availableCores(): ", availableCores())

  message("- Local variables with the same name as globals ...")

  for (strategy in supportedStrategies(cores)) {
    message(sprintf("- plan('%s') ...", strategy))
    plan(strategy)

    methods <- c("conservative", "ordered")
    for (method in methods) {
      options(future.globals.method = method)
      message(sprintf("Method for identifying globals: '%s' ...", method))

      a <- 3

      yTruth <- local({
        b <- a
        a <- 2
        a * b
      })

      y %<-% {
        b <- a
        a <- 2
        a * b
      }

      rm(list = "a")

      res <- try(y, silent = TRUE)
      if (method == "conservative" && strategy %in% c("multisession", "cluster")) {
        str(list(res = res))
        stopifnot(inherits(res, "try-error"))
      } else {
        message(sprintf("y = %g", y))
        stopifnot(identical(y, yTruth))
      }


      ## Same with forced lazy evaluation
      a <- 3

      y %<-% {
        b <- a
        a <- 2
        a * b
      } %lazy% TRUE

      rm(list = "a")

      res <- try(y, silent = TRUE)
      if (method == "conservative") {
        str(list(res = res))
        stopifnot(inherits(res, "try-error"))
      } else {
        message(sprintf("y = %g", y))
        stopifnot(identical(y, yTruth))
      }


      res <- listenv()
      a <- 1
      for (ii in 1:3) {
        res[[ii]] %<-% {
          b <- a * ii
          a <- 0
          b
        }
      }
      rm(list = "a")

      res <- try(unlist(res), silent = TRUE)
      if (method == "conservative" && strategy %in% c("multisession", "cluster")) {
        str(list(res = res))
        stopifnot(inherits(res, "try-error"))
      } else {
        print(res)
        stopifnot(all(res == 1:3))
      }


      ## Same with forced lazy evaluation
      res <- listenv()
      a <- 1
      for (ii in 1:3) {
        res[[ii]] %<-% {
          b <- a * ii
          a <- 0
          b
        } %lazy% TRUE
      }
      rm(list = "a")

      res <- try(unlist(res), silent = TRUE)
      if (method == "conservative") {
        str(list(res = res))
        stopifnot(inherits(res, "try-error"))
      } else {
        print(res)
        stopifnot(all(res == 1:3))
      }


      ## Assert that `a` is resolved and turned into a constant future
      ## at the moment when future `b` is created.
      ## Requires options(future.globals.resolve = TRUE).
      a <- future(1)
      b <- future(value(a) + 1)
      rm(list = "a")
      message(sprintf("value(b) = %g", value(b)))
      stopifnot(value(b) == 2)

      a <- future(1)
      b <- future(value(a) + 1, lazy = TRUE)
      rm(list = "a")
      message(sprintf("value(b) = %g", value(b)))
      stopifnot(value(b) == 2)

      a <- future(1, lazy = TRUE)
      b <- future(value(a) + 1)
      rm(list = "a")
      message(sprintf("value(b) = %g", value(b)))
      stopifnot(value(b) == 2)

      a <- future(1, lazy = TRUE)
      b <- future(value(a) + 1, lazy = TRUE)
      rm(list = "a")
      message(sprintf("value(b) = %g", value(b)))
      stopifnot(value(b) == 2)


      ## BUG FIX: In future (<= 1.0.0) a global 'pkg' would be
      ## overwritten by the name of the last package attached
      ## by the future.
      pkg <- "foo"
      f <- sequential({ pkg })
      v <- value(f)
      message(sprintf("value(f) = %s", sQuote(v)))
      stopifnot(pkg == "foo", v == "foo")
      
      message(sprintf("Method for identifying globals: '%s' ... DONE", method))
    }

    ## BUG FIX: In globals (<= 0.10.3) a global 'x' in LHS of an assignment
    ## would be missed.
    options(future.globals.method = "ordered")

    ## A local
    x <- 1
    f <- future({ x <- 0; x <- x + 1; x })
    v <- value(f)
    message(sprintf("value(f) = %s", sQuote(v)))
    stopifnot(v == 1)
    
    ## A global
    x <- 1
    f <- future({ x <- x + 1; x })
    v <- value(f)
    message(sprintf("value(f) = %s", sQuote(v)))
    stopifnot(v == 2)

    ## A global
    x <- function() TRUE
    f <- future({ x <- x(); x })
    v <- value(f)
    message(sprintf("value(f) = %s", sQuote(v)))
    stopifnot(v == TRUE)
  } ## for (strategy ...)

  message(sprintf("Testing with %d cores ... DONE", cores))
} ## for (cores ...)

message("*** Tricky use cases related to globals ... DONE")

source("incl/end.R")
