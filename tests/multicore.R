source("incl/start.R")
library("listenv")
plan(multicore)

message("*** multicore() ...")

for (cores in 1:min(2L, availableCores("multicore"))) {
  message(sprintf("Testing with %d cores ...", cores))
  options(mc.cores = cores)

  if (!supportsMulticore()) {
    message(sprintf("Multicore futures are not supporting on '%s'. Falling back to use synchronous sequential futures", .Platform$OS.type))
  }

  nworkers <- nbrOfWorkers()
  message("Number of workers: ", nworkers)
  stopifnot(nworkers == cores)
  
  for (globals in c(FALSE, TRUE)) {

    message(sprintf("*** multicore(..., globals = %s) without globals", globals))
  
    f <- multicore({
      42L
    }, globals = globals)
    stopifnot(inherits(f, "MulticoreFuture") || ((cores == 1 || !supportsMulticore()) && inherits(f, "SequentialFuture")))
  
    print(resolved(f))
    y <- value(f)
    print(y)
    stopifnot(y == 42L)
  
  
    message(sprintf("*** multicore(..., globals = %s) with globals", globals))
    ## A global variable
    a <- 0
    f <- multicore({
      b <- 3
      c <- 2
      a * b * c
    }, globals = globals)
    print(f)
  
  
    ## A multicore future is evaluated in a separated
    ## forked process.  Changing the value of a global
    ## variable should not affect the result of the
    ## future.
    a <- 7  ## Make sure globals are frozen
    v <- value(f)
    print(v)
    stopifnot(v == 0)
  
  
    message(sprintf("*** multicore(..., globals = %s) with globals and blocking", globals))
    x <- listenv()
    for (ii in 1:4) {
      message(sprintf(" - Creating multicore future #%d ...", ii))
      x[[ii]] <- multicore({ ii }, globals = globals)
    }
    message(sprintf(" - Resolving %d multicore futures", length(x)))
    v <- sapply(x, FUN = value)
    stopifnot(all(v == 1:4))
  
  
    message(sprintf("*** multicore(..., globals = %s) and errors", globals))
    f <- multicore({
      stop("Whoops!")
      1
    }, globals = globals)
    print(f)
    v <- value(f, signal = FALSE)
    print(v)
    stopifnot(inherits(v, "error"))
  
    res <- try(value(f), silent = TRUE)
    print(res)
    stopifnot(inherits(res, "try-error"))
  
    ## Error is repeated
    res <- try(value(f), silent = TRUE)
    print(res)
    stopifnot(inherits(res, "try-error"))

    ## Custom error class
    f <- multicore({
      stop(structure(list(message = "boom"),
                     class = c("MyError", "error", "condition")))
    })
    print(f)
    v <- value(f, signal = FALSE)
    print(v)
    stopifnot(inherits(v, "error"), inherits(v, "MyError"))
  
    ## Make sure error is signaled
    res <- tryCatch(value(f), error = identity)
    stopifnot(inherits(res, "error"))
  
    ## Issue #200: Custom condition class attributes are lost 
    stopifnot(inherits(res, "MyError"))    
  } # for (globals ...)


  message("*** multicore(..., workers = 1L) ...")

  a <- 2
  b <- 3
  yTruth <- a * b

  f <- multicore({ a * b }, globals = TRUE, workers = 1L)
  rm(list = c("a", "b"))

  v <- value(f)
  print(v)
  stopifnot(v == yTruth)

  message("*** multicore(..., workers = 1L) ... DONE")

  message(sprintf("Testing with %d cores ... DONE", cores))
} ## for (cores ...)

message("*** multicore() ... DONE")

source("incl/end.R")
