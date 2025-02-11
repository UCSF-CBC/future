source("incl/start.R")
library("listenv")

message("*** multisession() ...")

for (cores in 1:availCores) {
  message(sprintf("Testing with %d cores ...", cores))
  options(mc.cores = cores)

  ## No global variables
  f <- multisession({
    42L
  })
  print(f)
  stopifnot(inherits(f, "ClusterFuture") || (inherits(f, "SequentialFuture") && f$lazy))

  print(resolved(f))
  y <- value(f)
  print(y)
  stopifnot(y == 42L)


  ## A global variable
  a <- 0
  f <- multisession({
    b <- 3
    c <- 2
    a * b * c
  }, globals = TRUE)
  print(f)


  ## A multisession future is evaluated in a separate
  ## R session process.  Changing the value of a global
  ## variable should not affect the result of the
  ## future.
  a <- 7  ## Make sure globals are frozen
  v <- value(f)
  print(v)
  stopifnot(v == 0)


  message("*** multisession() with globals and blocking")
  x <- listenv()
  for (ii in 2:1) {
    message(sprintf(" - Creating multisession future #%d ...", ii))
    x[[ii]] <- multisession({ ii }, globals = TRUE)
  }
  message(sprintf(" - Resolving %d multisession futures", length(x)))
  v <- sapply(x, FUN = value)
  stopifnot(all(v == 1:2))


  message("*** multisession() - workers inherit .libPaths()")

  libs <- value(future(.libPaths()))
  str(list(
    main = .libPaths(),
    workers = libs
  ))
  stopifnot(identical(libs, .libPaths()))

  message("*** multisession() and errors")
  f <- multisession({
    stop("Whoops!")
    1
  })
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
  f <- multisession({
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
  ## https://github.com/HenrikBengtsson/Wishlist-for-R/issues/57
  ## stopifnot(inherits(res, "MyError"))    

  ## Make sure to stop these cluster processes to avoid triggering
  ## checking for detritus in the temp directory ... NOTE
  ClusterRegistry(action = "stop")

  message(sprintf("Testing with %d cores ... DONE", cores))
} ## for (cores ...)


message("*** multisession() - too large globals ... DONE")

message("*** multisession(..., workers = 1L) ...")

a <- 2
b <- 3
yTruth <- a * b

f <- multisession({ a * b }, globals = TRUE, workers = 1L)
rm(list = c("a", "b"))

v <- value(f)
print(v)
stopifnot(v == yTruth)

message("*** multisession(..., workers = 1L) ... DONE")


message("*** multisession(..., gc = TRUE) ...")
plan(multisession, workers = 2L)

f <- future({ gc() })
v <- value(f)
print(v)

f <- future({ integer(10e6) })
v <- value(f)
str(v)

f <- future({ gc() })
v <- value(f)
print(v)

f <- future({ integer(10e6) }, gc = TRUE)
v <- value(f)
str(v)

f <- future({ gc() })
v <- value(f)
print(v)

message("*** multisession(..., gc = TRUE) ... TRUE")


message("*** multisession(...) - stopping with plan() change ...")
  
plan(multisession, workers = 2L)
f <- future(1L)

## Collect value, to speep up the stopping of the parallel workers,
## and to make sure we're not leaving any stray processes behind.
v <- value(f)

cl <- ClusterRegistry("get")
stopifnot(inherits(cl, "cluster"), length(cl) >= 1L)

plan(sequential)
cl <- ClusterRegistry("get")
stopifnot(is.null(cl), length(cl) == 0L)
  
message("*** multisession(...) - stopping with plan() change ... DONE")

message("*** multisession() ... DONE")

source("incl/end.R")
