source("incl/start.R")
options(future.debug = FALSE)
message("*** cluster() ...")

message("Library paths: ", commaq(.libPaths()))
message("Package path: ", sQuote(system.file(package = "future")))

types <- "PSOCK"

if (supportsMulticore()) types <- c(types, "FORK")

## WORKAROUND: covr::package_coverage() -> merge_coverage() -> ... produces
## "Error in readRDS(x) : error reading from connection" for type = "FORK".
## Is this related to mcparallel() comments in help("package_coverage")?
## /HB 2017-05-20
if (covr_testing) types <- setdiff(types, "FORK")

## WORKAROUND: FORK:ed processing gives really odd type="FORK" results on
## macOS when running on GitHub Actions. /HB 2020-06-07
## This error is also appearing on CRANs' 'r-release-macos-arm64' and
## 'M1mac' checks. /HB 2021-08-11
if (on_macos) types <- setdiff(types, "FORK")

pid <- Sys.getpid()
message("Main PID (original): ", pid)
cl <- NULL
for (type in types) {
  message(sprintf("Test set #1 with cluster type %s ...", sQuote(type)))

  message("Main PID (original): ", pid)

  for (cores in 1:availCores) {
    message(sprintf("Testing with %d cores on type = %s ...",
                    cores, sQuote(type)))
    options(mc.cores = cores)
  
    ## Set up a cluster with <cores> nodes (explicitly)
    cl <- parallel::makeCluster(cores, type = type, timeout = 60)
    print(cl)
    
    plan(cluster, workers = cl)

    ## Assert that the worker's global environment is "empty"
    f <- future(ls(envir=globalenv(), all.names=TRUE))
    v <- value(f)
    v <- grep("^[.][.][.]future[.]", v, invert = TRUE, value = TRUE)
    if (length(v) > 0) {
      stop(sprintf("Stray variables in the global environment of %s: %s",
           class(f)[1], commaq(v)))
    }

    ## No global variables
    f <- try(cluster({
      42L
    }, workers = cl), silent = FALSE)
    print(f)
    stopifnot(inherits(f, "ClusterFuture"))
  
    print(resolved(f))
    y <- value(f)
    print(y)
    stopifnot(y == 42L)

    ## No global variables
    f <- try(cluster({
      42L
    }, workers = cl), silent = FALSE)
    print(f)
    stopifnot(inherits(f, "ClusterFuture"))
  
    print(resolved(f))
    y <- value(f)
    print(y)
    stopifnot(y == 42L)
  
    ## A global variable
    a <- 0
    f <- try(future({
      b <- 3
      c <- 2
      a * b * c
    }))
    print(f)
  
  
    ## A cluster future is evaluated in a separate
    ## R session process.  Changing the value of a global
    ## variable should not affect the result of the
    ## future.
    a <- 7  ## Make sure globals are frozen
    v <- value(f)
    print(v)
    stopifnot(v == 0)

    if (!"covr" %in% loadedNamespaces()) {
      message("*** cluster() with globals and blocking")
      fs <- list()
      for (ii in 1:3) {
        message(sprintf(" - Creating cluster future #%d ...", ii))
        fs[[ii]] <- future({ ii }, label = sprintf("future-%d", ii))
      }
      message(sprintf(" - Resolving %d cluster futures", length(fs)))
      v <- sapply(fs, FUN = value)
      stopifnot(all(v == 1:3))
    }
  
  
    message("*** cluster() and errors")
    f <- future({
      stop("Whoops!")
      1
    })
    print(f)
    v <- value(f, signal = FALSE)
    print(v)
    stopifnot(inherits(v, "error"))
  
    res <- tryCatch(value(f), error = identity)
    print(res)
    stopifnot(inherits(res, "error"))
  
    ## Error is repeated
    res <- tryCatch(value(f), error = identity)
    print(res)
    stopifnot(inherits(res, "error"))
  
    ## Custom error class
    f <- future({
      stop(structure(list(message = "boom"),
                     class = c("MyError", "error", "condition")))
    })
    print(f)
    v <- value(f, signal = FALSE)
    print(v)
    stopifnot(inherits(v, "error"), inherits(v, "MyError"))
  
    ## Make sure error is signaled
    res <- tryCatch(value(f), error = identity)
    stopifnot(inherits(res, "error"), inherits(res, "MyError"))    
  
    message("*** cluster() - installed libraries ...")
    f <- try(cluster({
      list(
        libPaths = .libPaths()
      )
    }, workers = cl), silent = FALSE)
    print(f)
    stopifnot(inherits(f, "ClusterFuture"))
    v <- value(f)
    message(paste(capture.output(str(v)), collapse = "\n"))
    message("*** cluster() - installed packages ... DONE")
  
  
    message("*** cluster() - assert covr workaround ...")
    f <- try(cluster({
      future:::hpaste(1:100)
    }, workers = cl), silent = FALSE)
    print(f)
    stopifnot(inherits(f, "ClusterFuture"))
    v <- value(f)
    message(v)
    stopifnot(v == hpaste(1:100))
    message("*** cluster() - assert covr workaround ... DONE")

    ## Assert that the worker's global environment is "empty"
    f <- future(ls(envir=globalenv(), all.names=TRUE))
    v <- value(f)
    v <- grep("^[.][.][.]future[.]", v, invert = TRUE, value = TRUE)
    if (length(v) > 0) {
      stop(sprintf("Stray variables in the global environment of %s: %s",
           class(f)[1], commaq(v)))
    }

    ## Sanity checks
    pid2 <- Sys.getpid()
    message("Main PID (original): ", pid)
    message("Main PID: ", pid2)
    stopifnot(pid2 == pid)

    ## Cleanup
    parallel::stopCluster(cl)
  
    message(sprintf("Testing with %d cores on type = %s ... DONE",
                    cores, sQuote(type)))
  } ## for (cores ...)

  message("*** cluster() - exceptions ...")
  
  res <- tryCatch(cluster(42L, workers = NA), error = identity)
  print(res)
  stopifnot(inherits(res, "error"))
  
  message("*** cluster() - exceptions ... DONE")

  message("*** cluster() - assert registry behavior ...")
  
  ## Explicitly created clusters are *not* added to the registry
  cl <- parallel::makeCluster(1L, type = type, timeout = 60)
  plan(cluster, workers = cl)
  clR <- ClusterRegistry("get")
  stopifnot(is.null(clR))
  
  ## ... and therefore changing plans shouldn't change anything
  plan(sequential)
  clR <- ClusterRegistry("get")
  stopifnot(is.null(clR))

  ## Cleanup
  print(cl)
  str(cl)
  parallel::stopCluster(cl)
  
  message("*** cluster() - assert registry behavior ... DONE")

  ## Sanity checks
  pid2 <- Sys.getpid()
  message("Main PID (original): ", pid)
  message("Main PID: ", pid2)
  stopifnot(pid2 == pid)

  ## Cleanup
  plan(sequential)

  cl1 <- parallel::makeCluster(1L, type = type, timeout = 60)
  plan(cluster, workers = cl1)
  f1 <- future(1)

  ## Cleanup
  print(cl1)
  str(cl1)
  parallel::stopCluster(cl1)

  message(sprintf("Test set #1 with cluster type %s ... DONE", sQuote(type)))
} ## for (type ...)

library("parallel")
for (type in types) {
  message(sprintf("Test set #2 with cluster type %s ...", sQuote(type)))

  message("*** cluster() - setDefaultCluster() ...")
  
  cl <- makeCluster(1L, type = type, timeout = 60)
  print(cl)
  
  setDefaultCluster(cl)
  ## FIXME: Make plan(cluster, workers = NULL) work such that
  ## setDefaultCluster() is actually tested.
  plan(cluster)
  
  pid <- Sys.getpid()
  message(pid)
  
  a %<-% Sys.getpid()
  message(a)
  
  setDefaultCluster(NULL)

  ## Cleanup
  print(cl)
  str(cl)
  parallel::stopCluster(cl)
  
  message("*** cluster() - setDefaultCluster() ... DONE")

  ## Sanity checks
  pid2 <- Sys.getpid()
  message("Main PID (original): ", pid)
  message("Main PID: ", pid2)
  stopifnot(pid2 == pid)

  ## Cleanup
  plan(sequential)

  message(sprintf("Test set #2 with cluster type %s ... DONE", sQuote(type)))
} ## for (type ...)

## Sanity checks
pid2 <- Sys.getpid()
message("Main PID (original): ", pid)
message("Main PID: ", pid2)
stopifnot(pid2 == pid)

message("*** cluster() - exception when re-creating workers ...")
## https://github.com/futureverse/future/issues/261

plan(cluster, workers = "localhost")
f <- future(1)
plan(cluster, workers = "localhost", .skip = FALSE)
res <- tryCatch({
  value(f)
}, FutureError = function(ex) {
  message(conditionMessage(ex))
  ex
})
stopifnot(inherits(res, "FutureError"))

message("*** cluster() - exception when re-creating workers ... DONE")


message("*** cluster() ... DONE")

## Sanity checks
pid2 <- Sys.getpid()
message("Main PID (original): ", pid)
message("Main PID: ", pid2)
stopifnot(pid2 == pid)

source("incl/end.R")
