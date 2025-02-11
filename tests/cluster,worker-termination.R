source("incl/start.R")
library("listenv")
options(future.debug = FALSE)

## IMPORTANT: Since we're killing parallel workers, some of them will not
## get a chance to clean up their R temporary folders.  Here we configuring
## them to use temporary folders with this R process temporary folder.
## This way they'll be removed when this R process terminates
Sys.setenv(TMPDIR = tempdir())

message("*** cluster() - terminating worker ...")

message("Library paths: ", commaq(.libPaths()))
message("Package path: ", sQuote(system.file(package = "future")))
message("TMPDIR for parallel workers: ", sQuote(Sys.getenv("TMPDIR")))

types <- "PSOCK"

if (supportsMulticore()) types <- c(types, "FORK")

pid <- Sys.getpid()
message("Main PID (original): ", pid)
cl <- NULL
for (type in types) {
  message(sprintf("Cluster type %s ...", sQuote(type)))
  
  cl <- parallel::makeCluster(1L, type = type, timeout = 60)
  print(cl)

  ## Crashing FORK:ed processes seems too harsh on R (< 3.3.0)
  if (type != "FORK" || getRversion() >= "3.3.0") {
    message("*** cluster() - crashed worker ...")
    
    plan(cluster, workers = cl, .skip = FALSE)
    x %<-% 42L
    stopifnot(x == 42L)
    
    ## Force R worker to terminate
    ## It's not safe to use quit() here when using type = "FORK" [1]
    ## [1] https://stat.ethz.ch/pipermail/r-devel/2021-August/080995.html
    x %<-% tools::pskill(pid = Sys.getpid())
    res <- tryCatch(y <- x, error = identity)
    print(res)
    stopifnot(
      inherits(res, "error"),
      inherits(res, "FutureError")
    )
  
    ## Cleanup
    print(cl)
    ## FIXME: Why doesn't this work here? It causes the below future to stall.
    # parallel::stopCluster(cl)
  
    ## Verify that the reset worked
    cl <- parallel::makeCluster(1L, type = type, timeout = 60)
    print(cl)
    plan(cluster, workers = cl, .skip = FALSE)
    x %<-% 43L
    stopifnot(x == 43L)
    
    message("*** cluster() - crashed worker ... DONE")
  } ## if (type != "FORK" || getRversion() >= "3.3.0")

  ## Sanity checks
  pid2 <- Sys.getpid()
  message("Main PID (original): ", pid)
  message("Main PID: ", pid2)
  stopifnot(pid2 == pid)
  
  ## Cleanup
  print(cl)
  str(cl)
  parallel::stopCluster(cl)

  ## Sanity checks
  pid2 <- Sys.getpid()
  message("Main PID (original): ", pid)
  message("Main PID: ", pid2)
  stopifnot(pid2 == pid)

  message(sprintf("Cluster type %s ... DONE", sQuote(type)))
} ## for (type ...)

message("*** cluster() - terminating worker ... DONE")

## Sanity checks
pid2 <- Sys.getpid()
message("Main PID (original): ", pid)
message("Main PID: ", pid2)
stopifnot(pid2 == pid)

source("incl/end.R")
