source("incl/start.R")
options(future.debug = FALSE)

message("*** cluster() ...")

message("Library paths: ", commaq(.libPaths()))
message("Package path: ", sQuote(system.file(package = "future")))

types <- "PSOCK"

if (supportsMulticore()) types <- c(types, "FORK")

setupClusterWithoutPkgs <- function(type = "PSOCK", withouts = c("future")) {
  message("setupClusterWithoutPkgs() ...")
  cl <- parallel::makeCluster(1L, type = type, timeout = 60)
  print(cl)
  
  ## Emulate a worker that does not have 'future' installed.
  ## by setting a different user library path on the worker.
  libs <- parallel::clusterEvalQ(cl, .libPaths(tempdir()))[[1]]
  print(libs)

  ## Check whether 'future' is still available on the worker or not.
  ## It could be that it is installed in the system library path, which
  ## in case we cannot "hide" the future package from the worker.
  has_pkgs <- parallel::clusterCall(cl, fun = sapply, X = withouts,
                                    FUN = requireNamespace)[[1]]
  print(has_pkgs)

  attr(cl, "libs") <- libs
  attr(cl, "has_pkgs") <- has_pkgs

  message("setupClusterWithoutPkgs() ... done")

  cl
}

cl <- NULL
for (type in types) {
  message(sprintf("Test set #1 with cluster type %s ...", sQuote(type)))

  cl <- setupClusterWithoutPkgs(type)  
  if (!all(attr(cl, "has_pkgs"))) {
    ## If worker does not have 'future' installed, then there will be an
    ## error already when plan() is called, because it will attempt to
    ## launch a quick future to validate that everything works.
        
    res <- tryCatch({
      plan(cluster, workers = cl)
    }, error = identity)
    print(res)
    stopifnot(inherits(res, "FutureError"))
  }
  parallel::stopCluster(cl)

  cl <- setupClusterWithoutPkgs(type)  
  if (!all(attr(cl, "has_pkgs"))) {
    ## However, if we disable the quick future test, we should not get
    ## an error here.
    plan(cluster, workers = cl, .skip = FALSE, .init = FALSE)

    ## But we will get:
    ##   <FutureError: Internal error: Unexpected result retrieved for
    ##   ClusterFuture future ('<none>'): '42L'>
    ## if we try to resolve a future:
    f <- future(42L)
    res <- tryCatch({
      result(f)
    }, error = identity)
    print(res)
    stopifnot(inherits(res, "FutureError"))

    ## Assert that the "result" was recorded for future use and
    ## it was recorded as a FutureError.
    str(f$result)
    stopifnot(!is.null(f$result),
              inherits(f$result, "FutureError"))

    ## Any attempts to re-request the result / value for this future
    ## should from now on re-throw that recorded FutureError.
    str(f$result)
    stopifnot(!is.null(f$result))
    
    ## The main rationale for this is that we don't want result() to
    ## wait for ever from retrying to retrieve already retrieved results
    ## from worker.
    res <- tryCatch({
       result(f)
    }, error = identity)
    stopifnot(inherits(res, "FutureError"))

    ## ... and obviously the same for value().
    res <- tryCatch({
       value(f)
    }, error = identity)
    stopifnot(inherits(res, "FutureError"))
    
    ## Retrying with a new future should give the same failure as above.
    f <- future(42L)
    res <- tryCatch({
      result(f)
    }, error = identity)
    print(res)
    stopifnot(inherits(res, "FutureError"))
  }
  
  parallel::stopCluster(cl)
  cl <- NULL
  
  plan(sequential)
  
  message(sprintf("Test set #1 with cluster type %s ... DONE", sQuote(type)))
} ## for (type ...)

message("*** cluster() ... DONE")

source("incl/end.R")
