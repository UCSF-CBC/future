import_from <- function(name, mode = "function", default = NULL, package) {
  ns <- getNamespace(package)
  if (exists(name, mode = mode, envir = ns, inherits = FALSE)) {
    get(name, mode = mode, envir = ns, inherits = FALSE)
  } else if (!is.null(default)) {
    default
  } else {
    stopf("No such '%s' %s: %s()", package, mode, name)
  }
}

import_parallelly <- function(...) {
  import_from(..., package = "parallelly")
}


## We are currently importing the following non-exported functions:
## * cluster futures:
##   - parallel:::defaultCluster()  ## non-critical / not really needed /
##                                  ## can be dropped in R (>= 3.5.0)
##   - parallel:::sendCall()        ## run()
##   - parallel:::recvResult()      ## value()
## * multicore futures:
##   - parallel:::selectChildren()  ## resolved()
##   - parallel:::rmChild()         ## value()
## As well as the following ones (because they are not exported on Windows):
## * multicore futures:
##   - parallel::mcparallel()       ## run()
##   - parallel::mccollect()        ## value()
importParallel <- import_parallelly("importParallel")
