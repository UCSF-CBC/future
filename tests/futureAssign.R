source("incl/start.R")

message("*** futureAssign() ...")

message("*** futureAssign() - sequential w/ lazy evaluation ...")

delayedAssign("a", {
  cat("Delayed assignment evaluated\n")
  1
})

futureAssign("b", {
  cat("Future assignment evaluated\n")
  2
}, lazy = TRUE)

## Because "lazy future" is used, the expression/value
## for 'b' will not be resolved at the point.  For other
## types of futures, it may already have been resolved
cat(sprintf("b = %s\n", b))

## The expression/value of 'a' is resolved at this point,
## because a delayed assignment (promise) was used.
cat(sprintf("a = %s\n", a))

stopifnot(identical(a, 1))
stopifnot(identical(b, 2))

message("*** futureAssign() - sequential w/ lazy evaluation ... DONE")


message("*** futureAssign() - lazy = TRUE / FALSE ...")

for (cores in 1:availCores) {
  message(sprintf("Testing with %d cores ...", cores))
  options(mc.cores = cores)

  for (strategy in supportedStrategies(cores)) {
    message(sprintf("*** futureAssign() with %s futures ...", sQuote(strategy)))
    plan(strategy)

    ## Potential task name clashes
    u <- new.env()
    v <- new.env()
    futureAssign("a", { 2 }, assign.env = u)
    futureAssign("a", { 4 }, assign.env = v)
    
    cat(sprintf("u$a = %s\n", u$a))
    cat(sprintf("v$a = %s\n", v$a))
    
    stopifnot(identical(u$a, 2))
    stopifnot(identical(v$a, 4))
    
    
    ## Global variables
    a <- 1
    futureAssign("b", { 2 * a })
    a <- 2
    stopifnot(b == 2)

    ## Explicit lazy evaluation
    for (lazy in c(FALSE, TRUE)) {
      a <- 1
      f <- futureAssign("b", { 2 * a }, lazy = lazy)
      a <- 2
      stopifnot(b == 2)
      stopifnot(f$lazy == lazy || (strategy %in% c("multisession", "multicore") && cores == 1L))
      
      ## Set 'lazy' via disposable option
      options(future.disposable = list(lazy = lazy))
      a <- 1
      f <- futureAssign("b", { 2 * a })
      a <- 2
      stopifnot(b == 2)
      stopifnot(f$lazy == lazy || (strategy %in% c("multisession", "multicore") && cores == 1L))
    }

    message(sprintf("*** futureAssign() with %s futures ... DONE", sQuote(strategy)))
  } # for (strategy in ...)

  message(sprintf("Testing with %d cores ... DONE", cores))
} ## for (cores ...)

message("*** futureAssign() - lazy = TRUE / FALSE ... DONE")

message("*** futureAssign() ... DONE")

source("incl/end.R")
