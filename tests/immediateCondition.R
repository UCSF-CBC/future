source("incl/start.R")
options(future.debug = FALSE)

f_try <- function(f, ...) {
  res <- tryCatch(..., error = identity)
  if (inherits(res, "error")) {
    label <- sQuote(f$label)
    if (length(label) == 0) label <- "<none>"
    msg <- sprintf("Test with future (%s) of class %s failed assertion: %s", label, sQuote(class(f)[1]), conditionMessage(res))
    stop(msg, call.=FALSE)
  }
  res
}

message("*** immediateCondition:s ...")

immediateMessage <- function(..., domain = NULL, appendLF = TRUE) {
   msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
   call <- sys.call()
   m <- simpleMessage(msg, call)
   class(m) <- c(class(m), "immediateCondition")
   message(m)
   invisible(m)
}

immediateWarning <- function(..., domain = NULL) {
   msg <- .makeMessage(..., domain = domain)
   call <- sys.call()
   w <- simpleWarning(msg, call)
   class(w) <- c(class(w), "immediateCondition")
   warning(w)
   invisible(w)
}

recordConditions <- function(expr, ...) {
  conditions <- list()
  withCallingHandlers(expr, condition = function(c) {
    attr(c, "received") <- Sys.time()
    conditions[[length(conditions) + 1L]] <<- c
  })
  conditions
}

recordMessages <- function(expr, ...) {
  sapply(recordConditions(expr, ...), FUN = conditionMessage)
}

## FIXME: Make sure to set also with cores = 2L /HB 2020-11-10

if (isTRUE(getOption("future.psock.relay.immediate"))) {
  excl <- "cluster"
  if (getRversion() < "3.4.0") excl <- c(excl, "multisession")
  strategies <- supportedStrategies(cores = 2L, excl = excl)
  strategies <- c(strategies, "sequential")
} else {
  strategies <- "sequential"
}
print(strategies)

for (ss in seq_along(strategies)) {
  strategy <- strategies[[ss]]
  message(sprintf("Immediate relay w/ %s ...", names(strategies)[ss]))
  plan(strategy)

  message("* A single future ...")

  message("- creating future")
  msgs <- recordMessages({
    f <- future({
      immediateMessage("IM1")
      message("M")
      immediateWarning("IW")
      immediateMessage("IM2")
      ## RACE CONDITION IN ASSERTION:
      ## Add some leeway for the immediateCondition files written by
      ## multicore futures to "settle" on the file system
      Sys.sleep(1.0)
      42L
    }, label = "single-future")
  })
  message("  class: ", commaq(class(f)))
  message(sprintf("  msgs [n=%d]: %s", length(msgs), commaq(msgs)))
  f_try(f, {
    if (inherits(f, "UniprocessFuture")) {
      stopifnot(identical(msgs, c("IM1\n", "IW", "IM2\n")))
    } else {
      stopifnot(length(msgs) == 0L)
    }
  })
  
  message("- checking if resolved")
  msgs <- recordMessages({
    r <- resolved(f)
  })
  message("  result: ", r)
  message(sprintf("  msgs [n=%d]: %s", length(msgs), commaq(msgs)))
  f_try(f, {
    if (inherits(f, "MultiprocessFuture")) {
      stopifnot(identical(msgs, c("IM1\n", "IW", "IM2\n")))
    } else {
      stopifnot(length(msgs) == 0L)
    }
  })

  message("- resolve w/out collecting results")
  msgs <- recordMessages({
    f <- resolve(f)
  })
  message(sprintf("  msgs [n=%d]: %s", length(msgs), commaq(msgs)))
  f_try(f, {
    stopifnot(length(msgs) == 0L)
  })
  
  message("- resolve w/ collect results")
  msgs <- recordMessages({
    f <- resolve(f, result = TRUE)
  })
  message(sprintf("  msgs [n=%d]: %s", length(msgs), commaq(msgs)))
  f_try(f, {
    if (inherits(f, c("UniprocessFuture", "MultiprocessFuture"))) {
      stopifnot(length(msgs) == 0L)
    } else {
      stopifnot(identical(msgs, c("IM1\n", "IW", "IM2\n")))
    }
  })
  
  message("- getting value")
  msgs <- recordMessages({
    v <- value(f)
  })
  message("  value: ", v)
  message(sprintf("  msgs [n=%d]: %s", length(msgs), commaq(msgs)))
  f_try(f, {
    if (inherits(f, c("UniprocessFuture", "MultiprocessFuture"))) {
      stopifnot(identical(msgs, "M\n"))
    } else {
      stopifnot(identical(msgs, c("IM1\n", "IW", "IM2\n", "M\n")))
    }
  })
  
  message("- getting value again")
  msgs <- recordMessages({
    v <- value(f)
  })
  message("  value: ", v)
  f_try(f, {
    stopifnot(identical(msgs, "M\n"))
  })

  message("* A single future ... DONE")


  message("* Two futures ... ")

  message("- list of two futures")
  fs <- list()
  msgs <- recordMessages({
    fs[[1]] <- future({
      immediateMessage("IM1")
      Sys.sleep(0.1)
      message("M1")
      immediateWarning("IW1")
      ## RACE CONDITION IN ASSERTION:
      ## Add some leeway for the immediateCondition files written by
      ## multicore futures to "settle" on the file system
      Sys.sleep(1.0)
      1L
    }, label = "future-1")
    fs[[2]] <- future({
      immediateMessage("IM2")
      Sys.sleep(0.1)
      message("M2")
      immediateWarning("IW2")
      ## RACE CONDITION IN ASSERTION:
      ## Add some leeway for the immediateCondition files written by
      ## multicore futures to "settle" on the file system
      Sys.sleep(1.0)
      2L
    }, label = "future-2")
  })
  message("  class: ", commaq(class(fs[[1]])))
  message(sprintf("  msgs [n=%d]: %s", length(msgs), commaq(msgs)))
  f_try(fs[[1]], {
    if (inherits(fs[[1]], "UniprocessFuture")) {
      stopifnot(identical(msgs, c("IM1\n", "IW1", "IM2\n", "IW2")))
    } else {
      stopifnot(length(msgs) == 0L)
    }
  })

  message("- check if resolved")
  msgs <- recordMessages({
    rs <- resolved(fs)
  })
  message("  result: ", paste(rs, collapse = ", "))
  message(sprintf("  msgs [n=%d]: %s", length(msgs), commaq(msgs)))
  f_try(f, {
    if (inherits(f, "MultiprocessFuture")) {
      stopifnot(all(msgs %in% c("IM1\n", "IM2\n", "IW1", "IW2")))
      if (inherits(f, "MulticoreFuture")) {
        stopifnot(identical(msgs, c("IM1\n", "IM2\n", "IW1", "IW2")))
      } else if (inherits(f, "MultisessionFuture")) {
        stopifnot(identical(msgs, c("IM1\n", "IW1", "IM2\n", "IW2")))
      }
    } else {
      stopifnot(length(msgs) == 0L)
    }
  })

  message("- resolve w/out collecting results")
  msgs <- recordMessages({
    fs <- resolve(fs)
  })
  message(sprintf("  msgs [n=%d]: %s", length(msgs), commaq(msgs)))
  f_try(f, {
    stopifnot(length(msgs) == 0L)
  })

  message("- resolve w/ collect results")
  msgs <- recordMessages({
    fs <- resolve(fs, result = TRUE)
  })
  message(sprintf("  msgs [n=%d]: %s", length(msgs), commaq(msgs)))
  f_try(f, {
    if (inherits(fs[[1]], c("UniprocessFuture", "MultiprocessFuture", "CallrFuture", "BatchtoolsFuture"))) {
      stopifnot(length(msgs) == 0L)
    } else {
      stopifnot(identical(msgs, c("IM1\n", "IW1", "IM2\n", "IW2")))
    }
  })

  message("- getting value")
  msgs <- recordMessages({
    vs <- value(fs)
  })
  message("  values: ", paste(vs, collapse = ", "))
  message(sprintf("  msgs [n=%d]: %s", length(msgs), commaq(msgs)))
  f_try(f, {
    stopifnot(identical(msgs, c("M1\n", "M2\n")))
  })
  
  message("- getting value again")
  msgs <- recordMessages({
    vs <- value(fs)
  })
  message("  values: ", paste(vs, collapse = ", "))
  message(sprintf("  msgs [n=%d]: %s", length(msgs), commaq(msgs)))
  f_try(f, {
    stopifnot(identical(msgs, c("M1\n", "M2\n")))
  })

  message("- getting values one by one")
  msgs <- recordMessages({
    vs <- c(
      value(fs[[1]]),
      value(fs[[2]])
    )
  })
  message("  values: ", paste(vs, collapse = ", "))
  f_try(f, {
    stopifnot(identical(msgs, c("M1\n", "M2\n")))
  })

  message("* Two futures ... DONE")

  message(sprintf("Immediate relay w/ %s ... done", names(strategies)[ss]))
} ## for (ss ...)

message("*** immediateCondition:s ... DONE")

source("incl/end.R")
