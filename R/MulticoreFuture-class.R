#' A multicore future is a future whose value will be resolved asynchronously in a parallel process
#'
#' @inheritParams MultiprocessFuture-class
#' @inheritParams Future-class
#'
#' @return
#' `MulticoreFuture()` returns an object of class `MulticoreFuture`.
#'
#' @section Usage:
#' To use 'multicore' futures, use `plan(multicore, ...)`, cf. [multicore].
#'
#' @export
#' @name MulticoreFuture-class
#' @keywords internal
MulticoreFuture <- function(expr = NULL, substitute = TRUE, envir = parent.frame(), ...) {
  if (substitute) expr <- substitute(expr)

  future <- MultiprocessFuture(expr = expr, substitute = FALSE, envir = envir, ...)

  future <- as_MulticoreFuture(future, ...)

  future
}


as_MulticoreFuture <- function(future, ...) {
  future$job <- NULL
  
  future <- structure(future, class = c("MulticoreFuture", class(future)))
  
  future
}


#' @export
run.MulticoreFuture <- function(future, ...) {
  debug <- getOption("future.debug", FALSE)
  
  if (future$state != "created") {
    label <- future$label
    if (is.null(label)) label <- "<none>"
    stop(FutureError(sprintf("A future ('%s') can only be launched once.", label), future = future))
  }
  
  ## Assert that the process that created the future is
  ## also the one that evaluates/resolves/queries it.
  assertOwner(future)

  mcparallel <- importParallel("mcparallel")

  expr <- getExpression(future)
  envir <- future$envir

  t_start <- Sys.time()
  
  ## Assign globals
  envir <- new.env(parent = envir)
  if (length(future$globals) > 0L) {
    envir <- assign_globals(envir, globals = future$globals)
  }

  ## Get a free worker
  reg <- sprintf("multicore-%s", session_uuid())
  requestCore(
    await = function() FutureRegistry(reg, action = "collect-first", earlySignal = TRUE),
    workers = future$workers
  )

  if (inherits(future$.journal, "FutureJournal")) {
    appendToFutureJournal(future,
         event = "getWorker",
      category = "other",
        parent = "launch",
         start = t_start,
          stop = Sys.time()
    )
  }

  ## Add to registry
  FutureRegistry(reg, action = "add", future = future, earlySignal = TRUE)

  future.args <- list(expr)
  job <- do.call(parallel::mcparallel, args = future.args, envir = envir)

  future$job <- job
  future$state <- "running"

  if (debug) mdebugf("%s started", class(future)[1])
  
  invisible(future)
}

#' @export
resolved.MulticoreFuture <- function(x, run = TRUE, timeout = NULL, ...) {
  ## A lazy future not even launched?
  if (x$state == "created") {
    if (run) {
      ## If free cores are available, then launch this lazy future
      if (x$workers > usedCores()) x <- run(x)
    }
    return(FALSE)
  }

  ## Is value already collected?
  if (!is.null(x$result)) {
    ## Signal conditions early?
    signalEarly(x, ...)
    return(TRUE)
  }

  ## Assert that the process that created the future is
  ## also the one that evaluates/resolves/queries it.
  assertOwner(x)

  job <- x$job
  stop_if_not(inherits(job, "parallelJob"))

  selectChildren <- importParallel("selectChildren")
  
  if (is.null(timeout)) {
    timeout <- getOption("future.multicore.resolved.timeout", NULL)
    if (is.null(timeout)) timeout <- getOption("future.resolved.timeout", 0.01)
    if (timeout < 0) {
      warning("Secret option 'future.resolved.timeout' is negative, which causes resolved() to wait until the future is resolved. This feature is only used for testing purposes of the future framework and must not be used elsewhere", immediate. = TRUE)
      timeout <- NULL
    }
  }

  ## NOTE: We cannot use mcollect(job, wait = FALSE, timeout),
  ## because that will return NULL if there's a timeout, which is
  ## an ambigous value because the future expression may return NULL.
  ## WORKAROUND: Adopted from parallel::mccollect().
  ## FIXME: Can we use result() instead? /HB 2018-07-16
  pid <- selectChildren(children = job, timeout = timeout)
  res <- (is.integer(pid) || is.null(pid))

  ## Collect and relay immediateCondition if they exists
  conditions <- readImmediateConditions(signal = TRUE)
  ## Record conditions as signaled
  signaled <- c(x$.signaledConditions, conditions)
  x$.signaledConditions <- signaled

  ## Signal conditions early? (happens only iff requested)
  if (res) signalEarly(x, ...)

  res
}


#' @export
result.MulticoreFuture <- function(future, ...) {
  debug <- getOption("future.debug", FALSE)
  if (debug) {
    mdebugf("result() for %s ...", class(future)[1])
    on.exit(mdebugf("result() for %s ... done", class(future)[1]))
  }

  ## Has the result already been collected?
  result <- future$result
  if (!is.null(result)) {
    if (inherits(result, "FutureError")) stop(result)
    return(result)
  }

  if (future$state == "created") {
    future <- run(future)
  }

  ## Assert that the process that created the future is
  ## also the one that evaluates/resolves/queries it.
  assertOwner(future)

  ## If not, wait for process to finish, and
  ## then collect and record the value
  mccollect <- importParallel("mccollect")
  job <- future$job
  stop_if_not(inherits(job, "parallelJob"))

  ## WORKAROUNDS for R (< 3.6.0):
  ##  1. Pass single job as list, cf.
  ##     https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=17413
  jobs <- if (getRversion() >= "3.6.0") job else list(job)
  
  result <- mccollect(jobs = jobs, wait = TRUE)[[1L]]
  
  ## NOTE: In Issue #218 it was suggested that parallel:::rmChild() could
  ## fix this, but there seems to be more to this story, because we still
  ## get some of those warning even after removing children here.
  rmChild <- importParallel("rmChild")
  rmChild(child = job)

  ## Sanity checks
  if (!inherits(result, "FutureResult")) {
    if (debug) mdebugf("Detected non-FutureResult result ...")
    alive <- NA
    pid <- job$pid
    if (is.numeric(pid)) {
      pid_exists <- import_parallelly("pid_exists")
      alive <- pid_exists(pid)
    }
    
    ## AD HOC: Record whether the forked process is alive or not
    job$alive <- alive
    future$job <- job

    ## SPECIAL: Check for fallback 'fatal error in wrapper code'
    ## try-error from parallel:::mcparallel().  If detected, then
    ## turn into an error with a more informative error message, cf.
    ## https://github.com/futureverse/future/issues/35
    if (is.null(result) || identical(result, structure("fatal error in wrapper code", class = "try-error"))) {
      label <- future$label
      if (is.null(label)) label <- "<none>"

      pid_info <- if (is.numeric(pid)) sprintf("PID %.0f", pid) else NULL
      info <- pid_info
      msg <- sprintf("Failed to retrieve the result of %s (%s) from the forked worker (on localhost; %s)", class(future)[1], label, info)

      if (identical(result, structure("fatal error in wrapper code", class = "try-error"))) {
        msg <- c(msg, sprintf("Error %s was reported by the 'parallel' package, which could be because the forked R process that evaluates the future was terminated before it was completed", sQuote(result)))
      }

      ## POST-MORTEM ANALYSIS:
      postmortem <- list()
      
      ## (a) Did the localhost worker process terminate?
      if (is.numeric(pid)) {
        if (is.na(alive)) {
          msg2 <- "Failed to determined whether a process with this PID exists or not, i.e. cannot infer whether the forked localhost worker is alive or not"
        } else if (alive) {
          msg2 <- "A process with this PID exists, which suggests that the forked localhost worker is still alive"
        } else {
          msg2 <- "No process exists with this PID, i.e. the forked localhost worker is no longer alive"
        }
        postmortem$alive <- msg2
      }

      ## (c) Any non-exportable globals?
      postmortem$non_exportable <- assert_no_references(future, action = "string")

      ## (d) Size of globals
      postmortem$global_sizes <- summarize_size_of_globals(globals(future))

      postmortem <- unlist(postmortem, use.names = FALSE)
      if (!is.null(postmortem)) {
         postmortem <- sprintf("Post-mortem diagnostic: %s",
                               paste(postmortem, collapse = ". "))
         msg <- c(msg, postmortem)
      }
      msg <- paste(msg, collapse = ". ")
      
      ex <- FutureError(msg, future = future)

    } else {
      ex <- UnexpectedFutureResultError(future)
      alive <- NA  ## For now, don't remove future when there's an unexpected error /HB 2023-04-19
    }
    future$result <- ex
    
    ## Remove future from FutureRegistry?
    if (!is.na(alive) && !alive) {
      reg <- sprintf("multicore-%s", session_uuid())
      exists <- FutureRegistry(reg, action = "contains", future = future)
      if (exists) {
        if (debug) mdebugf("Removing %s from FutureRegistry (%s)", class(future)[1], reg)
        FutureRegistry(reg, action = "remove", future = future, earlySignal = TRUE)
      }
    }

    if (debug) mdebugf("Detected non-FutureResult result ... done")

    stop(ex)
  }

  ## Collect and relay immediateCondition if they exists
  conditions <- readImmediateConditions()
  ## Record conditions as signaled
  signaled <- c(future$.signaledConditions, conditions)
  future$.signaledConditions <- signaled

  ## Record conditions
  result$conditions <- c(result$conditions, signaled)
  signaled <- NULL
  
  future$result <- result

  future$state <- "finished"

  ## Remove from registry
  reg <- sprintf("multicore-%s", session_uuid())
  FutureRegistry(reg, action = "remove", future = future, earlySignal = TRUE)
  
  ## Always signal immediateCondition:s and as soon as possible.
  ## They will always be signaled if they exist.
  signalImmediateConditions(future)

  result
}


#' @export
getExpression.MulticoreFuture <- local({
  function(future, expr = future$expr, mc.cores = 1L, immediateConditions = TRUE, conditionClasses = future$conditions, resignalImmediateConditions = getOption("future.multicore.relay.immediate", immediateConditions), ...) {
    ## Assert that no arguments but the first is passed by position
    assert_no_positional_args_but_first()
  
    debug <- getOption("future.debug", FALSE)
  
    ## Disable multi-threading in futures?
    threads <- NA_integer_
    multithreading <- getOption("future.fork.multithreading.enable", TRUE)  
    if (isFALSE(multithreading)) {
      if (supports_omp_threads(assert = TRUE, debug = debug)) {
        threads <- 1L
        if (debug) mdebug("- Updated expression to force single-threaded (OpenMP and RcppParallel) processing")
      } else {
        warning(FutureWarning("It is not possible to disable OpenMP multi-threading on this systems", future = future))
      }
    }
  
    ## Inject code for resignaling immediateCondition:s?
    if (resignalImmediateConditions && immediateConditions) {
      ## Preserve condition classes to be ignored
      exclude <- attr(conditionClasses, "exclude", exact = TRUE)
    
      immediateConditionClasses <- getOption("future.relay.immediate", "immediateCondition")
      conditionClasses <- unique(c(conditionClasses, immediateConditionClasses))
  
      if (length(conditionClasses) > 0L) {
        ## Communicate via the file system
        expr <- bquote_apply(tmpl_expr_send_immediateConditions_via_file)
      } ## if (length(conditionClasses) > 0)
      
      ## Set condition classes to be ignored in case changed
      attr(conditionClasses, "exclude") <- exclude
    } ## if (resignalImmediateConditions && immediateConditions)
  
    NextMethod(expr = expr, mc.cores = mc.cores, immediateConditions = immediateConditions, conditionClasses = conditionClasses, threads = threads)
  }
})
