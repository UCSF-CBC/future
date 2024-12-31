#' A future represents a value that will be available at some point in the future
#'
#' A _future_ is an abstraction for a _value_ that may
#' available at some point in the future.  A future can either be
#' `unresolved` or `resolved`, a state which can be checked
#' with [resolved()].  As long as it is _unresolved_, the
#' value is not available.  As soon as it is _resolved_, the value
#' is available via \code{\link[future]{value}()}.
#'
#' @param expr An \R \link[base]{expression}.
#'
#' @param envir The [environment] from where global objects should be
#' identified.
#'
#' @param substitute If TRUE, argument `expr` is
#' \code{\link[base]{substitute}()}:ed, otherwise not.
#'
#' @param stdout If TRUE (default), then the standard output is captured,
#' and re-outputted when `value()` is called.
#' If FALSE, any output is silenced (by sinking it to the null device as
#' it is outputted).
#' Using `stdout = structure(TRUE, drop = TRUE)` causes the captured
#' standard output to be dropped from the future object as soon as it has
#' been relayed. This can help decrease the overall memory consumed by
#' captured output across futures.
#' Using `stdout = NA` (not recommended) avoids intercepting the standard
#' output; behavior of such unhandled standard output depends on the future
#  backend and the environment from which R runs.
#' 
#' @param conditions A character string of conditions classes to be captured
#' and relayed.  The default is to relay all conditions, including messages
#' and warnings.  To drop all conditions, use `conditions = character(0)`.
#' Errors are always relayed.
#' Attribute `exclude` can be used to ignore specific classes, e.g.
#' `conditions = structure("condition", exclude = "message")` will capture
#' all `condition` classes except those that inherits from the `message` class.
#' Using `conditions = structure(..., drop = TRUE)` causes any captured
#' conditions to be dropped from the future object as soon as it has
#' been relayed, e.g. by `value(f)`. This can help decrease the overall
#' memory consumed by captured conditions across futures.
#' Using `conditions = NULL` (not recommended) avoids intercepting conditions,
#' except from errors; behavior of such unhandled conditions depends on the
#' future backend and the environment from which R runs.
#' 
#' @param globals (optional) a logical, a character vector, or a named list
#' to control how globals are handled.
#' For details, see section 'Globals used by future expressions'
#' in the help for [future()].
#' 
#' @param packages (optional) a character vector specifying packages
#' to be attached in the \R environment evaluating the future.
#'
#' @param seed (optional) If TRUE, the random seed, that is, the state of the
#' random number generator (RNG) will be set such that statistically sound
#' random numbers are produced (also during parallelization).
#' If FALSE (default), it is assumed that the future expression does neither
#' need nor use random numbers generation.
#' To use a fixed random seed, specify a L'Ecuyer-CMRG seed (seven integer)
#' or a regular RNG seed (a single integer).  If the latter, then a
#' L'Ecuyer-CMRG seed will be automatically created based on the given seed.
#' Furthermore, if FALSE, then the future will be monitored to make sure it
#' does not use random numbers.  If it does and depending on the value of
#' option \code{\link[=future.options]{future.rng.onMisuse}}, the check is
#' ignored, an informative warning, or error will be produced.
#' If `seed` is NULL, then the effect is as with `seed = FALSE`
#' but without the RNG check being performed.
#'
#' @param lazy If FALSE (default), the future is resolved
#' eagerly (starting immediately), otherwise not.
#'
#' @param gc If TRUE, the garbage collector run (in the process that
#' evaluated the future) only after the value of the future is collected.
#' Exactly when the values are collected may depend on various factors such
#' as number of free workers and whether `earlySignal` is TRUE (more
#' frequently) or FALSE (less frequently).
#' _Some types of futures ignore this argument._
#'
#' @param earlySignal Specified whether conditions should be signaled as soon
#' as possible or not.
#'
#' @param label An optional character string label attached to the future.
#'
#' @param \dots Additional named elements of the future.
#' 
#' @return
#' `Future()` returns an object of class `Future`.
#'
#' @details
#' A Future object is itself an \link{environment}.
#'
#' @seealso
#' One function that creates a Future is [future()].
#' It returns a Future that evaluates an \R expression in the future.
#' An alternative approach is to use the \code{\link{\%<-\%}} infix
#' assignment operator, which creates a future from the
#' right-hand-side (RHS) \R expression and assigns its future value
#' to a variable as a \emph{\link[base:delayedAssign]{promise}}.
#'
#' @importFrom parallel nextRNGStream nextRNGSubStream
#' @export
#' @keywords internal
#' @name Future-class
Future <- function(expr = NULL, envir = parent.frame(), substitute = TRUE, stdout = TRUE, conditions = "condition", globals = list(), packages = NULL, seed = FALSE, lazy = FALSE, gc = FALSE, earlySignal = FALSE, label = NULL, ...) {
  if (substitute) expr <- substitute(expr)
  t_start <- Sys.time()
  
  if (is.null(seed)) {
  } else if (isFALSE(seed)) {
  } else if (is_lecyer_cmrg_seed(seed)) {
  } else {
    if (isTRUE(seed)) {
      sample.int(n = 1L, size = 1L, replace = FALSE)
    }
    .seed <- as_lecyer_cmrg_seed(seed)
    seed <- nextRNGSubStream(.seed)
  }

  stop_if_not(is.logical(stdout), length(stdout) == 1L)
  if (!is.null(conditions)) {
    stop_if_not(is.character(conditions), !anyNA(conditions))
  }

  args <- list(...)
  args_names <- names(args)

  ## WORKAROUND: Skip scanning of globals if already done /HB 2021-01-18
  if (!inherits(globals, "Globals") ||
      !isTRUE(attr(globals, "already-done", exact = TRUE))) {
    ## Global objects?
    ## 'persistent' is only allowed for ClusterFuture:s, which will be
    ## asserted when run() is called /HB 2023-01-17
    gp <- getGlobalsAndPackages(expr, envir = envir, tweak = tweakExpression, globals = globals, persistent = isTRUE(args$persistent))
    globals <- gp$globals
    expr <- gp$expr
  
    ## Record packages?
    if (length(packages) > 0 || (length(gp$packages) > 0 && lazy)) {
      packages <- unique(c(gp$packages, packages))
    }
    
    gp <- NULL
  }

  if (!is.null(globals)) {
    stop_if_not(is.list(globals),
              length(globals) == 0 || inherits(globals, "Globals"))
  }

  if (!is.null(packages)) {
    stop_if_not(is.character(packages))
    packages <- unique(packages)
    stop_if_not(!anyNA(packages), all(nzchar(packages)))
  }

  if (!is.null(label)) {
    stop_if_not(is.character(label))
  }
  
  ## IMPORTANT: Do *not* set 'value' because that field is defunct but
  ## there might still be legacy code out there that rely on it.  By
  ## assert it is not set here, it is more likely to be caught.  This
  ## check will eventually be removed
  if ("value" %in% args_names) {
    .Defunct(msg = "Future field 'value' is defunct and must not be set", package = .packageName)
  }

  ## 'local' is defunct
  if ("local" %in% args_names) {
    .Defunct(msg = "Argument 'local' is defunct as of future 1.31.0 (2023-01-31)", package = .packageName)
  }

  core <- new.env(parent = emptyenv())

  ## Version of future
  version <- args$version
  if (is.null(version)) version <- "1.8"
  core$version <- version

  ## Future evaluation
  core$expr <- expr
  core$envir <- envir
  core$stdout <- stdout
  core$conditions <- conditions
  core$globals <- globals
  core$packages <- packages
  core$seed <- seed
  core$lazy <- lazy
  core$asynchronous <- TRUE  ## Reserved for future version (Issue #109)

  ## 'local' is now defunct and always TRUE, unless persistent = TRUE,
  ## which in turn may only be used for cluster futures. /HB 2023-01-11
  core$local <- TRUE

  ## Result
  core$result <- NULL

  ## Future miscellaneous
  core$label <- label
  core$earlySignal <- earlySignal
  core$gc <- gc
  core$owner <- session_uuid()
  .package$futureCounter <- .package$futureCounter + 1L
  core$uuid <- uuid(list(owner = core$owner, counter = .package$futureCounter))
  core$calls <- sys.calls()

  ## The current state of the future, e.g.
  ## 'created', 'running', 'finished', 'failed', 'interrupted'.
  core$state <- "created"

  ## Additional named arguments
  for (key in args_names) core[[key]] <- args[[key]]

  structure(core, class = c("Future", class(core)))
}


#' @importFrom utils head capture.output
#' @export
print.Future <- function(x, ...) {
  class <- class(x)
  cat(sprintf("%s:\n", class[1]))
  label <- x$label
  if (is.null(label)) label <- "<none>"
  cat("Label: ", sQuote(label), "\n", sep = "")
  cat("Expression:\n")
  print(x$expr)
  cat(sprintf("Lazy evaluation: %s\n", x$lazy))
  cat(sprintf("Asynchronous evaluation: %s\n", x$asynchronous))
  cat(sprintf("Local evaluation: %s\n", x$local))
  cat(sprintf("Environment: %s\n", envname(x$envir)))
  cat(sprintf("Capture standard output: %s\n", x$stdout))
  if (length(x$conditions) > 0) {
    exclude <- attr(x$conditions, "exclude", exact = TRUE)
    if (length(exclude) == 0) exclude <- "nothing"
    cat(sprintf("Capture condition classes: %s (excluding %s)\n",
                paste(sQuote(x$conditions), collapse = ", "),
                paste(sQuote(exclude), collapse = ", ")))
  } else {
    cat("Capture condition classes: <none>\n")
  }

  ## FIXME: Add method globals_of() for Future such that it's possible
  ## also for SequentialFuture to return something here. /HB 2017-05-17
  g <- globals(x)
  ng <- length(g)
  if (ng > 0) {
    gSizes <- sapply(g, FUN = objectSize)
    gTotalSize <- sum(gSizes)
    g <- head(g, n = 5L)
    gSizes <- head(gSizes, n = 5L)
    g <- sprintf("%s %s of %s", sapply(g, FUN = function(x) class(x)[1]), sQuote(names(g)), sapply(gSizes, FUN = asIEC))
    if (ng > length(g)) g <- c(g, "...")
    g <- hpaste(g, maxHead = 5L, maxTail = 0L)
    cat(sprintf("Globals: %d objects totaling %s (%s)\n", ng, asIEC(gTotalSize), g))
  } else {
    cat("Globals: <none>\n")
  }
  
  p <- packages(x)
  np <- length(p)
  if (np > 0) {
    cat(sprintf("Packages: %d packages (%s)\n", np, paste(sQuote(p), collapse = ", ")))
  } else {
    cat("Packages: <none>\n")
  }
  
  if (is.integer(x$seed)) {
    cat(sprintf("L'Ecuyer-CMRG RNG seed: c(%s)\n", paste(x$seed, collapse = ", ")))
  } else {
    cat("L'Ecuyer-CMRG RNG seed: <none> (seed = ", deparse(x$seed), ")\n", sep = "")
  }

  result <- x$result
  hasResult <- inherits(result, "FutureResult")
  ## BACKWARD COMPATIBILITY
  hasResult <- hasResult || exists("value", envir = x, inherits = FALSE)
  if (hasResult) {
    cat("Resolved: TRUE\n")
  } else if (x$state == "created") {
    ## Don't launch lazy futures here
    cat("Resolved: FALSE\n")
  } else if (inherits(x, "UniprocessFuture") && x$lazy) {
    ## FIXME: Special case; will there every be other cases
    ## for which we need to support this? /HB 2016-05-03
    cat("Resolved: FALSE\n")
  } else {
    ## Don't signal conditions here
    ## Note that resolved() may produce a FutureError, e.g.
    ## due to invalid connection in a MultisessionFuture
    is_resolved <- FALSE
    cat(sprintf("Resolved: %s\n", tryCatch(resolved(x, .signalEarly = FALSE), error = function(ex) NA)))
  }

  if (hasResult) {
    if (inherits(result, "FutureResult")) {
      value <- result$value
    } else if ("value" %in% names(x)) {
      .Defunct(msg = sprintf("Detected a %s object that rely on the defunct 'value' field of format version 1.7 or before.", class(x)[1]), package = .packageName)
    } else {
      stop(FutureError(sprintf("The %s object does not have a 'results' field", class(x)[1]), future = future))
    }
    cat(sprintf("Value: %s of class %s\n", asIEC(objectSize(value)), sQuote(class(value)[1])))
    if (inherits(result, "FutureResult")) {
      conditions <- result$conditions
      conditionClasses <- vapply(conditions, FUN = function(c) class(c$condition)[1], FUN.VALUE = NA_character_)
      cat(sprintf("Conditions captured: [n=%d] %s\n", length(conditionClasses), hpaste(sQuote(conditionClasses))))
    }
  } else {
    cat("Value: <not collected>\n")
    cat("Conditions captured: <none>\n")
  }
  cat(sprintf("Early signaling: %s\n", isTRUE(x$earlySignal)))
  cat(sprintf("Owner process: %s\n", x$owner))
  cat(sprintf("Class: %s\n", paste(sQuote(class), collapse = ", ")))
} ## print()


## Checks whether Future is owned by the current process or not
assertOwner <- function(future, ...) {
  hpid <- function(uuid) {
    info <- attr(uuid, "source", exact = TRUE)
    if (is.null(info)) info <- list(pid = NA_integer_, host = NA_character_)
    stop_if_not(is.list(info), length(info$pid) == 1L, length(info$host) == 1L)
    pid <- sprintf("%s; pid %d on %s", uuid, info$pid, info$host)
    stop_if_not(length(pid) == 1L)
    pid
  }

  if (!identical(future$owner, session_uuid())) {
    stop(FutureError(sprintf("Invalid usage of futures: A future (here %s) whose value has not yet been collected can only be queried by the R process (%s) that created it, not by any other R processes (%s): %s", sQuote(class(future)[1]), hpid(future$owner), hpid(session_uuid()), hexpr(future$expr)), future = future))
  }

  invisible(future)
}


#' Run a future
#'
#' @param future A \link{Future}.
#' @param \dots Not used.
#'
#' @return The [Future] object.
#'
#' @details
#' This function can only be called once per future.
#' Further calls will result in an informative error.
#' If a future is not run when its value is queried,
#' then it is run at that point.
#'
#' @aliases run
#' @rdname run
#' @export
#' @export run
#' @keywords internal
run.Future <- function(future, ...) {
  debug <- getOption("future.debug", FALSE)
  if (debug) {
    mdebug("run() for ", sQuote(class(future)[1]), " ...")
    mdebug("- state: ", sQuote(future$state))
    on.exit(mdebug("run() for ", sQuote(class(future)[1]), " ... done"), add = TRUE)
  }

  if (future$state != "created") {
    label <- future$label
    if (is.null(label)) label <- "<none>"
    msg <- sprintf("A future ('%s') can only be launched once.", label)
    stop(FutureError(msg, future = future))
  }

  ## Sanity check: This method should only called for lazy futures
  stop_if_not(future$lazy)

  if (is.null(future$owner)) {
    future$owner <- session_uuid()
  } else {  
    ## Be conservative for now; don't allow lazy futures created in another R
    ## session to be launched. This will hopefully change later, but we won't
    ## open this door until we understand the ramifications. /HB 2020-12-21
    if (getOption("future.lazy.assertOwner", TRUE)) {
      assertOwner(future)
    } else {
      future$owner <- session_uuid()
    }
  }

  ## Create temporary future for a specific backend, but don't launch it
  makeFuture <- plan("next")
  if (debug) mdebug("- Future backend: ", paste(sQuote(class(makeFuture)), collapse = ", "))

  ## AD HOC/WORKAROUND: /HB 2020-12-21
  args <- list(
    quote(future$expr),
    substitute = FALSE,
    envir = future$envir,
    lazy = TRUE,
    stdout = future$stdout,
    conditions = future$conditions,
    globals = future$globals,
    packages = future$packages,
    seed = future$seed,
    label = future$label,
    calls = future$calls
  )

  ## SPECIAL: 'cluster' takes argument 'persistent' for now /HB 2023-01-17
  has_persistent <- ("persistent" %in% names(future))
  if (has_persistent) args$persistent <- future$persistent
  
  tmpFuture <- do.call(makeFuture, args = args)

  ## SPECIAL: 'cluster' takes argument 'persistent' for now /HB 2023-01-17
  if (has_persistent) {
    if (inherits(makeFuture, "cluster") &&
        !inherits(makeFuture, "multisession")) {
      tmpFuture$local <- !tmpFuture$persistent
    } else {
      .Defunct(msg = "Future field 'persistent' is defunct and must not be set", package = .packageName)
    }
  }

  if (debug) mdebug("- Future class: ", paste(sQuote(class(tmpFuture)), collapse = ", "))

  ## AD HOC/SPECIAL:
  ## If 'earlySignal=TRUE' was set explicitly when creating the future,
  ## then override the plan, otherwise use what the plan() says
  if (isTRUE(future$earlySignal)) tmpFuture$earlySignal <- TRUE

  ## If 'gc=TRUE' was set explicitly when creating the future,
  ## then override the plan, otherwise use what the plan() says
  if (isTRUE(future$gc)) tmpFuture$gc <- TRUE

  ## Copy the full state of this temporary future into the main one
  ## This can be done because Future:s are environments and we can even
  ## assign attributes such as the class to existing environments
  
  if (debug) mdebugf("- Copy elements of temporary %s to final %s object ...", sQuote(class(tmpFuture)[1]), sQuote(class(future)[1]))
  ## (a) Copy all elements
  for (name in names(tmpFuture)) {
    if (debug) mdebug("  - Field: ", sQuote(name))
    future[[name]] <- tmpFuture[[name]]
  }
  if (debug) mdebugf("- Copy elements of temporary %s to final %s object ... done", sQuote(class(tmpFuture)[1]), sQuote(class(future)[1]))
  ## (b) Copy all attributes
  attributes(future) <- attributes(tmpFuture)

  ## (c) Temporary future no longer needed
  tmpFuture <- NULL

  ## Launch the future?
  if (future$lazy) {
    if (debug) mdebug("- Launch lazy future ...")
    future <- run(future)
    if (debug) mdebug("- Launch lazy future ... done")
  }
  
  ## Sanity check: This method was only called for lazy futures
  stop_if_not(future$state != "created", future$lazy)

  future
}

#' @export
#' @keywords internal
run <- function(future, ...) {
  ## Automatically update journal entries for Future object
  if (inherits(future, "Future") &&
      inherits(future$.journal, "FutureJournal")) {
    start <- Sys.time()
    on.exit({
      appendToFutureJournal(future,
          event = "launch",
       category = "overhead",
          start = start,
           stop = Sys.time()
      )
    })
  }
  UseMethod("run")
}


#' @export
#' @keywords internal
result <- function(future, ...) {
  ## Automatically update journal entries for Future object
  if (inherits(future, "Future") &&
      inherits(future$.journal, "FutureJournal")) {
    start <- Sys.time()
    on.exit({
      appendToFutureJournal(future,
           event = "gather",
        category = "overhead",
           start = start,
            stop = Sys.time()
      )

      ## Signal FutureJournalCondition?
      if (!isTRUE(future$.journal_signalled)) {
        journal <- journal(future)
        label <- future$label
        if (is.null(label)) label <- "<none>"
        msg <- sprintf("A future ('%s') of class %s was resolved", label, class(future)[1])
        cond <- FutureJournalCondition(message = msg, journal = journal) 
        signalCondition(cond)
        future$.journal_signalled <- TRUE
      }
    })
  }
  UseMethod("result")
}

#' Get the results of a resolved future
#'
#' @param future A \link{Future}.
#' @param \dots Not used.
#'
#' @return The [FutureResult] object.
#'
#' @details
#' This function is only part of the _backend_ Future API.
#' This function is _not_ part of the frontend Future API.
#'
#' @aliases result
#' @rdname result
#' @export
#' @keywords internal
result.Future <- function(future, ...) {
  ## Has the result already been collected?
  result <- future$result
  if (!is.null(result)) {
    ## Always signal immediateCondition:s and as soon as possible.
    ## They will always be signaled if they exist.
    signalImmediateConditions(future)

    if (inherits(result, "FutureError")) stop(result)
    return(result)
  }
  
  if (future$state == "created") {
    future <- run(future)
  }

  if (!future$state %in% c("finished", "failed", "interrupted")) {
    ## BACKWARD COMPATIBILITY:
    ## For now, it is value() that collects the results.  Later we want
    ## all future backends to use result() to do it. /HB 2018-02-22
    value(future, stdout = FALSE, signal = FALSE)

    ## Always signal immediateCondition:s and as soon as possible.
    ## They will always be signaled if they exist.
    signalImmediateConditions(future)
  }

  result <- future$result
  if (inherits(result, "FutureResult")) return(result)

  ## BACKWARD COMPATIBILITY
  result <- future$value
  if (inherits(result, "FutureResult")) return(result)

  version <- future$version
  if (is.null(version)) {
    warning(FutureWarning("Future version was not set. Using default %s",
                          sQuote(version)))
  }

  ## Sanity check
  if (version == "1.8") {
    if (is.null(result) && inherits(future, "MulticoreFuture")) {
      label <- future$label
      if (is.null(label)) label <- "<none>"
      msg <- sprintf("A future ('%s') of class %s did not produce a FutureResult object but NULL. This suggests that the R worker terminated (crashed?) before the future expression was resolved.", label, class(future)[1])
      stop(FutureError(msg, future = future))
    }
  }

  .Defunct(msg = "Future objects with an internal version of 1.7 or earlier are defunct. This error is likely coming from a third-party package or other R code. Please report this to the maintainer of the 'future' package so this can be resolved.", package = .packageName)
}


#' @export
resolved.Future <- function(x, run = TRUE, ...) {
  debug <- getOption("future.debug", FALSE)
  if (debug) {
    mdebug("resolved() for ", sQuote(class(x)[1]), " ...")
    on.exit(mdebug("resolved() for ", sQuote(class(x)[1]), " ... done"), add = TRUE)
    mdebug("- state: ", sQuote(x$state))
    mdebug("- run: ", run)
  }
  
  ## A lazy future not even launched?
  if (x$state == "created") {
    if (!run) return(FALSE)
    if (debug) mdebug("- run() ...")
    x <- run(x)
    if (debug) mdebug("- run() ... done")
    if (debug) mdebug("- resolved() ...")
    res <- resolved(x, ...)
    if (debug) {
      mdebug("- resolved: ", res)
      mdebug("- resolved() ... done")
    }
    return(res)
  }

  ## Signal conditions early, iff specified for the given future
  ## Note, collect = TRUE will block here, which is intentional
  signalEarly(x, collect = TRUE, ...)

  if (debug) mdebug("- result: ", sQuote(class(x$result)[1]))
  if (inherits(x$result, "FutureResult")) return(TRUE)
  
  res <- (x$state %in% c("finished", "failed", "interrupted"))

  if (debug) mdebug("- resolved: ", res)

  res
}


#' Inject code for the next type of future to use for nested futures
#'
#' @param future Current future.
#' @param \dots Not used.
#'
#' @return A future expression with code injected to set what
#' type of future to use for nested futures, iff any.
#'
#' @details
#' If no next future strategy is specified, the default is to
#' use [sequential] futures.  This conservative approach protects
#' against spawning off recursive futures by mistake, especially
#' [multicore] and [multisession] ones.
#' The default will also set `options(mc.cores = 1L)` (*) so that
#' no parallel \R processes are spawned off by functions such as
#' \code{parallel::mclapply()} and friends.
#'
#' Currently it is not possible to specify what type of nested
#' futures to be used, meaning the above default will always be
#' used.
#' See \href{https://github.com/futureverse/future/issues/37}{Issue #37}
#' for plans on adding support for custom nested future types.
#'
#' (*) Ideally we would set `mc.cores = 0` but that will unfortunately
#'     cause `mclapply()` and friends to generate an error saying
#'     "'mc.cores' must be >= 1".  Ideally those functions should
#'     fall back to using the non-multicore alternative in this
#'     case, e.g. `mclapply(...)` => `lapply(...)`.
#'     See \url{https://github.com/HenrikBengtsson/Wishlist-for-R/issues/7}
#'     for a discussion on this.
#'
#' @aliases getExpression.Future
#' @keywords internal
#'
#' @export
getExpression <- function(future, ...) UseMethod("getExpression")

#' @export
getExpression.Future <- local({

  tmpl_expr_evaluate2 <- bquote_compile({
    ## Evaluate future
    future:::evalFuture(expr = quote(.(expr)), local = .(local), stdout = .(stdout), conditionClasses = .(conditionClasses), split = .(split), seed = .(seed), immediateConditions = .(immediateConditions), immediateConditionClasses = .(immediateConditionClasses), globals = .(globals), packages = .(pkgs), strategiesR = .(strategiesR), forwardOptions = .(forwardOptions), threads = .(threads), cleanup = .(cleanup))
  })

  function(future, expr = future$expr, local = future$local, stdout = future$stdout, conditionClasses = future$conditions, split = future$split, seed = future$seed, immediateConditions = FALSE, mc.cores = NULL, threads = NA_integer_, cleanup = TRUE, ...) {
    debug <- getOption("future.debug", FALSE)
    ##  mdebug("getExpression() ...")
  
    if (is.null(split)) split <- FALSE
    stop_if_not(is.logical(split), length(split) == 1L, !is.na(split))
   
    version <- future$version
    if (is.null(version)) {
      warning(FutureWarning("Future version was not set. Using default %s",
                            sQuote(version)))
    }
  
    ## Globals needed by the future
    globals <- globals(future)
  
    ## Packages needed by the future
    pkgs <- packages(future)
    if (length(pkgs) > 0) {
      if (debug) mdebugf("Packages needed by the future expression (n = %d): %s", length(pkgs), paste(sQuote(pkgs), collapse = ", "))
    } else {
      if (debug) mdebug("Packages needed by the future expression (n = 0): <none>")
    }
  
    ## Future strategies
    strategies <- plan("list")
    stop_if_not(length(strategies) >= 1L)
  
    ## Pass down the default or the remain set of future strategies?
    strategiesR <- strategies[-1]
    ##  mdebugf("Number of remaining strategies: %d", length(strategiesR))
  
    ## Use default future strategy + identify packages needed by the futures
    if (length(strategiesR) == 0L) {
      if (debug) mdebug("Packages needed by future strategies (n = 0): <none>")
      strategiesR <- getOption("future.plan", "sequential")
    } else {
      ## Identify package namespaces needed for strategies
      pkgsS <- lapply(strategiesR, FUN = environment)
      pkgsS <- lapply(pkgsS, FUN = environmentName)
      pkgsS <- unique(unlist(pkgsS, use.names = FALSE))
      ## CLEANUP: Only keep those that are loaded in the current session
      pkgsS <- intersect(pkgsS, loadedNamespaces())
      if (debug) mdebugf("Packages needed by future strategies (n = %d): %s", length(pkgsS), paste(sQuote(pkgsS), collapse = ", "))
      pkgs <- unique(c(pkgs, pkgsS))
    }
  
    conditionClassesExclude <- attr(conditionClasses, "exclude", exact = TRUE)
    muffleInclude <- attr(conditionClasses, "muffleInclude", exact = TRUE)
    if (is.null(muffleInclude)) muffleInclude <- "^muffle"
    
    if (immediateConditions && !is.null(conditionClasses)) {
      immediateConditionClasses <- getOption("future.relay.immediate", "immediateCondition")
      conditionClasses <- unique(c(conditionClasses, immediateConditionClasses))
      attr(conditionClasses, "exclude") <- conditionClassesExclude
      attr(conditionClasses, "muffleInclude") <- muffleInclude
    } else {
      immediateConditionClasses <- character(0L)
    }
  
    forwardOptions <- list(
      ## Assert globals when future is created (or at run time)?
      future.globals.onMissing          = getOption("future.globals.onMissing"),
    
      ## Pass down other future.* options
      future.globals.maxSize            = getOption("future.globals.maxSize"),
      future.globals.method             = getOption("future.globals.method"),
      future.globals.onReference        = getOption("future.globals.onReference"),
      future.globals.resolve            = getOption("future.globals.resolve"),
      future.resolve.recursive          = getOption("future.resolve.recursive"),
      future.rng.onMisuse               = getOption("future.rng.onMisuse"),
      future.rng.onMisuse.keepFuture    = getOption("future.rng.onMisuse.keepFuture"),
      future.stdout.windows.reencode    = getOption("future.stdout.windows.reencode"),
  
      future.fork.multithreading.enable = getOption("future.fork.multithreading.enable"),
  
      future.globalenv.onMisuse         = getOption("future.globalenv.onMisuse"),
  
      future.makeExpression.skip        = getOption("future.makeExpression.skip"),
      future.makeExpression.skip.local  = getOption("future.makeExpression.skip.local"),
      
      ## Other options relevant to making futures behave consistently
      ## across backends
      width                             = getOption("width")
    )
  
    if (!is.null(mc.cores)) {
      forwardOptions$mc.cores <- mc.cores
    }
  
    expr <- bquote_apply(tmpl_expr_evaluate2)
    
    if (getOption("future.debug", FALSE)) mprint(expr)
  
    ##  mdebug("getExpression() ... DONE")
    
    expr
  }
}) ## getExpression()


globals <- function(future, ...) UseMethod("globals")

#' @exportS3Method
globals.Future <- function(future, ...) {
  future[["globals"]]
}

packages <- function(future, ...) UseMethod("packages")

#' @exportS3Method
packages.Future <- function(future, ...) {
  future[["packages"]]
}


#' @export
`$<-.Future` <- function(x, name, value) {
  if (name == "state") {
    if (!is.element(value, c("created", "running", "finished", "failed", "interrupted"))) {
      action <- getOption("future.state.onInvalid", "warning")
      
      if (action != "ignore") {
        msg <- sprintf("Trying to assign an invalid value to the internal '%s' field of a %s object: %s", name, class(x)[1], value)
        if (action == "error") {
          stop(FutureError(msg, call = sys.call(), future = x))
        } else {
          warning(FutureWarning(msg, call = sys.call(), future = x))
        }
      }
    }
  }
  
  NextMethod()
}
