#' Plan how to resolve a future
#'
#' This function allows _the user_ to plan the future, more specifically,
#' it specifies how [future()]:s are resolved,
#' e.g. sequentially or in parallel.
#'
#' @param strategy The evaluation function (or name of it) to use
#' for resolving a future. If `NULL`, then the current strategy is returned.
#'
#' @param \dots Additional arguments overriding the default arguments
#' of the evaluation function.  Which additional arguments are supported
#' depends on what evaluation function is used, e.g. several support
#' argument `workers` but not all. For details, see the individual
#' functions of which some are linked to below.
#"
#' @param substitute If `TRUE`, the `strategy` expression is
#' `substitute()`:d, otherwise not.
#'
#' @param .call (internal) Used for recording the call to this function.
#'
#' @param .skip (internal) If `TRUE`, then attempts to set a strategy
#' that is the same as what is currently in use, will be skipped.
#'
#' @param .cleanup (internal) Used to stop implicitly started clusters.
#'
#' @param .init (internal) Used to initiate workers.
#'
#' @return If a new strategy is chosen, then the previous one is returned
#' (invisible), otherwise the current one is returned (visibly).
#'
#' @example incl/plan.R
#'
#' @details
#' The default strategy is [`sequential`], but the default can be
#' configured by option \option{future.plan} and, if that is not set,
#' system environment variable \env{R_FUTURE_PLAN}.
#' To reset the strategy back to the default, use `plan("default")`.
#'
#' @section Built-in evaluation strategies:
#' The \pkg{future} package provides the following built-in backends:
#'
#' \describe{
#'  \item{[`sequential`]:}{
#'    Resolves futures sequentially in the current \R process, e.g.
#'    `plan(sequential)`.
#'  }
#'  \item{[`multisession`]:}{
#'    Resolves futures asynchronously (in parallel) in separate
#'    \R sessions running in the background on the same machine, e.g.
#'    `plan(multisession)` and `plan(multisession, workers = 2)`.
#'  }
#'  \item{[`multicore`]:}{
#'    Resolves futures asynchronously (in parallel) in separate
#'    _forked_ \R processes running in the background on
#'    the same machine, e.g.
#'    `plan(multicore)` and `plan(multicore, workers = 2)`.
#'    This backend is not supported on Windows.
#'  }
#'  \item{[`cluster`]:}{
#'    Resolves futures asynchronously (in parallel) in separate
#'    \R sessions running typically on one or more machines, e.g.
#'    `plan(cluster)`, `plan(cluster, workers = 2)`, and
#'    `plan(cluster, workers = c("n1", "n1", "n2", "server.remote.org"))`.
#'  }
#' }
#'
#' Other package provide additional evaluation strategies.
#' For example, the \pkg{future.callr} package implements an alternative
#' to the `multisession` backend on top of the \pkg{callr} package, e.g.
#' `plan(future.callr::callr, workers = 2)`.
#' Another example is the \pkg{future.batchtools} package, which implements,
#' on top of the \pkg{batchtools} package, e.g.
#' `plan(future.batchtools::batchtools_slurm)`.
#' These types of futures are resolved via job schedulers, which typically
#' are available on high-performance compute (HPC) clusters, e.g. LSF,
#' Slurm, TORQUE/PBS, Sun Grid Engine, and OpenLava.
#'
#' To "close" any background workers (e.g. `multisession`), change
#' the plan to something different; `plan(sequential)` is recommended
#' for this.
#'
#' @section For package developers:
#' Please refrain from modifying the future strategy inside your packages /
#' functions, i.e. do not call `plan()` in your code. Instead, leave
#' the control on what backend to use to the end user. This idea is part of
#' the core philosophy of the future framework---as a developer you can never
#' know what future backends the user have access to. Moreover, by not making
#' any assumptions about what backends are available, your code will also work
#' automatically with any new backends developed after you wrote your code.
#'
#' If you think it is necessary to modify the future strategy within a
#' function, then make sure to undo the changes when exiting the function.
#' This can be done using:
#'
#' \preformatted{
#'   oplan <- plan(new_set_of_strategies)
#'   on.exit(plan(oplan), add = TRUE)
#'   [...]
#' }
#'
#' This is important because the end-user might have already set the future
#' strategy elsewhere for other purposes and will most likely not known that
#' calling your function will break their setup.
#' _Remember, your package and its functions might be used in a greater
#' context where multiple packages and functions are involved and those might
#' also rely on the future framework, so it is important to avoid stepping on 
#' others' toes._
#'
#' @section Using plan() in scripts and vignettes:
#' When writing scripts or vignettes that use futures, try to place any
#' call to `plan()` as far up (i.e. as early on) in the code as possible.  
#' This will help users to quickly identify where the future plan is set up
#' and allow them to modify it to their computational resources.
#' Even better is to leave it to the user to set the `plan()` prior to
#' `source()`:ing the script or running the vignette.
#' If a \file{\link{.future.R}} exists in the current directory and / or in
#' the user's home directory, it is sourced when the \pkg{future} package is
#' _loaded_. Because of this, the \file{.future.R} file provides a
#' convenient place for users to set the `plan()`.
#' This behavior can be controlled via an \R option---see
#' \link[future:future.options]{future options} for more details.
#'
#' @importFrom utils capture.output
#' @export
plan <- local({
  defaultStrategy <- structure(sequential, call = substitute(plan(sequential)))

  defaultStack <- structure(list(defaultStrategy),
                            class = c("FutureStrategyList", "list"))

  ## Stack of type of futures to use
  stack <- defaultStack

  assert_no_disallowed_strategies <- function(stack) {
    noplans <- getOption("future.plan.disallow")
    if (length(noplans) == 0L) return()

    for (kk in seq_along(stack)) {
      evaluator <- stack[[kk]]
      if (!inherits(evaluator, noplans)) next
      clazz <- class(evaluator)[1]
      if (!clazz %in% noplans) next  ## <== sic!

      stop(FutureError(sprintf("Can not use %s in the future plan because it is on the list of future strategies that are not allow per option 'future.plan.disallow': %s", sQuote(clazz), commaq(noplans))))
    }
  }

  evaluator_uses <- function(evaluator, strategy) {
    if (!inherits(evaluator, strategy)) return(FALSE)
    ## NOTE: Yes, we are indeed inspecting the 'class' attribute itself
    class <- class(evaluator)
    if (class[1] == strategy) return(TRUE)
    if (length(class) == 1L) return(FALSE)
    if (class[1] == "tweaked" && class[2] == strategy) return(TRUE)
    ## Special case for strategy == "multiprocess"
    if (strategy == "multiprocess" && class[length(class)] == strategy) return(TRUE)
    FALSE
  }

  warn_about_multicore <- local({
    .warn <- TRUE

    function(stack) {
      if (!.warn) return()

      ## Is 'multicore' used despite not being supported on the current
      ## platform?    
      for (kk in seq_along(stack)) {
        if (evaluator_uses(stack[[kk]], "multicore")) {
          supportsMulticore(warn = TRUE)
          ## Warn only once, if at all
          .warn <<- FALSE
          break
        }
      }
    }
  })

  plan_cleanup <- function() {
    evaluator <- stack[[1L]]
    
    cleanup <- attr(evaluator, "cleanup", exact = TRUE)
    if (!is.null(cleanup)) {
      if (is.function(cleanup)) {
        cleanup()
      } else {
        stop(FutureError(sprintf("Unknown type of 'cleanup' attribute on current future strategy: %s", commaq(class(cleanup)))))
      }
    } else {
      ## Backward compatibility for future (<= 1.33.2)
      if (getOption("future.plan.cleanup.legacy", FALSE)) {
        ClusterRegistry(action = "stop")
      }
    }
  } ## plan_cleanup()

  plan_init <- function() {
    evaluator <- stack[[1L]]

    init <- attr(evaluator, "init", exact = TRUE)
    if (identical(init, TRUE)) {
      debug <- getOption("future.debug", FALSE)
      if (debug) {
        mdebugf("plan(): plan_init() of %s ...",
                commaq(class(evaluator)))
        mprint(evaluator)
      }

      ## IMPORANT: Initiate only once.  This avoids an infinite
      ## recursive loop caused by other plan() calls.
      attr(evaluator, "init") <- "done"
      stack[[1L]] <<- evaluator

      ## Create dummy future to trigger setup (minimum overhead)
      f <- evaluator(NA, label = "future-plan-test", 
                     globals = FALSE, lazy = FALSE)

      ## Cleanup, by resolving it
      ## (otherwise the garbage collector would have to do it)
      res <- tryCatch({
        value(f)
      }, FutureError = identity)
      if (inherits(res, "FutureError")) {
        res$message <- paste0(
          "Initialization of plan() failed, because the test future used for validation failed. The reason was: ", conditionMessage(res))
        stop(res)
      }

      if (!identical(res, NA)) {
        res <- if (is.null(res)) {
          "NULL"
        } else {
          commaq(res)
        }
        stop(FutureError(sprintf("Initialization of plan() failed, because the value of the test future is not NA as expected: %s", res)))
      }
      
      if (debug) {
        mdebugf("plan(): plan_init() of %s ... DONE",
                commaq(class(evaluator)))
      }
    }
  } ## plan_init()


  equal_strategy_stacks <- function(stack, other) {
    stop_if_not(is.list(stack), is.list(other))
    stack <- lapply(stack, FUN = function(s) { attr(s, "call") <- attr(s, "init") <- NULL; s })
    other <- lapply(other, FUN = function(s) { attr(s, "call") <- attr(s, "init") <- NULL; s })

    if (identical(stack, other)) return(TRUE)
    if (isTRUE(all.equal(stack, other))) return(TRUE)
    FALSE
  }

  plan_set <- function(newStack, skip = TRUE, cleanup = TRUE, init = TRUE) {
    stop_if_not(!is.null(newStack), is.list(newStack), length(newStack) >= 1L)

    oldStack <- stack

    ## Assign new stack
    class(newStack) <- unique(c("FutureStrategyList", class(newStack)))

    ## Skip if already set?
    if (skip && equal_strategy_stacks(newStack, oldStack)) {
      if (getOption("future.debug", FALSE)) {
        mdebug("plan(): Skip setting new future strategy stack because it is the same as the current one:")
        mprint(newStack)
      }
      return(oldStack)
    }

    if (getOption("future.debug", FALSE)) {
      mdebug("plan(): Setting new future strategy stack:")
      mprint(newStack)
    }

    assert_no_disallowed_strategies(newStack)

    ## Warn about 'multicore' on certain systems
    warn_about_multicore(newStack)

    ## Stop/cleanup any previously registered backends?
    if (cleanup) plan_cleanup()

    stack <<- newStack

    ## Initiate future workers?
    if (init) plan_init()

    ## Sanity checks
    nbrOfWorkers <- nbrOfWorkers()
    if (getOption("future.debug", FALSE)) {
      mdebugf(sprintf("plan(): nbrOfWorkers() = %.0f", nbrOfWorkers))
    }
    stop_if_not(is.numeric(nbrOfWorkers), length(nbrOfWorkers) == 1L, 
                !is.na(nbrOfWorkers), nbrOfWorkers >= 1L)

    invisible(oldStack)
  } ## plan_set()


  ## Main function
  function(strategy = NULL, ..., substitute = TRUE, .skip = FALSE, .call = TRUE,
           .cleanup = TRUE, .init = TRUE) {
    if (substitute) strategy <- substitute(strategy)
    if (is.logical(.skip)) stop_if_not(length(.skip) == 1L, !is.na(.skip))
    if (is.logical(.call)) stop_if_not(length(.call) == 1L, !is.na(.call))

    ## Predefined "actions":
    if (is.null(strategy) || identical(strategy, "next")) {
      ## Next future strategy?
      strategy <- stack[[1L]]
      if (!inherits(strategy, "FutureStrategy")) {
        class(strategy) <- c("FutureStrategy", class(strategy))
      }
      stop_if_not(is.function(strategy))
      return(strategy)
    } else if (identical(strategy, "default")) {
      strategy <- getOption("future.plan", sequential)
    } else if (identical(strategy, "list")) {
      ## List stack of future strategies?
      return(stack)
    } else if (identical(strategy, "reset")) {
      ## Stop/cleanup any previously registered backends?
      if (.cleanup) plan_cleanup()
      ## Reset stack of future strategies?
      stack <<- defaultStack
      return(stack)
    } else if (identical(strategy, "pop")) {
      ## Pop strategy stack and return old stack
      ## (so it can be pushed back later)
      oldStack <- stack
      stack <<- stack[-1L]
      if (length(stack) == 0L) stack <<- defaultStack
      return(oldStack)
    }

    ## Current and new stack of future strategies
    oldStack <- stack
    newStack <- NULL

    ## Arguments to be tweaked
    targs <- list(...)

    ## Set new stack?
    if (is.function(strategy)) {
      ## Tweak the strategy function?
      if (length(targs) > 0) {
        args <- c(list(strategy), targs, penvir = parent.frame())
        strategy <- do.call(tweak, args = args)
      }
      strategy <- list(strategy)
    }

    if (is.list(strategy)) {
      oldStack <- plan_set(strategy, skip = .skip, cleanup = .cleanup, init = .init)
      return(invisible(oldStack))
    }

    ## (a) Is a (plain) list of future strategies specified?
    if (is.language(strategy)) {
      first <- as.list(strategy)[[1]]
      if (is.symbol(first)) {
        ## If a function call, e.g. list(...), then make sure to look up
        ## a function to be used as 'first'.  This makes sure that base::list()
        ## is used with plan(list(...)) even when there is a non-function 
        ## 'list' on the search() path, e.g. gsubfn::list.
        if (is.call(strategy)) {
          first <- get(as.character(first), mode="function", 
                       envir = parent.frame(), inherits = TRUE)
        } else {
          first <- eval(first, envir = parent.frame(), enclos = baseenv())
        }

        ## A list object, e.g. plan(oplan)?
        if (is.list(first)) {
          strategies <- first
          res <- plan(strategies, substitute = FALSE,
                      .cleanup = .cleanup, .init = .init)
          return(invisible(res))
        }

        if (is.function(first) && !inherits(first, "future")) {
          strategies <- eval(strategy, envir = parent.frame(), enclos = baseenv())

          ## Specified explicitly using plan(list(...))?
          ## Example: plan(list(sequential, multicore))
          if (is.list(strategies)) {
            ## Coerce strings to functions, e.g.
            ## plan(list("sequential", multicore))
            for (kk in seq_along(strategies)) {
              strategy_kk <- strategies[[kk]]
              if (is.character(strategy_kk)) {
                strategy_kk <- tweak(strategy_kk, penvir = parent.frame())
                strategies[[kk]] <- strategy_kk
              }
            }
            newStack <- strategies
            stop_if_not(!is.null(newStack), is.list(newStack), length(newStack) >= 1L)
          } else if (is.function(strategies) && !inherits(strategies, "future")) {
            ## Example: plan(x$abc)
            strategies <- list(strategies)
            newStack <- strategies
            stop_if_not(!is.null(newStack), is.list(newStack), length(newStack) >= 1L)
          }
        }
      }
    }

    ## (b) Otherwise, assume a single future strategy
    if (is.null(newStack)) {
      if (is.symbol(strategy)) {
        strategy <- eval(strategy, envir = parent.frame(), enclos = baseenv())
      } else if (is.language(strategy)) {
        strategyT <- as.list(strategy)

        ## tweak(...)?
        if (strategyT[[1]] == as.symbol("tweak")) {
          strategy <- eval(strategy, envir = parent.frame(), enclos = baseenv())
        } else {
          isSymbol <- sapply(strategyT, FUN = is.symbol)
          if (!all(isSymbol)) {
            strategy <- eval(strategyT[[1L]], envir = parent.frame(), enclos = baseenv())
            if (length(strategyT) > 1L) {
              ## Tweak this part of the future strategy
              args <- c(list(strategy), strategyT[-1L], penvir = parent.frame())
              strategy <- do.call(tweak, args = args)
            }
          } else {
            strategy <- eval(strategy, envir = parent.frame(), enclos = baseenv())
          }
        }
      }

      ## Tweak future strategy accordingly
      args <- c(list(strategy), targs, penvir = parent.frame())
      tstrategy <- do.call(tweak, args = args, quote = TRUE)

      ## Setup a new stack of future strategies (with a single one)
      newStack <- list(tstrategy)
      stop_if_not(!is.null(newStack), is.list(newStack), length(newStack) >= 1L)
    }


    ## Attach call attribute to each strategy in the stack?
    if (!is.null(.call)) {
      ## The call to assign
      call <- if (isTRUE(.call)) sys.call() else .call

      for (kk in seq_along(newStack)) {
        strategy <- newStack[[kk]]
        ## Skip if already has a call attibute
        if (!is.null(attr(strategy, "call", exact = TRUE))) next
        ## Attach call
        attr(strategy, "call") <- call
        newStack[[kk]] <- strategy
      }
      stop_if_not(!is.null(newStack), is.list(newStack), length(newStack) >= 1L)
    }

    ## Set new strategy for futures
    oldStack <- plan_set(newStack, skip = .skip, cleanup = .cleanup, init = .init)
    invisible(oldStack)
  } # function()
}) # plan()


supportedStrategies <- function(strategies = c("sequential", "multicore",
                                               "multisession", "cluster")) {
  if (!supportsMulticore()) strategies <- setdiff(strategies, "multicore")
  strategies
}


#' @importFrom utils capture.output str
#' @export
print.future <- function(x, ...) {
  class <- setdiff(class(x), c("FutureStrategy", "tweaked", "function"))
  s <- sprintf("%s:", class[1])
  specs <- list()
  args <- args(x)

  ## Simplify the value on the 'workers' argument?
  formals <- formals(args)
  if (!is.atomic(formals$workers) && !is.language(formals$workers)) {
    bfr <- capture.output(print(formals$workers))
    if (length(bfr) > 6L) {
      bfr2 <- capture.output(str(formals$workers))
      if (length(bfr2) < length(bfr)) bfr <- bfr2
      if (length(bfr) > 6L) bfr <- c(bfr[1:6], "...")
    }
    formals$workers <- paste0("<", paste(bfr, collapse = "; "), ">")
    formals(args) <- formals
  }

  args <- deparse(args, width.cutoff = 500L)
  args <- args[-length(args)]
  args <- gsub("(^[ ]+|[ ]+$)", "", args)
  args <- paste(args, collapse = " ")
  specs$args <- args
  specs$tweaked <- inherits(x, "tweaked")
  specs$call <- paste(deparse(attr(x, "call", exact = TRUE), 
                              width.cutoff = 500L),
                      collapse="")
  specs <- paste0("- ", names(specs), ": ", unlist(specs))
  s <- c(s, specs)
  s <- paste(s, collapse = "\n")
  cat(s, "\n", sep = "")
  invisible(x)
}

#' @export
print.FutureStrategy <- print.future


#' @importFrom utils capture.output
#' @export
print.FutureStrategyList <- function(x, ...) {
  s <- "List of future strategies:"

  for (kk in seq_along(x)) {
    x_kk <- x[[kk]]
    class <- setdiff(class(x_kk), c("tweaked", "function"))
    s_kk <- sprintf("%d. %s:", kk, class[1])
    specs <- list()

    args <- args(x_kk)

    ## Simplify the value on the 'workers' argument?
    formals <- formals(args)
    if (!is.atomic(formals$workers) && !is.language(formals$workers)) {
      bfr <- capture.output(print(formals$workers))
      if (length(bfr) > 6L) {
        bfr2 <- capture.output(str(formals$workers))
        if (length(bfr2) < length(bfr)) bfr <- bfr2
        if (length(bfr) > 6L) bfr <- c(bfr[1:6], "...")
      }
      formals$workers <- paste0("<", paste(bfr, collapse = "; "), ">")
      formals(args) <- formals
    }

    args <- deparse(args, width.cutoff = 500L)
    args <- args[-length(args)]
    args <- gsub("(^[ ]+|[ ]+$)", "", args)
    args <- paste(args, collapse = " ")
    specs$args <- args
    specs$tweaked <- inherits(x_kk, "tweaked")
    specs$call <- paste(deparse(attr(x_kk, "call", exact = TRUE), 
                                width.cutoff = 500L),
                        collapse = "")
    specs <- paste0("   - ", names(specs), ": ", unlist(specs))
    s <- c(s, s_kk, specs)
  }

  s <- paste(s, collapse = "\n")
  cat(s, "\n", sep = "")
  invisible(x)
}


#' Free up active background workers
#'
#' @param x A FutureStrategy.
#'
#' @param \ldots Not used.
#'
#' @export
#'
#' @details
#' This function will resolve any active futures that is currently
#' being evaluated on background workers.
#'
#' @examples
#' resetWorkers(plan())
#'
#' @keywords internal
#' @export
resetWorkers <- function(x, ...) UseMethod("resetWorkers")


#' @export
resetWorkers.default <- function(x, ...) invisible(x)

#' @export
resetWorkers.multicore <- function(x, ...) {
  if (usedCores() == 0L) return(invisible(x))
  reg <- sprintf("multicore-%s", session_uuid())
  FutureRegistry(reg, action = "collect-all", earlySignal = FALSE)
  stop_if_not(usedCores() == 0L)
}
