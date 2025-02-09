#' A cluster future is a future whose value will be resolved asynchronously in a parallel process
#'
#' @inheritParams MultiprocessFuture-class
#' @inheritParams Future-class
#' 
#' @param persistent If FALSE, the evaluation environment is cleared
#' from objects prior to the evaluation of the future.
#' 
#' @param workers A \code{\link[parallel:makeCluster]{cluster}} object,
#' a character vector of host names, a positive numeric scalar,
#' or a function.
#' If a character vector or a numeric scalar, a `cluster` object
#' is created using \code{\link[parallelly:makeClusterPSOCK]{makeClusterPSOCK}(workers)}.
#' If a function, it is called without arguments _when the future
#' is created_ and its value is used to configure the workers.
#' The function should return any of the above types.
#' 
#' @return
#' `ClusterFuture()` returns an object of class `ClusterFuture`.
#'
#' @section Usage:
#' To use 'cluster' futures, use `plan(cluster, ...)`, cf. [cluster].
#'
#' @aliases MultisessionFuture MultisessionFuture-class
#' @export
#' @export MultisessionFuture
#' @importFrom digest digest
#' @name ClusterFuture-class
#' @keywords internal
ClusterFuture <- function(expr = NULL, substitute = TRUE, envir = parent.frame(), persistent = FALSE, workers = NULL, ...) {
  if (substitute) expr <- substitute(expr)
  
  stop_if_not(is.logical(persistent), length(persistent) == 1L,
              !is.na(persistent))

  args <- list(...)

  ## Which '...' arguments should be passed to Future() and 
  ## which should be passed to makeClusterPSOCK()?
  future_args <- !is.element(names(args), makeClusterPSOCK_args())
  
  future <- do.call(MultiprocessFuture, args = c(list(expr = quote(expr), substitute = FALSE, envir = envir, persistent = persistent, node = NA_integer_), args[future_args]), quote = FALSE)

  future <- do.call(as_ClusterFuture, args = c(list(future, workers = workers), args[!future_args]), quote = TRUE)
  
  future
}


as_ClusterFuture <- function(future, workers = NULL, ...) {
  if (is.function(workers)) workers <- workers()
  if (is.null(workers)) {
    getDefaultCluster <- importParallel("getDefaultCluster")
    workers <- getDefaultCluster()
    workers <- addCovrLibPath(workers)
  } else if (is.character(workers) || is.numeric(workers)) {
    workers <- ClusterRegistry("start", workers = workers, ...)
  } else {
    workers <- as.cluster(workers)
    workers <- addCovrLibPath(workers)
  }
  if (!inherits(workers, "cluster")) {
    stopf("Argument 'workers' is not of class 'cluster': %s", paste(sQuote(class(workers)), collapse = ", "))
  }
  stop_if_not(length(workers) > 0)


  ## Attached workers' session information, unless already done.
  ## FIXME: We cannot do this here, because it introduces a race condition
  ## where multiple similar requests may appear at the same time bringing
  ## the send/receive data to be out of sync and therefore corrupt the
  ## futures' values.
  ##  workers <- add_cluster_session_info(workers)
  
  ## Attach name to cluster?
  name <- attr(workers, "name", exact = TRUE)
  if (is.null(name)) {
    name <- digest(workers)
    stop_if_not(length(name) > 0, nzchar(name))
    attr(workers, "name") <- name
  }

  future$workers <- workers

  future <- structure(future, class = c("ClusterFuture", class(future)))

  future
}


#' @export
run.ClusterFuture <- function(future, ...) {
  debug <- getOption("future.debug", FALSE)
  
  if (future$state != 'created') {
    label <- future$label
    if (is.null(label)) label <- "<none>"
    stop(FutureError(sprintf("A future ('%s') can only be launched once.", label), future = future))
  }
  
  ## Assert that the process that created the future is
  ## also the one that evaluates/resolves/queries it.
  assertOwner(future)

  workers <- future$workers
  expr <- getExpression(future)
  persistent <- isTRUE(future$persistent)

  ## FutureRegistry to use
  reg <- sprintf("workers-%s", attr(workers, "name", exact = TRUE))

  ## Next available cluster node
  t_start <- Sys.time()
  node_idx <- requestNode(await = function() {
    FutureRegistry(reg, action = "collect-first", earlySignal = TRUE)
  }, workers = workers)
  future$node <- node_idx

  ## Cluster node to use
  cl <- workers[node_idx]
  
  if (inherits(future$.journal, "FutureJournal")) {
    appendToFutureJournal(future,
         event = "getWorker",
      category = "overhead",
        parent = "launch",
         start = t_start,
          stop = Sys.time()
    )
  }


  ## (i) Reset global environment of cluster node such that
  ##     previous futures are not affecting this one, which
  ##     may happen even if the future is evaluated inside a
  ##     local, e.g. local({ a <<- 1 }).
  if (!persistent) {
    t_start <- Sys.time()
    cluster_call(cl, fun = grmall, future = future, when = "call grmall() on")
    if (inherits(future$.journal, "FutureJournal")) {
      appendToFutureJournal(future,
           event = "eraseWorker",
        category = "overhead",
          parent = "launch",
           start = t_start,
            stop = Sys.time()
      )
    }
  }


  ## (ii) Attach packages that needs to be attached
  ##      NOTE: Already take care of by getExpression() of the Future class.
  ##      However, if we need to get an early error about missing packages,
  ##      we can get the error here before launching the future.
  t_start <- Sys.time()
  packages <- packages(future)
  if (future$earlySignal && length(packages) > 0) {
    if (debug) mdebugf("Attaching %d packages (%s) on cluster node #%d ...",
                      length(packages), hpaste(sQuote(packages)), node_idx)

    cluster_call(cl, fun = requirePackages, packages, future = future, when = "call requirePackages() on")

    if (debug) mdebugf("Attaching %d packages (%s) on cluster node #%d ... DONE",
                      length(packages), hpaste(sQuote(packages)), node_idx)
  }
  
  if (inherits(future$.journal, "FutureJournal")) {
    appendToFutureJournal(future,
         event = "attachPackages",
      category = "overhead",
        parent = "launch",
         start = t_start,
          stop = Sys.time()
    )
  }

  ## Add to registry
  FutureRegistry(reg, action = "add", future = future, earlySignal = FALSE)

  ## (iv) Launch future
  send_call(cl[[1L]], fun = geval, args = list(expr), future = future, when = "launch future on")

  future$state <- 'running'

  if (debug) mdebugf("%s started", class(future)[1])
  
  invisible(future)
}

#' @importFrom parallelly connectionId isConnectionValid
#' @export
resolved.ClusterFuture <- function(x, run = TRUE, timeout = NULL, ...) {
  workers <- x$workers
  
  ## A lazy future not even launched?
  if (x$state == "created") {
    if (run) {
      ## Can we launch it?  Are there available workers?
      reg <- sprintf("workers-%s", attr(workers, "name", exact = TRUE))

      ## Collect one resolved future, if one exists
      FutureRegistry(reg, action = "collect-first", earlySignal = TRUE)

       ## Find which node is available
       avail <- rep(TRUE, times = length(workers))
       futures <- FutureRegistry(reg, action = "list", earlySignal = FALSE)
       nodes <- unlist(lapply(futures, FUN = function(f) f$node), use.names = FALSE)
       avail[nodes] <- FALSE
       ## If one is available, then launch this lazy future
       if (any(avail)) x <- run(x)
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

  workers <- x$workers
  node_idx <- x$node
  cl <- workers[node_idx]

  ## Check if workers socket connection is available for reading
  node <- cl[[1]]
  
  if (!is.null(con <- node$con)) {
    ## AD HOC/SPECIAL CASE: Skip if connection has been serialized and lacks internal representation. /HB 2018-10-27
    connId <- connectionId(con)
    if (!is.na(connId) && connId < 0L) return(FALSE)

    isValid <- isConnectionValid(con)
    if (!isValid) {
      ex <- simpleError("Connection to the worker is corrupt")
      msg <- post_mortem_cluster_failure(ex, when = "checking resolved from", node = node, future = x)
      stop(FutureError(msg, future = x))
    }

    if (is.null(timeout)) {
      timeout <- getOption("future.cluster.resolved.timeout", NULL)
      if (is.null(timeout)) timeout <- getOption("future.resolved.timeout", 0.01)
      if (timeout < 0) {
        warning("Secret option 'future.resolved.timeout' is negative, which causes resolved() to wait until the future is resolved. This feature is only used for testing purposes of the future framework and must not be used elsewhere", immediate. = TRUE)
        timeout <- NULL
      }
    }

    ## WORKAROUND: Non-integer timeouts (at least < 2.0 seconds) may result in
    ## infinite waiting (PR17203).  Fixed in R devel r73470 (2017-10-05)
    ## and R 3.4.3 (https://github.com/HenrikBengtsson/Wishlist-for-R/issues/35)
    if (.Platform$OS.type != "windows" && getRversion() < "3.4.3") {
      timeout <- round(timeout, digits = 0L)
    }
    maxCount <- 100L
    count <- 0L
    while (count < maxCount) {
      ## Is there a message from the worker waiting?
      res <- socketSelect(list(con), write = FALSE, timeout = timeout)
      if (!res) break

      ## Receive it
      msg <- receiveMessageFromWorker(x)

      ## If the message contains a FutureResult, then the future is resolved
      ## and we are done here
      res <- inherits(msg, "FutureResult")
      msg <- NULL
      if (res) break

      ## If not, we received a condition that has already been signaled
      ## by receiveMessageFromWorker().  However, it could be that there is
      ## another condition messages available, so lets check again
      count <- count + 1L
    } ## while()
  } else if (inherits(node, "MPInode")) {
    res <- resolveMPI(x)
  } else {
    warnf("resolved() is not yet implemented for workers of class %s. Will use value() instead and return TRUE", sQuote(class(node)[1]))
    value(x, stdout = FALSE, signal = FALSE)
    res <- TRUE
  }

  ## Signal conditions early? (happens only iff requested)
  if (res) signalEarly(x, ...)

  res
}



#' @export
result.ClusterFuture <- function(future, ...) {
  debug <- getOption("future.debug", FALSE)
  if (debug) {
    mdebug("result() for ClusterFuture ...")
    on.exit(mdebug("result() for ClusterFuture ... done"))
  }

  ## Has the result already been collected?
  result <- future$result
  if (!is.null(result)) {
    if (debug) mdebugf("- result already collected: %s", class(result)[1])
    if (inherits(result, "FutureError")) stop(result)
    return(result)
  }

  msg <- NULL
  while (!inherits(msg, "FutureResult")) {
    msg <- receiveMessageFromWorker(future, ...)
  }

  msg
}


#' @importFrom parallelly isConnectionValid
receiveMessageFromWorker <- function(future, ...) {
  debug <- getOption("future.debug", FALSE)
  if (debug) {
    mdebug("receiveMessageFromWorker() for ClusterFuture ...")
    on.exit(mdebug("receiveMessageFromWorker() for ClusterFuture ... done"))
  }
  
  if (future$state == "created") {
    if (debug) mdebug("- starting non-launched future")
    future <- run(future)
  }

  ## Assert that the process that created the future is
  ## also the one that evaluates/resolves/queries it.
  assertOwner(future)

  recvResult <- importParallel("recvResult")

  workers <- future$workers
  node_idx <- future$node
  cl <- workers[node_idx]
  node <- cl[[1]]

  if (!is.null(con <- node$con)) {
    if (debug) mdebugf("- Validating connection of %s", class(future)[1])
    isValid <- isConnectionValid(con)
    if (!isValid) {
      ex <- simpleError("Connection to the worker is corrupt")
      msg <- post_mortem_cluster_failure(ex, when = "receiving message from", node = node, future = future)
      stop(FutureError(msg, future = future))
    }
  }

  t_start <- Sys.time()

  ## If not, wait for process to finish, and
  ## then collect and record the value
  msg <- NULL
  ack <- tryCatch({
    msg <- recvResult(node)
    TRUE
  }, error = function(ex) ex)

  if (inherits(ack, "error")) {
    if (debug) mdebugf("- parallel:::recvResult() produced an error: %s", conditionMessage(ack))
    msg <- post_mortem_cluster_failure(ack, when = "receive message results from", node = node, future = future)
    ex <- FutureError(msg, call = ack$call, future = future)
    future$result <- ex
    stop(ex)          
  }
  stop_if_not(isTRUE(ack))
  if (debug) mdebug("- received message: ", class(msg)[1])

  ## Non-expected message from worker?
  if (!inherits(msg, "FutureResult") && !inherits(msg, "condition")) {
    ## If parallel:::slaveLoop() ends up capturing the error, which should
    ## not happen unless there is a critical error, then it'll be of captured
    ## by try().
    if (inherits(msg, "try-error")) {
      ex <- FutureError(msg, future = future)
      future$result <- ex
      stop(ex)
    }
    
    node_info <- sprintf("%s #%d", sQuote(class(node)[1]), node_idx)
    if (inherits(node, "RichSOCKnode")) {
      specs <- summary(node)
      node_info <- sprintf("%s on host %s (%s, platform %s)",
                           node_info, sQuote(specs[["host"]]),
                           specs[["r_version"]], specs[["platform"]])
    }
    
    hint <- sprintf("This suggests that the communication with %s is out of sync.", node_info)
    ex <- UnexpectedFutureResultError(future, hint = hint)
    future$result <- ex
    stop(ex)
  }

  if (inherits(msg, "FutureResult")) {
    result <- msg

    if (inherits(future$.journal, "FutureJournal")) {
      appendToFutureJournal(future,
           event = "receiveResult",
        category = "overhead",
          parent = "gather",
           start = t_start,
            stop = Sys.time()
      )
    }

    ## Add back already signaled and muffled conditions so that also
    ## they will be resignaled each time value() is called.
    signaled <- future$.signaledConditions
    if (length(signaled) > 0) {
      result$conditions <- c(future$.signaledConditions, result$conditions)
      future$.signaledConditions <- NULL
    }

    future$result <- result
    future$state <- "finished"
    if (debug) mdebug("- Received FutureResult")
  
    ## FutureRegistry to use
    workers <- future$workers
    reg <- sprintf("workers-%s", attr(workers, "name", exact = TRUE))
  
    ## Remove from registry
    FutureRegistry(reg, action = "remove", future = future, earlySignal = FALSE)
    if (debug) mdebug("- Erased future from FutureRegistry")

    ## Always signal immediateCondition:s and as soon as possible.
    ## They will always be signaled if they exist.
    signalImmediateConditions(future)
  
    ## Garbage collect cluster worker?
    if (future$gc) {
      if (debug) mdebug("- Garbage collecting worker ...")
      ## Cleanup global environment while at it
      if (!future$persistent) cluster_call(cl[1], fun = grmall, future = future, when = "call grmall() on")
      
      ## WORKAROUND: Need to clear cluster worker before garbage collection.
      ## This is needed for workers running R (<= 3.3.1). It will create
      ## another teeny, dummy object on the worker allowing any previous
      ## objects to be garbage collected.  For more details, see
      ## https://github.com/HenrikBengtsson/Wishlist-for-R/issues/27.
      ## (We return a value identifiable for troubleshooting purposes)
      cluster_call(cl[1], function() "future-clearing-cluster-worker", future = future, when = "call dummy() on")
      
      cluster_call(cl[1], gc, verbose = FALSE, reset = FALSE, future = future, when = "call gc() on")
      if (debug) mdebug("- Garbage collecting worker ... done")
    }
  } else if (inherits(msg, "condition")) {
    condition <- msg
    
    if (debug) {
      mdebug("- Received condition")
      mstr(condition)
    }

    ## Sanity check
    if (inherits(condition, "error")) {
      label <- future$label
      if (is.null(label)) label <- "<none>"
      stop(FutureError(sprintf("Received a %s condition from the %s worker for future ('%s'), which is not possible to relay because that would break the internal state of the future-worker communication. The condition message was: %s", class(condition)[1], class(future)[1], label, sQuote(conditionMessage(condition))), future = future))
    }

    ## Resignal condition
    if (inherits(condition, "warning")) {
      warning(condition)
    } else if (inherits(condition, "message")) {
      message(condition)
    } else {
      signalCondition(condition)
    }

    ## Increment signal count
    signaled <- condition$signaled
    if (is.null(signaled)) signaled <- 0L
    condition$signaled <- signaled + 1L
    
    ## Record condition as signaled
    signaled <- future$.signaledConditions
    if (is.null(signaled)) signaled <- list()
    signaled <- c(signaled, list(condition))
    future$.signaledConditions <- signaled
  }

  msg
}


requestNode <- function(await, workers, timeout = getOption("future.wait.timeout", 30 * 24 * 60 * 60), delta = getOption("future.wait.interval", 0.01), alpha = getOption("future.wait.alpha", 1.01)) {
  debug <- getOption("future.debug", FALSE)
  
  stop_if_not(inherits(workers, "cluster"))
  stop_if_not(is.function(await))
  stop_if_not(is.finite(timeout), timeout >= 0)
  stop_if_not(is.finite(alpha), alpha > 0)

  ## Maximum number of nodes available
  total <- length(workers)

  ## FutureRegistry to use
  name <- attr(workers, "name", exact = TRUE)
  stop_if_not(is.character(name), length(name) == 1L)
  reg <- sprintf("workers-%s", name)
  
  usedNodes <- function() {
    ## Number of unresolved cluster futures
    length(FutureRegistry(reg, action = "list", earlySignal = FALSE))
  }


  t0 <- Sys.time()
  dt <- 0
  iter <- 1L
  interval <- delta
  finished <- FALSE
  while (dt <= timeout) {
    ## Check for available nodes
    used <- usedNodes()
    finished <- (used < total)
    if (finished) break

    if (debug) mdebugf("Poll #%d (%s): usedNodes() = %d, workers = %d", iter, format(round(dt, digits = 2L)), used, total)

    ## Wait
    Sys.sleep(interval)
    interval <- alpha * interval
    
    ## Finish/close workers, iff possible
    await()

    iter <- iter + 1L
    dt <- difftime(Sys.time(), t0)
  }

  if (!finished) {
    msg <- sprintf("TIMEOUT: All %d cluster nodes are still occupied after %s (polled %d times)", total, format(round(dt, digits = 2L)), iter)
    if (debug) mdebug(msg)
    ex <- FutureError(msg, future = future)
    stop(ex)
  }

  ## Find which node is available
  avail <- rep(TRUE, times = length(workers))
  futures <- FutureRegistry(reg, action = "list", earlySignal = FALSE)
  nodes <- unlist(lapply(futures, FUN = function(f) f$node), use.names = FALSE)
  avail[nodes] <- FALSE

  ## Sanity check
  stop_if_not(any(avail))

  node_idx <- which(avail)[1L]
  stop_if_not(is.numeric(node_idx), is.finite(node_idx), node_idx >= 1)

  node_idx
}



#' @export
getExpression.ClusterFuture <- local({
  tmpl_expr_conditions <- future:::bquote_compile({
    "# future:::getExpression.ClusterFuture(): inject code for instant"
    "# relaying of 'immediateCondition' objects back to the parent R  "
    "# process via the existing PSOCK channel                         "
    ...future.makeSendCondition <- base::local({
      sendCondition <- NULL

      function(frame = 1L) {
        if (is.function(sendCondition)) return(sendCondition)

        ns <- getNamespace("parallel")
        if (exists("sendData", mode = "function", envir = ns)) {
          parallel_sendData <- get("sendData", mode = "function", envir = ns)

          ## Find the 'master' argument of the worker's {slave,work}Loop()
          envir <- sys.frame(frame)
          master <- NULL
          while (!identical(envir, .GlobalEnv) && !identical(envir, emptyenv())) {
            if (exists("master", mode = "list", envir = envir, inherits=FALSE)) {
              master <- get("master", mode = "list", envir = envir, inherits = FALSE)
              if (inherits(master, c("SOCKnode", "SOCK0node"))) {
                sendCondition <<- function(cond) {
                  data <- list(type = "VALUE", value = cond, success = TRUE)
                  parallel_sendData(master, data)
                }
                return(sendCondition)
              }
            }
            frame <- frame + 1L
            envir <- sys.frame(frame)
          }
        }  

        ## Failed to locate 'master' or 'parallel:::sendData()',
        ## so just ignore conditions
        sendCondition <<- function(cond) NULL
      }
    })

    withCallingHandlers({
      .(expr)
    }, immediateCondition = function(cond) {
      sendCondition <- ...future.makeSendCondition()
      sendCondition(cond)

      ## Avoid condition from being signaled more than once
      ## muffleCondition <- future:::muffleCondition()
      muffleCondition <- .(muffleCondition)
      muffleCondition(cond)
    })
  })


  function(future, expr = future$expr, immediateConditions = TRUE, ...) {
    ## Assert that no arguments but the first is passed by position
    assert_no_positional_args_but_first()
  
    ## Inject code for resignaling immediateCondition:s?
    if (immediateConditions) {
      resignalImmediateConditions <- getOption("future.psock.relay.immediate", TRUE)
      if (resignalImmediateConditions) {
        immediateConditionClasses <- getOption("future.relay.immediate", "immediateCondition")
        if (length(immediateConditionClasses) > 0L) {
          ## Does the cluster node communicate with a connection?
          ## (if not, it's via MPI)
          workers <- future$workers
          ## AD HOC/FIXME: Here 'future$node' is yet not assigned, so we look at
          ## the first worker and assume the others are the same. /HB 2019-10-23
          cl <- workers[1L]
          node <- cl[[1L]]
          con <- node$con
          if (!is.null(con)) {
            expr <- bquote_apply(tmpl_expr_conditions)
          } ## if (!is.null(con))
        }
      }
    } ## if (resignalImmediateConditions && immediateConditions)
    
    NextMethod(expr = expr, immediateConditions = immediateConditions)
  }
})


send_call <- function(node, ..., when = "send call to", future) {
  sendCall <- importParallel("sendCall")
  tryCatch({
    sendCall(node, ...)
  }, error = function(ex) {
    msg <- post_mortem_cluster_failure(ex, when = when, node = node, future = future)
    ex <- FutureError(msg, future = future)
    stop(ex)          
  })
}


#' @importFrom parallel clusterCall
cluster_call <- function(cl, ..., when = "call function on", future) {
  stop_if_not(inherits(cl, "cluster"), length(cl) == 1L)
  stop_if_not(inherits(future, "Future"))
  node <- cl[[1]]
  
  tryCatch({
    clusterCall(cl = cl, ...)
  }, error = function(ex) {
    msg <- post_mortem_cluster_failure(ex, when = when, node = node, future = future)
    ex <- FutureError(msg, future = future)
    future$result <- ex
    stop(ex)          
  })
}


#' @importFrom parallelly isNodeAlive
post_mortem_cluster_failure <- function(ex, when, node, future) {
  stop_if_not(inherits(ex, "error"))
  stop_if_not(length(when) == 1L, is.character(when))
  stop_if_not(inherits(future, "Future"))
  
  node_idx <- future$node
  if (is.null(node_idx)) {
    node_idx <- NA_integer_
  } else {
    stop_if_not(length(node_idx) == 1L, is.numeric(node_idx))
    node_idx <- as.integer(node_idx)
  }
  
  ## (1) Trimmed error message
  reason <- conditionMessage(ex)

  ## (2) Information on the cluster node
  
  ## (a) Process information on the worker, if available
  pid <- node$session_info$process$pid
  pid_info <- if (is.numeric(pid)) sprintf("PID %.0f", pid) else NULL

  ## (b) Host information on the worker, if available
  ##     AD HOC: This assumes that the worker has a hostname, which is not
  ##     the case for MPI workers. /HB 2017-03-07
  host <- node$host
  localhost <- isTRUE(attr(host, "localhost", exact = TRUE))
  host_info <- if (!is.null(host)) {
    sprintf("on %s%s", if (localhost) "localhost " else "", sQuote(host))
  } else NULL
  
  node_info <- sprintf("cluster %s #%d (%s)",
                       class(node)[1], node_idx,
                       paste(c(pid_info, host_info), collapse = " "))
  stop_if_not(length(node_info) == 1L)
  
  ## (3) Information on the future
  label <- future$label
  if (is.null(label)) label <- "<none>"
  stop_if_not(length(label) == 1L)

  ## (4) POST-MORTEM ANALYSIS:
  postmortem <- list()
  ## (a) Did the worker process terminate?
  if (!is.null(host) && is.numeric(pid)) {
    if (localhost) {
      pid_exists <- import_parallelly("pid_exists")
      alive <- pid_exists(pid)
      if (is.na(alive)) {
        msg2 <- "Failed to determined whether a process with this PID exists or not, i.e. cannot infer whether localhost worker is alive or not"
      } else if (alive) {
        msg2 <- "A process with this PID exists, which suggests that the localhost worker is still alive"
      } else {
        msg2 <- "No process exists with this PID, i.e. the localhost worker is no longer alive"
      }
    } else {
      ## Checking remote workers on hosts requires parallelly (>= 1.36.0)
      alive <- isNodeAlive(node, timeout = getOption("future.alive.timeout", 30.0))
      if (is.na(alive)) {
        msg2 <- "Failed to determined whether the process with this PID exists or not on the remote host, i.e. cannot infer whether remote worker is alive or not"
      } else if (alive) {
        msg2 <- "A process with this PID exists on the remote host, which suggests that the remote worker is still alive"
      } else {
        msg2 <- "No process exists with this PID on the remote host, i.e. the remote worker is no longer alive"
      }
    }
    postmortem$alive <- msg2
  }

  ## (b) Did the worker use a connection that changed?
  if (inherits(node$con, "connection")) {
    postmortem$connection <- check_connection_details(node, future = future)
  }

  ## (c) Any non-exportable globals?
  globals <- globals(future)
  postmortem$non_exportable <- assert_no_references(globals, action = "string")

  ## (d) Size of globals
  postmortem$global_sizes <- summarize_size_of_globals(globals)

  ## (5) The final error message
  msg <- sprintf("%s (%s) failed to %s %s. The reason reported was %s",
                 class(future)[1], label, when, node_info, sQuote(reason))
  stop_if_not(length(msg) == 1L)
  if (length(postmortem) > 0) {
    postmortem <- unlist(postmortem, use.names = FALSE)
    msg <- sprintf("%s. Post-mortem diagnostic: %s",
                   msg, paste(postmortem, collapse = ". "))
    stop_if_not(length(msg) == 1L)
  }

  msg
} # post_mortem_cluster_failure()
