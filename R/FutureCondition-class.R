#' A condition (message, warning, or error) that occurred while orchestrating a future
#'
#' While _orchestrating_ (creating, launching, querying, collection)
#' futures, unexpected run-time errors (and other types of conditions) may
#' occur.  Such conditions are coerced to a corresponding FutureCondition
#' class to help distinguish them from conditions that occur due to the
#' _evaluation_ of the future.
#' 
#' @param message A message condition.
#' 
#' @param call The call stack that led up to the condition.
#' 
#' @param uuid A universally unique identifier for the future associated with
#' this FutureCondition.
#' 
#' @param future The [Future] involved.
#' 
#' @return An object of class FutureCondition which inherits from class
#' \link[base:conditions]{condition} and FutureMessage, FutureWarning,
#' and FutureError all inherits from FutureCondition.
#' Moreover, a FutureError inherits from \link[base:conditions]{error},
#' a FutureWarning from \link[base:conditions]{warning}, and
#' a FutureMessage from \link[base:conditions]{message}.
#'
#' @export
#' @keywords internal
FutureCondition <- function(message, call = NULL, uuid = future$uuid, future = NULL) {
  ## Support different types of input
  if (inherits(message, "condition")) {
    cond <- message
    message <- conditionMessage(cond)
    class <- class(cond)
  } else if (is.null(message)) {
    stop("INTERNAL ERROR: Trying to set up a FutureCondition with message = NULL")
  } else {
    class <- "condition"
  }

  message <- as.character(message)
  if (length(message) != 1L) {
    stopf("INTERNAL ERROR: Trying to set up a FutureCondition with length(message) != 1L: %d", length(message))
  }

  if (!is.null(uuid)) {
    stop_if_not(is.character(uuid), length(uuid) == 1L, !is.na(uuid))
  }
  if (!is.null(future)) stop_if_not(inherits(future, "Future"))

  if (!getOption("future.onFutureCondition.keepFuture", TRUE)) {
    future <- NULL
  }
  
  ## Create a condition object
  class <- c("FutureCondition", class)
  structure(list(message = message, call = call), 
            class = class[!duplicated(class, fromLast = TRUE)],
            uuid = uuid, future = future)
}


#' @importFrom utils tail
#' @export
print.FutureCondition <- function(x, ...) {
  NextMethod()

  uuid <- attr(x, "uuid", exact = TRUE)
  cat(sprintf("\n\nFuture UUID: %s\n", if (is.null(uuid)) "<NA>" else uuid))

  future <- attr(x, "future", exact = TRUE)

  if (!is.null(future)) {
    cat("\n\nDEBUG: BEGIN TROUBLESHOOTING HELP\n")

    if (!is.null(future)) {
      cat("Future involved:\n")
      print(future)
      cat("\n")
    }

    cat("DEBUG: END TROUBLESHOOTING HELP\n")
  }

  invisible(x)
} ## print()



#' @rdname FutureCondition
#' @export
FutureMessage <- function(message, call = NULL, uuid = future$uuid, future = NULL) {
  cond <- FutureCondition(message = message, call = call, uuid = uuid, future = future)
  class <- c("FutureMessage", "message", class(cond))
  class(cond) <- class[!duplicated(class, fromLast = TRUE)]
  cond
}


#' @rdname FutureCondition
#' @export
FutureWarning <- function(message, call = NULL, uuid = future$uuid, future = NULL) {
  cond <- FutureCondition(message = message, call = call, uuid = uuid, future = future)
  class <- c("FutureWarning", "warning", class(cond))
  class(cond) <- class[!duplicated(class, fromLast = TRUE)]
  cond
}


#' @rdname FutureCondition
#' @export
FutureError <- function(message, call = NULL, uuid = future$uuid, future = NULL) {
  cond <- FutureCondition(message = message, call = call, uuid = uuid, future = future)
  class <- c("FutureError", "error", class(cond))
  class(cond) <- class[!duplicated(class, fromLast = TRUE)]
  cond
}


#' @rdname FutureCondition
#' @export
RngFutureCondition <- function(message = NULL, call = NULL, uuid = future$uuid, future = NULL) {
  if (is.null(message)) {
    label <- future$label
    if (is.null(label)) label <- "<none>"
    message <- sprintf("UNRELIABLE VALUE: Future (%s) unexpectedly generated random numbers without specifying argument 'seed'. There is a risk that those random numbers are not statistically sound and the overall results might be invalid. To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe random numbers are produced via the L'Ecuyer-CMRG method. To disable this check, use 'seed=NULL', or set option 'future.rng.onMisuse' to \"ignore\".", sQuote(label))
  }
  cond <- FutureCondition(message = message, call = call, uuid = uuid, future = future)
  class <- c("RngFutureCondition", class(cond))
  class(cond) <- class[!duplicated(class, fromLast = TRUE)]
  cond
}

#' @rdname FutureCondition
#' @export
RngFutureWarning <- function(...) {
  cond <- RngFutureCondition(...)
  class <- c("RngFutureWarning", "FutureWarning", "warning", class(cond))
  class(cond) <- class[!duplicated(class, fromLast = TRUE)]
  cond
}

#' @rdname FutureCondition
#' @export
RngFutureError <- function(...) {
  cond <- RngFutureCondition(...)
  class <- c("RngFutureError", "FutureError", "error", class(cond))
  class(cond) <- class[!duplicated(class, fromLast = TRUE)]
  cond
}


#' @param hint (optional) A string with a suggestion on what might be wrong.
#'
#' @rdname FutureCondition
#' @export
UnexpectedFutureResultError <- function(future, hint = NULL) {
  label <- future$label
  if (is.null(label)) label <- "<none>"
  expr <- hexpr(future$expr)
  result <- future$result
  result_string <- hpaste(as.character(result))
  if (length(result_string) == 0L)
    result_string <- ""
  else if (nchar(result_string) > 512L)
    result_string <- paste(substr(result_string, start = 1L, stop = 512L),
                           "...")
  if (!is.null(hint)) {
    result_string <- if (nzchar(result_string)) {
      sprintf("%s. %s", result_string, hint)
    } else {
      hint
    }
  }
  msg <- sprintf("Unexpected result (of class %s != %s) retrieved for %s future (label = %s, expression = %s): %s",
                 sQuote(class(result)[1]), sQuote("FutureResult"),
                 class(future)[1], sQuote(label), sQuote(expr),
                 result_string)
  cond <- FutureError(msg, future = future)
  class <- c("UnexpectedFutureResultError", class(cond))
  class(cond) <- class[!duplicated(class, fromLast = TRUE)]
  cond
}



#' @rdname FutureCondition
#' @export
GlobalEnvFutureCondition <- function(message = NULL, call = NULL, globalenv = globalenv, uuid = future$uuid, future = NULL) {
  if (is.null(message)) {
    label <- future$label
    if (is.null(label)) label <- "<none>"
    message <- sprintf("Future (%s) added variables to the global environment. A future expression should never assign variables to the global environment - neither by assign() nor by <<-: [n=%d] %s", label, length(globalenv$added), commaq(globalenv$added))
  }
  cond <- FutureCondition(message = message, call = call, uuid = uuid, future = future)
  cond$globalenv <- globalenv
  class <- c("GlobalEnvFutureCondition", class(cond))
  class(cond) <- class[!duplicated(class, fromLast = TRUE)]
  cond
}

#' @rdname FutureCondition
#' @export
GlobalEnvFutureWarning <- function(...) {
  cond <- GlobalEnvFutureCondition(...)
  class <- c("GlobalEnvFutureWarning", "FutureWarning", "warning", class(cond))
  class(cond) <- class[!duplicated(class, fromLast = TRUE)]
  cond
}

#' @rdname FutureCondition
#' @export
GlobalEnvFutureError <- function(...) {
  cond <- GlobalEnvFutureCondition(...)
  class <- c("GlobalEnvFutureError", "FutureError", "error", class(cond))
  class(cond) <- class[!duplicated(class, fromLast = TRUE)]
  cond
}
