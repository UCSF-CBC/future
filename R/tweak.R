#' Tweak a future function by adjusting its default arguments
#'
#' @param strategy An existing future function or the name of one.
#' @param \dots Named arguments to replace the defaults of existing
#' arguments.
#' @param penvir The environment used when searching for a future
#' function by its name.
#'
#' @return a future function.
#'
#' @seealso
#' Use [plan()] to set a future to become the
#' new default strategy.
#'
#' @export
tweak <- function(strategy, ..., penvir = parent.frame()) UseMethod("tweak")

#' @export
tweak.character <- function(strategy, ..., penvir = parent.frame()) {
  parts <- strsplit(strategy, split = "::", fixed = TRUE)[[1]]
  nparts <- length(parts)
  if (nparts == 2) {
    envir <- getNamespace(parts[[1]])
    s <- parts[[2]]
    if (!exists(s, mode = "function", envir = envir, inherits = TRUE)) {
      stopf("No such strategy for futures: %s", sQuote(strategy))
    }
    strategy <- get(s, mode = "function", envir = envir, inherits = TRUE)
  } else {
    ## Search attached packages and the 'future' package
    ## for a future function with this name
    envirs <- list(penvir, future = getNamespace("future"), NULL)
    for (envir in envirs) {
      ## Reached the end? Nothing found.
      if (is.null(envir)) {
        stopf("No such strategy for futures: %s", sQuote(strategy))
      }
  
      if (exists(strategy, mode = "function", envir = envir, inherits = TRUE)) {
        strategy <- get(strategy, mode = "function", envir = envir, inherits = TRUE)
        break
      }
    }
  }

  ## Sanity check
  stop_if_not(is.function(strategy))

  tweak(strategy, ..., penvir = penvir)
}

#' @export
tweak.future <- function(strategy, ..., penvir = parent.frame()) {
  args <- list(...)

  ## Nothing to tweak?
  if (length(args) == 0L) return(strategy)

  names <- names(args)
  if (is.null(names)) {
    stop("Additional arguments to tweak() must be named.")
  }

  ## Identify arguments that must not be tweaked
  
  ## (i) All future strategies inherits from the 'future' class
  untweakable <- attr(future, "untweakable")
  tweakable <- eval(attr(future, "tweakable"))

  ## (ii) Others that are specific to this future strategy, if any
  for (class in class(strategy)) {
    if (class == "future") break
    if (!exists(class, mode = "function")) next
    fcn <- get(class, mode = "function")
    if (!inherits(fcn, "future")) next
    untweakable <- c(attr(fcn, "untweakable"), untweakable)
    tweakable <- c(eval(attr(fcn, "tweakable")), tweakable)
  }
  
  ## Add temporary, secret option for disabling these checks in case to
  ## give users some time to sort out legacy mistakes
  untweakable <- getOption("future.tweak.untweakable", untweakable)
  if (any(names %in% untweakable)) {
    untweakable <- intersect(names, untweakable)
    untweakable <- commaq(untweakable)
    stopf("Detected arguments that must not be set via plan() or tweak(): %s",
         untweakable)
  }
  
  ## formals()<- drops any attributes including class
  attrs <- attributes(strategy)
  class <- class(strategy)

  ## Tweak arguments
  formals <- names(formals(strategy))

  known <- c(formals, names(formals(future)), tweakable)
  unknown <- setdiff(names, known)
  if (length(unknown) > 0L) {
    warnf("Detected %d unknown future arguments: %s", length(unknown), commaq(unknown))
  }

  strategy2 <- function(...) NULL
  args2 <- args
  for (kk in seq_along(args)) {
    name <- names[kk]
    value <- args[[name]]
    if (is.call(value)) {
      ## enquote()
      value <- as.call(list(quote(quote), value))
    }
    formals(strategy2)[name] <- list(value)
    args2[[name]] <- as.symbol(name)
  }

  ## Arguments 'envir' and 'workers' must exist in the wrapper, if
  ## they exist in the "future" function
  formals2 <- names(formals(strategy2))
  for (name in c("workers", "envir")) {
    if (is.element(name, formals) && !is.element(name, formals2)) {
      formals(strategy2) <- c(formals(strategy2), formals(strategy)[name])
      args2[[name]] <- as.symbol(name)
    }
  }

  body(strategy2) <- bquote_splice(strategy(..., ..(args2)), splice = TRUE)

  ## Avoid strategy2() depending on the calling frame, which would cause it
  ## to pick up package dependencies from there, which then are attached on
  ## the future worker.
  environment(strategy2) <- new.env(parent = environment(strategy))
  environment(strategy2)$strategy <- strategy

  ## Restore attributes including class
  attributes(strategy2) <- attrs

  ## Flag that it is tweaked
  class(strategy2) <- c("tweaked", class)

  strategy2
} ## tweak()


#' @export
tweak.function <- function(strategy, ...) {
  strategy_name <- NULL

  ## Try to find the name of the function
  env <- environment(strategy)
  env_name <- environmentName(env)
  if (nchar(env_name) == 0) env_name <- "<unknown>"

  names <- ls(envir = env, all.names = TRUE)
  if (length(names) > 0) {
    is_fcn <- sapply(names, FUN = exists, mode = "function",
                     envir = env, inherits = FALSE)
    names <- names[is_fcn]
    if (length(names) > 0) {
      for (name in names) {
        fcn <- get(name, mode = "function", envir = env, inherits = FALSE)
        if (identical(fcn, strategy)) {
          strategy_name <- sprintf("%s::%s", env_name, name)
          break
        }
      }
    }
  }

  msg <- "Trying to use non-future function"
  if (!is.null(strategy_name)) {
    msg <- sprintf("%s %s", msg, sQuote(strategy_name))
  } else if (nzchar(env_name)) {
    msg <- sprintf("%s from environment / package %s", msg, sQuote(env_name))
  }
  
  args <- deparse(args(strategy), width.cutoff = 500L)[1L]
  msg <- sprintf("%s: %s { ... }", msg, args)
  stop(msg)
}
