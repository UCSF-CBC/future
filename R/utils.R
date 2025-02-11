isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

isNA <- function(x) {
  is.logical(x) && length(x) == 1L && is.na(x)
}

assert_no_positional_args_but_first <- function(call = sys.call(sys.parent())) {
  ast <- as.list(call)
  if (length(ast) <= 2L) return()
  ast <- ast[-(1:2)]
  dots <- vapply(ast, FUN = identical, as.symbol("..."), FUN.VALUE = FALSE)
  ast <- ast[!dots]
  if (length(ast) == 0L) return()
  names <- names(ast)
  if (is.null(names) || any(names == "")) {    
    stopf("Function %s() requires that all arguments beyond the first one are passed by name and not by position: %s", as.character(call[[1L]]), deparse(call, width.cutoff = 100L))
  }
}

stop_if_not <- function(...) {
  res <- list(...)
  for (ii in 1L:length(res)) {
    res_ii <- .subset2(res, ii)
    if (length(res_ii) != 1L || is.na(res_ii) || !res_ii) {
        mc <- match.call()
        call <- deparse(mc[[ii + 1]], width.cutoff = 60L)
        if (length(call) > 1L) call <- paste(call[1L], "....")
        stopf("%s is not TRUE", sQuote(call), call. = FALSE, domain = NA)
    }
  }
  
  NULL
}

## From R.utils 2.0.2 (2015-05-23)
hpaste <- function(..., sep = "", collapse = ", ", lastCollapse = NULL, maxHead = if (missing(lastCollapse)) 3 else Inf, maxTail = if (is.finite(maxHead)) 1 else Inf, abbreviate = "...") {
  if (is.null(lastCollapse)) lastCollapse <- collapse

  # Build vector 'x'
  x <- paste(..., sep = sep)
  n <- length(x)

  # Nothing todo?
  if (n == 0) return(x)
  if (is.null(collapse)) return(x)

  # Abbreviate?
  if (n > maxHead + maxTail + 1) {
    head <- x[seq_len(maxHead)]
    tail <- rev(rev(x)[seq_len(maxTail)])
    x <- c(head, abbreviate, tail)
    n <- length(x)
  }

  if (!is.null(collapse) && n > 1) {
    if (lastCollapse == collapse) {
      x <- paste(x, collapse = collapse)
    } else {
      xT <- paste(x[1:(n-1)], collapse = collapse)
      x <- paste(xT, x[n], sep = lastCollapse)
    }
  }

  x
} # hpaste()


trim <- function(s) {
  sub("[\t\n\f\r ]+$", "", sub("^[\t\n\f\r ]+", "", s))
} # trim()

comma <- function(x, sep = ", ") paste(x, collapse = sep)

commaq <- function(x, sep = ", ") paste(sQuote(x), collapse = sep)

hexpr <- function(expr, trim = TRUE, collapse = "; ", maxHead = 6L, maxTail = 3L, ...) {
  code <- deparse(expr, width.cutoff = 60L, nlines = getOption("future.hexpr.nlines", 100L))
  if (trim) code <- trim(code)
  hpaste(code, collapse = collapse, maxHead = maxHead, maxTail = maxTail, ...)
} # hexpr()


## From R.filesets
asIEC <- function(size, digits = 2L) {
  if (length(size) > 1L) return(sapply(size, FUN = asIEC, digits = digits))
  units <- c("bytes", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB")
  for (unit in units) {
    if (size < 1000) break;
    size <- size / 1024
  }

  if (unit == "bytes") {
    fmt <- sprintf("%%.0f %s", unit)
  } else {
    fmt <- sprintf("%%.%df %s", digits, unit)
  }
  sprintf(fmt, size)
} # asIEC()

#' @importFrom utils capture.output
envname <- function(env) {
  if (!is.environment(env)) return(NA_character_)
  name <- environmentName(env)
  if (name == "") {
    ## NOTE: I might be that:
    ## 1. 'env' is of a class that extends 'environment', e.g.
    ##    R.oo::Object() or R6::R6Class(), or
    ## 2. another package defines print() for 'environment'
    ## Because of this, we call print.default() instead of generic print().
    name <- capture.output(print.default(env))
    if (length(name) > 1L) name <- name[1]
    name <- gsub("(.*: |>)", "", name)
  } else {
    ## e.g. globals:::where("plan")
    name <- gsub("package:", "", name, fixed = TRUE)
  }
  name
}


inherits_from_namespace <- function(env) {
  while (!identical(env, emptyenv())) {
    if (is.null(env)) return(TRUE) ## primitive functions, e.g. base::sum()
    if (isNamespace(env)) return(TRUE)
    if (identical(env, globalenv())) return(FALSE)
    env <- parent.env(env)
  }
  FALSE
}


## Assign globals to an specific environment and set that environment
## for functions.  If they are functions of namespaces/packages
## and exclude == "namespace", then the globals are not assigned
## Reference: https://github.com/futureverse/future/issues/515
assign_globals <- function(envir, globals, exclude = getOption("future.assign_globals.exclude", c("namespace")), debug = getOption("future.debug", FALSE)) {
  stop_if_not(is.environment(envir), is.list(globals))
  if (length(globals) == 0L) return(envir)

  if (debug) {
    mdebug("assign_globals() ...")
    mstr(globals)
  }
  
  exclude_namespace <- ("namespace" %in% exclude)

  names <- names(globals)
  where <- attr(globals, "where")
  for (name in names) {
    global <- globals[[name]]

    if (exclude_namespace) {
      e <- environment(global)
      if (!is.null(e) && !inherits_from_namespace(e)) {
        w <- where[[name]]

        ## If global has 'where' with emptyenv() it means it was
        ## specified via 'globals' argument. For this closure
        ## (function or formula) to find its free variables,
        ## it's environment needs to be reassigned to the 'envir'
        ## environment.
        ## Related to:
        ## * https://github.com/futureverse/future/issues/475
        ## * https://github.com/futureverse/future/issues/515
        ## * https://github.com/futureverse/future/issues/608
        if (identical(w, emptyenv())) {
          environment(global) <- envir
          if (debug) mdebugf("- reassign environment for %s [where=emptyenv()]", sQuote(name))
        } else if (identical(w, globalenv()) && identical(environment(global), globalenv())) {
          environment(global) <- envir
          if (debug) mdebugf("- reassign environment for %s [where=globalenv()]", sQuote(name))
        }

      }
    }
    
    envir[[name]] <- global
    if (debug) mdebugf("- copied %s to environment", sQuote(name))
  }


  if (debug) mdebug("assign_globals() ... done")

  invisible(envir)
}


now <- function(x = Sys.time(), format = "[%H:%M:%OS3] ") {
  ## format(x, format = format) ## slower
  format(as.POSIXlt(x, tz = ""), format = format)
}

mdebug <- function(..., prefix = now(), debug = getOption("future.debug", FALSE)) {
  if (!debug) return()
  message(prefix, ...)
}

mdebugf <- function(..., appendLF = TRUE,
                    prefix = now(), debug = getOption("future.debug", FALSE)) {
  if (!debug) return()
  message(prefix, sprintf(...), appendLF = appendLF)
}

#' @importFrom utils capture.output
mprint <- function(..., appendLF = TRUE, prefix = now(), debug = getOption("future.debug", FALSE)) {
  if (!debug) return()
  message(paste(prefix, capture.output(print(...)), sep = "", collapse = "\n"), appendLF = appendLF)
}

#' @importFrom utils capture.output str
mstr <- function(..., appendLF = TRUE, prefix = now(), debug = getOption("future.debug", FALSE)) {
  if (!debug) return()
  message(paste(prefix, capture.output(str(...)), sep = "", collapse = "\n"), appendLF = appendLF)
}


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Used by run() for ClusterFuture.
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Because these functions are exported, we want to keep their
## environment() as small as possible, which is why we use local().
## Without, the environment would be that of the package itself
## and all of the package content would be exported.

## Removes all variables in the global environment.
grmall <- local(function(envir = .GlobalEnv) {
  vars <- ls(envir = envir, all.names = TRUE)
  rm(list = vars, envir = envir, inherits = FALSE)
  ## Return a value identifiable for troubleshooting purposes
  invisible("future-grmall")
})

## Assigns a value to the global environment.
gassign <- local(function(name, value, envir = .GlobalEnv) {
  assign(name, value = value, envir = envir)
  ## Return a value identifiable for troubleshooting purposes
  invisible("future-grassign")
})

## Evaluates an expression in global environment.
geval <- local(function(expr, substitute = FALSE, envir = .GlobalEnv, enclos = baseenv(), ...) {
  if (substitute) expr <- substitute(expr)
  eval(expr, envir = envir, enclos = enclos)
})

## Vectorized version of require() with bells and whistles
requirePackages <- local(function(pkgs) {
  requirePackage <- function(pkg) {
    if (require(pkg, character.only = TRUE)) return()

    ## Failed to attach package
    msg <- sprintf("Failed to attach package %s in %s", sQuote(pkg), R.version$version.string)
    data <- utils::installed.packages()

    ## Installed, but fails to load/attach?
    if (is.element(pkg, data[, "Package"])) {
      keep <- (data[, "Package"] == pkg)
      data <- data[keep, ,drop = FALSE]
      pkgs <- sprintf("%s %s (in %s)", data[, "Package"], data[, "Version"], sQuote(data[, "LibPath"]))
      msg <- sprintf("%s, although the package is installed: %s", msg, paste(pkgs, collapse = ", "))
    } else {
      paths <- .libPaths()
      msg <- sprintf("%s, because the package is not installed in any of the libraries (%s), which contain %d installed packages.", msg, commaq(paths), nrow(data))
    }

    stop(msg)
  } ## requirePackage()

  ## require() all packages
  pkgs <- unique(pkgs)
  lapply(pkgs, FUN = requirePackage)
}) ## requirePackages()


if (getRversion() < "4.0.0") {
  ## When 'default' is specified, this is 30x faster than
  ## base::getOption().  The difference is that here we use
  ## use names(.Options) whereas in 'base' names(options())
  ## is used.
  getOption <- local({
    go <- base::getOption
    function(x, default = NULL) {
      if (missing(default) || match(x, table = names(.Options), nomatch = 0L) > 0L) go(x) else default
    }
  })
}


parseCmdArgs <- function() {
  cmdargs <- getOption("future.cmdargs", commandArgs())
  args <- list()

  ## Option --parallel=<n> or -p <n>
  idx <- grep("^(-p|--parallel=.*)$", cmdargs)
  if (length(idx) > 0) {
    ## Use only last, iff multiple are given
    if (length(idx) > 1) idx <- idx[length(idx)]

    cmdarg <- cmdargs[idx]
    if (cmdarg == "-p") {
      cmdarg <- cmdargs[idx+1L]
      value <- as.integer(cmdarg)
      cmdarg <- sprintf("-p %s", cmdarg)
    } else {
      value <- as.integer(gsub("--parallel=", "", cmdarg))
    }

    max <- availableCores(methods = "system")
    if (is.na(value) || value <= 0L) {
      msg <- sprintf("future: Ignoring invalid number of processes specified in command-line option: %s", cmdarg)
      warning(msg, call. = FALSE, immediate. = TRUE)
    } else if (value > max) {
      msg <- sprintf("future: Ignoring requested number of processes, because it is greater than the number of cores/child processes available (= %d) to this R process: %s", max, cmdarg)
      warning(msg, call. = FALSE, immediate. = TRUE)
    } else {
      args$p <- value
    }
  }

  args
} # parseCmdArgs()


#' Gets the length of an object without dispatching
#'
#' @param x Any \R object.
#'
#' @return A non-negative integer.
#'
#' @details
#' This function returns `length(unclass(x))`, but tries to avoid
#' calling `unclass(x)` unless necessary.
#' 
#' @seealso \code{\link{.subset}()} and \code{\link{.subset2}()}.
#' 
#' @keywords internal
#' @rdname private_length
#' @importFrom utils getS3method
.length <- function(x) {
  nx <- length(x)
  
  ## Can we trust base::length(x), i.e. is there a risk that there is
  ## a method that overrides with another definition?
  classes <- class(x)
  if (length(classes) == 1L && classes == "list") return(nx)

  ## Identify all length() methods for this object
  for (class in classes) {
    fun <- getS3method("length", class, optional = TRUE)
    if (!is.null(fun)) {
      nx <- length(unclass(x))
      break
    }
  }

  nx
} ## .length()


#' Creates a connection to the system null device
#'
#' @return Returns a open, binary \code{\link[base:connections]{base::connection()}}.
#' 
#' @keywords internal
nullcon <- local({
  nullfile <- switch(.Platform$OS.type, windows = "NUL", "/dev/null")
  .nullcon <- function() file(nullfile, open = "wb", raw = TRUE)

  ## Assert that a null device exists
  tryCatch({
    con <- .nullcon()
    on.exit(close(con))
    cat("test", file = con)
  }, error = function(ex) {
    stopf("Failed to write to null file (%s) on this platform (%s). Please report this the maintainer of the 'future' package.", sQuote(nullfile), sQuote(.Platform$OS.type))
  })
  
  .nullcon
})


## https://github.com/futureverse/future/issues/130
#' @importFrom utils packageVersion
resolveMPI <- local({
  cache <- list()
  
  function(future) {
    resolveMPI <- cache$resolveMPI
    if (is.null(resolveMPI)) {
      resolveMPI <- function(future) {
        node <- future$workers[[future$node]]
        warnf("resolved() on %s failed to load the Rmpi package. Will use blocking value() instead and return TRUE", sQuote(class(node)[1]))
        value(future, stdout = FALSE, signal = FALSE)
        TRUE
      }

      if (requireNamespace(pkg <- "Rmpi", quietly = TRUE)) {
        ns <- getNamespace("Rmpi")

        resolveMPI <- function(future) {
          node <- future$workers[[future$node]]
          warnf("resolved() on %s failed to find mpi.iprobe() and mpi.any.tag() in Rmpi %s. Will use blocking value() instead and return TRUE", sQuote(class(node)[1]), packageVersion("Rmpi"))
          value(future, stdout = FALSE, signal = FALSE)
          TRUE
        }

        if (all(sapply(c("mpi.iprobe", "mpi.any.tag"), FUN = exists,
                       mode = "function", envir = ns, inherits = FALSE))) {
          mpi.iprobe <- get("mpi.iprobe", mode = "function", envir = ns,
                            inherits = FALSE)
          mpi.any.tag <- get("mpi.any.tag", mode = "function", envir = ns,
                             inherits = FALSE)
          resolveMPI <- function(future) {
            node <- future$workers[[future$node]]
            mpi.iprobe(source = node$rank, comm = node$comm, tag = mpi.any.tag())
          }
        }
      }
      stop_if_not(is.function(resolveMPI))
      cache$resolveMPI <<- resolveMPI
    }

    resolveMPI(future)
  }
})


supports_omp_threads <- function(assert = FALSE, debug = getOption("future.debug", FALSE)) {
  if (!requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    if (assert) {
      stop(FutureError(sprintf("In order to disable multi-threading in multicore futures, the %s package must be installed", sQuote("RhpcBLASctl"))))
    }
    return(FALSE)
  }

  ## Current number of OpenMP threads
  old_omp_threads <- RhpcBLASctl::omp_get_max_threads()

  ## RhpcBLASctl compiled without OpenMP support?
  ## Then it returns NULL in RhpcBLASctl (< 0.20-17) otherwise NA_integer_
  if (is.null(old_omp_threads)) old_omp_threads <- NA_integer_
  
  res <- !is.na(old_omp_threads)

  if (debug) mdebugf("supports_omp_threads() = %s", res, debug = debug)

  res
}


## On MS Windows, support for capturing UTF8 symbols was added in R 4.2.0,
## but it requires a certain setup.
can_capture_utf8 <- if (.Platform$OS.type == "windows") {
  if (getRversion() >= "4.2.0") {
    local({
      truth <- "\u2713" ## checkmark
      truth_raw <- charToRaw(truth)
      success <- NA
      
      function() {
        if (is.na(success)) {
          con <- rawConnection(raw(), open = "w")
          on.exit(close(con))
          sink(file = con)
          sunk <- TRUE
          on.exit(if (sunk) sink(NULL), add = TRUE)
    
          ## Output UTF-symbol checkmark
          cat(truth)
    
          sink(NULL)
          sunk <- FALSE
          output <- rawConnectionValue(con)
          close(con)
          on.exit()
          success <<- identical(output, truth_raw)
        }
        success
      }
    })
  } else {
    function() FALSE
  }
} else {
  function() TRUE
}

## https://github.com/futureverse/future/issues/473
adhoc_native_to_utf8 <- function(x) {
  code <- gsub("<U[+]([[:alnum:]]+)>", "\\\\u\\1", x)
  if (identical(code, x)) return(x)
  code <- gsub('"', '\\"', code, fixed = TRUE)
  code <- paste('"', code, '"', sep = "")
  tryCatch({
    expr <- parse(text = code)
    eval(expr, envir = emptyenv())
  }, error = function(ex) x)
}
