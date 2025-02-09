#' Retrieves global variables of an expression and their associated packages 
#'
#' @inheritParams globals::globalsOf
#'
#' @param expr An \R expression whose globals should be found.
#' 
#' @param envir The environment from which globals should be searched.
#' 
#' @param tweak (optional) A function that takes an expression and returned a modified one.
#' 
#' @param globals (optional) a logical, a character vector, a named list, or a \link[globals]{Globals} object.  If TRUE, globals are identified by code inspection based on `expr` and `tweak` searching from environment `envir`.  If FALSE, no globals are used.  If a character vector, then globals are identified by lookup based their names `globals` searching from environment `envir`.  If a named list or a Globals object, the globals are used as is.
#'
#' @param resolve If TRUE, any future that is a global variables (or part of one) is resolved and replaced by a "constant" future.
#'
#' @param persistent If TRUE, non-existing globals (= identified in expression but not found in memory) are always silently ignored and assumed to be existing in the evaluation environment.  If FALSE, non-existing globals are by default ignore, but may also trigger an informative error if option \option{future.globals.onMissing} in `"error"` (should only be used for troubleshooting).
#'
#' @param maxSize The maximum allowed total size (in bytes) of globals---for
#' the purpose of preventing too large exports / transfers happening by
#' mistake.  If the total size of the global objects are greater than this
#' limit, an informative error message is produced. If
#' `maxSize = +Inf`, then this assertion is skipped. (Default: 500 MiB).
#'
#' @param \dots Not used.
#'
#' @return A named list with elements `expr` (the tweaked expression), `globals` (a named list of class [FutureGlobals]) and `packages` (a character string).
#'
#' @seealso Internally, \code{\link[globals]{globalsOf}()} is used to identify globals and associated packages from the expression.
#'
#' @importFrom globals globalsOf globalsByName as.Globals packagesOf cleanup
#' @export
#'
#' @keywords internal
getGlobalsAndPackages <- function(expr, envir = parent.frame(), tweak = tweakExpression, globals = TRUE, locals = getOption("future.globals.globalsOf.locals", TRUE), resolve = getOption("future.globals.resolve", NULL), persistent = FALSE, maxSize = getOption("future.globals.maxSize", 500 * 1024 ^ 2), ...) {
  if (is.null(resolve)) {
    resolve <- FALSE
  } else {
    stop_if_not(is.logical(resolve), length(resolve) == 1L, !is.na(resolve))
    .Deprecated(msg = sprintf("R option %s may only be used for troubleshooting. It must not be used in production since it changes how futures are evaluated and there is a great risk that the results cannot be reproduced elsewhere: %s", sQuote("future.globals.resolve"), sQuote(resolve)), package = .packageName)
  }
  
  debug <- getOption("future.debug", FALSE)
  if (debug) mdebug("getGlobalsAndPackages() ...")
  
  ## Assert that all identified globals exists when future is created?
  if (persistent) {
    ## If future relies on persistent storage, then the globals may
    ## already exist in the environment that the future is evaluated in.
    mustExist <- FALSE
  } else {
    ## Default for 'future.globals.onMissing':
    ## Note: It's possible to switch between 'ignore' and 'error'
    ##       at any time. Tests handle both cases. /HB 2016-06-18
    globals.onMissing <- getOption("future.globals.onMissing", NULL)
    if (is.null(globals.onMissing)) {
      globals.onMissing <- "ignore"
      mustExist <- FALSE
    } else {
      globals.onMissing <- match.arg(globals.onMissing,
                                     choices = c("error", "ignore"))
      .Deprecated(msg = sprintf("R option %s may only be used for troubleshooting. It must not be used in production since it changes how futures are evaluated and there is a great risk that the results cannot be reproduced elsewhere: %s", sQuote("future.globals.onMissing"), sQuote(globals.onMissing)), package = .packageName)
      mustExist <- is.element(globals.onMissing, "error")
    }
  }

  if (is.logical(globals)) {
    stop_if_not(length(globals) == 1, !is.na(globals))

    ## Any manually added globals?
    add <- attr(globals, "add", exact = TRUE)
    if (!is.null(add)) {
      if (is.character(add)) {
        if (debug) mdebug("Retrieving 'add' globals ...")
        add <- globalsByName(add, envir = envir, mustExist = mustExist)
        if (debug) mdebugf("- 'add' globals retrieved: [%d] %s", length(add), paste(sQuote(names(add)), collapse = ", "))
        if (debug) mdebug("Retrieving 'add' globals ... DONE")
      } else if (inherits(add, "Globals")) {
        if (debug) mdebugf("- 'add' globals passed as-is: [%d] %s", length(add), paste(sQuote(names(add)), collapse = ", "))
      } else if (is.list(add)) {
        if (debug) mdebugf("- 'add' globals passed as-list: [%d] %s", length(add), paste(sQuote(names(add)), collapse = ", "))
      } else {
        stopf("Attribute 'add' of argument 'globals' must be either a character vector or a named list: %s", mode(add))
      }
      add <- as.FutureGlobals(add)
      stop_if_not(inherits(add, "FutureGlobals"))
    }
    
    ## Any manually dropped/ignored globals?
    ignore <- attr(globals, "ignore", exact = TRUE)
    if (!is.null(ignore)) {
      stop_if_not(is.character(ignore))
    }
  
    if (globals) {
      if (debug) mdebug("Searching for globals ...")
      ## Algorithm for identifying globals
      globals.method <- getOption("future.globals.method", NULL)
      if (is.null(globals.method)) {
        globals.method <- "ordered"
      } else {
        .Deprecated(msg = sprintf("R option %s may only be used for troubleshooting. It must not be used in production since it changes how futures are evaluated and there is a great risk that the results cannot be reproduced elsewhere: %s", sQuote("future.globals.method"), sQuote(globals.method)), package = .packageName)
      }
      
      globals <- globalsOf(
                   ## Passed to globals::findGlobals()
                   expr, envir = envir, substitute = FALSE, tweak = tweak,
                   ## Requires globals (>= 0.14.0.9004); ignored otherwise
                   locals = locals,
                   ## Passed to globals::findGlobals() via '...'
                   dotdotdot = "return",
                   method = globals.method,
                   unlist = TRUE,
                   ## Passed to globals::globalsByName()
                   mustExist = mustExist,
                   recursive = TRUE
                 )
      if (debug) mdebugf("- globals found: [%d] %s", length(globals), paste(sQuote(names(globals)), collapse = ", "))
      if (debug) mdebug("Searching for globals ... DONE")
    } else {
      if (debug) mdebug("Not searching for globals")
      globals <- FutureGlobals()
    }

    ## Drop 'ignore' globals?
    ## FIXME: This should really be implemented in globals::globalsOf()
    if (!is.null(ignore)) {
      if (any(ignore %in% names(globals))) {
        globals <- globals[setdiff(names(globals), ignore)]
      }
    }
  
    ## Append 'add' globals?
    if (inherits(add, "FutureGlobals")) {
      globals <- unique(c(globals, add))
    }
  } else if (is.character(globals)) {
    if (debug) mdebug("Retrieving globals ...")
    globals <- globalsByName(globals, envir = envir, mustExist = mustExist)
    if (debug) mdebugf("- globals retrieved: [%d] %s", length(globals), paste(sQuote(names(globals)), collapse = ", "))
    if (debug) mdebug("Retrieving globals ... DONE")
  } else if (inherits(globals, "Globals")) {
    if (debug) mdebugf("- globals passed as-is: [%d] %s", length(globals), paste(sQuote(names(globals)), collapse = ", "))
  } else if (is.list(globals)) {
    if (debug) mdebugf("- globals passed as-list: [%d] %s", length(globals), paste(sQuote(names(globals)), collapse = ", "))
  } else {
    stopf("Argument 'globals' must be either a logical scalar or a character vector: %s", mode(globals))
  }
  ## Make sure to preserve 'resolved' attribute
  globals <- as.FutureGlobals(globals)
  stop_if_not(inherits(globals, "FutureGlobals"))

  ## Nothing more to do?
  if (length(globals) == 0) {
    if (debug) {
      mdebug("- globals: [0] <none>")
      mdebug("- packages: [0] <none>")
      mdebug("getGlobalsAndPackages() ... DONE")
    }
    attr(globals, "resolved") <- TRUE
    attr(globals, "total_size") <- 0
    return(list(expr = expr, globals = globals, packages = character(0)))
  }

  ## Are globals already resolved?
  t <- attr(globals, "resolved", exact = TRUE)
  if (isTRUE(t)) {
    resolve <- FALSE
    if (debug) mdebugf("Resolving globals: %s (because already done)", resolve)
  } else {
    if (debug) mdebugf("Resolving globals: %s", resolve)
  }
  stop_if_not(is.logical(resolve), length(resolve) == 1L, !is.na(resolve))

  exprOrg <- expr

  ## Tweak expression to be called with global ... arguments?
  if (length(globals) > 0 && inherits(globals[["..."]], "DotDotDotList")) {
    if (debug) mdebug("Tweak future expression to call with '...' arguments ...")
    has_dotdotdot <- TRUE
    ## Missing global '...'?
    if (!is.list(globals[["..."]])) {
      if (!is.na(globals[["..."]])) {
        msg <- sprintf("Did you mean to create the future within a function?  Invalid future expression tries to use global '...' variables that do not exist: %s", hexpr(exprOrg))
        if (debug) mdebug(msg)
        stop(msg)
      }
      globals[["..."]] <- NULL
      where <- attr(globals, "where", exact = TRUE)
      where[["..."]] <- NULL
      attr(globals, "where") <- where
      has_dotdotdot <- FALSE
    }

    if (has_dotdotdot) {
      names <- names(globals)
      names[names == "..."] <- "future.call.arguments"
      names(globals) <- names

      ## AD HOC: Drop duplicated 'future.call.arguments' elements, cf.
      ## https://github.com/futureverse/future/issues/417.
      ## The reason for duplicates being possible, is that '...' is renamed
      ## to 'future.call.arguments' so the former won't override the latter.
      ## This might have to be fixed in future.apply and furrr. /HB 2020-09-21
      idxs <- which(names == "future.call.arguments")
      if (length(idxs) > 1L) {
        if (debug) {
          mdebugf("- Detected %d 'future.call.arguments' global entries:", length(idxs))
          mstr(globals[idxs])
        }
        # Drop all empty entries
        ns <- vapply(globals[idxs], FUN = length, FUN.VALUE = 0L)
        if (debug) mprint(ns)
        keep <- (ns > 0)
        nkeep <- sum(keep)
        if (nkeep == 0L) {
          if (debug) mdebugf("- All 'future.call.arguments' global entries are empty. Keeping the first one.")
          ## All are empty, keep first
          keep[1L] <- TRUE
        } else if (nkeep > 1L) {
          # Drop all but the last non-empty replicate
          if (debug) mdebugf("- Detected %d non-empty 'future.call.arguments' global entries. Keeping the last one.", nkeep)
          keep2 <- logical(length = length(idxs))
          keep2[max(which(keep))] <- TRUE
          keep <- keep2
        }
        globals <- globals[-idxs[!keep]]
        if (debug) {
          names <- names(globals)
          idxs <- which(names == "future.call.arguments")
          mdebugf("- 'future.call.arguments' global entries:")
          mstr(globals[idxs])
        }
      }
      idxs <- NULL
      names <- NULL
  
      ## To please R CMD check
      a <- `future.call.arguments` <- NULL
      rm(list = c("a", "future.call.arguments"))
      expr <- substitute({
        ## covr: skip=1
        "# future::getGlobalsAndPackages(): wrapping the original future"
        "# expression in do.call(), because function called uses '...'"
        "# as a global variable"
        do.call(function(...) a, args = `future.call.arguments`)
      }, list(a = expr))
      if (debug) {
        mprint(expr)
        mdebug("Tweak future expression to call with '...' arguments ... DONE")
      }
    }
  }

  ## Resolve futures and turn into already-resolved "constant" futures
  ## We restrict ourselves to this here in order to avoid having to
  ## recursively try to resolve everything in every global which may
  ## or may not point to packages (include base R package)
  if (resolve && length(globals) > 0L) {
    if (debug) mdebug("Resolving any globals that are futures ...")
    globals <- as.FutureGlobals(globals)

    ## Unless already resolved, perform a shallow resolve
    if (attr(globals, "resolved", exact = TRUE)) {
      idxs <- which(unlist(lapply(globals, FUN = inherits, "Future"), use.names = FALSE))
      if (debug) mdebugf("Number of global futures: %d", length(idxs))
      
      ## Nothing to do?
      if (length(idxs) > 0) {
        if (debug) mdebugf("Global futures (not constant): %s", paste(sQuote(names(globals[idxs])), collapse = ", "))
        valuesF <- value(globals[idxs])
        globals[idxs] <- lapply(valuesF, FUN = ConstantFuture)
      }
    }

    if (debug) {
      mdebugf("- globals: [%d] %s", length(globals), paste(sQuote(names(globals)), collapse = ", "))
      mdebug("Resolving any globals that are futures ... DONE")
    }
  }

  if (debug) {
    mdebugf("- Search for packages associated with the globals ...")
  }
  pkgs <- NULL
  if (length(globals) > 0L) {
    asPkgEnvironment <- function(pkg) {
      name <- sprintf("package:%s", pkg)
      if (!name %in% search()) return(emptyenv())
      as.environment(name)
    } ## asPkgEnvironment()

    ## Append packages associated with globals
    pkgs <- packagesOf(globals)
    if (debug) {
      mdebugf("  - Packages associated with globals: [%d] %s", length(pkgs), paste(sQuote(pkgs), collapse = ", "))
    }

    ## Drop all globals which are located in one of
    ## the packages in 'pkgs'.  They will be available
    ## since those packages are attached.
    where <- attr(globals, "where", exact = TRUE)

    names <- names(globals)
    keep <- rep(TRUE, times = length(globals))
    names(keep) <- names
    for (name in names) {
      pkg <- environmentName(where[[name]])
      pkg <- gsub("^package:", "", pkg)
      if (pkg %in% pkgs) {
        ## Only drop exported objects
        if (exists(name, envir = asPkgEnvironment(pkg)))
          keep[name] <- FALSE
      }
    }

    if (!all(keep)) globals <- globals[keep]

    ## Now drop globals that are primitive functions or
    ## that are part of the base packages, which now are
    ## part of 'pkgs' if needed.
    globals <- cleanup(globals)
  }
  if (debug) {
    mdebugf("  - Packages: [%d] %s", length(pkgs), paste(sQuote(pkgs), collapse = ", "))
    mdebugf("- Search for packages associated with the globals ... DONE")
  }
  

  ## Can we skip some of the tasks below?
  if (length(globals) == 0) {
    resolve <- FALSE
    attr(globals, "resolved") <- TRUE
    attr(globals, "total_size") <- 0
  }

  ## Resolve all remaing globals
  ## FIXME: Should we resolve package names spaces too? Should
  ## We assume they can contain futures?  We do it for now, but
  ## if this turns out to be too expensive, maybe we should
  ## only dive into such environments if they have a certain flag
  ## set.  /HB 2016-02-04
  if (resolve && length(globals) > 0L) {
    if (debug) mdebug("Resolving futures part of globals (recursively) ...")
    globals <- resolve(globals, result = TRUE, recursive = TRUE)
    if (debug) {
      mdebugf("- globals: [%d] %s", length(globals), paste(sQuote(names(globals)), collapse = ", "))
      mdebug("Resolving futures part of globals (recursively) ... DONE")
    }
  }


  ## Protect against references?
  if (length(globals) > 0L) {
    action <- getOption("future.globals.onReference", "ignore")
    if (action != "ignore") {
      if (debug) {
        mdebugf("Checking for globals with references (future.globals.onReference = \"%s\") ...", action, appendLF = FALSE)
      }
      t <- system.time({
        assert_no_references(globals, action = action)
      }, gcFirst = FALSE)
      if (debug) mdebugf("[%.3f s]", t[3])
    }
  }


  ## Protect against user error exporting too large objects?
  total_size <- attr(globals, "total_size")
  if (length(globals) > 0L && (is.null(total_size) || is.na(total_size))) {
    maxSize <- as.numeric(maxSize)
    stop_if_not(!is.na(maxSize), maxSize > 0)
    if (is.finite(maxSize)) {
      sizes <- lapply(globals, FUN = objectSize)
      sizes <- unlist(sizes, use.names = TRUE)
      total_size <- sum(sizes, na.rm = TRUE)
      attr(globals, "total_size") <- total_size
      msg <- summarize_size_of_globals(globals, sizes = sizes,
                                       maxSize = maxSize, exprOrg = exprOrg,
                                       debug = debug)
      if (debug) mdebug(msg)
      if (sum(sizes, na.rm = TRUE) > maxSize) stop(msg)
    }
  } ## if (length(globals) > 0)


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Any packages to export?
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Never attach the 'base' package, because that is always
  ## available for all R sessions / implementations.
  pkgs <- setdiff(pkgs, "base")
  if (debug) {
    mdebugf("- Packages after dropping 'base': [%d] %s", length(pkgs), paste(sQuote(pkgs), collapse = ", "))
  }
  if (length(pkgs) > 0L) {
    ## Local functions
    attachedPackages <- function() {
      pkgs <- search()
      pkgs <- grep("^package:", pkgs, value = TRUE)
      pkgs <- gsub("^package:", "", pkgs)
      pkgs
    }
    
    ## Record which packages in 'pkgs' that are loaded and
    ## which of them are attached (at this point in time).
    ## isLoaded <- is.element(pkgs, loadedNamespaces())
    isAttached <- is.element(pkgs, attachedPackages())
    pkgs <- pkgs[isAttached]

    mdebugf("- Packages after dropping non-attached packages: [%d] %s", length(pkgs), paste(sQuote(pkgs), collapse = ", "))
  }

  keepWhere <- getOption("future.globals.keepWhere", FALSE)
  if (!keepWhere) {
    where <- attr(globals, "where")
    for (kk in seq_along(where)) where[[kk]] <- emptyenv()
    attr(globals, "where") <- where
  }
  
  if (debug) {
    mdebugf("- globals: [%d] %s", length(globals), paste(sQuote(names(globals)), collapse = ", "))
    mdebugf("- packages: [%d] %s", length(pkgs), paste(sQuote(pkgs), collapse = ", "))
    mdebug("getGlobalsAndPackages() ... DONE")
  }

  stop_if_not(inherits(globals, "FutureGlobals"))
  
  list(expr = expr, globals = globals, packages = pkgs)
} ## getGlobalsAndPackages()



summarize_size_of_globals <- function(globals, sizes = NULL, maxSize = NULL, exprOrg = NULL, debug = FALSE) {
  if (length(globals) == 0L) return(NULL)

  ## Get the size of the globals
  if (is.null(sizes)) {
    sizes <- lapply(globals, FUN = objectSize)
    sizes <- unlist(sizes, use.names = TRUE)
  }
  total_size <- sum(sizes, na.rm = TRUE)
  if (debug) {
    mdebugf("The total size of the %d globals is %s (%s bytes)",
            length(globals), asIEC(total_size), total_size)
  }
  
  n <- length(sizes)
  o <- order(sizes, decreasing = TRUE)[1:3]
  o <- o[is.finite(o)]
  sizes <- sizes[o]
  classes <- lapply(globals[o], FUN = mode)
  classes <- unlist(classes, use.names = FALSE)
  largest <- sprintf("%s (%s of class %s)",
                     sQuote(names(sizes)), asIEC(sizes), sQuote(classes))

  if (is.null(exprOrg)) {
    msg <- sprintf("The total size of the %d globals exported is %s", length(globals), asIEC(total_size))
  } else {
    msg <- sprintf("The total size of the %d globals exported for future expression (%s) is %s", length(globals), sQuote(hexpr(exprOrg)), asIEC(total_size))
  }

  if (!is.null(maxSize)) {
    if (total_size > maxSize) {
      msg <- sprintf("%s. This exceeds the maximum allowed size %s (option 'future.globals.maxSize').", msg, asIEC(maxSize))
    } else {
      msg <- sprintf("%s. The maximum allowed size is %s (option 'future.globals.maxSize').", msg, asIEC(maxSize))
    }
  }

  if (n == 1) {
    fmt <- "%s There is one global: %s"
  } else if (n == 2) {
    fmt <- "%s There are two globals: %s"
  } else if (n == 3) {
    fmt <- "%s There are three globals: %s"
  } else {
    fmt <- "%s The three largest globals are %s"
  }
  
  msg <- sprintf(fmt, msg, hpaste(largest, lastCollapse = " and "))
  
  msg
} # summarize_size_of_globals()
