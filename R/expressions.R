makeExpression <- local({
  tmpl_expr_evaluate2 <- future:::bquote_compile({
    ## Evaluate future
    future:::evalFuture(expr = quote(.(expr)), local = .(local), stdout = .(stdout), conditionClasses = .(conditionClasses), split = .(split), immediateConditions = .(immediateConditions), immediateConditionClasses = .(immediateConditionClasses), globals = .(globals), globalenv = .(globalenv), packages = .(packages), seed = .(seed), strategiesR = .(strategiesR), forwardOptions = .(forwardOptions), cleanup = .(cleanup))
  })


  function(expr, local = TRUE, immediateConditions = FALSE, stdout = TRUE, conditionClasses = NULL, split = FALSE, globals = NULL, globalenv = (getOption("future.globalenv.onMisuse", "ignore") != "ignore"), enter = NULL, exit = NULL, version = "1.8", packages = NULL, seed = NULL, mc.cores = NULL, cleanup = TRUE) {
    if (version != "1.8") {    
      stop(FutureError("Internal error: Non-supported future expression version: ", version))
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
    
    strategies <- plan("list")
    strategiesR <- strategies[-1]
    if (length(strategiesR) == 0L) {
      strategiesR <- getOption("future.plan", sequential)
    } else {
      ## Identify package namespaces needed for strategies
      pkgsS <- lapply(strategiesR, FUN = environment)
      pkgsS <- lapply(pkgsS, FUN = environmentName)
      pkgsS <- unique(unlist(pkgsS, use.names = FALSE))
      ## CLEANUP: Only keep those that are loaded in the current session
      pkgsS <- intersect(pkgsS, loadedNamespaces())
      packages <- unique(c(packages, pkgsS))
    }

    if (is.function(strategiesR)) {
      if (!inherits(strategiesR, "future")) {
        stop(FutureError(sprintf("Argument 'strategiesR' is a function, but does not inherit 'future': %s", paste(sQuote(class(strategiesR)), collapse = ", "))))
      }
    } else if (is.list(strategiesR)) {
      for (kk in seq_along(strategiesR)) {
        strategy <- strategiesR[[kk]]
        if (!inherits(strategy, "future")) {
          stop(FutureError(sprintf("Element #%d of list 'strategiesR' is a function, but does not inherit 'future': %s", kk, paste(sQuote(class(strategy)), collapse = ", "))))
        }
      }
    } else if (is.character(strategiesR)) {
    } else {
      stop(FutureError(sprintf("Unknown value of argument 'strategiesR': %s", paste(sQuote(class(strategiesR)), collapse = ", "))))
    }

    forwardOptions <- list(
      ## Assert globals when future is created (or at run time)?
      future.globals.onMissing         = getOption("future.globals.onMissing"),
    
      ## Pass down other future.* options
      future.globals.maxSize           = getOption("future.globals.maxSize"),
      future.globals.method            = getOption("future.globals.method"),
      future.globals.onReference       = getOption("future.globals.onReference"),
      future.globals.resolve           = getOption("future.globals.resolve"),
      future.resolve.recursive         = getOption("future.resolve.recursive"),
      future.rng.onMisuse              = getOption("future.rng.onMisuse"),
      future.rng.onMisuse.keepFuture   = getOption("future.rng.onMisuse.keepFuture"),
      future.stdout.windows.reencode   = getOption("future.stdout.windows.reencode"),

      future.makeExpression.skip       = getOption("future.makeExpression.skip"),
      future.makeExpression.skip.local = getOption("future.makeExpression.skip.local"),
      
      ## Other options relevant to making futures behave consistently
      ## across backends
      width                            = getOption("width")
    )

    if (!is.null(mc.cores)) {
      forwardOptions$mc.cores <- mc.cores
    }

    expr <- bquote_apply(tmpl_expr_evaluate2)

    expr
  }
}) ## makeExpression()



logme <- function(expr, envir = parent.frame()) {
  expr <- substitute(expr)
  stdout <- utils::capture.output(eval(expr, envir = envir))
  stdout <- sprintf("[evalFuture()] %s\n", stdout)
  stdout <- paste(stdout, collapse = "")
  cat(stdout, file = "callr.log", append = TRUE)
}

FutureEvalError <- function(...) {
  ex <- FutureError(...)
  class(ex) <- c("FutureEvalError", class(ex))
  ex
}

evalFuture <- function(expr, local = FALSE, stdout = TRUE, conditionClasses = character(0L), split = FALSE, immediateConditions = NULL, immediateConditionClasses = character(0L), globals = NULL, globalenv = (getOption("future.globalenv.onMisuse", "ignore") != "ignore"), packages = NULL, seed = NULL, forwardOptions = NULL, strategiesR = NULL, envir = parent.frame(), cleanup = TRUE) {
  stop_if_not(
    length(local) == 1L && is.logical(local) && !is.na(local),
    length(stdout) == 1L && is.logical(stdout),
    length(split) == 1L && is.logical(split) && !is.na(split),
    is.null(conditionClasses) || (is.character(conditionClasses) && !anyNA(conditionClasses) && all(nzchar(conditionClasses))),
    length(immediateConditions) == 1L && is.logical(immediateConditions) && !is.na(immediateConditions),
    is.character(immediateConditionClasses) && !anyNA(immediateConditionClasses) && all(nzchar(immediateConditionClasses)),
    length(globalenv) == 1L && is.logical(globalenv) && !is.na(globalenv),
    is.null(seed) || is_lecyer_cmrg_seed(seed) || (is.logical(seed) && !is.na(seed) || !seed),
    length(cleanup) == 1L && is.logical(cleanup) && !is.na(cleanup)
  )

  if (is.function(strategiesR)) {
    if (!inherits(strategiesR, "future")) {
      stop(FutureEvalError(sprintf("Argument 'strategiesR' is a function, but does not inherit 'future': %s", paste(sQuote(class(strategiesR)), collapse = ", "))))
    }
  } else if (is.list(strategiesR)) {
    for (kk in seq_along(strategiesR)) {
      strategy <- strategiesR[[kk]]
      if (!inherits(strategy, "future")) {
        stop(FutureEvalError(sprintf("Element #%d of list 'strategiesR' is a function, but does not inherit 'future': %s", kk, paste(sQuote(class(strategy)), collapse = ", "))))
      }
    }
  } else if (is.character(strategiesR)) {
  } else {
    stop(FutureEvalError(sprintf("Unknown value of argument 'strategiesR': %s", paste(sQuote(class(strategiesR)), collapse = ", "))))
  }

  ## Start time for future evaluation
  ...future.startTime <- Sys.time()


  ## -----------------------------------------------------------------
  ## Record current state
  ## -----------------------------------------------------------------
  ## Current working directory
  ...future.workdir <- getwd()

  ## mc.cores
  ...future.mc.cores.old <- getOption("mc.cores")

  ## RNG state
  ...future.rngkind <- RNGkind()[1]
  ...future.rng <- globalenv()$.Random.seed
  
  ## Record the original future strategy set on this worker
  ...future.plan.old <- getOption("future.plan")
  ...future.plan.old.envvar <- Sys.getenv("R_FUTURE_PLAN", NA_character_)
  ...future.strategy.old <- plan("list")

  ## Load and attached packages
  ## TROUBLESHOOTING: If the package fails to load, then library()
  ## suppress that error and generates a generic much less
  ## informative error message.  Because of this, we load the
  ## namespace first (to get a better error message) and then
  ## call library(), which attaches the package. /HB 2016-06-16
  ## NOTE: We use local() here such that 'pkg' is not assigned
  ##       to the future environment. /HB 2016-07-03
  if (length(packages) > 0L) {
    res <- tryCatch({
      for (pkg in packages) {
        loadNamespace(pkg)
        library(pkg, character.only = TRUE)
      }
      NULL
    }, error = identity)
    if (inherits(res, "error")) {
      res <- FutureResult(conditions = list(res), started = ...future.startTime)
      return(res)
    }
  }

  ## Note, we record R options and environment variables _after_
  ## loading and attaching packages, in case they set options/env vars
  ## needed for the session, e.g.
  ## https://github.com/Rdatatable/data.table/issues/5375
  
  ## R options
  ...future.oldOptions <- as.list(.Options)

  ## Environment variables
  ...future.oldEnvVars <- Sys.getenv()


  ## -----------------------------------------------------------------
  ## Reset the current state on exit
  ## -----------------------------------------------------------------
  if (cleanup) {
    on.exit({
      ## (d) Reset environment variables
      if (.Platform$OS.type == "windows") {
        ## On MS Windows, there are two special cases to consider:
        ##
        ## (1) You cannot have empty environment variables. When one is assigned
        ## an empty string, MS Windows interprets that as it should be removed.
        ## That is, if we do Sys.setenv(ABC = ""), it'll have the
        ## same effect as Sys.unsetenv("ABC").
        ## However, when running MS Windows on msys2, we might see empty
        ## environment variables also MS Windows. We can also observe this on
        ## GitHub Actions and when running R via Wine.
        ## Because of this, we need to take extra care to preserve empty ("")
        ## environment variables.
        ##
        ## (2) Environment variable names are case insensitive. However, it is
        ## still possible to have two or more environment variables that have
        ## the exact same toupper() names, e.g. 'TEMP', 'temp', and 'tEmP'.
        ## This can happen if 'temp' and 'tEmP' are inherited from the host
        ## environment (e.g. msys2), and 'TEMP' is set by MS Windows.
        ## What complicates our undoing here is that Sys.setenv() is non-case
        ## sensitive.  This means, if we do Sys.setenv(temp = "abc") when
        ## both 'temp' and 'TEMP' exists, then we'll lose 'TEMP'.  So, we
        ## should on undo an environment variable if the upper-case version
        ## does not exist.
  
        old_names <- names(...future.oldEnvVars)
        envs <- Sys.getenv()
        names <- names(envs)
        common <- intersect(names, old_names)
        added <- setdiff(names, old_names)
        removed <- setdiff(old_names, names)
        
        ## (a) Update environment variables that have changed
        changed <- common[...future.oldEnvVars[common] != envs[common]]
        NAMES <- toupper(changed)
        args <- list()
        for (kk in seq_along(NAMES)) {
          name <- changed[[kk]]
          NAME <- NAMES[[kk]]
          ## Skip if Case (2), e.g. 'temp' when 'TEMP' also exists?
          if (name != NAME && is.element(NAME, old_names)) next
          args[[name]] <- ...future.oldEnvVars[[name]]
        }
  
        ## (b) Remove newly added environment variables
        NAMES <- toupper(added)
        for (kk in seq_along(NAMES)) {
          name <- added[[kk]]
          NAME <- NAMES[[kk]]
          ## Skip if Case (2), e.g. 'temp' when 'TEMP' also exists?
          if (name != NAME && is.element(NAME, old_names)) next
          args[[name]] <- ""
        }
  
        ## (c) Add removed environment variables
        NAMES <- toupper(removed)
        for (kk in seq_along(NAMES)) {
          name <- removed[[kk]]
          NAME <- NAMES[[kk]]
          ## Skip if Case (2), e.g. 'temp' when 'TEMP' also exists?
          if (name != NAME && is.element(NAME, old_names)) next
          args[[name]] <- ...future.oldEnvVars[[name]]
        }
  
        if (length(args) > 0) do.call(Sys.setenv, args = args)
  
        ## Not needed anymore
        args <- names <- old_names <- NAMES <- envs <- common <- added <- removed <- NULL
      } else {
        do.call(Sys.setenv, args = as.list(...future.oldEnvVars))
      }
      
      ## For the same reason as we don't remove added R options, we don't
      ## remove added environment variables until we know it's safe.
      ## /HB 2022-04-30
      ## (d) Remove any environment variables added
      ## diff <- setdiff(names(Sys.getenv()), names(...future.oldEnvVars))
      ## Sys.unsetenv(diff)
      
      ## (a) Reset options
      ## WORKAROUND: Do not reset 'nwarnings' unless changed, because
      ## that will, as documented, trigger any warnings collected
      ## internally to be removed.
      ## https://github.com/futureverse/future/issues/645
      if (identical(getOption("nwarnings"), ...future.oldOptions$nwarnings)) {
        ...future.oldOptions$nwarnings <- NULL
      }
      options(...future.oldOptions)
  
      ## There might be packages that add essential R options when
      ## loaded or attached, and if their R options are removed, some of
      ## those packages might break. Because we don't know which these
      ## packages are, and we cannot detect when a random packages is
      ## loaded/attached, we cannot reliably workaround R options added
      ## on package load/attach.  For this reason, I'll relax the
      ## resetting of R options to only be done to preexisting R options
      ## for now. These thoughts were triggered by a related data.table
      ## issue, cf. https://github.com/futureverse/future/issues/609
      ## /HB 2022-04-29
      
      ## (b) Remove any options added
      ## diff <- setdiff(names(.Options),
      ##                       names(...future.oldOptions))
      ## if (length(diff) > 0L) {
      ##    opts <- vector("list", length = length(diff))
      ##    names(opts) <- diff
      ##    options(opts)
      ## }
  
      ## Revert to the original future strategy
      ## Reset option 'future.plan' and env var 'R_FUTURE_PLAN'
      options(future.plan = ...future.plan.old)
      plan(...future.strategy.old, .cleanup = FALSE, .init = FALSE)
      if (is.na(...future.plan.old.envvar)) {
        Sys.unsetenv("R_FUTURE_PLAN")
      } else {
        Sys.setenv(R_FUTURE_PLAN = ...future.plan.old.envvar)
      }
  
      ## Undo RNG state
      genv <- globalenv()
      RNGkind(...future.rngkind)
      if (is.null(...future.rng)) {
        if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
          rm(list = ".Random.seed", envir = genv, inherits = FALSE)
        }
      } else {
        assign(".Random.seed", ...future.rng, envir = genv, inherits = FALSE)
      }
      
      ## Reset R option 'mc.cores'
      options(mc.cores = ...future.mc.cores.old)
  
      ## Reset working directory
      setwd(...future.workdir)
    }, add = TRUE)
  }


  ## Prevent .future.R from being source():d when future is attached
  options(future.startup.script = FALSE)

  ## Options forwarded from parent process
  if (length(forwardOptions) > 0) {
    stopifnot(!is.null(names(forwardOptions)))
    do.call(options, args = forwardOptions)
  }

  ## -----------------------------------------------------------------
  ## Preserve future options added
  ## -----------------------------------------------------------------
  ...future.futureOptionsAdded <- setdiff(names(.Options), names(...future.oldOptions))
  if (cleanup) {
    on.exit({
      ## Remove any "future" options added
      if (length(...future.futureOptionsAdded) > 0L) {
        opts <- vector("list", length = length(...future.futureOptionsAdded))
        names(opts) <- ...future.futureOptionsAdded
        options(opts)
      }
    }, add = TRUE)
  }
  

  ## -----------------------------------------------------------------
  ## Evaluate future in the correct context
  ## -----------------------------------------------------------------
  ## Evaluate expression in a local() environment?
  if (local) {
    tmpl_expr_local <- bquote_compile(base::local(.(expr)))
    expr <- bquote_apply(tmpl_expr_local)
    ## WORKAROUND: This makes assumption about withCallingHandlers()
    ## and local(). In case this changes, provide internal options to
    ## adjust this. /HB 2018-12-28
    skip <- getOption("future.makeExpression.skip.local", c(12L, 3L))
  } else {
    ## WORKAROUND: This makes assumption about withCallingHandlers()
    ## In case this changes, provide internal options to adjust this.
    ## /HB 2018-12-28
    skip <- getOption("future.makeExpression.skip", c(6L, 3L))
  }
  
  if (globalenv) {
    ## Record names of variables in the global environment
    ...future.globalenv.names <- c(names(.GlobalEnv), "...future.value", "...future.globalenv.names", ".Random.seed")
  }

  if (length(globals) > 0) {
    base_attach <- base::attach ## To please R CMD check
    base_attach(globals, pos = 2L, name = "future:globals", warn.conflicts = FALSE)
    if (cleanup) {
      on.exit({
        detach(name = "future:globals")
      }, add = TRUE)
    }
  }

  ## Ignore, capture or discard standard output?
  if (is.na(stdout)) {  ## stdout = NA
    ## Don't capture, but also don't block any output
  } else {
    if (stdout) {  ## stdout = TRUE
      ## Capture all output
      ## NOTE: Capturing to a raw connection is much more efficient
      ## than to a character connection, cf.
      ## https://www.jottr.org/2014/05/26/captureoutput/
      ...future.stdout <- rawConnection(raw(0L), open = "w")
    } else {  ## stdout = FALSE
      ## Silence all output by sending it to the void
      ...future.stdout <- file(
        switch(.Platform$OS.type, windows = "NUL", "/dev/null"),
        open = "w"
      )
    }
    sink(...future.stdout, type = "output", split = split)
    on.exit(if (!is.null(...future.stdout)) {
      sink(type = "output", split = split)
      close(...future.stdout)
    }, add = TRUE)
  }

  ## Prevent 'future.plan' / R_FUTURE_PLAN settings from being nested
  options(future.plan = NULL)
  Sys.unsetenv("R_FUTURE_PLAN")
  
#  logme("future:plan() ...")
#  logme(utils::str(strategiesR))
#  logme(print(strategiesR))
  
  ## Use the next-level-down ("popped") future strategy
  future::plan(strategiesR, .cleanup = FALSE, .init = FALSE)

#  logme("future:plan() ... done")

  ## Set RNG seed?
  if (is.numeric(seed)) {
    genv <- globalenv()
    genv$.Random.seed <- seed
  }

  conditionClassesExclude <- attr(conditionClasses, "exclude", exact = TRUE)
  muffleInclude <- attr(conditionClasses, "muffleInclude", exact = TRUE)
  if (is.null(muffleInclude)) muffleInclude <- "^muffle"

  ...future.frame <- sys.nframe()
  ...future.conditions <- list()

  ## NOTE: We don't want to use local(body) w/ on.exit() because
  ## evaluation in a local is optional, cf. argument 'local'.
  ## If this was mandatory, we could.  Instead we use
  ## a tryCatch() statement. /HB 2016-03-14
  ...future.result <- tryCatch({
    withCallingHandlers({
      ...future.value <- withVisible(eval(expr, envir = envir))
      FutureResult(
        value = ...future.value$value,
        visible = ...future.value$visible,
        rng = !identical(globalenv()$.Random.seed, ...future.rng),
        globalenv = if (globalenv) list(added = setdiff(names(.GlobalEnv), ...future.globalenv.names)) else NULL,
        started = ...future.startTime
      )
    }, condition = base::local({
      sysCalls <- function(calls = sys.calls(), from = 1L) {
        calls[seq.int(from = from + skip[1L], to = length(calls) - skip[2L])]
      }
      
      function(cond) {
        is_error <- inherits(cond, "error")
          
        ## Ignore condition?
        ignore <- !is_error &&
                  !is.null(conditionClassesExclude) && 
                  inherits(cond, conditionClassesExclude)
        
        ## Handle error:s specially
        if (is_error) {
          sessionInformation <- function() {
            list(
              r          = R.Version(),
              locale     = Sys.getlocale(),
              rngkind    = RNGkind(),
              namespaces = loadedNamespaces(),
              search     = search(),
              system     = Sys.info()
            )
          }
          
          ## Record condition
          ...future.conditions[[length(...future.conditions) + 1L]] <<- list(
            condition = cond,
            calls     = c(sysCalls(from = ...future.frame), cond$call),
            session   = sessionInformation(),
            timestamp = Sys.time(),
            signaled  = 0L
          )
      
          signalCondition(cond)
        } else if (!ignore &&
                   !is.null(conditionClasses) &&
                   inherits(cond, conditionClasses)
                  ) {
          ## Relay 'immediateCondition' conditions immediately?
          ## If so, then do not muffle it and flag it as signalled
          ## already here.
          signal <- immediateConditions && inherits(cond, immediateConditionClasses)
          ## Record condition
          ...future.conditions[[length(...future.conditions) + 1L]] <<- list(
            condition = cond,
            signaled = as.integer(signal)
          )
          if (immediateConditions && !split && !signal) {
            muffleCondition(cond, pattern = muffleInclude)
          }
        } else {
          if (!split && !is.null(conditionClasses)) {
            ## Muffle all non-captured conditions
            muffleCondition(cond, pattern = muffleInclude)
          }
        }
      } ## function(cond)
    })) ## local() + withCallingHandlers()
  }, error = function(ex) {
    FutureResult(
      rng = !identical(globalenv()$.Random.seed, ...future.rng),
      globalenv = if (globalenv) list(added = setdiff(names(.GlobalEnv), ...future.globalenv.names)) else NULL,
      started = ...future.startTime
    )
  }) ## tryCatch()
  

  ## -----------------------------------------------------------------
  ## Get captured standard output?
  ## -----------------------------------------------------------------
  if (!is.na(stdout)) {
    sink(type = "output", split = split)
    if (stdout) {
      ...future.result$stdout <- rawToChar(
        rawConnectionValue(...future.stdout)
      )
    } else {
      ...future.result["stdout"] <- list(NULL)
    }
    close(...future.stdout)
    ...future.stdout <- NULL
  }

  ...future.result$conditions <- ...future.conditions
  ...future.result$finished <- Sys.time()

  ...future.result
} ## evalFuture()
