makeExpression <- local({
  skip <- skip.local <- NULL

  tmpl_expr_local <- future:::bquote_compile(base::local(.(expr)))

  tmpl_expr_evaluate2 <- future:::bquote_compile({
    ## Evaluate future   
    future:::evalFuture(expr = quote(.(expr)), stdout = .(stdout), conditionClasses = .(conditionClasses), split = .(split), immediateConditions = .(immediateConditions), immediateConditionClasses = .(immediateConditionClasses), globals.onMissing = .(globals.onMissing), globalenv = .(globalenv), skip = .(skip), packages = .(packages), seed = .(seed), strategiesR = .(strategiesR), mc.cores = .(mc.cores))
  })


  function(expr, local = TRUE, immediateConditions = FALSE, stdout = TRUE, conditionClasses = NULL, split = FALSE, globals.onMissing = getOption("future.globals.onMissing", NULL), globalenv = (getOption("future.globalenv.onMisuse", "ignore") != "ignore"), enter = NULL, exit = NULL, version = "1.8", packages = NULL, seed = NULL, mc.cores = NULL) {
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
    
    if (is.null(skip)) {
      ## WORKAROUND: skip = c(7/12, 3) makes assumption about withCallingHandlers()
      ## and local().  In case this changes, provide internal options to adjust this.
      ## /HB 2018-12-28
      skip <<- getOption("future.makeExpression.skip", c(6L, 3L))
      skip.local <<- getOption("future.makeExpression.skip.local", c(12L, 3L))
    }
    
    ## Evaluate expression in a local() environment?
    if (local) {
      expr <- bquote_apply(tmpl_expr_local)
      skip <- skip.local
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

    expr <- bquote_apply(tmpl_expr_evaluate2)

    expr
  }
}) ## makeExpression()






evalFuture <- function(expr, stdout = TRUE, conditionClasses = character(0L), split = FALSE, immediateConditions = NULL, immediateConditionClasses = character(0L), globals.onMissing = getOption("future.globals.onMissing", NULL), globalenv = (getOption("future.globalenv.onMisuse", "ignore") != "ignore"), skip = NULL, packages = NULL, seed = NULL, mc.cores = NULL, strategiesR = future::sequential, envir = parent.frame()) {
  stop_if_not(
    length(stdout) == 1L && is.logical(stdout),
    length(split) == 1L && is.logical(split) && !is.na(split),
    is.null(conditionClasses) || (is.character(conditionClasses) && !anyNA(conditionClasses) && all(nzchar(conditionClasses))),
    length(immediateConditions) == 1L && is.logical(immediateConditions) && !is.na(immediateConditions),
    is.character(immediateConditionClasses) && !anyNA(immediateConditionClasses) && all(nzchar(immediateConditionClasses)),
    length(globalenv) == 1L && is.logical(globalenv) && !is.na(globalenv),
    length(skip) == 2L && is.integer(skip) && !anyNA(skip) && all(skip >= 0L),
    !is.null(strategiesR),
    is.null(seed) || is_lecyer_cmrg_seed(seed) || (is.logical(seed) && !is.na(seed) || !seed),
    is.null(mc.cores) || (is.numeric(mc.cores) && length(mc.cores) == 1L && !is.na(mc.cores) && mc.cores >= 1)
  )

#  packages <- c(packages, "future")

  conditionClassesExclude <- attr(conditionClasses, "exclude", exact = TRUE)
  muffleInclude <- attr(conditionClasses, "muffleInclude", exact = TRUE)
  if (is.null(muffleInclude)) muffleInclude <- "^muffle"

  ## Capture standard output?
  if (is.na(stdout)) {  ## stdout = NA
    ## Don't capture, but also don't block any output
  } else {
    if (stdout) {  ## stdout = TRUE
      ## Capture all output
      ## NOTE: Capturing to a raw connection is much more efficient
      ## than to a character connection, cf.
      ## https://www.jottr.org/2014/05/26/captureoutput/
      ...future.stdout <- base::rawConnection(base::raw(0L), open = "w")
    } else {  ## stdout = FALSE
      ## Silence all output by sending it to the void
      ...future.stdout <- base::file(
        base::switch(.Platform$OS.type, windows = "NUL", "/dev/null"),
        open = "w"
      )
    }
    base::sink(...future.stdout, type = "output", split = split)
    base::on.exit(if (!base::is.null(...future.stdout)) {
      base::sink(type = "output", split = split)
      base::close(...future.stdout)
    }, add = TRUE)
  }

  ...future.frame <- base::sys.nframe()
  ...future.conditions <- base::list()
  ...future.rngkind <- base::RNGkind()[1]
  ...future.rng <- base::globalenv()$.Random.seed

  if (is.numeric(seed)) {
    genv <- base::globalenv()
    genv$.Random.seed <- seed
  }

  ## Temporarily limit R option 'mc.cores'?
  if (!is.null(mc.cores)) {
    ...future.mc.cores.old <- base::getOption("mc.cores")
    base::options(mc.cores = mc.cores)
  }

  ## Record R options and environment variables
  ## Note, we do this _after_ loading and attaching packages, in
  ## case they set options/env vars needed for the session, e.g.
  ## https://github.com/Rdatatable/data.table/issues/5375
  ...future.oldOptions <- base::as.list(base::.Options)
  ...future.oldEnvVars <- base::Sys.getenv()

  ## covr: skip=7
  base::options(
    ## Prevent .future.R from being source():d when future is attached
    future.startup.script          = FALSE,
    
    ## Assert globals when future is created (or at run time)?
    future.globals.onMissing       = globals.onMissing,
    
    ## Pass down other future.* options
    future.globals.maxSize         = getOption("future.globals.maxSize"),
    future.globals.method          = getOption("future.globals.method"),
    future.globals.onReference     = getOption("future.globals.onReference"),
    future.globals.resolve         = getOption("future.globals.resolve"),
    future.resolve.recursive       = getOption("future.resolve.recursive"),
    future.rng.onMisuse            = getOption("future.rng.onMisuse"),
    future.rng.onMisuse.keepFuture = getOption("future.rng.onMisuse.keepFuture"),
    future.stdout.windows.reencode = getOption("future.stdout.windows.reencode"),

    ## Other options relevant to making futures behave consistently
    ## across backends
    width = getOption("width")
  )

  base::options(
    ## Prevent .future.R from being source():d when future is attached
    future.startup.script          = FALSE,
    
    ## Assert globals when future is created (or at run time)?
    future.globals.onMissing       = globals.onMissing
  )
  
  ## Record above future options
  ...future.futureOptionsAdded <- base::setdiff(base::names(base::.Options), base::names(...future.oldOptions))

  ## Record workding directory
  ...future.workdir <- getwd()

  if (globalenv) {
    ## Record names of variables in the global environment
    ...future.globalenv.names <- c(base::names(base::.GlobalEnv), "...future.value", "...future.globalenv.names", ".Random.seed")
  }

  ## Record the original future strategy set on this worker
  ...future.plan.old <- getOption("future.plan")
  ...future.plan.old.envvar <- Sys.getenv("R_FUTURE_PLAN", NA_character_)
  ...future.strategy.old <- future::plan("list")
    
  ## Prevent 'future.plan' / R_FUTURE_PLAN settings from being nested
  options(future.plan = NULL)
  Sys.unsetenv("R_FUTURE_PLAN")

  ## Use the next-level-down ("popped") future strategy
  ...future.oldPlan <- future::plan(strategiesR, .cleanup = FALSE, .init = FALSE)

  ## Start time for future evaluation
  ...future.startTime <- Sys.time()

  ## TROUBLESHOOTING: If the package fails to load, then library()
  ## suppress that error and generates a generic much less
  ## informative error message.  Because of this, we load the
  ## namespace first (to get a better error message) and then
  ## calls library(), which attaches the package. /HB 2016-06-16
  ## NOTE: We use local() here such that 'pkg' is not assigned
  ##       to the future environment. /HB 2016-07-03
  if (length(packages) > 0L) {
    base::local({
      for (pkg in packages) {
        base::loadNamespace(pkg)
        base::library(pkg, character.only = TRUE)
      }
    })
  }

  ## NOTE: We don't want to use local(body) w/ on.exit() because
  ## evaluation in a local is optional, cf. argument 'local'.
  ## If this was mandatory, we could.  Instead we use
  ## a tryCatch() statement. /HB 2016-03-14
  ...future.result <- base::tryCatch({
    base::withCallingHandlers({
      ...future.value <- base::withVisible(eval(expr, envir = envir))
      future::FutureResult(
        value = ...future.value$value,
        visible = ...future.value$visible,
        rng = !identical(base::globalenv()$.Random.seed, ...future.rng),
        globalenv = if (globalenv) list(added = base::setdiff(base::names(base::.GlobalEnv), ...future.globalenv.names)) else NULL,
        started = ...future.startTime,
        version = "1.8"
      )
    }, condition = base::local({
      ## WORKAROUND: If the name of any of the below objects/functions
      ## coincides with a promise (e.g. a future assignment) then we
      ## we will end up with a recursive evaluation resulting in error:
      ##   "promise already under evaluation: recursive default argument
      ##    reference or earlier problems?"
      ## To avoid this, we make sure to import the functions explicitly
      ## /HB 2018-12-22
      c <- base::c
      inherits <- base::inherits
      invokeRestart <- base::invokeRestart
      length <- base::length
      list <- base::list
      seq.int <- base::seq.int
      signalCondition <- base::signalCondition
      sys.calls <- base::sys.calls
      `[[` <- base::`[[`
      `+` <- base::`+`
      `<<-` <- base::`<<-`
        
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
              r          = base::R.Version(),
              locale     = base::Sys.getlocale(),
              rngkind    = base::RNGkind(),
              namespaces = base::loadedNamespaces(),
              search     = base::search(),
              system     = base::Sys.info()
            )
          }
         ## Record condition
          ...future.conditions[[length(...future.conditions) + 1L]] <<- list(
            condition = cond,
            calls     = c(sysCalls(from = ...future.frame), cond$call),
            session   = sessionInformation(),
            timestamp = base::Sys.time(),
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
            signaled = base::as.integer(signal)
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
    base::structure(base::list(
      value = NULL,
      visible = NULL,
      conditions = ...future.conditions,
      rng = !identical(base::globalenv()$.Random.seed, ...future.rng),
      started = ...future.startTime,
      finished = Sys.time(),
      session_uuid = NA_character_,
      version = "1.8"
    ), class = "FutureResult")
  }, finally = {
    ## Reset working directory
    if (!identical(...future.workdir, getwd())) setwd(...future.workdir)
    
    ## Reset R option 'mc.cores'
    if (!is.null(mc.cores)) {
      base::options(mc.cores = ...future.mc.cores.old)
    }
    
    ## (a) Reset options
    ## WORKAROUND: Do not reset 'nwarnings' unless changed, because
    ## that will, as documented, trigger any warnings collected
    ## internally to be removed.
    ## https://github.com/futureverse/future/issues/645
    if (identical(getOption("nwarnings"), ...future.oldOptions$nwarnings)) {
      ...future.oldOptions$nwarnings <- NULL
    }
    base::options(...future.oldOptions)

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
    ## diff <- base::setdiff(base::names(base::.Options),
    ##                       base::names(...future.oldOptions))
    ## if (base::length(diff) > 0L) {
    ##    opts <- base::vector("list", length = base::length(diff))
    ##    base::names(opts) <- diff
    ##    base::options(opts)
    ## }

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
      envs <- base::Sys.getenv()
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

      if (length(args) > 0) base::do.call(base::Sys.setenv, args = args)

      ## Not needed anymore
      args <- names <- old_names <- NAMES <- envs <- common <- added <- removed <- NULL
    } else {
      base::do.call(base::Sys.setenv, args = base::as.list(...future.oldEnvVars))
    }
    
    ## For the same reason as we don't remove added R options, we don't
    ## remove added environment variables until we know it's safe.
    ## /HB 2022-04-30
    ## (d) Remove any environment variables added
    ## diff <- base::setdiff(base::names(base::Sys.getenv()), base::names(...future.oldEnvVars))
    ## base::Sys.unsetenv(diff)

    ## Remove any "future" options added
    if (base::length(...future.futureOptionsAdded) > 0L) {
      opts <- base::vector("list", length = base::length(...future.futureOptionsAdded))
      base::names(opts) <- ...future.futureOptionsAdded
      base::options(opts)
    }

    ## Revert to the original future strategy
    ## Reset option 'future.plan' and env var 'R_FUTURE_PLAN'
    options(future.plan = ...future.plan.old)
    future::plan(...future.strategy.old, .cleanup = FALSE, .init = FALSE)
    if (is.na(...future.plan.old.envvar)) {
      Sys.unsetenv("R_FUTURE_PLAN")
    } else {
      Sys.setenv(R_FUTURE_PLAN = ...future.plan.old.envvar)
    }
  }) ## tryCatch(..., finally = { ... })
  
  if (!base::is.na(stdout)) {
    base::sink(type = "output", split = split)
    if (stdout) {
      ...future.result$stdout <- base::rawToChar(
        base::rawConnectionValue(...future.stdout)
      )
    } else {
      ...future.result["stdout"] <- base::list(NULL)
    }
    base::close(...future.stdout)
    ...future.stdout <- NULL
  }

  ...future.result$conditions <- ...future.conditions
  ...future.result$finished <- base::Sys.time()

  ## Undo .Random.seed
  genv <- base::globalenv()
  base::RNGkind(...future.rngkind)
  if (is.null(...future.rng)) {
    if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      rm(list = ".Random.seed", envir = genv, inherits = FALSE)
    }
  } else {
    assign(".Random.seed", ...future.rng, envir = genv, inherits = FALSE)
  }

  ...future.result
} ## evalFuture()

