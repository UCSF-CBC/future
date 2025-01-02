# Version (development version)

## New Features

 * All parallel backends now prevent nested parallelization, unless
   explicitly allowed, e.g. settings recognized by
   `parallelly::availableCores()` or set by the future
   `plan()`. Previously, this had to be implemented by each backend,
   but now it's handled automatically by the future framework.


# Version 1.34.0 [2024-07-29]

## New Features

 * Added support for backend maintainers to specify "cleanup" hook
   functions on future strategies, which are called when switching
   future plan. These hook functions are specified via the optional
   `cleanup` attribute, cf. `attr(cluster, "cleanup")`.

## Performance

  * Size calculation of globals is now done using the much faster
    `parallelly::serializedSize()`.

## Bug Fixes

 * `resolved()` for `ClusterFuture`:s would produce `Error:
   'inherits(future, "Future")' is not TRUE` instead of an intended,
   informative error message that the connection to the parallel
   worker is broken.


# Version 1.33.2 [2024-03-23]

## Performance

 * Decreased the overhead of launching futures that occurred for future
   strategies that used a complex `workers` argument. For example,
   `plan(cluster, workers = cl)`, where `cl` is a `cluster` object,
   would come with an extra overhead, because the `workers` object was
   unnecessarily transferred to the cluster nodes.

## Miscellaneous

 * Now `plan(multisession, workers = I(n))`, and same for `cluster`,
   preserves the "AsIs" class attribute on the `workers` argument so
   that it is propagated to `parallelly::makeClusterWorkers()`.

## Documentation

 * Clarify that packages must not change any of the `future.*` options.
 

# Version 1.33.1 [2023-12-21]

## Bug Fixes

 * `getExpression()` on 'cluster' future could under some
   circumstances call `local()` on the global search path rather than
   `base::local()` as intended.  For example, if a package that
   exports its own `local()` function was attached, then that would be
   called instead, often leading to a hard-to-troubleshoot error.
 

# Version 1.33.0 [2023-07-01]

## New Features

 * When a 'cluster' future fails to communicate with the parallel
   worker, it does a post-mortem analysis to figure out why, including
   inspecting whether the worker process is still alive or not.  In
   previous versions, this only worked for workers running on the
   current machine. Starting with this version, it also attempts to
   check this for remote versions.

## Bug Fixes

 * If a 'multicore' future failed, because the parallel process
   crashed, the corresponding parallel-worker slot was never released.
   Now it is removed if it can confirm that the forked worker process
   is no longer alive.

## Deprecated and Defunct

 * The 'multiprocess' strategy has now been fully removed.  Please use
   'multisession' (recommended) or 'multicore' instead.


# Version 1.32.0 [2023-03-06]

## New Features

 * Add prototype of an internal event-logging framework for the
   purpose of profiling futures and their backends.

 * Add option `future.globalenv.onMisuse` for optionally asserting
   that a future expression does not result in variables being added
   to the global environment.

 * Add option `future.onFutureCondition.keepFuture` for controlling
   whether `FutureCondition` objects should keep a copy of the
   `Future` object or not.  The default is to keep a copy, but if the
   future carries large global objects, then the `FutureCondition`
   will also be large, which can result in memory issues and slow
   downs.

## Miscellaneous

 * Fix a **future.tests** check that occurred only on MS Windows.

## Deprecated and Defunct

 * The 'multiprocess' strategy, which has been deprecated since future
   1.20.0 [2020-10-30] is now defunct.  Please use 'multisession'
   (recommended) or 'multicore' instead.
 
 * Add optional assertion of the internal Future `state` field.


# Version 1.31.0 [2023-01-31]

## Significant Changes

 * Remove function `remote()`.  Note that `plan(remote, ...)` has been
   deprecated since **future** 1.24.0 [2022-02-19] and defunct since
   **future** 1.30.0 (2022-12-15).

## Documentation

 * Add example to the 'Common Issues with Solutions' vignette on how
   **magrittr** pipes can result in an error when used with the future
   assignment operator and how to fix it.

## Bug Fixes

 * Error messages that contain a deparsed version of the future
   expression could become very large in cases where the expression
   comprise expanded, large objects. Now only the first 100 lines
   of the expression is deparsed.
   
## Deprecated and Defunct

 * Deprecated `plan(multiprocess, ...)` now equals `plan(sequential)`,
   while still producing one warning each time a future is created.

 * Argument `local` is defunct and has been removed.  Previously only
   `local = FALSE` was defunct.

 * Remove defunct argument `value` from all `resolve()` methods.

 * Remove defunct functions `transparent()` and `TransparentFuture()`.


# Version 1.30.0 [2022-12-15]

## Bug Fixes

 * `futureOf()` used `listenv::map()`, which is deprecated in
   **listenv** (>= 0.9.0) in favor of `listenv::mapping()`.

 * Starting with R (>= 4.2.0), the internal function `myInternalIP()`
   no longer detected when an attempted system call failed, resulting
   in an obscure error instead of falling back to alternatives.  This
   was because errors produced by `system2()` no longer inherits from
   class `simpleError`.

## Deprecated and Defunct

 * Strategy 'remote' was deprecated in **future** 1.24.0 and is now
   defunct. Use `plan(cluster, ..., persistent = TRUE)` instead. Note
   that `persistent = TRUE` will eventually also become deprecated and
   defunct, but by then we will have an alternative solution
   available.


# Version 1.29.0 [2022-11-05]

## Documentation

 * Add section 'Making sure to stop parallel workers' to the 'Best
   Practices for Package Developers', which explains why `R CMD check`
   may produce "checking for detritus in the temp directory ... NOTE"
   and how to avoid them.

## Bug Fixes

 * The evaluation of a _sequential_ future would reset any warnings
   collected by R prior to creating the future.  This only happened
   with `plan(sequential)` and when `getOption("warn") == 0`.
   This bug was introduced in **future** 1.26.0 [2022-05-27].

## Deprecated and Defunct

 * Using the deprecated `plan(multiprocess)` will now trigger a
   deprecation warning _each_ time a `multiprocess` future is created.
   This means that there could be a lot of warnings produced.  Note
   that `multiprocess` has been deprecated since **future** 1.20.0
   [2020-10-30].  Please use `multisession` (recommended) or
   `multicore` instead.

 * Removing `values()`, which has been defunct since **future**
   1.23.0. Use `value()` instead.



# Version 1.28.0 [2022-09-02]

## Documentation

 * Mention how `source(..., local = TRUE)` is preferred over
   `source()` when used inside futures.

## Bug Fixes

 * `do.call(plan, args = list(multisession, workers = 2))` would
   ignore the `workers` argument, and any other arguments.

## Deprecated and Defunct

 * Previously deprecated use of `local = FALSE` with futures is now 
   defunct.

 * The R option to temporarily allow `plan(transparent)` although it
   was declared defunct has now been removed; `plan(transparent)`,
   together with functions `transparent()` and `TransparentFuture()`
   are now formally defunct.

 * Using argument `persistent` with multisession futures is now
   defunct.  Previously only `persistent = TRUE` was defunct.

## Miscellaneous

 * Use CSS style to align image to the right instead of non-HTML5
   attribute `align="right"`.

 * Avoid nested `<em>` tags in HTML-generated help pages.
 

# Version 1.27.0 [2022-07-21]

## New Features

 * The fallback to sequential processing done by 'multicore' and
   'multisession' when `workers = 1` can now be overridden by
   specifying `workers = I(1)`.
 
## Bug Fixes

 * Some warnings and errors showed the wrong call.

 * `print()` for `FutureResult` would report captured conditions all
   with class `list`, instead of their condition classes.
   

# Version 1.26.1 [2022-05-28]

## Miscellaneous

 * TESTS: `R CMD check --as-cran` on R-devel and MS Windows would
   trigger a NOTE on "Check: for detritus in the temp directory" and
   "Found the following files/directories: 'Rscript1349cb8aeeba0'
   ...". There were two package tests that explicitly created PSOCK
   cluster without stopping them. A third test launched multisession
   future without resolving it, which prevented the PSOCK worker to
   terminate. This was not detected in R 4.2.0.  It is not a problem
   on macOS and Linux, because there background workers are
   automatically terminated when the main R session terminates.
   

# Version 1.26.0 [2022-05-27]

## Significant Changes

 * R options and environment variables are now reset on the workers
   after future is resolved as they were after any packages required
   by the future has been loaded and attached. Previously, they were
   reset to what they were before these were loaded and attached. In
   addition, only pre-existing R options and environment variables are
   reset. Any new ones added are not removed for now, because we do
   not know which added R options or environment variables might have
   been added from loading a package and that are essential for that
   package to work.

 * If it was changed while evaluating the future expression, the
   current working directory is now reset when the future has been
   resolved.

## New Features

 * `futureSessionInfo()` gained argument `anonymize`. If TRUE
   (default), host and user names are anonymized.

 * `futureSessionInfo()` now also report on the main R session
   details.

## Bug Fixes

 * The bug fix in **future** 1.22.0 that addressed the issue where
   object `a` in `future(fcn(), globals = list(a = 42, fcn =
   function() a))` would not be found has been redesigned in a more
   robust way.

 * Use of packages such as **data.table** and **ff** in cluster and
   multisession futures broke in **future** 1.25.0. For
   **data.table**, we saw "Error in setalloccol(ans) : verbose must be
   TRUE or FALSE".  For **ff**, we saw "Error in splitted$path[nopath]
   <- getOption("fftempdir") : replacement has length zero".  See
   'Significant Changes' for why and how this was fixed.

 * The deprecation warning for using `local = FALSE` was silenced for
   sequential futures since **future** 1.25.0.

 * `futureCall()` ignored arguments `stdout`, `conditions`,
   `earlySignal`, `label`, and `gc`.

## Deprecated and Defunct

 * Strategy 'transparent' was deprecated in **future** 1.24.0 and is
   now defunct. Use `plan(sequential, split = TRUE)` instead.

 * Strategy 'multiprocess' was deprecated in **future** 1.20.0, and
   'remote' was deprecated in **future** 1.24.0.  Since then, attempts
   to use them in `plan()` would produce a deprecation warning, which
   was limited to one per R session.  Starting with this release, this
   warning is now produced whenever using `plan()` with these
   deprecated future strategies.


# Version 1.25.0 [2022-04-23]

## Significant Changes

 * R options and environment variables are now reset on the workers
   after future is resolved so that any changes to them by the future
   expression have no effect on following futures.

## New Features

 * Now `f <- future(..., stdout = structure(TRUE, drop = TRUE))` will
   cause the captured standard output to be dropped from the future
   object as soon as it has been relayed once, for instance, by
   `value(f)`. Similarly, `conditions = structure("conditions", drop =
   TRUE)` will drop captured non-error conditions as soon as they have
   been relayed.  This can help decrease the amount of memory used,
   especially if there are many active futures.

 * Now `resolve()` respects option `future.wait.interval`. Previously,
   it was hardcoded to poll for results every 0.1 seconds.

## Beta Features

  * Now, `value()` will only attempt to recover UTF-8 symbols in the
    captured standard output if the future was evaluated on an MS
    Windows that does not support capturing of UTF-8 symbols. Support
    for UTF-8 capturing on also MS Windows was added in R 4.2.0, but
    it typically requires an up-to-date MS Windows 10 or MS Windows
    Server 2022.

## Performance

 * The default value for option `future.wait.interval` was decreased
   from 0.2 seconds to 0.01 seconds. This controls the polling
   frequency for finding an available worker when all workers are
   currently busy. Starting with this release, this option also
   controls the polling frequency of `resolve()`.

## Bug Fixes

 * A bug was introduced in **future** 1.24.0 [2022-02-19] that caused
   future plan tweaking to break, e.g. `plan(multicore, workers = 2)`
   and `plan(sequential, split = TRUE)` introduced breaking side
   effects to the futures evaluated.


# Version 1.24.0 [2022-02-19]

## Significant Changes

 * Now `future(..., seed = TRUE)` forwards the RNG state in the
   calling R session. Previously, it would leave it intact.

## New Features

 * Now `plan()` and `tweak()` preserve calls in arguments,
   e.g. `plan(multisession, workers = 2, rscript_startup =
   quote(options(socketOptions="no-delay")))`, and `tweak(..., abc =
   quote(x == y))`.

## Bug Fixes

 * `nbrOfFreeWorkers()` would produce "Error: 'is.character(name)' is
   not TRUE" for `plan(multisession, workers = 1)`.

 * Internal calls to `FutureRegistry(action = "collect-first")` and
   `FutureRegistry(action = "collect-last")` could signal errors early
   when polling `resolved()`.

## Deprecated and Defunct

 * Strategy 'remote' is deprecated in favor of 'cluster'.  The
   `plan()` function will give an informative deprecation warning when
   'remote' is used.  For now, this warning is given only once per R
   session.

 * Strategy 'transparent' is deprecated in favor of 'sequential' with
   argument `split = TRUE` set.  The `plan()` function will give an
   informative deprecation warning when 'transparent' is used.  For
   now, this warning is given only once per R session.


# Version 1.23.0 [2021-10-30]

## Significant Changes

 * `plan()` now produces a one-time warning if a 'transparent'
   strategy is set.  The warning reminds the user that 'transparent'
   should only be used for troubleshooting purposes and never be used
   in production. These days `plan(sequential, split = TRUE)` together
   with `debug()` is probably a better approach for
   troubleshooting. The long-term plan is to deprecate the
   'transparent' strategy.

 * Support for `persistent = TRUE` with multisession futures is
   defunct.

## Beta Features

 * UTF-8 symbols outputted on MS Windows would be relayed as escaped
   symbols, e.g. a UTF-8 check mark symbol (`\u2713`) would be relayed
   as `<U+2713>` (8 characters).  The reason for this is a limitation
   in R itself on MS Windows.  Now, `value()` attempts to recover such
   MS Windows output to UTF-8 before relaying it.  There is an option
   for disabling this new feature.

## Miscellaneous

 * TESTS: Using more robust emulation of crashed forked parallel
   workers after understanding that `quit()` must not be used in
   forked R processes.

## Bug Fixes

 * Now `future(..., seed)` will set the random seed as late as
   possible just before the future expression is evaluated.
   Previously it was done before package dependencies where attached,
   which could lead to non-reproduce random numbers in case a package
   dependency would update the RNG seed when attached.

## Deprecated and Defunct

 * `values()`, which has been deprecated since **future** 1.20.0, is
   now defunct.  Use `value()` instead.

 * Support for `persistent = TRUE` with multisession futures is
   defunct.  If still needed, a temporary workaround is to use cluster
   futures. However, it is likely that support for `persistent` will
   eventually be deprecated for all future backends.

 * Argument `value` of `resolve()`, deprecated since **future**
   1.15.0, is defunct in favor of argument `result`.
   

# Version 1.22.1 [2021-08-11]

## Miscellaneous

 * Disable package test that emulates crashing of forked parallel
   workers when using `parallel::makeCluster(..., type = "FORK")`.
   This test is disabled on macOS, where it appears that the main R
   session becomes unstable after the FORK node is terminated.
 

# Version 1.22.0 [2021-08-11]

## Significant Changes

 * A lazy future remains a generic future until it is launched, which
   means it is not assigned a future backend class until launched.

 * Argument `seed` for `futureAssign()` and `futureCall()` now
   defaults to FALSE just like for `future()`.

 * `R_FUTURE_*` environment variables are now only read when the
   **future** package is loaded, where they set the corresponding
   `future.*` option.  Previously, some of these environment variables
   were queried by different functions as a fallback to when an option
   was not set.  By only parsing them when the package is loaded, it
   decrease the overhead in functions, and it clarifies that options
   can be changed at runtime whereas environment variables should only
   be set at startup.

## Performance

 * The overhead of initiating futures have been significantly reduced.
   For example, the roundtrip time for `value(future(NULL))` is about
   twice as fast for 'sequential', 'cluster', and 'multisession'
   futures.  For 'multicore' futures the roundtrip speedup is about
   20%.  The speedup comes from pre-compiling the R expression that
   will be used to resolve the future expression into R expression
   templates which then can quickly compiled for each future. This
   speeds up the creation of these expression by ~10 times, compared
   when re-compiling them each time.

 * The default timeout for `resolved()` was decreased from 0.20 to
   0.01 seconds for cluster/multisession and multicore futures, which
   means they will spend less time waiting for results when they are
   not available.

## New Features

 * Analogously to how globals may be scanned for "non-exportable"
   objects when option `future.globals.onReference` is set to
   `"error"` or `"warning"`, `value()` will now check for similar
   problems in the resolved value object.  An example of this is `f <-
   future(xml2::read_xml("<body></body>"))`, which will result in an
   invalid `xml_document` object if run in parallel, because such
   objects cannot be transferred between R processes.

 * In addition to specify which condition classes to be captured and
   relayed, it is now possible to also specify condition classes to be
   ignored.  For example, `conditions = structure("condition", exclude
   = "message")` captures all conditions but message conditions.

 * Now cluster futures use `homogeneous = NULL` as the default instead
   of `homogeneous = TRUE`.  The new default will result in the
   **parallelly** package trying to infer whether TRUE or FALSE should
   be used based on the `workers` argument.

 * Now the the post-mortem analysis report of multicore and cluster
   futures in case their results could not be retrieved include
   information on globals and their sizes, and if some of them are
   non-exportable.  A similar, detailed report is also produced when a
   cluster future fails to set up and launch itself on a parallel
   worker.

 * if option `future.fork.multithreading.enable` is FALSE,
   **RcppParallel**, in addition to **OpenMP**, is forced to run with
   a single threaded whenever running in a forked process
   (='multicore' futures).  This is done by setting environment
   variable `RCPP_PARALLEL_NUM_THREADS` to 1.

 * Add `futureSessionInfo()` to get a quick overview of the future
   framework, its current setup, and to run simple tests on it.

 * Now `plan(multicore)` warns immediately if multicore processing,
   that is, forked processing, is not supported, e.g. when running in
   the RStudio Console.

## Bug Fixes

 * `plan(multiprocess, workers = n)` did not warn about 'multiprocess'
   being deprecated when argument `workers` was specified.

 * `getGlobalsAndPackages()` could throw a false error on "Did you
   mean to create the future within a function? Invalid future
   expression tries to use global `...` variables that do not exist:
   <some expression>" when `...`  is solely part of a formula or used
   in some S4 generic functions.

 * When enabled, option `future.globals.onReference` could falsely
   alert on 'Detected a non-exportable reference (externalptr) in one
   of the globals (<unknown\>) used in the future expression' in
   globals, e.g. when using **future.apply** or **furrr** map-reduce
   functions when using a 'multisession' backend.

 * `future(fcn(), globals = list(a = 42, fcn = function() a))` would
   fail with "Error in fcn() : object 'a' not found" when using
   sequential or multicore futures.  This affected also map-reduce
   calls such as `future.apply::future_lapply(1, function(x) a,
   future.globals = list(a = 42))`.

 * Resolving a 'sequential' future without globals would result in
   internal several `...future.*` objects being written to the calling
   environment, which might be the global environment.

 * Environment variable `R_FUTURE_PLAN` would propagate down with
   nested futures, forcing itself onto also nested future plans.  Now
   it is unset in nested futures, resulting in a sequential future
   strategy unless another was explicitly set by `plan()`.

 * Transparent futures no longer warn about `local = FALSE` being
   deprecated.  Although `local = FALSE` is being deprecated, it is
   still used internally by 'transparent' futures for a while longer.
   Please do not use 'transparent' futures in production code and
   never in a package.

 * `remote()` could produce an error on "object 'homogeneous' not
   found".

 * `nbrOfFreeWorkers()` for 'cluster' futures assumed that the current
   plan is set to cluster too.


# Version 1.21.0 [2020-12-09]

## New Features

 * In order to handle them conditionally higher up in the call chain,
   warnings and errors produced from using the random number generator
   (RNG) in a future without declaring the intention to use one are
   now of class `RngFutureWarning` and `RngFutureError`, respectively.
   Both of these classes inherits from `RngFutureCondition`.

 * Now run-time errors from resolving a future take precedence over
   `RngFutureError`:s. That is, `future({ rnorm(1); log("a") }, seed =
   FALSE)` will signal an error 'log("a")' instead of an RNG error
   when option `future.rng.onMisuse` is set to `"error"`.

## Beta Features

 * Add `nbrOfFreeWorkers()` to query how many workers are free to take
   on futures immediately.  Until all third-party future backends have
   implemented this, some backends might produce an error saying it is
   not yet supported.
   
## Bug Fixes

 * `future(..., seed = TRUE)` with 'sequential' futures would set the
   RNG kind of the parent process.  Now it behaves the same regardless
   of future backend.

 * Signaling `immediateCondition`:s with 'multicore' could result in
   `Error in save_rds(obj, file) : save_rds() failed to rename
   temporary save file
   '/tmp/RtmpxNyIyK/progression21f3f31eadc.rds.tmp' (NA bytes; last
   modified on NA) to '/tmp/RtmpxNyIyK/progression21f3f31eadc.rds' (NA
   bytes; last modified on NA)`.  There was an assertion at the end of
   the internal `save_rds()` function that incorrectly assumed that
   the target file should exist.  However, the file might have already
   been processed and removed by the future in the main R session.

 * `value()` with both a run-time error and an RNG mistake would
   signal the RNG warning instead of the run-time error when the
   for-internal-use-only argument `signal` was set to FALSE.

 * Due to a mistake introduced in **future** 1.20.0, the package would
   end up assigning a `.packageVersion` object to the global
   environment when loaded.
 

# Version 1.20.1 [2020-10-30]

## Bug Fixes

 * `future::plan("multisession")` would produce 'Error in if (debug)
   mdebug("covr::package_coverage() workaround ...") : argument is not
   interpretable as logical' if and only if the **covr** package was
   loaded.


# Version 1.20.0 [2020-10-30]

## Significant Changes

 * Strategy 'multiprocess' is deprecated in favor of either
   'multisession' or 'multicore', depending on operating system and R
   setup.  The `plan()` function will give an informative deprecation
   warning when 'multiprocess' is used.  This warning is given only
   once per R session.

 * Launching R or Rscript with command-line option `--parallel=n`,
   where n > 1, will now use 'multisession' as future strategy.
   Previously, it would use 'multiprocess', which is now deprecated.

 * Support for `local = FALSE` is deprecated.  For the time being, it
   remains supported for 'transparent' futures and 'cluster' futures
   that use `persistent = TRUE`.  However, note that `persistent =
   TRUE` will also deprecated at some point in the future.  These
   deprecations are required in order to further standardize the
   Future API across various types of parallel backends.

 * Now multisession workers inherit the package library path from the
   main R session _when they are created_, that is, when calling
   `plan(multisession)`.  To avoid this, use `plan(multisession,
   rscript_libs = NULL)`, which is an argument passed down to
   `makeClusterPSOCK()`.  With this update, 'sequential',
   'multisession', and 'multicore' futures see the exact same library
   path.

 * Several functions for managing **parallel**-style processing have
   been moved to a new **parallelly** package.  Specifically,
   functions `availableCores()`, `availableWorkers()`,
   `supportsMulticore()`, `as.cluster()`, `autoStopCluster()`,
   `makeClusterMPI()`, `makeClusterPSOCK()`, and `makeNodePSOCK()`
   have been moved.  None of them are specific to futures per se and
   are likely useful elsewhere too.  Also, having them in a separate,
   standalone package will speed up the process of releasing any
   updates to these functions.  The code base of the **future**
   package shrunk about 10-15% from this migration.  For backward
   compatibility, the migrated functions remain in this package as
   re-exports.

## New Features

 * Setting up a future strategy with argument `split = TRUE` will
   cause the standard output and non-error conditions to be split
   ("tee:d") on the worker's end, while still relaying back to the
   main R session as before.  This can be useful when debugging with
   `browse()` or `debug()`, e.g.  `plan(sequential, split = TRUE)`.
   Without it, debug output is not displayed.

 * Now multicore futures relay `immediateCondition`:s in a near-live
   fashion.

 * It is now possible to pass any arguments that `makeClusterPSOCK()`
   accepts in the call to `plan(cluster, ...)` and `plan(multisession,
   ...)`.  For instance, to set the working directory of the cluster
   workers to a temporary folder, pass argument `rscript_startup =
   "setwd(tempdir())"`.  Another example is `rscript_libs = c(libs,
   "*")` to prepend the library path on the worker with the paths in
   `libs`.

 * `plan()` and `tweak()` check for even more arguments that must not
   be set by either of them.  Specifically, attempts to adjust the
   following arguments of `future()` will result in an error:
   `conditions`, `envir`, `globals`, `packages`, `stdout`, and
   `substitute` in addition to already validated `lazy` and `seed`.
 
 * `tweak()` now returns a wrapper function that calls the original future
   strategy function with the modified defaults.  Previously, it would make a
   copy of the original function with modified argument defaults.  This new
   approach will make it possible to introduce new future arguments that can
   be modified by `tweak()` and `plan()` without having to update every future
   backend package, e.g. the new `split = TRUE` argument.

## Documentation

 * Add a 'Best Practices for Package Developers' vignette.

 * Add a 'How the Future Framework is Validated' vignette.

## Miscellaneous

 * Harmonizing Future constructor functions to also use `substitute =
   TRUE`.

## Bug Fixes

 * Since last version, **future** 1.19.1, `future(..., conditions =
   character(0L))` would no longer avoid intercepting conditions as
   intended; instead, it muffles all conditions.  From now on, use
   `conditions = NULL`.

 * Relaying of `immediateCondition`:s was not near-live for
   multisession and cluster if the underlying PSOCK cluster used
   `useXDR=FALSE` for communication.

 * `print()` for Future would also print any attributes of its
   environment.

 * The error message produced by ``nbrOfWorkers()`` was incomplete.
 
 * Renamed environment variable `R_FUTURE_MAKENODEPSOCK_tries` used by
   `makeClusterPSOCK()` to `R_FUTURE_MAKENODEPSOCK_TRIES`.

 * The Mandelbrot demo would produce random numbers without declaring so.

## Deprecated and Defunct

 * Strategy 'multiprocess' is deprecated in favor of either
   'multisession' or 'multicore', depending on operating system and R
   setup.

 * `values()` is deprecated. Use `value()` instead.

 * All backward compatible code for the legacy, defunct, internal
   `Future` element `value` is now removed.  Using or relying on it is
   an error.
 

# Version 1.19.1 [2020-09-21]

## Bug Fixes

 * When passing `...` as a globals, rather than via arguments, in
   higher-level map-reduce APIs such as **future.apply** and
   **furrr**, arguments in `...` could produce an error on "unused
   argument".
   
 
# Version 1.19.0 [2020-09-19]

## Significant Changes

 * Futures detect when random number generation (RNG) was used to
   resolve them.  If a future uses RNG without parallel RNG was
   requested, then an informative warning is produced. To request
   parallel RNG, specify argument `seed`, e.g.  `f <- future(rnorm(3),
   seed = TRUE)` or `y %<-% { rnorm(3) } %seed% TRUE`.  Higher-level
   map-reduce APIs provide similarly named "seed" arguments to achieve
   the same.  To, escalate these warning to errors, set option
   `future.rng.onMisuse` to `"error"`.  To silence them, set it to
   `"ignore"`.

 * Now, all non-captured conditions are muffled, if possible.  For
   instance, `future(warning("boom"), conditions = c("message"))` will
   truly muffle the warning regardless of backend used.  This was
   needed to fix below bug.

## New Features

 * `makeClusterPSOCK()` will now retry to create a cluster node up to
   `tries` (default: 3) times before giving up.  If argument `port`
   species more than one port (e.g. `port = "random"`) then it will
   also attempt find a valid random port up to `tries` times before
   giving up.  The pre-validation of the random port is only supported
   in R (>= 4.0.0) and skipped otherwise.

 * `makeClusterPSOCK()` skips shell quoting of the elements in
   `rscript` if it inherits from `AsIs`.

 * `makeClusterPSOCK()`, or actually `makeNodePSOCK()`, gained
   argument `quiet`, which can be used to silence output produced by
   `manual = TRUE`.

 * If multithreading is disabled but multicore futures fail to
   acknowledge the setting on the current system, then an informative
   `FutureWarning` is produced by such futures.

 * Now `availableCores()` better supports Slurm.  Specifically, if
   environment variable `SLURM_CPUS_PER_TASK` is not set, which
   requires that option `--slurm-cpus-per-task=n` is specified and
   `SLURM_JOB_NUM_NODES=1`, then it falls back to using
   `SLURM_CPUS_ON_NODE`, e.g. when using `--ntasks=n`.

 * Now `availableCores()` and `availableWorkers()` supports
   LSF/OpenLava.  Specifically, they acknowledge environment variable
   `LSB_DJOB_NUMPROC` and `LSB_HOSTS`, respectively.

## Performance

 * Now `plan(multisession)`, `plan(cluster, workers = <number>)`, and
   `makeClusterPSOCK()` which they both use internally, sets up
   localhost workers twice as fast compared to versions since
   **future** 1.12.0, which brings it back to par with a bare-bone
   `parallel::makeCluster(..., setup_strategy = "sequential")` setup.
   The slowdown was introduced in **future** 1.12.0 (2019-03-07) when
   protection against leaving stray R processes behind from failed
   worker startup was implemented.  This protection now makes use of
   memoization for speedup.

## Bug Fixes

 * Sequential and multicore backends, but not multisession, would
   produce errors on "'...' used in an incorrect context" in cases
   where `...` was part of argument `globals` and not the evaluation
   environment.

 * Contrary to other future backends, any conditions produced while
   resolving a sequential future using `future(..., conditions =
   character())` would be signaled, although the most reasonable
   expectation would be that they are silenced. Now, all non-captured
   conditions are muffled, if possible.

 * Option `future.rng.onMisuse` was not passed down to nested futures.
 
 * Disabling multithreading in forked processes by setting R option
   `future.fork.multithreading.enable` or environment variable
   `R_FUTURE_FORK_MULTITHREADING_ENABLE` to `FALSE` would cause
   multicore futures to always return value `1L`.  This bug was
   introduced in **future** 1.17.0 (2020-04-17).

 * `getGlobalsAndPackages()` did not always return a `globals` element
   that was of class `FutureGlobals`.
   
 * `getGlobalsAndPackages(..., globals)` would recalculate
   `total_size` even when it was already calculated or known to be
   zero.
   
 * `getGlobalsAndPackages(Formula::Formula(~ x))` would produce "the
   condition has length > 1" warnings (which will become errors in
   future R versions).


# Version 1.18.0 [2020-07-08]

## Significant Changes

 * Support for `persistent = TRUE` with multisession futures is
   deprecated.

## New Features

 * `print()` on `RichSOCKcluster` gives information not only on the
   name of the host but also on the version of R and the platform of
   each node ("worker"), e.g. "Socket cluster with 3 nodes where 2
   nodes are on host 'localhost' (R version 4.0.0 (2020-04-24),
   platform x86_64-w64-mingw32), 1 node is on host 'n3' (R version
   3.6.3 (2020-02-29), platform x86_64-pc-linux-gnu)".

 * Error messages from cluster future failures are now more
   informative than "Unexpected result (of class 'NULL' !=
   'FutureResult')".  For example, if the **future** package is not
   installed on the worker, then the error message clearly says so.
   Even, if there is an unexpected result error from a PSOCK cluster
   future, then the error produced give extra information on node
   where it failed, e.g. "Unexpected result (of class 'NULL' !=
   'FutureResult') retrieved for ClusterFuture future (label =
   '<none>', expression = '...'): This suggests that the communication
   with `ClusterFuture` worker ('RichSOCKnode' #1 on host 'n3' (R
   version 3.6.3 (2020-02-29), platform x86_64-pc-linux-gnu)) is out
   of sync."

 * It is now possible to set environment variables on workers before
   they are launched by `makeClusterPSOCK()` by specify them as as
   `"<name>=<value>"` as part of the `rscript` vector argument,
   e.g. `rscript = c("ABC=123", "DEF='hello world'", "Rscript")`. This
   works because elements in `rscript` that match regular expression
   `[[:alpha:]_][[:alnum:]_]*=.*` are no longer shell quoted.

 * `makeClusterPSOCK()` now returns a cluster that in addition to
   inheriting from `SOCKcluster` it will also inherit from
   `RichSOCKcluster`.

## Bug Fixes

 * Made `makeClusterPSOCK()` and `makeNodePSOCK()` agile to the name
   change from `parallel:::.slaveRSOCK()` to `parallel:::.workRSOCK()`
   in R (>= 4.1.0).

 * `makeClusterPSOCK(..., rscript)` will not try to locate
   `rscript[1]` if argument `homogeneous` is FALSE (or inferred to be
   FALSE).

 * `makeClusterPSOCK(..., rscript_envs)` would result in a syntax
   error when starting the workers due to non-ASCII quotation marks if
   option `useFancyQuotes` was not set to FALSE.

 * `plan(list(...))` would produce 'Error in UseMethod("tweak") : no
   applicable method for 'tweak' applied to an object of class "list"'
   if a non-function object named 'list' was on the search path.

 * `plan(x$abc)` with x <- list(abc = sequential) would produce 'Error
   in UseMethod("tweak") : no applicable method for 'tweak' applied to
   an object of class "c('FutureStrategyList', 'list')"'.

 * TESTS: `R_FUTURE_FORK_ENABLE=false R CMD check ...` would produce
   'Error: connections left open: ...' when checking the
   'multiprocess' example.

## Deprecated and Defunct

 * Support for `persistent = TRUE` with multisession futures is
   deprecated.  If still needed, a temporary workaround is to use
   cluster futures. However, it is likely that support for
   `persistent` will eventually be deprecated for all future backends.
 
 * Options `future.globals.method`, `future.globals.onMissing`, and
   `future.globals.resolve` are deprecated and produce warnings if
   set. They may only be used for troubleshooting purposes because
   they may affect how futures are evaluated, which means that
   reproducibility cannot be guaranteed elsewhere.
 

# Version 1.17.0 [2020-04-17]

## Significant Changes

 * Renamed `values()` to `value()` to clean up and simplify the API.

## New Features

 * `makeClusterPSOCK()` gained argument `rscript_envs` for setting
   environment variables in workers on startup, e.g. `rscript_envs =
   c(FOO = "3.14", "BAR")`.

 * Now the result of a future holds session details in case an error
   occurred while evaluating the future.

## Miscellaneous

 * Not all CRAN servers have `_R_CHECK_LIMIT_CORES_` set.  To better
   emulate CRAN submission checks, the **future** package will, when
   loaded, set this environment variable to 'TRUE' if unset and if `R
   CMD check` is running.  Note that `future::availableCores()`
   respects `_R_CHECK_LIMIT_CORES_` and returns at most `2L` (two
   cores) if detected.

## Bug Fixes

 * Any globals named `version` and `has_future` would be overwritten
   with "garbage" values internally.

 * Disabling of multi-threading when using 'multicore' futures did not
   work on all platforms.

## Deprecated and Defunct

 * All `values()` S3 methods have been renamed to `value()` since they
   are closely related to the original purpose `value()`.  The
   `values()` methods will continue to work but will soon be formally
   deprecated and later be made defunct and finally be removed.
   Please replace all `values()` with `value()` calls.
 

# Version 1.16.0 [2020-01-15]

## Significant Changes

 * Now `oplan <- plan(new_strategy)` returns the list of all nested
   strategies previously set, instead of just the strategy on top of
   this stack. This makes it easier to temporarily use another
   plan. For the old behavior, use `oplan <- plan(new_strategy)[[1]]`.

## New Features

 * Now `value()` detects if a `future(..., seed = FALSE)` call
   generated random numbers, which then might give unreliable results
   because non-parallel safe, non-statistically sound random number
   generation (RNG) was used.  If option `future.rng.onMisuse` is
   `"warning"`, a warning is produced. If `"error"`, an error is
   produced.  If `"ignore"` (default), the mistake is silently
   ignored. Using `seed = NULL` is like `seed = FALSE` but without
   performing the RNG validation.

 * For convenience, argument `seed` of `future()` may now also be an
   ordinary single integer random seed.  If so, a L'Ecuyer-CMRG RNG
   seed is created from this seed.  If `seed = TRUE`, then a
   L'Ecuyer-CMRG RNG seed based on the current RNG state is used.  Use
   `seed = FALSE` when it is known that the future does not use RNG.

 * `ClusterFuture`:s now relay `immediateCondition`:s back to the main
   process momentarily after they are signaled and before the future
   is resolved.

## Beta Features

 * Add support for automatically disable multi-threading when using
   'multicore' futures. For now, the default is to allow
   multi-threaded processing but this might change in the future. To
   disable multi-threaded, set option
   `future.fork.multithreading.enable` or environment variable
   `R_FUTURE_FORK_MULTITHREADING_ENABLE` to `FALSE`. This requires
   that **RhpcBLASctl** package is installed. Parallelization via
   multi-threaded processing (done in native code by some packages and
   externally library) while at the same time using forked (aka
   "multicore") parallel processing is unstable in some cases.  Note
   that this is not only true when using `plan(multicore)` but also
   when using, for instance, `parallel::mclapply()`.  This is in beta
   so the above names and options might change later.

## Bug Fixes

 * Evaluation of futures could fail if the global environment
   contained *functions* with the same names as a small set of base R
   functions, e.g.  `raw()`, `list()`, and `options()`.

 * `future(alist(a =))` would produce "Error in objectSize_list(x,
   depth = depth - 1L) : argument "x_kk" is missing, with no default"

## Deprecated and Defunct

 * `Future` and `FutureResult` objects with an internal version 1.7 or
   older have been deprecated since 1.14.0 (July 2019) and are now
   defunct.
   
 * Defunct hidden argument `progress` of `resolve()`, and hidden
   arguments/fields `condition` and `calls` of `FutureResult` are now
   gone.


# Version 1.15.1 [2019-11-23]

## New Features

 * The default range of ports that `makeClusterPSOCK()` draws a random
   port from (when argument `port` is not specified) can now be
   controlled by environment variable `R_FUTURE_RANDOM_PORTS`.  The
   default range is still `11000:11999` as with the **parallel**
   package.
 
## Bug Fixes

 * The change introduced to `resolved()` in **future** 1.15.0 would
   cause lazy futures to block if all workers were occupied.


# Version 1.15.0 [2019-11-07]

## Significant Changes

 * `resolved()` will now launch lazy futures.

## New Features

 * Now the "visibility" of future values is recorded and reflected by
   `value()`.

 * Now option `future.globals.onReference` defaults to environment
   variable `R_FUTURE_GLOBALS_ONREFERENCE`.

## Documentation

 * Added 'Troubleshooting' section to `?makeClusterPSOCK` with
   instructions on how to troubleshoot when the setup of local and
   remote clusters fail.

## Bug Fixes

 * `values()` would resignal `immediateCondition`:s despite those
   should only be signaled at most once per future.

 * `makeClusterPSOCK()` could produce warnings like "cannot open file
   '/tmp/alice/Rtmpi69yYF/future.parent=2622.a3e32bc6af7.pid': No such
   file", e.g. when launching R workers running in Docker containers.
   
 * Package would set or update the RNG state of R (`.Random.seed`)
   when loaded, which could affect RNG reproducibility.
   
 * Package could set `.Random.seed` to NULL, instead of removing it,
   which in turn would produce a warning on "'.Random.seed' is not an
   integer vector but of type 'NULL', so ignored" when the next random
   number generated.

 * Now a future assignment to list environments produce more
   informative error messages if attempting to assign to more than one
   element.
   
 * `makeClusterMPI()` did not work for MPI clusters with `comm` other
   than `1`.

## Deprecated and Defunct

 * Argument `value` of `resolve()` is deprecated. Use `result`
   instead.

 * Use of internal argument `evaluator` to `future()` is now defunct.


# Version 1.14.0 [2019-07-01]

## Significant Changes

 * All types of conditions are now captured and relayed.  Previously,
   only conditions of class `message` and `warning` were relayed.

 * If one of the futures in a collection produces an error, then
   `values()` will signal that error as soon as it is detected.  This
   means that while calling `values()` guarantees to resolve all
   futures, it does not guarantee that the result from all futures are
   gathered back to the master R session before the error is relayed.

## New Features

 * `values()` now relays `stdout` and signal as soon as possible as
   long as the standard output and the conditions are relayed in their
   original order.

 * If a captured condition can be "muffled", then it will be muffled.
   This helps to prevent conditions from being handled twice by
   condition handlers when futures are evaluated in the main R
   session, e.g. `plan(sequential)`.  Messages and warnings were
   already muffled in the past.

 * Forked processing is considered unstable when running R from
   certain environments, such as the RStudio environment.  Because of
   this, 'multicore' futures have been disabled in those cases since
   **future** 1.13.0.  This change caught several RStudio users by
   surprise.  Starting with **future** 1.14.0, an informative
   one-time-per-session warning will be produced when attempts to use
   'multicore' is made in non-supported environments such as RStudio.
   This warning will also be produced when using 'multiprocess', which
   will fall back to using 'multisession' futures.  The warning can be
   disabled by setting R option `future.supportsMulticore.unstable`,
   or environment variable `FUTURE_SUPPORTSMULTICORE_UNSTABLE` to
   `"quiet"`.

 * Now option `future.startup.script` falls back to environment
   variable `R_FUTURE_STARTUP_SCRIPT`.
   
 * Conditions inheriting `immediateCondition` are signaled as soon as
   possible.  Contrary to other types of conditions, these will be
   signaled only once per future, despite being collected.

## Bug Fixes

 * Early signaling did not take place for `resolved()` for
   `ClusterFuture` and `MulticoreFuture`.

 * When early signaling was enabled, functions such as `resolved()` and
   `resolve()` would relay captured conditions multiple times.  This would,
   for instance, result in the same messages and warnings being outputted
   more than once.  Now it is only `value()` that will resignal conditions.

 * The validation of connections failed to detect when the connection
   had been serialized (= a `NIL` external pointer) on some macOS
   systems.

## Deprecated and Defunct

 * Argument `progress` of `resolve()` is now defunct (was deprecated
   since **future** 1.12.0).  Option `future.progress` is ignored.
   This will make room for other progress-update mechanisms that are
   in the works.

 * Usage of internal argument `evaluator` to `future()` is now
   deprecated.
 
 * Removed defunct argument `output` from `FutureError()`.

 * `FutureResult` fields/arguments `condition` and `calls` are now
   defunct.  Use `conditions` instead.

 * `Future` and `FutureResult` objects with an internal version 1.7 or
   older are deprecated and will eventually become defunct.  Future
   backends that implement their own `Future` classes should update to
   implement a `result()` method instead of a `value()` method for
   their `Future` classes.  All future backends available on CRAN and
   Bioconductor have already been updated accordingly.
 

# Version 1.13.0 [2019-05-08]

## Significant Changes

 * Forked processing is now disabled by default when running R via
   RStudio When disabled, 'multicore' futures fall back to a
   'sequential' futures.  This update follows from an RStudio
   recommendation against using _forked_ parallel processing from
   within RStudio because it is likely to break the RStudio R session.
   See `help("supportsMulticore")` for more details, e.g.  how to
   re-enable process forking.  Note that parallelization via
   'multisession' is unaffected and will still work as before.  Also,
   when forked processing is disabled, or otherwise not supported,
   using `plan("multiprocess")` will fall back to using 'multisession'
   futures.
   
## New Features

 * Forked processing can be disabled by setting R option
   `future.fork.enable` to FALSE (or environment variable
   `R_FUTURE_FORK_ENABLE=false`).  When disabled, 'multicore' futures
   fall back to a 'sequential' futures even if the operating system
   supports process forking.  If set of TRUE, 'multicore' will not
   fall back to 'sequential'.  If NA, or not set (the default), a set
   of best-practices rules will decide whether forking is enabled or
   not.  See `help("supportsMulticore")` for more details.

 * Now `availableCores()` also recognizes PBS environment variable
   `NCPUS`, because the PBSPro scheduler does not set `PBS_NUM_PPN`.

 * If, option `future.availableCores.custom` is set to a function,
   then `availableCores()` will call that function and interpret its
   value as number of cores.  Analogously, option
   `future.availableWorkers.custom` can be used to specify a hostnames
   of a set of workers that `availableWorkers()` sees.  These new
   options provide a mechanism for anyone to customize
   `availableCores()` and `availableWorkers()` in case they do not
   (yet) recognize, say, environment variables that are specific the
   user's compute environment or HPC scheduler.

 * `makeClusterPSOCK()` gained support for argument `rscript_startup`
   for evaluating one or more R expressions in the background R worker
   prior to the worker event loop launching.  This provides a more
   convenient approach than having to use, say, `rscript_args =
   c("-e", sQuote(code))`.

 * `makeClusterPSOCK()` gained support for argument `rscript_libs` to
   control the R package library search path on the workers.  For
   example, to _prepend_ the folder `~/R-libs` on the workers, use
   `rscript_libs = c("~/R-libs", "*")`, where `"*"` will be resolved
   to the current `.libPaths()` on the workers.

 * Debug messages are now prepended with a timestamp.

## Documentation

 * Add vignette on 'Non-Exportable Objects' (extracted from another vignette).

## Bug Fixes

 * `makeClusterPSOCK()` did not shell quote the Rscript executable
   when running its pre-tests checking whether localhost Rscript
   processes can be killed by their PIDs or not.

## Deprecated and Defunct

 * Argument `value` of `resolve()` has been renamed to `result` to
   better reflect that not only values are collected when this
   argument is used.  Argument `value` still works for backward
   compatibility, but will eventually be formally deprecated and then
   defunct.


# Version 1.12.0 [2019-03-07]

## New Features

 * If `makeClusterPSOCK()` fails to create one of many nodes, then it
   will attempt to stop any nodes that were successfully created.
   This lowers the risk for leaving R worker processes behind.

 * Future results now hold the timestamps when the evaluation of the
   future started and finished.
 
## Bug Fixes

 * Functions no longer produce "partial match of 'condition' to
   'conditions'" warnings with `options(warnPartialMatchDollar =
   TRUE)`.

 * When future infix operators (`%conditions%`, `%globals%`,
   `%label%`, `%lazy%`, `%packages%`, `%seed%`, and `%stdout%`) that
   are intended for future assignments were used in the wrong context,
   they would incorrectly be applied to the next future created. Now
   they're discarded.

 * `makeClusterPSOCK()` in **future** (>= 1.11.1) produced warnings
   when argument `rscript` had `length(rscript) > 1`.

 * Validation of L'Ecuyer-CMRG RNG seeds failed in recent R devel.

 * With `options(OutDec = ",")`, the default value of several argument
   would resolve to `NA_real_` rather than a numeric value resulting
   in errors such as "is.finite(alpha) is not TRUE".

## Deprecated and Defunct

 * Argument `progress` of `resolve()` is now deprecated.
 
 * Argument `output` of `FutureError()` is now defunct.

 * `FutureError` no longer inherits `simpleError`.


# Version 1.11.1.1 [2019-01-25]

## Bug Fixes

 * When `makeClusterPSOCK()` fails to connect to a worker, it produces
   an error with detailed information on what could have happened.  In
   rare cases, another error could be produced when generating the
   information on what the workers PID is.
 

# Version 1.11.1 [2019-01-25]

## New Features

 * The defaults of several arguments of `makeClusterPSOCK()` and
   `makeNodePSOCK()` can now be controlled via environment variables
   in addition to R options that was supported in the past. An
   advantage of using environment variables is that they will be
   inherited by child processes, also nested ones.

 * The printing of future plans is now less verbose when the `workers`
   argument is a complex object such as a PSOCK cluster object.
   Previously, the output would include verbose output of attributes,
   etc.

## Software Quality

 * TESTS: When the **future** package is loaded, it checks whether `R
   CMD check` is running or not.  If it is, then a few future-specific
   environment variables are adjusted such that the tests play nice
   with the testing environment.  For instance, it sets the socket
   connection timeout for PSOCK cluster workers to 120 seconds
   (instead of the default 30 days!).  This will lower the risk for
   more and more zombie worker processes cluttering up the test
   machine (e.g. CRAN servers) in case a worker process is left behind
   despite the main R processes is terminated.  Note that these
   adjustments are applied automatically to the checks of any package
   that depends on, or imports, the **future** package.

## Bug Fixes

 * Whenever `makeClusterPSOCK()` would fail to connect to a worker,
   for instance due to a port clash, then it would leave the R worker
   process running - also after the main R process terminated.  When
   the worker is running on the same machine, `makeClusterPSOCK()`
   will now attempt to kill such stray R processes.  Note that
   `parallel::makePSOCKcluster()` still has this problem.


# Version 1.11.0 [2019-01-21]

## Significant Changes

 * Message and warning conditions are now captured and relayed by
   default.

## New Features

 * The future call stack ("traceback") is now recorded when the
   evaluation of a future produces an error.  Use `backtrace()` on the
   future to retrieve it.

 * Now `futureCall()` defaults to `args = list()` making is easier to
   call functions that do not take arguments,
   e.g. `futureCall(function() 42)`.

 * `plan()` gained argument `.skip = FALSE`.  When TRUE, setting the
   same future strategy as already set will be skipped, e.g. calling
   `plan(multisession)` consecutively will have the same effect as
   calling it just once.

 * `makeClusterPSOCK()` produces more informative error messages
   whenever the setup of R workers fails.  Also, its verbose messages
   are now prefixed with `[local output] ` to help distinguish the
   output produced by the current R session from that produced by
   background workers.
   
 * It is now possible to specify what type of SSH clients
   `makeClusterPSOCK()` automatically searches for and in what order,
   e.g.  `rshcmd = c("<rstudio-ssh>", "<putty-plink>")`.

 * Now `makeClusterPSOCK()` preserves the global RNG state
   (`.Random.seed`) also when it draws a random port number.
   
 * `makeClusterPSOCK()` gained argument `rshlogfile`.

 * Cluster futures provide more informative error messages when the
   communication with the worker node is out of sync.

## Bug Fixes

 * Argument `stdout` was forced to TRUE when using single-core
   multicore or single-core multisession futures.

 * When evaluated in a local environment, `futureCall(..., globals =
   "a")` would set the value of global `a` to NULL, regardless if it
   exists or not and what its true value is.

 * `makeClusterPSOCK(..., rscript = "my_r")` would in some cases fail
   to find the intended `my_r` executable.

 * ROBUSTNESS: A cluster future, including a multisession one, could
   retrieve results from the wrong workers if a new set of cluster
   workers had been set up after the future was created/launched but
   before the results were retrieved.  This could happen because
   connections in R are indexed solely by integers which are recycled
   when old connections are closed and new ones are created.  Now
   cluster futures assert that the connections to the workers are
   valid, and if not, an informative error message is produced.

 * Calling `result()` on a non-resolved `UniprocessFuture` would
   signal evaluation errors.

## Deprecated and Defunct

 * Removed defunct `future::future_lapply()`.  Please use the one in
   the **future.apply** package instead.


# Version 1.10.0 [2018-10-16]

## New Features

 * Add support for manually specifying globals in addition to those
   that are automatically identified via argument `globals` or
   `%globals%`. Two examples are `globals = structure(TRUE, add =
   list(a = 42L, b = 3.14))` and `globals = structure(TRUE, add =
   c("a", "b"))`.  Analogously, attribute `ignore` can be used to
   exclude automatically identified globals.

 * The error reported when failing to retrieve the results of a future
   evaluated on a localhost cluster/multisession worker or a
   forked/multicore worker is now more informative. Specifically, it
   mentions whether the worker process is still alive or not.

 * Add `makeClusterMPI(n)` for creating MPI-based clusters of a
   similar kind as `parallel::makeCluster(n, type = "MPI")` but that
   also attempts to workaround issues where `parallel::stopCluster()`
   causes R to stall.
   
 * `makeClusterPSOCK()` and `makeClusterMPI()` gained argument
   `autoStop` for controlling whether the cluster should be
   automatically stopped when garbage collected or not.

 * BETA: Now `resolved()` for `ClusterFuture` is non-blocking also for
   clusters of type `MPIcluster` as created by
   `parallel::makeCluster(..., type = "MPI")`.

## Bug Fixes

 * On Windows, `plan(multiprocess)` would not initiate the
   workers. Instead workers would be set up only when the first future
   was created.


# Version 1.9.0 [2018-07-22]

## Significant Changes

 * Standard output is now captured and re-outputted when `value()` is
   called.  This new behavior can be controlled by the argument
   `stdout` to `future()` or by specifying the `%stdout%` operator if
   a future assignment is used.

## New Features

 * R option `width` is passed down so that standard output is captured
   consistently across workers and consistently with the master
   process.
   
 * Now more `future.*` options are passed down so that they are also
   acknowledged when using nested futures.

## Documentation

 * Add vignette on 'Outputting Text'.

 * CLEANUP: Only the core parts of the API are now listed in the help
   index.  This was done to clarify the Future API.  Help for non-core
   parts are still via cross references in the indexed API as well via
   `help()`.

## Bug Fixes

 * When using forced, nested 'multicore' parallel processing, such as,
   `plan(list(tweak(multicore, workers = 2), tweak(multicore, workers
   = 2)))`, then the child process would attempt to resolve futures
   owned by the parent process resulting in an error (on 'bad error
   message').
   
 * When using `plan(multicore)`, if a forked worker would terminate
   unexpectedly, it could corrupt the master R session such that any
   further attempts of using forked workers would fail.  A forked
   worker could be terminated this way if the user pressed
   <kbd>Ctrl-C</kbd> (the worker receives a `SIGINT` signal).

 * `makeClusterPSOCK()` produced a warning when environment variable
   `R_PARALLEL_PORT` was set to `random` (e.g. as on CRAN).

 * Printing a `plan()` could produce an error when the deparsed call
   used to set up the `plan()` was longer than 60 characters.
 
## Deprecated and Defunct

 * `future::future_lapply()` is defunct (gives an error if called).
   Please use the one in the **future.apply** package instead.

 * Argument `output` of `FutureError()` is formally deprecated.

 * Removed all `FutureEvaluationCondition` classes and related
   methods.
 

# Version 1.8.1 [2018-05-02]

## New Features

 * `getGlobalsAndPackages()` gained argument `maxSize`.

 * `makeClusterPSOCK()` now produces a more informative warning if
   environment variable `R_PARALLEL_PORT` specifies a non-numeric
   port.

 * Now `plan()` gives a more informative error message in case it
   fails, e.g.  when the internal future validation fails and why.

 * Added `UnexpectedFutureResultError` to be used by backends for
   signaling in a standard way that an unexpected result was retrieved
   from a worker.

## Bug Fixes

 * When the communication between an asynchronous future and a
   background R process failed, further querying of the future
   state/results could end up in an infinite waiting loop.  Now the
   failed communication error is recorded and re-signaled if any
   further querying attempts.
 
 * Internal, seldom used `myExternalIP()` failed to recognize IPv4 answers
   from some of the lookup servers. This could in turn produce another error.

 * In R (>= 3.5.0), multicore futures would produce multiple warnings
   originating from querying whether background processes have completed
   or not. These warnings are now suppressed.
   

# Version 1.8.0 [2018-04-08]

## Significant Changes

 * Errors produces when evaluating futures are now (re-)signaled on
   the master R process as-is with the original content and class
   attributes.
 
## New Features

 * More errors related to orchestration of futures are of class
   `FutureError` to make it easier to distinguish them from future
   evaluation errors.
   
 * Add support for a richer set of results returned by resolved
   futures.  Previously only the value of the future expression, which
   could be a captured error to be resignaled, was expected. Now a
   `FutureResult` object may be returned instead.  Although not
   supported in this release, this update opens up for reporting on
   additional information from the evaluation of futures,
   e.g. captured output, timing and memory benchmarks, etc. Before
   that can take place, existing future backend packages will have to
   be updated accordingly.

 * `backtrace()` returns only the last call that produced the error.
   It is unfortunately not possible to capture the call stack that led
   up to the error when evaluating a future expression.
   
## Bug Fixes

 * `value()` for `MulticoreFuture` would not produce an error when a
   (forked) background R workers would terminate before the future
   expression is resolved.  This was a limitation inherited from the
   **parallel** package.  Now an informative `FutureError` message is
   produced.

 * `value()` for `MulticoreFuture` would not signal errors unless they
   inherited from `simpleError` - now it's enough for them to inherits
   from `error`.

 * `value()` for `ClusterFuture` no longer produces a
   `FutureEvaluationError`, but `FutureError`, if the connection to
   the R worker has changed (which happens if something as drastic as
   `closeAllConnections()` have been called.)

 * `futureCall(..., globals = FALSE)` would produce "Error: second
   argument must be a list", because the explicit arguments where not
   exported.  This could also happen when specifying globals by name
   or as a named list.

 * Nested futures were too conservative in requiring global variables
   to exist, even when they were false positives.

## Deprecated and Defunct

 * `future::future_lapply()` is formally deprecated. Please use the
   one in the **future.apply** package instead.
  
 * Recently introduced `FutureEvaluationCondition` classes are
   deprecated, because they no longer serve a purpose since future
   evaluation conditions are now signaled as-is.
   

# Version 1.7.0 [2018-02-10]

## Significant Changes

 * `future_lapply()` has moved to the **future.apply** package
   available on CRAN.

## New Features

 * Argument `workers` of future strategies may now also be a function,
   which is called without argument when the future strategy is set up
   and used as is.  For instance, `plan(multiprocess, workers =
   halfCores)` where `halfCores <- function() { max(1,
   round(`availableCores()` / 2)) }` will use half of the number of
   available cores.  This is useful when using nested future
   strategies with remote machines.
 
 * On Windows, `makeClusterPSOCK()`, and therefore
   `plan(multisession)` and `plan(multiprocess)`, will use the SSH
   client distributed with RStudio as a fallback if neither `ssh` nor
   `plink` is available on the system `PATH`.

 * Now `plan()` makes sure that `nbrOfWorkers()` will work for the new
   strategy.  This will help catch mistakes such as `plan(cluster,
   workers = cl)` where `cl` is a basic R list rather than a `cluster`
   list early on.

 * Added `%packages%` to explicitly control packages to be attached
   when a future is resolved, e.g. `y %<-% { YT[2] } %packages%
   "data.table"`.  Note, this is only needed in cases where the
   automatic identification of global and package dependencies is not
   sufficient.

 * Added condition classes `FutureCondition`, `FutureMessage`,
   `FutureWarning`, and `FutureError` representing conditions that
   occur while a future is setup, launched, queried, or retrieved.
   They do *not* represent conditions that occur while evaluating the
   future expression.  For those conditions, new classes
   `FutureEvaluationCondition`, `FutureEvaulationMessage`,
   `FutureEvaluationWarning`, and `FutureEvaluationError` exists.

## Documentation

 * Vignette 'Common Issues with Solutions' now documents the case
   where the future framework fails to identify a variable as being
   global because it is only so conditionally, e.g. `if (runif(1) <
   1/2) x <- 0; y <- 2 * x`.

## Beta Features

 * Added mechanism for detecting globals that _may_ not be exportable
   to an external R process (a "worker").  Typically, globals that
   carry connections and external pointers (`externalptr`) can not be
   exported, but there are exceptions.  By setting options
   `future.globals.onReference` to `"warning"`, a warning is produced
   informing the user about potential problems.  If `"error"`, an
   error is produced.  Because there might be false positive, the
   default is `"ignore"`, which will cause above scans to be skipped.
   If there are non-exportable globals and these tests are skipped, a
   run-time error may be produced only when the future expression is
   evaluated.

## Bug Fixes

 * The total size of global variables was overestimated, and
   dramatically so if defined in the global environment and there were
   are large objects there too.  This would sometimes result in a
   false error saying that the total size is larger than the allowed
   limit.

 * An assignment such as `x <- x + 1` where the left-hand side (LHS)
   `x` is a global failed to identify `x` as a global because the
   right-hand side (RHS) `x` would override it as a local variable.
   Updates to the **globals** package fixed this problem.

 * `makeClusterPSOCK(..., renice = 19)` would launch each PSOCK worker
   via `nice +19` resulting in the error "nice: '+19': No such file or
   directory".  This bug was inherited from
   `parallel::makePSOCKcluster()`.  Now using `nice --adjustment=19`
   instead.

 * Protection against passing future objects to other futures did not
   work for future strategy 'multicore'.
 
## Deprecated and Defunct

 * `future_lapply()` has moved to the new **future.apply** package
   available on CRAN.  The `future::future_lapply()` function will
   soon be deprecated, then defunct, and eventually be removed from
   the **future** package. Please update your code to make use of
   `future.apply::future_lapply()` instead.
   
 * Dropped defunct 'eager' and 'lazy' futures; use 'sequential'
   instead.

 * Dropped defunct arguments `cluster` and `maxCores`; use `workers`
   instead.

 * In previous version of the **future** package the `FutureError`
   class was used to represent both orchestration errors (now
   `FutureError`) and evaluation errors (now `FutureEvaluationError`).
   Any usage of class `FutureError` for the latter type of errors is
   deprecated and should be updated to `FutureEvaluationError`.


# Version 1.6.2 [2017-10-16]

## New Features

 * Now `plan()` accepts also strings such as `"future::cluster"`.

 * Now `backtrace(x[[ER]])` works also for non-environment `x`:s,
   e.g. lists.

## Bug Fixes

 * When measuring the size of globals by scanning their content, for
   certain types of classes the inferred lengths of these objects were
   incorrect causing internal subset out-of-range issues.

 * `print()` for `Future` would output one global per line instead of
   concatenating the information with commas.


# Version 1.6.1 [2017-09-08]

## New Features

 * Now exporting `getGlobalsAndPackages()`.

## Bug Fixes

 * `future_lapply()` would give "Error in objectSize.env(x, depth =
   depth - 1L): object 'nnn' not found" when for instance 'nnn' is
   part of an unresolved expression that is an argument value.

## Software Quality

 * FIX: Some of the package assertion tests made too precise
   assumptions about the object sizes, which fails with the
   introduction of ALTREP in R-devel which causes the R's SEXP header
   size to change.
 

# Version 1.6.0 [2017-08-11]

## New Features

 * Now `tweak()`, and hence `plan()`, generates a more informative
   error message if a non-future function is specified by mistake,
   e.g. calling `plan(cluster)` with the **survival** package attached
   after **future** is equivalent to calling `plan(survival::cluster)`
   when `plan(future::cluster)` was intended.

## Bug Fixes

 * `nbrOfWorkers()` gave an error with `plan(remote)`. Fixed by making
   the 'remote' future inherit `cluster` (as it should).

## Software Quality

 * TESTS: No longer testing forced termination of forked cluster
   workers when running on Solaris. The termination was done by
   launching a future that called `quit()`, but that appeared to have
   corrupted the main R session when running on Solaris.

## Deprecated and Defunct

 * Formally defunct 'eager' and 'lazy' futures; use 'sequential'
   instead.

 * Dropped previously defunct `%<=%` and `%=>%` operators.
 

# Version 1.5.0 [2017-05-24]

## Significant Changes

 * Multicore and multisession futures no longer reserve one core for
   the main R process, which was done to lower the risk for producing
   a higher CPU load than the number of cores available for the R
   session.
 
## New Features

 * `makeClusterPSOCK()` now defaults to use the Windows PuTTY
   software's SSH client `plink -ssh`, if `ssh` is not found.

 * Argument `homogeneous` of `makeNodePSOCK()`, a helper function of
   `makeClusterPSOCK()`, will default to FALSE also if the hostname is
   a fully qualified domain name (FQDN), that is, it "contains
   periods".  For instance, `c('node1', 'node2.server.org')` will use
   `homogeneous = TRUE` for the first worker and `homogeneous = FALSE`
   for the second.

 * `makeClusterPSOCK()` now asserts that each cluster node is
   functioning by retrieving and recording the node's session
   information including the process ID of the corresponding R
   process.

 * Nested futures sets option `mc.cores` to prevent spawning of
   recursive parallel processes by mistake.  Because 'mc.cores'
   controls _additional_ processes, it was previously set to zero.
   However, since some functions
   such as `mclapply()` does not support that, it is now set to one instead.   

## Documentation

 * Help on `makeClusterPSOCK()` gained more detailed descriptions on
   arguments and what their defaults are.
 
## Deprecated and Defunct

 * Formally deprecated eager futures; use sequential instead.

## Bug Fixes

 * `future_lapply()` with multicore / multisession futures, would use
   a suboptimal workload balancing where it split up the data in one
   chunk too many.  This is no longer a problem because of how
   argument `workers` is now defined for those type of futures (see
   note on top).
 
 * `future_lapply()`, as well as lazy multicore and lazy sequential
   futures, did not respect option `future.globals.resolve`, but was
   hardcoded to always resolve globals (`future.globals.resolve =
   TRUE`).

 * When globals larger than the allowed size (option
   `future.globals.maxSize`) are detected an informative error message
   is generated.  Previous version introduced a bug causing the error
   to produce another error.

 * Lazy sequential futures would produce an error when resolved if
   required packages had been detached.

 * `print()` would not display globals gathered for lazy sequential
   futures.

## Software Quality

 * Added package tests for globals part of formulas part of other
   globals, e.g. `purrr::map(x, ~ rnorm(.))`, which requires
   **globals** (>= 0.10.0).

 * Now package tests with `parallel::makeCluster()` not only test for
   `type = "PSOCK"` clusters but also `"FORK"` (when supported).

 * TESTS: Cleaned up test scripts such that the overall processing
   time for the tests was roughly halved, while preserving the same
   test coverage.
 

# Version 1.4.0 [2017-03-12]

## Significant Changes

 * The default for `future_lapply()` is now to _not_ generate RNG
   seeds (`future.seed = FALSE`).  If proper random number generation
   is needed, use `future.seed = TRUE`.  For more details, see help
   page.
   
## New Features

 * `future()` and `future_lapply()` gained argument `packages` for
   explicitly specifying packages to be attached when the futures are
   evaluated.  Note that the default throughout the **future** package
   is that all globals and all required packages are automatically
   identified and gathered, so in most cases those do not have to be
   specified manually.

 * The default values for arguments `connectTimeout` and `timeout` of
   `makeNodePSOCK()` can now be controlled via global options.

## Random Number Generation

 * Now `future_lapply()` guarantees that the RNG state of the calling
   R process after returning is updated compared to what it was before
   and in the exact same way regardless of `future.seed` (except
   FALSE), `future.scheduling` and future strategy used.  This is done
   in order to guarantee that an R script calling `future_lapply()`
   multiple times should be numerically reproducible given the same
   initial seed.

 * It is now possible to specify a pre-generated sequence of
   `.Random.seed` seeds to be used for each `FUN(x[[i]], ...)` call in
   `future_lapply(x, FUN, ...)`.

## Performance

 * `future_lapply()` scans global variables for non-resolved futures
   (to resolve them) and calculate their total size once.  Previously,
   each chunk (a future) would redo this.

## Bug Fixes

 * Now `future_lapply(X, FUN, ...)` identifies global objects among
   `X`, `FUN` and `...` recursively until no new globals are found.
   Previously, only the first level of globals were scanned.  This is
   mostly thanks to a bug fix in **globals** 0.9.0.

 * A future that used a global object `x` of a class that overrides
   `length()` would produce an error if `length(x)` reports more
   elements than what can be subsetted.

 * `nbrOfWorkers()` gave an error with `plan(cluster, workers = cl)`
   where `cl` is a `cluster` object created by
   `parallel::makeCluster()`, etc.  This prevented for instance
   `future_lapply()` to work with such setups.

 * `plan(cluster, workers = cl)` where `cl <- makeCluster(..., type =
   MPI")` would give an instant error due to an invalid internal
   assertion.

## Deprecated and Defunct

 * Previously deprecated arguments `maxCores` and `cluster` are now
   defunct.

 * Previously deprecated assignment operators `%<=%` and `%=>%` are
   now defunct.

 * `availableCores(method = "mc.cores")` is now defunct in favor of
   `"mc.cores+1"`.


# Version 1.3.0 [2017-01-18]

## Significant Changes

 * Where applicable, workers are now initiated when calling `plan()`,
   e.g.  `plan(cluster)` will set up workers on all cluster nodes.
   Previously, this only happened when the first future was created.

## New Features

 * Renamed 'eager' futures to 'sequential', e.g. `plan(sequential)`.
   The 'eager' futures will be deprecated in an upcoming release.
   
 * Added support for controlling whether a future is resolved eagerly
   or lazily when creating the future, e.g. `future(..., lazy =
   TRUE)`, `futureAssign(..., lazy = TRUE)`, and `x %<-% { ... }
   %lazy% TRUE`.

 * `future()`, `futureAssign()` and `futureCall()` gained argument
   `seed`, which specifies a L'Ecuyer-CMRG random seed to be used by
   the future.  The seed for future assignment can be specified via
   `%seed%`.
   
 * `futureAssign()` now passes all additional arguments to `future()`.

 * Added `future_lapply()` which supports load balancing ("chunking")
   and perfect reproducibility (regardless of type of load balancing
   and how futures are resolved) via initial random seed.
 
 * Added `availableWorkers()`.  By default it returns localhost
   workers according to `availableCores()`.  In addition, it detects
   common HPC allocations given in environment variables set by the
   HPC scheduler.
   
 * The default for `plan(cluster)` is now `workers =
   availableWorkers()`.
 
 * Now `plan()` stops any clusters that were implicitly created. For
   instance, a multisession cluster created by `plan(multisession)`
   will be stopped when `plan(eager)` is called.

 * `makeClusterPSOCK()` treats workers that refer to a local machine
   by its local or canonical hostname as "localhost".  This avoids
   having to launch such workers over SSH, which may not be supported
   on all systems / compute cluster.

 * Option `future.debug = TRUE` also reports on total size of globals
   identified and for cluster futures also the size of the individual
   global variables exported.

 * Option `future.wait.timeout` (replaces `future.wait.times`)
   specifies the maximum waiting time for a free workers (e.g. a core
   or a
   compute node) before generating a timeout error.  

 * Option `future.availableCores.fallback`, which defaults to
   environment variable `R_FUTURE_AVAILABLECORES_FALLBACK` can now be
   used to specify the default number of cores / workers returned by
   `availableCores()` and `availableWorkers()` when no other settings
   are available.  For instance, if
   `R_FUTURE_AVAILABLECORES_FALLBACK=1` is set system wide in an HPC
   environment, then all R processes that uses `availableCores()` to
   detect how many cores can be used will run as single-core
   processes.  Without this fallback setting, and without other
   core-specifying settings, the default will be to use all cores on
   the machine, which does not play well on multi-user systems.

## Globals

 * Globals part of locally defined functions are now also identified
   thanks to **globals** (>= 0.8.0) updates.
   
## Deprecated and Defunct

 * Lazy futures and `plan(lazy)` are now deprecated. Instead, use
   `plan(eager)` and then `f <- future(..., lazy = TRUE)` or `x %<-% {
   ... } %lazy% TRUE`.  The reason behind this is that in some cases
   code that uses futures only works under eager evaluation (`lazy =
   FALSE`; the default), or vice verse. By removing the "lazy" future
   strategy, the user can no longer
   override the `lazy = TRUE / FALSE` that the developer is using.  
 
## Bug Fixes

 * Creation of cluster futures (including multisession ones) would
   time out already after 40 seconds if all workers were busy.  New
   default timeout is 30 days (option `future.wait.timeout`).

 * `nbrOfWorkers()` gave an error for `plan(cluster, workers)` where
   `workers` was a character vector or a `cluster` object of the
   **parallel** package.  Because of this, `future_lapply()` gave an
   error with such setups.

 * `availableCores(methods = "_R_CHECK_LIMIT_CORES_")` would give an
   error if not running `R CMD check`.
   
    
# Version 1.2.0 [2016-11-12]

## New Features

 * Added `makeClusterPSOCK()` - a version of
   `parallel::makePSOCKcluster()` that allows for more flexible
   control of how PSOCK cluster workers are set up and how they are
   launched and communicated with if running on external machines.

 * Added generic `as.cluster()` for coercing objects to cluster
   objects to be used as in `plan(cluster, workers = as.cluster(x))`.
   Also added a `c()` implementation for cluster objects such that
   multiple cluster objects can be combined into a single one.

 * Added `sessionDetails()` for gathering details of the current R
   session.

 * `plan()` and `plan("list")` now prints more user-friendly output.

 * On Unix, internal `myInternalIP()` tries more alternatives for
   finding the local IP number.

## Deprecated and Defunct

 * `%<=%` is deprecated. Use `%<-%` instead. Same for `%=>%`.
    
## Bug Fixes

 * `values()` for lists and list environments of futures where one or
   more of the futures resolved to NULL would give an error.

 * `value()` for `ClusterFuture` would give cryptic error message
   "Error in stop(ex) : bad error message" if the cluster worker had
   crashed / terminated.  Now it will instead give an error message
   like "Failed to retrieve the value of `ClusterFuture` from cluster
   node #1 on 'localhost'. The reason reported was "error reading from
   connection".

 * Argument `user` to `remote()` was ignored (since 1.1.0).
    
    
# Version 1.1.1 [2016-10-10]

## Bug Fixes

 * For the special case where 'remote' futures use `workers =
   "localhost"` they (again) use the exact same R executable as the
   main / calling R session (in all other cases it uses whatever
   `Rscript` is found in the `PATH`).  This was already indeed
   implemented in 1.0.1, but with the added support for reverse SSH
   tunnels in 1.1.0 this default behavior was lost.
    
    
# Version 1.1.0 [2016-10-09]

## New Features

 * REMOTE CLUSTERS: It is now very simple to use `cluster()` and
   `remote()` to connect to remote clusters / machines.  As long as
   you can connect via SSH to those machines, it works also with these
   future.  The new code completely avoids incoming firewall and
   incoming port forwarding issues previously needed.  This is done by
   using reverse SSH tunneling.  There is also no need to worry about
   internal or external IP numbers.

 * Added optional argument `label` to all futures, e.g.  `f <-
   future(42, label = "answer")` and `v %<-% { 42 } %label% "answer"`.

 * Added argument `user` to `cluster()` and `remote()`.

 * Now all `Future` classes supports `run()` for launching the future
   and `value()` calls `run()` if the future has not been launched.

 * MEMORY: Now `plan(cluster, gc = TRUE)` causes the background R
   session to be garbage collected immediately after the value is
   collected.  Since multisession and remote futures are special cases
   of cluster futures, the same is true for these as well.

 * ROBUSTNESS: Now the default future strategy is explicitly set when
   no strategies are set, e.g. when used nested futures.  Previously,
   only mc.cores was set so that only a single core was used, but now
   also `plan("default")` set.

 * WORKAROUND: `resolved()` on cluster futures would block on Linux
   until future was resolved.  This is due to a bug in R.  The
   workaround is to use round the timeout (in seconds) to an integer,
   which seems to always work / be respected.

## Globals

 * Global variables part of subassignments in future expressions are
   recognized and exported (iff found), e.g. `x$a <- value`, `x[["a"]]
   <- value`, and `x[1,2,3] <- value`.

 * Global variables part of formulae in future expressions are
   recognized and exported (iff found), e.g. `y ~ x | z`.

 * As an alternative to the default automatic identification of
   globals, it is now also possible to explicitly specify them either
   by their names (as a character vector) or by their names and values
   (as a named list), e.g. `f <- future({ 2*a }, globals = c("a"))` or
   `f <- future({ 2*a }, globals = list(a = 42))`.  For future
   assignments one can use the `%globals%` operator, e.g. `y %<-% {
   2*a } %globals% c("a")`.

## Documentation

 * Added vignette on command-line options and other methods for
   controlling the default type of futures to use.
    
    
# Version 1.0.1 [2016-07-04]

## New Features

 * ROBUSTNESS: For the special case where 'remote' futures use
   `workers = "localhost"` they now use the exact same R executable as
   the main / calling R session (in all other cases it uses whatever
   `Rscript` is found in the `PATH`).

 * `FutureError` now extends `simpleError` and no longer the error
   class of captured errors.

## Documentation

 * Adding section to vignette on globals in formulas describing how
   they are currently not automatically detected and how to explicitly
   export them.

## Bug Fixes
  
 * Since **future** 0.13.0, a global `pkg` would be overwritten by the
   name of the last package attached in future.

 * Futures that generated `R.oo::Exception` errors, they triggered
   another internal error.
   
    
# Version 1.0.0 [2016-06-24]

## New Features

 * Add support for `remote(..., myip = "<external>")`, which now
   queries a set of external lookup services in case one of them
   fails.

 * Add `mandelbrot()` function used in demo to the API for
   convenience.

 * ROBUSTNESS: If `.future.R` script, which is sourced when the
   **future** package is attached, gives an error, then the error is
   ignored with a warning.

 * TROUBLESHOOTING: If the future requires attachment of packages,
   then each namespace is loaded separately and before attaching the
   package.  This is done in order to see the actual error message in
   case there is a problem while loading the namespace.  With
   `require()`/`library()` this error message is otherwise suppressed
   and replaced with a generic one.

## Globals

 * Falsely identified global variables no longer generate an error
   when the future is created.  Instead, we leave it to R and the
   evaluation of the individual futures to throw an error if the a
   global variable is truly missing.  This was done in order to
   automatically handle future expressions that use non-standard
   evaluation (NSE), e.g. `subset(df, x < 3)` where `x` is falsely
   identified as a global variable.

 * Dropped support for system environment variable
   `R_FUTURE_GLOBALS_MAXSIZE`.

## Documentation

 * DEMO: Now the Mandelbrot demo tiles a single Mandelbrot region with
   one future per tile. This better illustrates parallelism.

 * Documented R options used by the **future** package.
    
## Bug Fixes

 * Custom futures based on a constructor function that is defined
   outside a package gave an error.

 * `plan("default")` assumed that the `future.plan` option was a
   string; gave an error if it was a function.

 * Various future options were not passed on to futures.

 * A startup `.future.R` script is no longer sourced if the
   **future** package is attached by a future expression.
    
    
# Version 0.15.0 [2016-06-13]

## New Features

 * Added remote futures, which are cluster futures with convenient
   default arguments for simple remote access to R, e.g.
   `plan(remote, workers = "login.my-server.org")`.

 * Now `.future.R` (if found in the current directory or otherwise in
   the user's home directory) is sourced when the **future** package
   is attach (but not loaded).  This helps separating scripts from
   configuration of futures.

 * Added support for `plan(cluster, workers = c("n1", "n2", "n2",
   "n4"))`, where `workers` (also for `ClusterFuture()`) is a set of
   host names passed to `parallel::makeCluster(workers)`.  It can also
   be the number of localhost workers.

 * Added command line option `--parallel=<p>`, which is long for `-p <p>`.

 * Now command line option `-p <p>` also set the default future
   strategy to multiprocessing (if p >= 2 and eager otherwise), unless
   another strategy is already specified via option `future.plan` or
   system environment variable `R_FUTURE_PLAN`.

 * Now `availableCores()` also acknowledges environment variable
   `NSLOTS` set by Sun/Oracle Grid Engine (SGE).

 * MEMORY: Added argument `gc = FALSE` to all futures.  When TRUE, the
   garbage collector will run at the very end in the process that
   evaluated the future (just before returning the value).  This may
   help lowering the overall memory footprint when running multiple
   parallel R processes.  The user can enable this by specifying
   `plan(multiprocess, gc = TRUE)`.  The developer can control this
   using `future(expr, gc = TRUE)` or `v %<-% { expr } %tweak% list(gc
   = TRUE)`.

## Performance

 * Significantly decreased the overhead of creating a future,
   particularly multicore futures.
    
## Bug Fixes

 * Future would give an error with `plan(list("eager"))`, whereas it
   did work with `plan("eager")` and `plan(list(eager))`.
    
    
# Version 0.14.0 [2016-05-16]

## New Features

 * Added `nbrOfWorkers()`.

 * Added informative `print()` method for the `Future` class.

 * `values()` passes arguments `...` to `value()` of each future.

 * Added `FutureError` class.

## Deprecated and Defunct

 * Renamed arguments `maxCores` and `cluster` to `workers`.  If using
   the old argument names a deprecation warning will be generated, but
   it will still work until made defunct in a future release.
    
## Bug Fixes

 * `resolve()` for lists and environments did not work
   properly when the set of futures was not resolved in order,
   which could happen with asynchronous futures.
    
    
# Version 0.13.0 [2016-04-13]

## New Features

 * Add support to `plan()` for specifying different future strategies
   for the different levels of nested futures.

 * Add `backtrace()` for listing the trace the expressions evaluated
   (the calls made) before a condition was caught.

 * Add transparent futures, which are eager futures with early
   signaling of conditioned enabled and whose expression is evaluated
   in the calling environment.  This makes the evaluation of such
   futures as similar as possible to how R evaluates expressions,
   which in turn simplifies troubleshooting errors, etc.

 * Add support for early signaling of conditions.  The default is (as
   before) to signal conditions when the value is queried.  In
   addition, they may be signals as soon as possible, e.g. when
   checking whether a future is resolved or not.

 * Signaling of conditions when calling `value()` is now controlled by
   argument `signal` (previously `onError`).

 * Now `UniprocessFuture`:s captures the call stack for errors
   occurring while resolving futures.

 * `ClusterFuture()` gained argument `persistent = FALSE`. With
   `persistent = TRUE`, any objects in the cluster R session that was
   created during the evaluation of a previous future is available for
   succeeding futures that are evaluated in the same session.
   Moreover, globals are still identified and exported but "missing"
   globals will not give an error - instead it is assumed such globals
   are available in the environment where the future is evaluated.

 * OVERHEAD: Utility functions exported by `ClusterFuture` are now
   much smaller; previously they would export all of the package
   environment.
    
## Bug Fixes
  
 * `f <- multicore(NA, maxCores = 2)` would end up in an endless
   waiting loop for a free core if `availableCores()` returned one.

 * `ClusterFuture()` would ignore `local = TRUE`.
    
    
# Version 0.12.0 [2016-02-23]

## New Features

 * Added multiprocess futures, which are multicore futures if
   supported, otherwise multisession futures.  This makes it possible
   to use `plan(multiprocess)` everywhere regardless of operating
   system.

 * Future strategy functions gained class attributes such that it is
   possible to test what type of future is currently used, e.g.
   `inherits(plan(), "multicore")`.

 * ROBUSTNESS: It is only the R process that created a future that can
   resolve it. If a non-resolved future is queried by another R
   process, then an informative error is generated explaining that
   this is not possible.

 * ROBUSTNESS: Now `value()` for multicore futures detects if the
   underlying forked R process was terminated before completing and if
   so generates an informative error messages.

## Performance

 * Adjusted the parameters for the schema used to wait for next
   available cluster node such that nodes are polled more frequently.

## Globals

 * `resolve()` gained argument `recursive`.

 * Added option `future.globals.resolve` for controlling whether
   global variables should be resolved for futures or not.  If TRUE,
   then globals are searched recursively for any futures and if found
   such "global" futures are resolved.  If FALSE, global futures are
   not located, but if they are later trying to be resolved by the
   parent future, then an informative error message is generated
   clarifying that only the R process that created the future can
   resolve it.  The default is currently FALSE.

## Bug Fixes

 * FIX: Exports of objects available in packages already attached by
   the future were still exported.

 * FIX: Now `availableCores()` returns `3L` (=`2L+1L`) instead of `2L`
   if `_R_CHECK_LIMIT_CORES_` is set.
    
    
# Version 0.11.0 [2016-01-15]

## New Features

 * Add multisession futures, which analogously to multicore ones, use
   multiple cores on the local machine with the difference that they
   are evaluated in separate R session running in the background
   rather than separate forked R processes.  A multisession future is
   a special type of cluster futures that do not require explicit
   setup of cluster nodes.

 * Add support for cluster futures, which can make use of a cluster of
   nodes created by `parallel::makeCluster()`.

 * Add `futureCall()`, which is for futures what `do.call()` is
   otherwise.

 * Standardized how options are named, i.e. `future.<option>`.  If you
   used any future options previously, make sure to check they follow
   the above format.

## Globals

 * All futures now validates globals by default (`globals = TRUE`).
   
    
# Version 0.10.0 [2015-12-30]

## New Features

 * Now `%<=%` can also assign to multi-dimensional list environments.

 * Add `futures()`, `values()` and `resolved()`.

 * Add `resolve()` to resolve futures in lists and environments.

 * Now `availableCores()` also acknowledges the number of CPUs
   allotted by Slurm.

 * CLEANUP: Now the internal future variable created by `%<=%` is
   removed when the future variable is resolved.
    
## Bug Fixes

 * `futureOf(envir = x)` did not work properly when `x` was a list
   environment.
    
    
# Version 0.9.0 [2015-12-11]

## New Features

 * ROBUSTNESS: Now values of environment variables are trimmed before
   being parsed.

 * ROBUSTNESS: Add reproducibility test for random number generation
   using Pierre L'Ecuyer's RNG stream regardless of how futures are
   evaluated, e.g. eager, lazy and multicore.

## Globals

 * Now globals ("unknown" variables) are identified using the new
   `findGlobals(..., method = "ordered")` in **globals** (> 0.5.0)
   such that a global variable preceding a local variable with the
   same name is properly identified and exported/frozen.

## Documentation

 * Updated vignette on common issues with the case where a global
   variable is not identified because it is hidden by an element
   assignment in the future expression.
    
## Bug Fixes
 
 * Errors occurring in multicore futures could prevent further
   multicore futures from being created.
    
    
# Version 0.8.2 [2015-10-14]
    
## Bug Fixes
  
 * Globals that were copies of package objects were not exported to
   the future environments.

 * The **future** package had to be attached or `future::future()` had
   to be imported, if `%<=%` was used internally in another package.
   Similarly, it also had to be attached if multicore futures where
   used.
    
    
# Version 0.8.1 [2015-10-05]

## Documentation

 * Added vignette 'Futures in R: Common issues with solutions'.

## Globals

 * `eager()` and `multicore()` gained argument `globals`, where
   `globals = TRUE` will validate that all global variables identified
   can be located already before the future is created.  This provides
   the means for providing the same tests on global variables with
   eager and multicore futures as with lazy futures.

## Bug Fixes

 * `lazy(sum(x, ...), globals = TRUE)` now properly passes `...`  from
   the function from which the future is setup.  If not called within
   a function or called within a function without `...` arguments, an
   informative error message is thrown.
    
    
# Version 0.8.0 [2015-09-06]

## New Features

 * `plan("default")` resets to the default strategy, which is
   synchronous eager evaluation unless option `future_plan` or
   environment variable `R_FUTURE_PLAN` has been set.

 * `availableCores("mc.cores")` returns `getOption("mc.cores") + 1L`,
   because option `mc.cores` specifies "allowed number of _additional_
   R processes" to be used in addition to the main R process.
    
## Bug Fixes

 * `plan(future::lazy)` and similar gave errors.
    
    
# Version 0.7.0 [2015-07-13]

## New Features

 * `multicore()` gained argument `maxCores`, which makes it possible
   to use for instance `plan(multicore, maxCores = 4L)`.

 * Add `availableMulticore()` [from (in-house) **async** package].

## Documentation

 * More colorful `demo("mandelbrot", package = "future")`.
    
## Bug Fixes
  
 * ROBUSTNESS: `multicore()` blocks until one of the CPU cores is
   available, iff all are currently occupied by other multicore
   futures.

 * `old <- plan(new)` now returns the old plan/strategy (was the newly
   set one).
    
    
# Version 0.6.0 [2015-06-18]

## New Features

 * Add multicore futures, which are futures that are resolved
   asynchronously in a separate process.  These are only supported on
   Unix-like systems, but not on Windows.
    
    
# Version 0.5.1 [2015-06-18]

## New Features

 * Eager and lazy futures now records the result internally such that
   the expression is only evaluated once, even if their error values
   are requested multiple times.

 * Eager futures are always created regardless of error or not.

 * All `Future` objects are environments themselves that record the
   expression, the call environment and optional variables.
    
    
# Version 0.5.0 [2015-06-16]

## Globals

 * `lazy()` "freezes" global variables at the time when the future is
   created.  This way the result of a lazy future is more likely to be
   the same as an 'eager' future.  This is also how globals are likely
   to be handled by asynchronous futures.
    
    
# Version 0.4.2 [2015-06-15]

## New Features

 * `plan()` records the call.

## Documentation

 * Added `demo("mandelbrot", package = "future")`, which can be
   re-used by other future packages.
    
    
# Version 0.4.1 [2015-06-14]

## New Features

 * Added `plan()`.

 * Added eager future - useful for troubleshooting.
    
    
# Version 0.4.0 [2015-06-07]

 * Distilled Future API from (in-house) **async** package.
