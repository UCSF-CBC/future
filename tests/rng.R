source("incl/start.R")

message("*** rng ...")

okind <- RNGkind()

## A valid regular seed
f <- Future(42, seed = 42L)
print(f)
stopifnot(identical(RNGkind(), okind))

## A valid L'Ecuyer-CMRG RNG seed
seed <- c(407L, 1420090545L, 65713854L, -990249945L,
          1780737596L, -1213437427L, 1082168682L)
f <- Future(42, seed = seed)
print(f)
stopifnot(identical(RNGkind(), okind))

f <- Future(42, seed = TRUE)
print(f)
stopifnot(identical(RNGkind(), okind))

f <- Future(42, seed = FALSE)
print(f)
stopifnot(identical(RNGkind(), okind))

f <- Future(42, seed = NULL)
print(f)
stopifnot(identical(RNGkind(), okind))


## See Section 6 on 'Random-number generation' in
## vignette("parallel", package = "parallel")
fsample <- function(x, size = 4L, seed = NULL, what = c("future", "%<-%")) {
  what <- match.arg(what)
  
  ## Must use session-specific '.GlobalEnv' here
  .GlobalEnv <- globalenv()
  
  oseed <- .GlobalEnv$.Random.seed
  orng <- RNGkind("L'Ecuyer-CMRG")[1L]
  on.exit(RNGkind(orng))

  if (isFALSE(seed) || isNA(seed) || is.null(seed)) {
    if (what == "future") {
      fs <- list()
      for (ii in seq_len(size)) {
        label <- sprintf("fsample_%d-%d", ii, sample.int(1e6, size=1L))
        fs[[ii]] <- future({ sample(x, size = 1L) }, seed = seed, label = label)
	print(fs[[ii]])
      }
      res <- value(fs)
    } else {
      res <- listenv::listenv()
      for (ii in seq_len(size)) {
        label <- sprintf("fsample_%d-%d", ii, sample.int(1e6, size=1L))
        res[[ii]] %<-% { sample(x, size = 1L) } %seed% seed %label% label
      }
      res <- as.list(res)
    }
  } else {
    ## Reset state of random seed afterwards?
    on.exit({
      if (is.null(oseed)) {
        rm(list = ".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      } else {
        .GlobalEnv$.Random.seed <- oseed
      }
    }, add = TRUE)

    set.seed(seed)

    .seed <- .Random.seed
  
    if (what == "future") {
      fs <- list()
      for (ii in seq_len(size)) {
        .seed <- parallel::nextRNGStream(.seed)
        fs[[ii]] <- future({ sample(x, size = 1L) }, seed = .seed)
      }
      res <- value(fs)
    } else {
      res <- listenv::listenv()
      for (ii in seq_len(size)) {
        .seed <- parallel::nextRNGStream(.seed)
        res[[ii]] %<-% { sample(x, size = 1L) } %seed% .seed
      }
      res <- as.list(res)
    }
  }
  
  res
} # fsample()


dummy <- sample(0:3, size = 1L)
seed0 <- .Random.seed
stopifnot(identical(RNGkind(), okind))

## Reference sample with fixed random seed
plan("sequential")
y0 <- fsample(0:3, seed = 42L)

## Assert that random seed is reset
stopifnot(
  identical(.GlobalEnv$.Random.seed, seed0),
  identical(RNGkind(), okind)
)


for (cores in 1:availCores) {
  message(sprintf("Testing with %d cores ...", cores))
  options(mc.cores = cores)

  for (strategy in supportedStrategies(cores)) {
    message(sprintf("%s ...", strategy))

    plan(strategy)

    for (what in c("future", "%<-%")) {
      .GlobalEnv$.Random.seed <- seed0

      ## Fixed random seed
      y1 <- fsample(0:3, seed = 42L, what = what)
      print(y1)
      stopifnot(identical(y1, y0))
  
      ## Assert that random seed is reset
      stopifnot(
        identical(.GlobalEnv$.Random.seed, seed0),
        identical(RNGkind(), okind)
      )
  
      ## Fixed random seed
      y2 <- fsample(0:3, seed = 42L, what = what)
      print(y2)
      stopifnot(identical(y2, y1))
      stopifnot(identical(y2, y0))
  
      ## Assert that random seed is reset
      stopifnot(
        identical(.GlobalEnv$.Random.seed, seed0),
        identical(RNGkind(), okind)
      )
  
      ## No seed
      for (misuse in c("ignore", "warning", "error")) {
        options(future.rng.onMisuse = misuse)

        y3 <- tryCatch({
	  ## WORKAROUND: fsample() triggers a R_FUTURE_GLOBALS_ONREFERENCE
	  ##             warning.  Not sure why. /HB 2019-12-27
	  ovalue <- Sys.getenv("R_FUTURE_GLOBALS_ONREFERENCE")
	  on.exit(Sys.setenv("R_FUTURE_GLOBALS_ONREFERENCE" = ovalue))
	  Sys.setenv("R_FUTURE_GLOBALS_ONREFERENCE" = "ignore")
	  
          fsample(0:3, what = what, seed = FALSE)
        }, warning = identity, error = identity)
        print(y3)
        if (misuse %in% c("warning", "error")) {
          stopifnot(
            inherits(y3, misuse),
            inherits(y3, "RngFutureCondition"),
            inherits(y3, switch(misuse,
              warning = "RngFutureWarning",
              error   = "RngFutureError"
            ))
          )
        }

        ## seed = NULL equals seed = FALSE but without the check of misuse
        y4 <- fsample(0:3, what = what, seed = NULL)
        print(y4)
      }
      
      options(future.rng.onMisuse = "ignore")
    }

    message(sprintf("%s ... done", strategy))
  }

  message(sprintf("Testing with %d cores ... DONE", cores))
} ## for (cores ...)

message("- Assert that RNG mistakes does not muffle run-time errors")

options(
  future.debug = FALSE,
  future.rng.onMisuse = "warning"
)
for (signal in c(TRUE, FALSE)) {
  message("signal=", signal)
  f <- future({ sample.int(2L); log("a") }, seed = FALSE)
  r <- result(f)
  print(r)
  res <- tryCatch(value(f, signal = signal), error = identity)
  print(res)
  stopifnot(inherits(res, "error"))
}

stopifnot(identical(RNGkind(), okind))

message("*** rng ... DONE")

source("incl/end.R")
