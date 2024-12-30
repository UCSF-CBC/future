source("incl/start.R")
library("listenv")
options(future.debug = FALSE)

## WORKAROUND: R CMD check --as-cran on Windows will report
## 
## * checking for detritus in the temp directory ... NOTE
## Found the following files/directories:
##  'Rscript171866c62e
##
## which I think is due to a bug in R. For details, see R-devel thread 
## 'R for Windows leaves detritus in the temp directory' on 2021-06-15
## <https://stat.ethz.ch/pipermail/r-devel/2021-June/080830.html>.
## Until fixed, we skip the one test that triggers this bug.
winWorkaround <- (.Platform$OS.type == "windows" && getRversion() >= "4.0.0")

message("*** Nested futures - mc.cores ...")

strategies <- NULL
## Speed up CRAN checks: Skip on CRAN Windows 32-bit
if (!isWin32) strategies <- c(strategies, "multisession")
if (supportsMulticore()) strategies <- c(strategies, "multicore")
pid <- Sys.getpid()
cat(sprintf("Main PID: %d\n", pid))

cat("Available cores on this machine:\n")
cores <- availableCores()
print(cores)

for (mc in 1:2) {
  ## Speed up CRAN checks: Skip on CRAN Windows 32-bit
  if (!fullTest && isWin32) next
  
  message(sprintf("- mc.cores = %d ...", mc))
  options(mc.cores = mc)
  mc2 <- min(mc, cores)
  
  for (strategy in strategies) {
    message(sprintf("plan(list('sequential', '%s')):", strategy))
    plan(list('sequential', strategy))
    a %<-% {
      b1 %<-% Sys.getpid()
      b2 %<-% Sys.getpid()
      list(pid = Sys.getpid(), mc.cores = getOption("mc.cores"), cores = availableCores(), plan = plan("list"), pid1 = b1, pid2 = b2)
    }
    utils::str(a)
    stopifnot(a$pid == pid)
    stopifnot((mc2 <= 1 && a$pid1 == pid) || (a$pid1 != pid))
    stopifnot((mc2 <= 1 && a$pid2 == pid) || (a$pid2 != pid))
    stopifnot(((mc2 <= 1 || a$cores <= 2) && a$pid2 == a$pid1) || (a$pid2 != a$pid1))

    if (mc == 1L) {
      message(sprintf("plan(list('sequential', '%s':2)):", strategy))
      plan(list('sequential', tweak(strategy, workers = I(2))))
      a %<-% {
        b1 %<-% Sys.getpid()
        b2 %<-% Sys.getpid()
        list(pid = Sys.getpid(), mc.cores = getOption("mc.cores"), cores = availableCores(), plan = plan("list"), pid1 = b1, pid2 = b2)
      }
      utils::str(a)
      stopifnot(a$pid == pid)
      stopifnot((mc2 <= 1 && a$pid1 == pid) || (a$pid1 != pid))
      stopifnot((mc2 <= 1 && a$pid2 == pid) || (a$pid2 != pid))
      stopifnot((mc2 <= 1 && a$pid2 == a$pid1) || (a$pid2 != a$pid1))
    }

    message(sprintf("plan(list('%s', 'sequential')):", strategy))
    plan(list(strategy, 'sequential'))
    a %<-% {
      b1 %<-% Sys.getpid()
      b2 %<-% Sys.getpid()
      list(pid = Sys.getpid(), mc.cores = getOption("mc.cores"), cores = availableCores(), plan = plan("list"), pid1 = b1, pid2 = b2)
    }
    utils::str(a)
    stopifnot((mc2 <= 1 && a$pid  == pid) || (a$pid  != pid))
    stopifnot((mc2 <= 1 && a$pid1 == pid) || (a$pid1 != pid))
    stopifnot((mc2 <= 1 && a$pid2 == pid) || (a$pid2 != pid))
    stopifnot(a$pid2 == a$pid1)

    message(sprintf("plan(list('%s', '%s')):", strategy, strategy))
    plan(list(strategy, strategy))
    a %<-% {
      b1 %<-% { Sys.sleep(0.2); Sys.getpid() }
      b2 %<-% Sys.getpid()
      list(pid = Sys.getpid(), mc.cores = getOption("mc.cores"), cores = availableCores(), plan = plan("list"), pid1 = b1, pid2 = b2)
    }
    utils::str(a)
    stopifnot((mc2 <= 1 && a$pid  == pid) || (a$pid  != pid))
    stopifnot((mc2 <= 1 && a$pid1 == pid) || (a$pid1 != pid))
    stopifnot((mc2 <= 1 && a$pid2 == pid) || (a$pid2 != pid))
    stopifnot(a$pid2 == a$pid1)

    if (mc == 1L && !winWorkaround) {
      message(sprintf("plan(list('%s':2, '%s':2)):", strategy, strategy))
      plan(list(tweak(strategy, workers = I(2)), tweak(strategy, workers = I(2))))
      a %<-% {
        b1 %<-% Sys.getpid()  ## This stalls
        b2 %<-% Sys.getpid()
        list(pid = Sys.getpid(), mc.cores = getOption("mc.cores"), cores = availableCores(), plan = plan("list"), pid1 = b1, pid2 = b2)
      }
      utils::str(a)
      stopifnot(a$pid  != pid)
      stopifnot(a$pid1 != pid)
      stopifnot(a$pid2 != pid)
      stopifnot(a$pid2 != a$pid1)
    }

    ## Assert that nested the FutureRegistry is not inherited when using
    ## nested strategies, particularly 'multicore'.
    ## https://github.com/futureverse/future/issues/231
    fs <- lapply(1:2, FUN = function(i) future( value(future(TRUE)) ))
    v <- value(fs)
  } ## for (strategy ...)

  message(sprintf(" - mc.cores = %d ... DONE", mc))
} ## for (mc ...)

message("*** Nested futures - mc.cores ... DONE")

source("incl/end.R")
