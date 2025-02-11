source("incl/start.R")

## Avoid 'Error: C stack usage  7971940 is too close to the limit'
## on R (< 4.1.0)
if (getRversion() < "4.1") options(future.debug = FALSE)

message("*** Demos ...")

message("*** Fibonacci demo of the 'future' package ...")
# Temporarily protect against non-default R_FUTURE_PLAN
oopts <- options(future.plan = NULL)
demo("fibonacci", package = "future", ask = FALSE)
options(oopts)
message("*** Fibonacci demo of the 'future' package ... DONE")


message("*** Mandelbrot demo of the 'future' package ...")
if (getRversion() >= "3.2.0") {
  options(future.demo.mandelbrot.nrow = 2L)
  options(future.demo.mandelbrot.resolution = 50L)
  options(future.demo.mandelbrot.delay = FALSE)
  
  for (cores in 1:availCores) {
    message(sprintf("Testing with %d cores ...", cores))
    options(mc.cores = cores)
  
    for (strategy in supportedStrategies(cores)) {
      message(sprintf("- plan('%s') ...", strategy))
      plan(strategy)
      demo("mandelbrot", package = "future", ask = FALSE)
      message(sprintf("- plan('%s') ... DONE", strategy))
    }
  
    message(sprintf("Testing with %d cores ... DONE", cores))
  } ## for (cores ...)
} else {
  message(" - This demo requires R (>= 3.2.0). Skipping test. (Skipping also on Win32 i386 for speed)")
}
message("*** Mandelbrot demo of the 'future' package ... DONE")

message("*** Demos ... DONE")

source("incl/end.R")
