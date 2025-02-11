## Load namespace already here; it'll set some options based on
## environment variables
loadNamespace("future")

## Record original state
ovars <- ls()
oenvs <- oenvs0 <- Sys.getenv()
oopts0 <- options()

covr_testing <- ("covr" %in% loadedNamespaces())
on_macos <- grepl("^darwin", R.version$os)
on_githubactions <- as.logical(Sys.getenv("GITHUB_ACTIONS", "FALSE"))

## Default options
oopts <- options(
  warn = 1L,
  mc.cores = 2L,
  future.debug = TRUE,
  ## Reset the following during testing in case
  ## they are set on the test system
  future.availableCores.system = NULL,
  future.availableCores.fallback = NULL
)

## Comment: The below should be set automatically whenever the future package
## is loaded and 'R CMD check' runs.  The below is added in case R is changed
## in the future and we fail to detect 'R CMD check'.
Sys.setenv(R_PARALLELLY_MAKENODEPSOCK_CONNECTTIMEOUT = 2 * 60)
Sys.setenv(R_PARALLELLY_MAKENODEPSOCK_TIMEOUT = 2 * 60)
Sys.setenv(R_PARALLELLY_MAKENODEPSOCK_SESSIONINFO_PKGS = TRUE)
Sys.setenv(R_FUTURE_WAIT_INTERVAL = 0.01) ## 0.01s (instead of default 0.2s)

## Label PSOCK cluster workers (to help troubleshooting)
test_script <- grep("[.]R$", commandArgs(), value = TRUE)[1]
if (is.na(test_script)) test_script <- "UNKNOWN"
worker_label <- sprintf("future/tests/%s:%s:%s:%s", test_script, Sys.info()[["nodename"]], Sys.info()[["user"]], Sys.getpid())
Sys.setenv(R_PARALLELLY_MAKENODEPSOCK_RSCRIPT_LABEL = worker_label)

## Reset the following during testing in case
## they are set on the test system
oenvs2 <- Sys.unsetenv(c(
  "R_PARALLELLY_AVAILABLECORES_SYSTEM",
  "R_PARALLELLY_AVAILABLECORES_FALLBACK",
  ## SGE
  "NSLOTS", "PE_HOSTFILE",
  ## Slurm
  "SLURM_CPUS_PER_TASK",
  ## TORQUE / PBS
  "NCPUS", "PBS_NUM_PPN", "PBS_NODEFILE", "PBS_NP", "PBS_NUM_NODES"
))

oplan <- future::plan()

## Use sequential futures by default
future::plan("sequential")

fullTest <- (Sys.getenv("_R_CHECK_FULL_") != "")

## Private future functions
.onLoad <- future:::.onLoad
.onAttach <- future:::.onAttach
asIEC <- future:::asIEC
ClusterRegistry <- future:::ClusterRegistry
constant <- future:::constant
FutureRegistry <- future:::FutureRegistry
gassign <- future:::gassign
get_future <- future:::get_future
geval <- future:::geval
grmall <- future:::grmall
commaq <- future:::commaq
hpaste <- future:::hpaste
importParallel <- future:::importParallel
mdebug <- future:::mdebug
mdebugf <- future:::mdebugf
parseCmdArgs <- future:::parseCmdArgs
requestCore <- future:::requestCore
requestNode <- future:::requestNode
requirePackages <- future:::requirePackages
tweakExpression <- future:::tweakExpression
whichIndex <- future:::whichIndex
isFALSE <- future:::isFALSE
isNA <- future:::isNA
supports_omp_threads <- future:::supports_omp_threads

get_random_seed <- future:::get_random_seed
set_random_seed <- future:::set_random_seed
as_lecyer_cmrg_seed <- future:::as_lecyer_cmrg_seed
is_lecyer_cmrg_seed <- future:::is_lecyer_cmrg_seed

## Local functions for test scripts
printf <- function(...) cat(sprintf(...))
mstr <- function(...) message(paste(capture.output(str(...)), collapse = "\n"))
attachLocally <- function(x, envir = parent.frame()) {
  for (name in names(x)) {
    assign(name, value = x[[name]], envir = envir)
  }
}

supportedStrategies <- function(cores = NA_integer_, excl = "cluster", ...) {
  strategies <- future:::supportedStrategies(...)
  strategies <- setdiff(strategies, excl)
  
  if (!is.na(cores)) {
    if (cores == 1L) {
      strategies <- setdiff(strategies, c("multicore", "multisession"))
    } else if (cores > 1L) {
      strategies <- setdiff(strategies, "sequential")
    }
  }
  
  strategies
}

availCores <- min(2L, future::availableCores())


## WORKAROUND: capture.output() gained argument 'split' in R 3.3.0
if (getRversion() >= "3.3.0") {
  capture.output <- utils::capture.output 
} else {
  capture.output <- function(..., split = FALSE) utils::capture.output(...)
}

recordConditions <- function(expr, ..., parse = TRUE) {
  conditions <- list()
  withCallingHandlers(expr, condition = function(c) {
    attr(c, "received") <- Sys.time()
    conditions[[length(conditions) + 1L]] <<- c
  })
  conditions
}

recordRelay <- function(...) {
  stdout <- capture.output(conditions <- recordConditions(...), split = TRUE)
  if (length(stdout) > 0) stdout <- paste0(stdout, "\n")
  msgs <- sapply(conditions, FUN = conditionMessage)
  list(stdout = stdout, msgs = msgs)
}
