source("incl/start.R")
options(future.debug = FALSE)
message("*** Futures - undo R options and environment variables ...")

strategies <- supportedStrategies()

options(digits = 6L)
Sys.setenv(R_DEFAULT_INTERNET_TIMEOUT = "300")
old_options <- options()
old_envvars <- Sys.getenv()

for (strategy in strategies) {
  message(sprintf("- plan('%s') ...", strategy))
  plan(strategy)

  f <- future({
    options(digits = 4L)
    options(abc = 42L)
    Sys.setenv(R_DEFAULT_INTERNET_TIMEOUT = "30")
    Sys.setenv(ABC = "42")
  })
  v <- value(f)
  stopifnot(
    getOption("digits") == 6L,
    #DISABLED# is.null(getOption("abc")),
    identical(options()[names(old_options)], old_options),
    identical(Sys.getenv("R_DEFAULT_INTERNET_TIMEOUT"), "300")
    #DISABLED# is.na(Sys.getenv("ABC", NA_character_))
  )
  
  envvars <- Sys.getenv()
  if (.Platform$OS.type == "windows") {
    ## Drop empty environment variables, because they are not supported by
    ## MS Windows, but may exist because they're inherited from a host OS
    old_envvars <- old_envvars[nzchar(old_envvars)]
    envvars <- envvars[nzchar(envvars)]
  }

  envvars <- envvars[names(old_envvars)]
  
  ## Any added?
  diff <- setdiff(names(envvars), names(old_envvars))
  if (length(diff) > 0) {
    stop(sprintf("Detected added environment variables: %s", paste(diff, collapse = ", ")))
  }
  
  ## Any removed?
  diff <- setdiff(names(old_envvars), names(envvars))
  if (length(diff) > 0) {
    stop(sprintf("Detected removed environment variables: %s", paste(diff, collapse = ", ")))
  }

  ## Any changed?
  for (name in names(old_envvars)) {
    if (!identical(envvars[[name]], old_envvars[[name]])) {
      if (getRversion() < "4.1.0") {
        old_envvars[[name]] <- sub("=$", "", old_envvars[[name]])
      }
      if (!identical(envvars[[name]], old_envvars[[name]])) {
        stop(sprintf("Detected modified environment variable: %s=%s (was %s)", name, sQuote(envvars[[name]]), sQuote(old_envvars[[name]])))
      }
    }
  }

  stopifnot(all.equal(envvars, old_envvars))
  stopifnot(identical(envvars, old_envvars))

  message(sprintf("- plan('%s') ... DONE", strategy))
} ## for (strategy ...)

message("*** Futures - undo R options and environment variables ... DONE")

source("incl/end.R")
