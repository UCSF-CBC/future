source("incl/start.R")

options(future.debug = FALSE)

message("*** Relaying of standard output and conditions ...")

strategies <- supportedStrategies()

for (ss in seq_along(strategies)) {
  strategy <- strategies[[ss]]
  message(sprintf("Relaying w/ %s ...", names(strategies)[ss]))
  plan(strategy)

  message("* A single future ...")

  for (exclude in list(NULL, "message", "warning")) {
    if (is.null(exclude)) {
      msgs_truth <- c("M\n", "W")
    } else if (exclude == "message") {
      msgs_truth <- c("W")
    } else if (exclude == "warning") {
      msgs_truth <- c("M\n")
    }

    message("- creating future")
    relay <- recordRelay({
      f <- future({
        cat("O\n")
        message("M")
        warning("W")
        1L
      }, conditions = structure("condition", exclude = exclude))
    })
    message("  class: ", commaq(class(f)))
    stopifnot(length(relay$stdout) == 0L)
    stopifnot(length(relay$msgs) == 0L)
    
    message("- checking if resolved")
    relay <- recordRelay({
      r <- resolved(f)
    })
    message("  result: ", r)
    stopifnot(length(relay$stdout) == 0L)
    stopifnot(length(relay$msgs) == 0L)
  
    message("- resolve w/ collecting results")
    relay <- recordRelay({
      f <- resolve(f, result = TRUE)
    })
    stopifnot(length(relay$stdout) == 0L)
    stopifnot(length(relay$msgs) == 0L)
  
    message("- resolve w/out collecting results")
    relay <- recordRelay({
      f <- resolve(f, result = FALSE)
    })
    stopifnot(length(relay$stdout) == 0L)
    stopifnot(length(relay$msgs) == 0L)
  
    message("- getting value")
    relay <- recordRelay({
      v <- value(f)
    })
    message("  value: ", v)
    stopifnot(identical(relay$stdout, "O\n"))
    stopifnot(identical(relay$msgs, msgs_truth))
    
    message("- getting value again")
    relay <- recordRelay({
      v <- value(f)
    })
    message("  value: ", v)
    stopifnot(identical(relay$stdout, "O\n"))
    stopifnot(identical(relay$msgs, msgs_truth))
  } ## for (exclude ...)

  message("* A single future ... DONE")


  message("* Two futures ... ")

  message("- list of two futures")
  fs <- list()
  relay <- recordRelay({
    fs[[1]] <- future({ cat("O1\n"); message("M1"); 1L })
    fs[[2]] <- future({ cat("O2\n"); message("M2"); 2L })
  })
  message("  class: ", commaq(class(fs[[1]])))
  stopifnot(length(relay$stdout) == 0L)
  stopifnot(length(relay$msgs) == 0L)

  message("- check if resolved")
  relay <- recordRelay({
    rs <- resolved(fs)
  })
  message("  result: ", paste(rs, collapse = ", "))
  stopifnot(length(relay$stdout) == 0L)
  stopifnot(length(relay$msgs) == 0L)

  message("- resolve w/ collecting results")
  relay <- recordRelay({
    fs <- resolve(fs, result = FALSE)
  })
  str(relay)
  stopifnot(length(relay$stdout) == 0L)
  stopifnot(length(relay$msgs) == 0L)

  message("- resolve w/out collecting results")
  relay <- recordRelay({
    fs <- resolve(fs, result = TRUE)
  })
  str(relay)
  stopifnot(length(relay$stdout) == 0L)
  stopifnot(length(relay$msgs) == 0L)

  message("- getting value")
  relay <- recordRelay({
    vs <- value(fs)
  })
  message("  values: ", paste(vs, collapse = ", "))
  str(relay)
  stopifnot(identical(relay$stdout, c("O1\n", "O2\n")))
  stopifnot(identical(relay$msgs, c("M1\n", "M2\n")))
  
  message("- getting value again")
  relay <- recordRelay({
    vs <- value(fs)
  })
  message("  values: ", paste(vs, collapse = ", "))
  str(relay)
  stopifnot(identical(relay$stdout, c("O1\n", "O2\n")))
  stopifnot(identical(relay$msgs, c("M1\n", "M2\n")))

  message("* Two futures ... DONE")


  message("* Two futures - out of order  ... ")

  message("- list of two futures")
  fs <- list()
  relay <- recordRelay({
    fs[[1]] <- future({ cat("O1\n"); Sys.sleep(1); message("M1"); 1L })
    fs[[2]] <- future({ cat("O2\n"); message("M2"); 2L })
  })
  message("  class: ", commaq(class(fs[[1]])))
  stopifnot(length(relay$stdout) == 0L)
  stopifnot(length(relay$msgs) == 0L)

  message("- getting value")
  relay <- recordRelay({
    vs <- value(fs)
  })
  message("  values: ", paste(vs, collapse = ", "))
  str(relay)
  stopifnot(identical(relay$stdout, c("O1\n", "O2\n")))
  stopifnot(identical(relay$msgs, c("M1\n", "M2\n")))
  
  message("- getting value again")
  relay <- recordRelay({
    vs <- value(fs)
  })
  message("  values: ", paste(vs, collapse = ", "))
  str(relay)
  stopifnot(identical(relay$stdout, c("O1\n", "O2\n")))
  stopifnot(identical(relay$msgs, c("M1\n", "M2\n")))

  message("* Two futures - out of order ... DONE")

  message("- conditions = structure(\"condition\", drop = TRUE)")
  f <- future(message("42"), conditions = structure("condition", drop = TRUE))
  r <- result(f)
  stopifnot(length(r$conditions) > 0L)
  v <- value(f)
  r <- result(f)
  stopifnot(length(r$conditions) == 0L)

  message(sprintf("Relaying w/ %s ... DONE", names(strategies)[ss]))
} ## for (ss ...)

message("*** Relaying of standard output and conditions ... DONE")

source("incl/end.R")
