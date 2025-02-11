source("incl/start.R")

options(future.debug = FALSE)

message("*** Relaying and split standard output and conditions ...")

message(" - default (implicit split = FALSE)")

plan(sequential)

relay <- recordRelay({
  f <- future({
    cat("O\n")
    message("M")
    1L
  })
})
message("  class: ", commaq(class(f)))
stopifnot(length(relay$stdout) == 0L)
stopifnot(length(relay$msgs) == 0L)
relay <- recordRelay(v <- value(f))
stopifnot(identical(relay$stdout, "O\n"))
stopifnot(identical(relay$msgs, "M\n"))


message(" - split = FALSE")

plan(sequential, split = FALSE)

relay <- recordRelay({
  f <- future({
    cat("O\n")
    message("M")
    1L
  })
})
message("  class: ", commaq(class(f)))
stopifnot(length(relay$stdout) == 0L)
stopifnot(length(relay$msgs) == 0L)
relay <- recordRelay(v <- value(f))
stopifnot(identical(relay$stdout, "O\n"))
stopifnot(identical(relay$msgs, "M\n"))


message(" - split = TRUE")

plan(sequential, split = TRUE)

relay <- recordRelay({
  f <- future({
    cat("O\n")
    message("M")
    1L
  })
})
message("  class: ", commaq(class(f)))
stopifnot(identical(relay$stdout, "O\n"))
stopifnot(identical(relay$msgs, "M\n"))
relay <- recordRelay(v <- value(f))
stopifnot(identical(relay$stdout, "O\n"))
stopifnot(identical(relay$msgs, "M\n"))


message("*** Relaying and split standard output and conditions ... DONE")

source("incl/end.R")
