source("incl/start.R")

message("*** FutureRegistry() ...")

for (where in c("multicore-dummy-uuid", "rscript")) {
  message(sprintf("*** FutureRegistry('%s', 'list') ...", where))
  futures <- FutureRegistry(where, action = "list")
  print(futures)
  stopifnot(length(futures) == 0L)


  message(sprintf("*** FutureRegistry('%s', 'add') ...", where))
  f <- future({ 1 })
  print(f)
  FutureRegistry(where, action = "add", future = f)


  message(sprintf("*** FutureRegistry('%s', 'list') ...", where))
  futures <- FutureRegistry(where, action = "list")
  print(futures)
  stopifnot(length(futures) == 1L)


  message(sprintf("*** FutureRegistry('%s', 'remove') ...", where))
  FutureRegistry(where, action = "remove", future = f)


  message(sprintf("*** FutureRegistry('%s', 'list') ...", where))
  futures <- FutureRegistry(where, action = "list")
  print(futures)
  stopifnot(length(futures) == 0L)


  message(sprintf("*** FutureRegistry('%s', 'add') ...", where))
  f <- future({ 2 })
  print(f)
  FutureRegistry(where, action = "add", future = f)


  message(sprintf("*** FutureRegistry('%s', 'list') ...", where))
  futures <- FutureRegistry(where, action = "list")
  print(futures)
  stopifnot(length(futures) == 1L)


  message(sprintf("*** FutureRegistry('%s', 'collect-first') ...", where))
  FutureRegistry(where, action = "collect-first")

  futures <- FutureRegistry(where, action = "list")
  print(futures)
  stopifnot(length(futures) < 1L)

  message(sprintf("*** FutureRegistry('%s', 'add') ...", where))
  f <- future({ 2 })
  print(f)
  FutureRegistry(where, action = "add", future = f)


  message(sprintf("*** FutureRegistry('%s', 'reset') ...", where))
  FutureRegistry(where, action = "reset")


  message(sprintf("*** FutureRegistry('%s', 'list') ...", where))
  futures <- FutureRegistry(where, action = "list")
  print(futures)
  stopifnot(length(futures) == 0L)
}


message("*** FutureRegistry() - exceptions ...")

futures <- FutureRegistry(where = "test", action = "list")
stopifnot(length(futures) == 0)

f <- future(1)
FutureRegistry(where = "test", action = "add", future = f)
futures <- FutureRegistry(where = "test", action = "list")
stopifnot(length(futures) == 1)

res <- tryCatch(FutureRegistry(where = "test", action = "add", future = f), error = identity)
stopifnot(inherits(res, "error"))
futures <- FutureRegistry(where = "test", action = "list")
stopifnot(length(futures) == 1)


FutureRegistry(where = "test", action = "remove", future = f)
futures <- FutureRegistry(where = "test", action = "list")
stopifnot(length(futures) == 0)

res <- tryCatch(FutureRegistry(where = "test", action = "remove", future = f), error = identity)
stopifnot(inherits(res, "error"))
futures <- FutureRegistry(where = "test", action = "list")
stopifnot(length(futures) == 0)

FutureRegistry(where = "test", action = "reset")
futures <- FutureRegistry(where = "test", action = "list")
stopifnot(length(futures) == 0)

res <- tryCatch(FutureRegistry(where = "test", action = "<unknown>"), error = identity)
stopifnot(inherits(res, "error"))

message("*** FutureRegistry() - exceptions ... DONE")


message("*** FutureRegistry() ... DONE")

source("incl/end.R")
