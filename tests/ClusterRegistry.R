source("incl/start.R")

message("*** ClusterRegistry() ... ")

allCores <- 1:availCores

for (cores in allCores) {
  message(sprintf("Testing with %d cores ...", cores))
  options(mc.cores = cores)

  message("Available cores: ", availableCores())

  ## In case sessions have been created in previous tests
  message("Stopping any running cluster ...")
  ClusterRegistry("stop")
  cluster <- ClusterRegistry("get")
  stopifnot(length(cluster) == 0)
  message("Stopping any running cluster ... DONE")

  message("Starting cluster ...")
  res <- tryCatch({
    cluster <- ClusterRegistry("set", workers = availableCores() - 1L)
    str(cluster)
    print(cluster)
  }, error = identity)
  if (cores == 1) stopifnot(inherits(res, "error"))
  message("Starting cluster ... DONE")

  message("Starting dual-worker cluster ...")
  cluster <- ClusterRegistry(action = "start", workers = 2L)
  str(cluster)
  print(cluster)
  stopifnot(length(cluster) == 2L)
  message("Starting duel-worker cluster ... DONE")
  
  message("Starting single-worker cluster ...")
  cluster <- ClusterRegistry(action = "start", workers = 1L)
  str(cluster)
  print(cluster)
  stopifnot(length(cluster) == 1L)
  message("Starting single-worker cluster ... DONE")

  cluster <- ClusterRegistry(action = "get")
  print(cluster)
  stopifnot(length(cluster) == 1L)

  message("Stopping single-worker cluster ...")
  cluster <- ClusterRegistry(action = "stop")
  print(cluster)
  stopifnot(length(cluster) == 0L)
  message("Stopping single-worker cluster ... DONE")

  message(sprintf("Testing with %d cores ... DONE", cores))
} ## for (cores ...)


message("*** ClusterRegistry() - exceptions ...")

res <- tryCatch(ClusterRegistry(action = "start", workers = TRUE), error = identity)
stopifnot(inherits(res, "error"))

message("*** ClusterRegistry() - exceptions ... DONE")


message("*** ClusterRegistry() ... DONE")

source("incl/end.R")
