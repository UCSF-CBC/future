source("incl/start.R")

message("*** %<-% ...")

for (cores in 1:availCores) {
  message(sprintf("Testing with %d cores ...", cores))
  options(mc.cores = cores)

  for (strategy in supportedStrategies(cores)) {
    message(sprintf("*** %%<-%% with %s futures ...", sQuote(strategy)))
    plan(strategy)

    rm(list = intersect(c("x", "y"), ls()))

    message("** Future evaluation without globals")
    v1 %<-% { x <- 1 }
    stopifnot(!exists("x", inherits = FALSE), identical(v1, 1))

    message("** Future evaluation with globals")
    a <- 2
    v2 %<-% { x <- a }
    stopifnot(!exists("x", inherits = FALSE), identical(v2, a))

    message("** Future evaluation with errors")
    v3 %<-% {
      x <- 3
      stop("Woops!")
      x
    }
    stopifnot(!exists("x", inherits = FALSE))
    res <- tryCatch(identical(v3, 3), error = identity)
    stopifnot(inherits(res, "error"))


    y <- listenv::listenv()
    for (ii in 1:3) {
      y[[ii]] %<-% {
        if (ii %% 2 == 0) stop("Woops!")
        ii
      }
    }
    res <- tryCatch(as.list(y), error = identity)
    stopifnot(inherits(res, "error"))
    z <- y[c(1, 3)]
    z <- unlist(z)
    stopifnot(all(z == c(1, 3)))
    res <- tryCatch(y[[2]], error = identity)
    stopifnot(inherits(res, "error"))
    res <- tryCatch(y[1:2], error = identity)
    stopifnot(inherits(res, "error"))


    message("** Future evaluation with a poor-man's \"progress bar\"")
    v4 %<-% {
      cat("Processing: ")
      for (ii in 1:10) { cat(".") }
      cat(" [100%]\n")
      4
    }


    message("** Collecting results")
    printf("v1 = %s\n", v1)
    stopifnot(v1 == 1)

    printf("v2 = %s\n", v2)
    stopifnot(v2 == a)

    stopifnot(tryCatch({
      printf("v3 = %s\n", v3)
    }, error = function(ex) {
      printf("v3: <%s> (as expect)\n", class(ex)[1])
      TRUE
    }))

    printf("v4 = %s\n", v4)
    #stopifnot(v4 == 4)


    message("** Left-to-right and right-to-left future assignments")
    c %<-% 1
    printf("c = %s\n", c)
    1 %->% d
    printf("d = %s\n", d)
    stopifnot(d == c)



    message("** Nested future assignments")
    a %<-% {
      b <- 1
      c %<-% 2
      3 -> d
      4 %->% e
      b + c + d + e
    }
    printf("a = %s\n", a)
    stopifnot(a == 10)

    { a + 1 } %->% b
    printf("b = %s\n", b)
    stopifnot(b == a + 1)

    message(sprintf("*** %%<-%% with %s futures ... DONE", sQuote(strategy)))
  } # for (strategy in ...)

  message(sprintf("Testing with %d cores ... DONE", cores))
} ## for (cores ...)

message("*** %<-% ... DONE")

source("incl/end.R")
