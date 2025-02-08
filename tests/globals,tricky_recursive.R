source("incl/start.R")

## Test adopted from http://stackoverflow.com/questions/42561088/nested-do-call-within-a-foreach-dopar-environment-cant-find-function-passed-w

options(future.debug = FALSE)

message("*** Tricky globals requiring recursive search ...")

my_add <- function(a, b) a + b

call_my_add <- function(a, b) {
  do.call(my_add, args = list(a = a, b = b))
}

call_my_add_caller <- function(a, b, FUN = call_my_add) {
  do.call(FUN, args = list(a = a, b = b))
}

main_future <- function(x = 1L, caller = call_my_add_caller,
                            args = list(FUN = call_my_add)) {
  f <- future(caller(a = x, b = x + 1L, FUN = args$FUN))
  value(f)
}

main_future_no_FUN <- function(x = 1L, caller = call_my_add_caller,
                            args = list(FUN = call_my_add)) {
  f <- future(caller(a = x, b = x + 1L))
  value(f)
}

main_futureCall <- function(x = 1L, caller = call_my_add_caller,
                            args = list(FUN = call_my_add)) {
  f <- futureCall(caller, args = c(list(a = x, b = x+1L), args))
  value(f)
}

main_futureCall_no_FUN <- function(x = 1L, caller = call_my_add_caller,
                            args = list(FUN = call_my_add)) {
  f <- futureCall(caller, args = list(a = x, b = x+1L))
  value(f)
}

main_lapply <- function(x = 1:2, caller = call_my_add_caller,
                               args = list(FUN = call_my_add)) {
  lapply(x, FUN = function(i) {
    do.call(caller, args = c(list(a = i, b = i+1L), args))
  })
}

main_lapply_no_FUN <- function(x = 1:2, caller = call_my_add_caller,
                               args = list(FUN = call_my_add)) {
  lapply(x, FUN = function(i) {
    do.call(caller, args = list(a = i, b = i+1L))
  })
}


x0 <- y0 <- z0 <- NULL
for (strategy in supportedStrategies()) {
  message(sprintf("*** strategy = %s ...", sQuote(strategy)))
  
  plan(strategy)

  x <- main_future()
  str(list(x = x))
  if (is.null(x0)) x0 <- x
  stopifnot(identical(x, x0))
  
  x2 <- main_future_no_FUN()
  str(list(x2 = x2))
  stopifnot(identical(x2, x0))
  
#  y <- main_futureCall()
#  str(list(y = y))
#  if (is.null(y0)) y0 <- y
#  stopifnot(identical(y, y0))

#  y2 <- main_futureCall_no_FUN()
#  str(list(y2 = y2))
#  stopifnot(identical(y2, y0))
  
  z <- main_lapply()
  str(list(z = z))
  if (is.null(z0)) z0 <- z
  stopifnot(identical(z, z0))

  z2 <- main_lapply_no_FUN()
  str(list(z2 = z2))
  stopifnot(identical(z2, z0))
    
  message(sprintf("*** strategy = %s ... DONE", sQuote(strategy)))
}

message("*** Tricky globals requiring recursive search ... DONE")

source("incl/end.R")
