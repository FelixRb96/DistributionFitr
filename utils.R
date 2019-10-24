# Function for stopping an expression if it takes too long
# Simplified version of https://github.com/HenrikBengtsson/R.utils/issues/74
eval_with_timeout <- function(expr, envir = parent.frame(), timeout, return_value_on_timeout=NULL) {
  # substitute expression so it is not executed as soon it is used
  expr <- substitute(expr)
  
  # for Borui
  if (.Platform$OS.type != "unix") {
    return(eval(expr, envir = envir))
  }
  
  # execute expr in separate fork
  myfork <- parallel::mcparallel({
    eval(expr, envir = envir)
  }, silent = FALSE)
  
  # wait max n seconds for a result.
  myresult <- parallel::mccollect(myfork, wait = FALSE, timeout = timeout)
  # kill fork after collect has returned
  tools::pskill(myfork$pid, tools::SIGKILL)
  tools::pskill(-1 * myfork$pid, tools::SIGKILL)
  
  # clean up:
  parallel::mccollect(myfork, wait = FALSE)
  
  # timeout?
  if (is.null(myresult)) {
    myresult <- return_value_on_timeout
  }
  
  # move this to distinguish between timeout and NULL returns
  myresult <- myresult[[1]]
  
  if ("try-error" %in% class(myresult)) {
    stop(attr(myresult, "condition"))
  }
  
  # send the buffered response
  return(myresult)
}

# t <- Sys.time()
# res <- eval_with_timeout(dsignrank(1, 2000), timeout=0.01, return_value_on_timeout = "TIMEOUT")
# print(res)
# print(Sys.time() -t)
