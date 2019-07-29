# The purpose of this API is to investigate how to log details about requests
# and responses within Plumber. Requests can be logged using a filter, but 
# response logs need to be generated in the actual endpoint, or perhaps using
# hooks

library(plumber)

#* @apiTitle Logging

#* Simulate a long running process and log
#* @param max_s Maximum number of seconds a process will sleep for
#* @get /delay
delay <- function(max_s = 1, fail = 0, req, res) {
  max_s <- as.numeric(max_s)
  s <- runif(1, max = max_s)
  Sys.sleep(s)
  # Randomly fail certain responses
  res$status <- ifelse(runif(1) <= as.numeric(fail), 404, 200)
  list(msg = paste0("Slept for ", s, " seconds"))
}

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @png
#* @get /plot
function() {
  rand <- rnorm(100)
  hist(rand)
}
