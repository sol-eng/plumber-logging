library(plumber)

# Configure logging with the logger package
# pak::pkg_install("daroczig/logger")
library(logger)
log_appender(appender_tee(tempfile("plumber_", "logs", ".log")))

convert_empty <- function(string) {
  ifelse(string == "", "-", string)
}

pr <- plumb("plumber.R")


pr$registerHooks(
  list(
    preroute = function() {
      # Start timer for log info
      tictoc::tic()
    },
    postroute = function(data, req, res) {
      end <- tictoc::toc(quiet = TRUE)
      # Log details about the request and the response
      log_info('{convert_empty(req$REMOTE_ADDR)} "{convert_empty(req$HTTP_USER_AGENT)}" {convert_empty(req$HTTP_HOST)} {convert_empty(req$REQUEST_METHOD)} {convert_empty(req$PATH_INFO)} {convert_empty(res$status)} {end$toc - end$tic}')
    }
  )
)

pr$run()
