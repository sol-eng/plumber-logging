## ---- entrypoint
library(plumber)

# Config
config <- config::get()

# logging
library(logger)
# Ensure glue is a specific dependency so it's avaible for logger
library(glue)

# Specify how logs are written 
log_appender(appender_tee(tempfile("plumber_", config$log_dir, ".log")))

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
    postroute = function(req, res) {
      end <- tictoc::toc(quiet = TRUE)
      # Log details about the request and the response
      log_info('{convert_empty(req$REMOTE_ADDR)} "{convert_empty(req$HTTP_USER_AGENT)}" {convert_empty(req$HTTP_HOST)} {convert_empty(req$REQUEST_METHOD)} {convert_empty(req$PATH_INFO)} {convert_empty(res$status)} {end$toc - end$tic}')
    }
  )
)

pr
