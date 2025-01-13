library(R6)
library(checkmate)
library(httr2)
library(logger)
library(data.table)
library(jsonlite)

source("R/auth.R")
source("R/market_data.R")
source("R/tradestation.R")

# Initialize and authenticate
ts <- TradeStation$new(secrets_dir = ".secrets")
ts$authenticate()

if (!ts$is_authenticated()) {
  stop("Authentication failed")
}

# Stream real-time quotes
cat("\nStarting real-time quote stream for AAPL...\n")
cat("♥ = heartbeat, $ = quote\n\n")

# Keep track of messages
msg_count <- 0
start_time <- Sys.time()

ts$market_data()$stream_quotes(
  symbols = "AAPL",
  callback = function(data) {
    msg_count <<- msg_count + 1
    cat("\nMessage", msg_count, "received at", format(Sys.time(), "%H:%M:%S"), "\n")
    cat("Raw data length:", nchar(data), "characters\n")
    
    # Try to parse as JSON if it looks like JSON
    if (grepl("^\\s*[{[]", data)) {
      tryCatch({
        parsed <- fromJSON(data)
        if (!is.null(parsed$Heartbeat)) {
          cat("♥")  # Visual heartbeat indicator
        } else {
          cat("\nQuote:", 
              "\nLast:", parsed$Last,
              "\nBid:", parsed$Bid,
              "\nAsk:", parsed$Ask,
              "\nVolume:", parsed$Volume,
              "\nTime:", parsed$TradeTime,
              "\n")
        }
      }, error = function(e) {
        cat("Failed to parse JSON:", e$message, "\n")
        cat("First 100 chars of data:", substr(data, 1, 100), "...\n")
      })
    } else {
      cat("Non-JSON data:", substr(data, 1, 100), "...\n")
    }
    flush.console()
  }
) 